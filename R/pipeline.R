# Minimal target-pipeline orchestration.

read_pipeline_config <- function(path = file.path("config", "pipeline.yml")) {
  if (!file.exists(path)) {
    stop("Pipeline config file does not exist: ", path, call. = FALSE)
  }
  read_workflow_config(path)
}

pipeline_config_values <- function(value, default = NULL) {
  if (is.null(value)) {
    return(default)
  }
  value <- as.character(unlist(value, use.names = FALSE))
  value <- trimws(value)
  value[nzchar(value)]
}

pipeline_config_integer <- function(value, default = NULL, nullable = FALSE) {
  value <- pipeline_config_values(value, default = if (is.null(default)) NULL else as.character(default))
  if (!length(value) || (nullable && tolower(value[[1L]]) %in% c("all", "none", "null", "na"))) {
    return(NULL)
  }
  out <- as.integer(value[[1L]])
  if (is.na(out)) {
    stop("Pipeline config value must be an integer.", call. = FALSE)
  }
  out
}

normalize_pipeline_config <- function(config) {
  engines <- pipeline_config_values(config$m4_osp_engines, default = "xgboost")
  tasks <- pipeline_config_values(config$m4_osp_tasks, default = "classification")
  list(
    m4_frequency = validate_m4_frequency(pipeline_config_values(config$m4_frequency, "Yearly")[[1L]]),
    m4_base_model = match.arg(pipeline_config_values(config$m4_base_model, "ets")[[1L]], c("ets", "thetaf")),
    m4_m = pipeline_config_integer(config$m4_m, default = 5L),
    m4_n = pipeline_config_integer(config$m4_n, default = 4L),
    m4_max_series = pipeline_config_integer(config$m4_max_series, default = 20L, nullable = TRUE),
    m4_osp_engines = vapply(engines, match.arg, character(1), choices = osp_all_engines()),
    m4_osp_tasks = vapply(tasks, match.arg, character(1), choices = osp_tasks()),
    m4_results_root = pipeline_config_values(config$m4_results_root, "results")[[1L]]
  )
}

m4_pipeline_spec <- function(pipeline_config = read_pipeline_config(),
                             m4_config = read_m4_config()) {
  pipeline_config <- normalize_pipeline_config(pipeline_config)
  runs <- expand_m4_config(
    m4_config,
    frequency = pipeline_config$m4_frequency,
    base_model = pipeline_config$m4_base_model,
    m = pipeline_config$m4_m,
    n = pipeline_config$m4_n
  )
  if (nrow(runs) != 1L) {
    stop("Minimal M4 pipeline config must resolve to exactly one configured run.", call. = FALSE)
  }
  list(
    run = runs[1L, , drop = FALSE],
    max_series = pipeline_config$m4_max_series,
    model_engines = unname(pipeline_config$m4_osp_engines),
    model_tasks = unname(pipeline_config$m4_osp_tasks),
    results_root = pipeline_config$m4_results_root
  )
}

run_m4_pipeline_spec <- function(spec) {
  run <- spec$run[1L, , drop = FALSE]
  run_m4_candidate_workflow(
    frequency = run$frequency,
    base_model = run$base_model,
    m = run$m,
    n = run$n,
    train_fraction = run$train_fraction,
    seed = run$seed,
    max_series = spec$max_series,
    model_engines = spec$model_engines,
    model_tasks = spec$model_tasks
  )
}

write_m4_pipeline_tables <- function(result, spec) {
  paths <- write_m4_workflow_tables(result, root = spec$results_root)
  stats::setNames(paths$path, paths$artifact)
}

write_m4_pipeline_manifest <- function(spec, table_paths) {
  run <- spec$run[1L, , drop = FALSE]
  artifact <- names(table_paths)
  if (is.null(artifact) || length(artifact) != length(table_paths)) {
    prefix <- m4_result_prefix(run$frequency, run$base_model, run$m, run$n)
    artifact <- sub(paste0("^", prefix, "_"), "", tools::file_path_sans_ext(basename(table_paths)))
  }
  paths <- data.frame(
    artifact = artifact,
    path = unname(table_paths),
    stringsAsFactors = FALSE
  )
  manifest <- cbind(
    frequency = run$frequency,
    base_model = run$base_model,
    m = run$m,
    n = run$n,
    max_series = if (is.null(spec$max_series)) NA_integer_ else spec$max_series,
    osp_engines = paste(spec$model_engines, collapse = ","),
    osp_tasks = paste(spec$model_tasks, collapse = ","),
    paths
  )
  path <- result_table_path("m4_pipeline_manifest", root = spec$results_root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(manifest, path, row.names = FALSE)
  path
}

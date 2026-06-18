args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=")
  value <- args[grepl(pattern, args)]
  if (length(value)) {
    return(sub(pattern, "", value[[length(value)]]))
  }
  default
}
flag_present <- function(name) {
  paste0("--", name) %in% args
}

mode <- arg_value("mode", "full")

if (identical(mode, "smoke")) {
  source(file.path("R", "pipeline_smoke.R"))
  paths <- write_smoke_m4_tables()
  message("Smoke M4 workflow completed: ", paste(paths, collapse = ", "))
  quit(save = "no", status = 0L)
}

source(file.path("R", "io.R"))
source(file.path("R", "config.R"))
source(file.path("R", "paths.R"))
source(file.path("R", "intervals.R"))
source(file.path("R", "labels.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "forecast_candidates.R"))
source(file.path("R", "features.R"))
source(file.path("R", "osp_models.R"))
source(file.path("R", "m4_workflow.R"))

if (identical(mode, "targets")) {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("The targets package is required for --mode=targets. Use --mode=smoke for the minimal local workflow.", call. = FALSE)
  }
  targets::tar_make()
  quit(save = "no", status = 0L)
}

if (!identical(mode, "full")) {
  stop("Unknown mode: ", mode, ". Use --mode=smoke, --mode=targets, or --mode=full.", call. = FALSE)
}

config <- read_m4_config(arg_value("config", file.path("config", "m4.yml")))
frequency <- arg_value("frequency")
base_model <- arg_value("base-model")
m <- arg_value("m")
n <- arg_value("n")
max_series <- arg_value("max-series")
root <- arg_value("results-root", "results")
osp_engine <- arg_value("osp-engine")
osp_task <- arg_value("osp-task")
m4_data_path <- arg_value("m4-data")

runs <- expand_m4_config(
  config,
  frequency = frequency,
  base_model = base_model,
  m = if (is.null(m)) NULL else as.integer(m),
  n = if (is.null(n)) NULL else as.integer(n)
)
if (!nrow(runs)) {
  stop("No M4 runs match the supplied config and filters.", call. = FALSE)
}

if (flag_present("dry-run")) {
  print(runs)
  message("Dry run only; no M4 data source or forecast artifacts were required.")
  quit(save = "no", status = 0L)
}

m4_data <- load_m4_data(path = m4_data_path)
manifest <- vector("list", nrow(runs))
for (run_id in seq_len(nrow(runs))) {
  run <- runs[run_id, ]
  message(
    "Running M4 candidate workflow: ",
    run$frequency, ", ", run$base_model, ", m=", run$m, ", n=", run$n
  )
  result <- run_m4_candidate_workflow(
    frequency = run$frequency,
    base_model = run$base_model,
    m = run$m,
    n = run$n,
    train_fraction = run$train_fraction,
    seed = run$seed,
    max_series = if (is.null(max_series)) NULL else as.integer(max_series),
    m4_data = m4_data,
    model_engines = if (is.null(osp_engine)) NULL else strsplit(osp_engine, ",", fixed = TRUE)[[1L]],
    model_tasks = if (is.null(osp_task)) c("classification", "regression") else strsplit(osp_task, ",", fixed = TRUE)[[1L]]
  )
  paths <- write_m4_workflow_tables(result, root = root)
  manifest[[run_id]] <- cbind(
    run_id = run_id,
    frequency = run$frequency,
    base_model = run$base_model,
    m = run$m,
    n = run$n,
    paths
  )
}
manifest <- do.call(rbind, manifest)
manifest_path <- result_table_path("m4_run_manifest", root = root)
dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
write.csv(manifest, manifest_path, row.names = FALSE)
message("M4 configured workflow completed. Manifest: ", manifest_path)

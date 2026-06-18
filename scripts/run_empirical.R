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

source(file.path("R", "io.R"))
source(file.path("R", "config.R"))
source(file.path("R", "paths.R"))
source(file.path("R", "labels.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "m4_workflow.R"))
source(file.path("R", "empirical_workflow.R"))

config <- read_empirical_config(arg_value("config", file.path("config", "empirical.yml")))
grid <- expand_empirical_config(
  config,
  dataset = arg_value("dataset"),
  method = arg_value("method"),
  m = {
    value <- arg_value("m")
    if (is.null(value)) NULL else as.integer(value)
  },
  n = {
    value <- arg_value("n")
    if (is.null(value)) NULL else as.integer(value)
  }
)

if (!nrow(grid)) {
  stop("No empirical runs match the supplied config and filters.", call. = FALSE)
}

if (flag_present("dry-run")) {
  print(grid)
  message("Dry run only; no empirical artifacts were generated.")
  quit(save = "no", status = 0L)
}

manifest_path <- write_empirical_manifest(grid, root = arg_value("results-root", "results"))
input_status <- validate_empirical_inputs(grid, strict = flag_present("strict"))

message("Empirical manifest written: ", manifest_path)
if (!input_status$complete) {
  if (length(input_status$missing_raw)) {
    message("Missing raw files: ", paste(input_status$missing_raw, collapse = ", "))
  }
  if (length(input_status$missing_scripts)) {
    message("Missing legacy scripts: ", paste(input_status$missing_scripts, collapse = ", "))
  }
  message("The empirical computation stage is not fully migrated yet; this command validates config and input availability.")
}

if (flag_present("baseline")) {
  if (!input_status$complete) {
    stop("Cannot run empirical baselines until configured raw files and legacy scripts are available.", call. = FALSE)
  }
  dataset_names <- unique(grid$dataset)
  models_arg <- arg_value("models", "ets,thetaf,arima")
  models <- strsplit(models_arg, ",", fixed = TRUE)[[1L]]
  max_series <- arg_value("max-series")
  dataset_config <- normalize_empirical_config(config)$datasets
  rows <- lapply(dataset_names, function(name) {
    dataset <- dataset_config[[match(name, vapply(dataset_config, `[[`, character(1), "name"))]]
    evaluate_empirical_baselines(
      dataset,
      models = models,
      max_series = if (is.null(max_series)) NULL else as.integer(max_series)
    )
  })
  baseline <- do.call(rbind, rows)
  baseline_path <- write_empirical_baseline_table(baseline, root = arg_value("results-root", "results"))
  message("Empirical baseline evaluation written: ", baseline_path)
}

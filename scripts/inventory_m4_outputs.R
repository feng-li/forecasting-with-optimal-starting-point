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

source(file.path("R", "config.R"))
source(file.path("R", "paths.R"))
source(file.path("R", "m4_workflow.R"))

config <- read_m4_config(arg_value("config", file.path("config", "m4.yml")))
runs <- expand_m4_config(
  config,
  frequency = arg_value("frequency"),
  base_model = arg_value("base-model"),
  m = {
    value <- arg_value("m")
    if (is.null(value)) NULL else as.integer(value)
  },
  n = {
    value <- arg_value("n")
    if (is.null(value)) NULL else as.integer(value)
  }
)
if (!nrow(runs)) {
  stop("No M4 runs match the supplied config and filters.", call. = FALSE)
}

root <- arg_value("results-root", "results")
run_grid_path <- write_m4_configured_run_grid(runs, root = root)
manifest <- m4_expected_artifact_manifest(runs, root = root, include_model = !flag_present("candidate-only"))
manifest_path <- write_m4_expected_artifact_manifest(manifest, root = root)

print(runs, row.names = FALSE)
print(stats::aggregate(exists ~ stage, manifest, function(x) paste0(sum(x), "/", length(x))), row.names = FALSE)
message("M4 configured run grid written: ", run_grid_path)
message("M4 expected artifact manifest written: ", manifest_path)

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
source(file.path("R", "m4_workflow.R"))
source(file.path("R", "empirical_workflow.R"))

config <- read_empirical_config(arg_value("config", file.path("config", "empirical.yml")))
manifest <- prepare_empirical_raw_data(
  config,
  overwrite = flag_present("overwrite"),
  dry_run = flag_present("dry-run")
)
print(manifest)

manifest_path <- write_empirical_data_manifest(manifest, root = arg_value("results-root", "results"))
message("Empirical data manifest written: ", manifest_path)

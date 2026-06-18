args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=")
  value <- args[grepl(pattern, args)]
  if (length(value)) {
    return(sub(pattern, "", value[[length(value)]]))
  }
  default
}

source(file.path("R", "paths.R"))
source(file.path("R", "artifacts.R"))

root <- arg_value("results-root", "results")
inventory <- legacy_artifact_inventory()
summary <- legacy_artifact_summary(inventory)
inventory_path <- write_legacy_artifact_inventory(inventory, root = root)
summary_path <- write_legacy_artifact_summary(summary, root = root)

print(summary, row.names = FALSE)
message("Legacy artifact inventory written: ", inventory_path)
message("Legacy artifact summary written: ", summary_path)

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
source(file.path("R", "dependencies.R"))

manifest <- read_dependency_manifest(arg_value("manifest", dependency_manifest_path()))
status <- dependency_status(manifest)

print(status[c("package", "role", "installed", "version")], row.names = FALSE)

path <- write_dependency_status(status, root = arg_value("results-root", "results"))
message("Dependency status written: ", path)

missing_core <- missing_dependencies(status, roles = "core")
missing_optional <- missing_dependencies(status, roles = setdiff(unique(status$role), "core"))

if (nrow(missing_core)) {
  stop(
    "Missing packages required for the smoke workflow: ",
    paste(missing_core$package, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(missing_optional)) {
  message("Packages missing for non-smoke workflows: ", format_missing_dependencies(missing_optional))
}

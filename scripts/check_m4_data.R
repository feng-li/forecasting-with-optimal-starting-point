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
source(file.path("R", "paths.R"))
source(file.path("R", "m4_workflow.R"))

status <- m4_data_status(path = arg_value("m4-data"))
print(status, row.names = FALSE)

path <- write_m4_data_status(status, root = arg_value("results-root", "results"))
message("M4 data status written: ", path)

if (flag_present("strict") && !isTRUE(status$available)) {
  quit(save = "no", status = 1L)
}

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
source(file.path("R", "paper.R"))

root <- arg_value("results-root", "results")
inventory <- paper_table_inventory(arg_value("paper", file.path("docs", "main.tex")))
inventory_path <- write_paper_table_inventory(inventory, root = root)
status <- paper_table_status(inventory, root = root)
status_path <- write_paper_table_status(status, root = root)
available <- build_available_paper_tables(root = root)

print(status[c("label", "category", "expected_artifact", "status")], row.names = FALSE)
if (nrow(available)) {
  print(available, row.names = FALSE)
}
message("Paper table inventory written: ", inventory_path)
message("Paper table status written: ", status_path)

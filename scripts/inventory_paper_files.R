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
paper_dir <- arg_value("paper-dir", "docs")
inventory <- paper_file_inventory(paper_dir = paper_dir)
summary <- paper_file_summary(inventory)
inventory_path <- write_paper_file_inventory(inventory, root = root)
summary_path <- write_paper_file_summary(summary, root = root)

print(summary, row.names = FALSE)
message("Paper file inventory written: ", inventory_path)
message("Paper file summary written: ", summary_path)

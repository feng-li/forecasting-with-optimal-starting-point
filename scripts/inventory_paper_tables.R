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

inventory <- paper_table_inventory(arg_value("paper", file.path("docs", "main.tex")))
print(inventory)
path <- write_paper_table_inventory(inventory, root = arg_value("results-root", "results"))
message("Paper table inventory written: ", path)

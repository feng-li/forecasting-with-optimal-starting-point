args <- commandArgs(trailingOnly = TRUE)
flag_present <- function(name) {
  paste0("--", name) %in% args
}

paper_dir <- "docs"
paper <- file.path(paper_dir, "main.tex")
source(file.path("R", "paper.R"))

if (flag_present("check")) {
  print(paper_status(paper_dir = paper_dir, paper = paper))
  quit(save = "no", status = 0L)
}

if (!file.exists(paper)) {
  stop("Paper source not found: ", paper, call. = FALSE)
}

if (!nzchar(Sys.which("latexmk"))) {
  stop("latexmk is required to render the paper. Use --check to inspect paper-source status without building.", call. = FALSE)
}

oldwd <- getwd()
on.exit(setwd(oldwd), add = TRUE)
setwd(paper_dir)
status <- system2("latexmk", c("-pdf", "-interaction=nonstopmode", "main.tex"))
if (!identical(status, 0L)) {
  stop("Paper rendering failed.", call. = FALSE)
}

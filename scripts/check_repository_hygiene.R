args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args

latex_generated_patterns <- c("[.]aux$", "[.]bbl$", "[.]blg$", "[.]fdb_latexmk$", "[.]fls$", "[.]log$", "[.]out$", "[.]synctex[.]gz$", "[.]xdv$")

existing_files <- function(path, pattern = NULL) {
  if (!dir.exists(path)) {
    return(character())
  }
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  if (is.null(pattern)) {
    return(files)
  }
  files[grepl(pattern, files)]
}

notebook_checkpoint_dirs <- function(paths = c("notebooks", "Empirical analysis", "OSP-TSP on M4")) {
  paths <- paths[dir.exists(paths)]
  if (!length(paths)) {
    return(character())
  }
  dirs <- unlist(lapply(paths, list.dirs, recursive = TRUE, full.names = TRUE), use.names = FALSE)
  dirs[basename(dirs) == ".ipynb_checkpoints"]
}

checks <- data.frame(
  check = c(
    "nested_docs_git",
    "local_raw_data_csvs",
    "docs_generated_latex_files",
    "notebook_checkpoint_dirs",
    "generated_result_csvs",
    "renv_library"
  ),
  count = c(
    as.integer(dir.exists(file.path("docs", ".git"))),
    length(existing_files(file.path("data", "raw"), "[.]csv$")),
    length(existing_files("docs", paste(latex_generated_patterns, collapse = "|"))),
    length(notebook_checkpoint_dirs()),
    length(existing_files(file.path("results", "tables"), "[.]csv$")),
    as.integer(dir.exists(file.path("renv", "library")))
  ),
  action = c(
    "decide submodule versus top-level tracked paper source",
    "ignored by top-level .gitignore; prepare from legacy inputs when needed",
    "ignored by top-level .gitignore; delete when no longer needed locally",
    "ignored by top-level .gitignore; do not commit",
    "ignored by top-level .gitignore; regenerate from scripts",
    "ignored by top-level .gitignore; restore with renv when needed"
  ),
  row.names = NULL
)

print(checks)

if (strict && any(checks$count > 0L)) {
  stop("Repository hygiene check found local generated or nested artifacts.", call. = FALSE)
}

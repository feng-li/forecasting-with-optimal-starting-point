# Canonical paths for cleaned workflow artifacts.

project_root <- function() {
  getOption("osp.project_root", ".")
}

artifact_path <- function(name, type = c("tables", "models", "intermediate"), root = "results", ext = NULL) {
  type <- match.arg(type)
  if (length(name) != 1L || !nzchar(name)) {
    stop("name must be a single non-empty string.", call. = FALSE)
  }
  if (grepl("^[/~]|(^|/)[.][.]($|/)", name)) {
    stop("name must be a relative artifact name inside the selected results directory.", call. = FALSE)
  }
  if (!is.null(ext)) {
    ext <- sub("^[.]", "", ext)
    if (!grepl(paste0("[.]", ext, "$"), name)) {
      name <- paste0(name, ".", ext)
    }
  }
  file.path(root, type, name)
}

result_table_path <- function(name, root = "results") {
  artifact_path(name = name, type = "tables", root = root, ext = "csv")
}

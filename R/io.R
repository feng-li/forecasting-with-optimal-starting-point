# Artifact IO helpers.

load_single_object <- function(path, expected_name = NULL) {
  if (!file.exists(path)) {
    stop("Artifact does not exist: ", path, call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  object_names <- load(path, envir = env)
  if (!is.null(expected_name)) {
    if (!expected_name %in% object_names) {
      stop("Artifact does not contain expected object: ", expected_name, call. = FALSE)
    }
    return(env[[expected_name]])
  }
  if (length(object_names) != 1L) {
    stop("Artifact must contain exactly one object or expected_name must be supplied.", call. = FALSE)
  }
  env[[object_names]]
}

require_packages <- function(packages, strict = TRUE) {
  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  missing <- names(installed)[!installed]
  if (length(missing) && strict) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(installed)
}

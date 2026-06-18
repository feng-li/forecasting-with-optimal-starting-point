if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The testthat package is required to run tests.", call. = FALSE)
}

Sys.setenv(PROJECT_ROOT = normalizePath(getwd(), mustWork = TRUE))

testthat::test_dir("tests/testthat", reporter = "summary")

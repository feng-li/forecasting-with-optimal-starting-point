# Helpers for converting candidate forecast errors into OSP interval labels.

finite_values <- function(x) {
  x[is.finite(x)]
}

finite_mean <- function(x) {
  values <- finite_values(x)
  if (!length(values)) {
    return(NA_real_)
  }
  mean(values)
}

positive_min <- function(x) {
  values <- finite_values(x)
  values <- values[values > 0]
  if (!length(values)) {
    return(NA_real_)
  }
  min(values)
}

label_best_interval <- function(metric_matrix, method = c("min", "mean")) {
  method <- match.arg(method)
  metric_matrix <- as.matrix(metric_matrix)
  if (!nrow(metric_matrix) || !ncol(metric_matrix)) {
    stop("metric_matrix must have at least one row and one column.", call. = FALSE)
  }

  scores <- apply(
    metric_matrix,
    1L,
    switch(
      method,
      min = positive_min,
      mean = function(x) {
        values <- finite_values(unique(x))
        values <- values[values > 0]
        if (!length(values)) NA_real_ else mean(values)
      }
    )
  )

  if (all(is.na(scores))) {
    return(0L)
  }
  scores[is.na(scores)] <- Inf
  as.integer(which.min(scores) - 1L)
}

label_best_intervals <- function(metric_array, method = c("min", "mean")) {
  method <- match.arg(method)
  dims <- dim(metric_array)
  if (length(dims) != 3L) {
    stop("metric_array must have dimensions series x interval x point.", call. = FALSE)
  }
  labels <- integer(dims[[1L]])
  for (i in seq_len(dims[[1L]])) {
    labels[[i]] <- label_best_interval(metric_array[i, , ], method = method)
  }
  labels
}

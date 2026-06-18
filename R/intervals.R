# Helpers for constructing OSP candidate intervals.

validate_interval_args <- function(series_length, m, n = 1L) {
  if (length(series_length) != 1L || is.na(series_length) || series_length < 2L) {
    stop("series_length must be a single integer >= 2.", call. = FALSE)
  }
  if (length(m) != 1L || is.na(m) || m < 1L) {
    stop("m must be a single positive integer.", call. = FALSE)
  }
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("n must be a single positive integer.", call. = FALSE)
  }
  if (series_length < m) {
    stop("series_length must be at least m.", call. = FALSE)
  }
  invisible(TRUE)
}

legacy_interval_width <- function(series_length, m) {
  validate_interval_args(series_length, m)
  loc_m <- as.integer(seq_len(series_length) * m / series_length)
  width <- sum(loc_m == 0L)
  if (width < 1L) {
    stop("interval width is zero; choose a smaller m or longer series.", call. = FALSE)
  }
  width
}

make_start_indices <- function(series_length, m, n) {
  validate_interval_args(series_length, m, n)
  width <- legacy_interval_width(series_length, m)
  step <- width / n
  starts <- outer(
    seq.int(0L, m - 1L),
    seq.int(0L, n - 1L),
    function(interval, point) round(interval * width + point * step) + 1L
  )
  starts <- pmin(pmax(starts, 1L), series_length)
  storage.mode(starts) <- "integer"
  starts
}

make_interval_start_indices <- function(series_length, m) {
  make_start_indices(series_length, m, 1L)[, 1L]
}

as_one_based_interval <- function(interval, m, zero_based = TRUE, clamp = FALSE) {
  if (length(m) != 1L || is.na(m) || m < 1L) {
    stop("m must be a single positive integer.", call. = FALSE)
  }
  values <- suppressWarnings(as.integer(round(as.numeric(interval))))
  if (!length(values) || anyNA(values)) {
    stop("interval must contain numeric interval indices.", call. = FALSE)
  }
  if (isTRUE(zero_based)) {
    values <- values + 1L
  }
  if (isTRUE(clamp)) {
    values <- pmin(pmax(values, 1L), as.integer(m))
  }
  if (any(values < 1L | values > m)) {
    stop("interval indices are outside [1, m].", call. = FALSE)
  }
  as.integer(values)
}

as_zero_based_interval <- function(interval, m, zero_based = TRUE, clamp = FALSE) {
  as_one_based_interval(interval, m = m, zero_based = zero_based, clamp = clamp) - 1L
}

trim_series_from_start <- function(series, start_index) {
  if (length(start_index) != 1L || is.na(start_index)) {
    stop("start_index must be a single integer.", call. = FALSE)
  }
  start_index <- as.integer(start_index)
  if (start_index < 1L || start_index > length(series)) {
    stop("start_index is outside the series.", call. = FALSE)
  }
  series[start_index:length(series)]
}

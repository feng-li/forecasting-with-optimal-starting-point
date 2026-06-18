# Candidate forecast generation for refactored workflows.

forecast_candidate_grid <- function(series, h, base_model = c("ets", "thetaf"), m, n) {
  require_packages("forecast")
  base_model <- match.arg(base_model)
  starts <- make_start_indices(length(series), m = m, n = n)
  predictions <- array(NA_real_, dim = c(m, n, h))

  for (interval in seq_len(m)) {
    for (point in seq_len(n)) {
      start_index <- starts[interval, point]
      trimmed <- stats::ts(
        trim_series_from_start(series, start_index),
        frequency = stats::frequency(series)
      )
      prediction <- switch(
        base_model,
        ets = forecast::forecast(forecast::ets(trimmed), h = h)$mean,
        thetaf = forecast::thetaf(trimmed, h = h)$mean
      )
      predictions[interval, point, ] <- as.numeric(prediction)
    }
  }

  list(starts = starts, predictions = predictions)
}

interval_prediction_matrix <- function(prediction_array, interval, zero_based = TRUE, clamp = FALSE) {
  dims <- dim(prediction_array)
  if (length(dims) != 3L) {
    stop("prediction_array must have dimensions interval x point x horizon.", call. = FALSE)
  }
  interval <- as_one_based_interval(interval, m = dims[[1L]], zero_based = zero_based, clamp = clamp)
  if (length(interval) != 1L) {
    stop("interval must select exactly one interval.", call. = FALSE)
  }
  matrix(prediction_array[interval, , ], nrow = dims[[2L]], ncol = dims[[3L]])
}

average_interval_predictions <- function(prediction_array, interval, zero_based = TRUE,
                                         clamp = FALSE, unique_rows = TRUE) {
  predictions <- interval_prediction_matrix(
    prediction_array = prediction_array,
    interval = interval,
    zero_based = zero_based,
    clamp = clamp
  )
  if (isTRUE(unique_rows)) {
    predictions <- unique(predictions)
  }
  apply(predictions, 2L, function(values) {
    values <- values[is.finite(values)]
    if (!length(values)) NA_real_ else mean(values)
  })
}

forecast_from_interval <- function(series, h, interval, base_model = c("ets", "thetaf"),
                                   m, n, zero_based = TRUE, clamp = FALSE) {
  grid <- forecast_candidate_grid(series = series, h = h, base_model = base_model, m = m, n = n)
  average_interval_predictions(
    prediction_array = grid$predictions,
    interval = interval,
    zero_based = zero_based,
    clamp = clamp
  )
}

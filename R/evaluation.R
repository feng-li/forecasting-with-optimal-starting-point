# Small evaluation helpers used by smoke checks and refactored workflows.

summarize_accuracy <- function(metric_matrix) {
  metric_matrix <- as.matrix(metric_matrix)
  apply(metric_matrix, 2L, finite_mean)
}

mape <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length.", call. = FALSE)
  }
  finite_mean(abs((predicted - actual) / actual)) * 100
}

mase <- function(train, actual, predicted) {
  train <- as.numeric(train)
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length.", call. = FALSE)
  }
  scale <- mean(abs(diff(train)), na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) {
    return(NA_real_)
  }
  mean(abs(actual - predicted), na.rm = TRUE) / scale
}

smape <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length.", call. = FALSE)
  }
  denominator <- abs(actual) + abs(predicted)
  values <- ifelse(denominator == 0, NA_real_, 200 * abs(predicted - actual) / denominator)
  finite_mean(values)
}

accuracy_row <- function(train, actual, predicted) {
  train <- as.numeric(train)
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length.", call. = FALSE)
  }
  errors <- predicted - actual
  data.frame(
    ME = finite_mean(errors),
    RMSE = sqrt(finite_mean(errors^2)),
    MAE = finite_mean(abs(errors)),
    MPE = finite_mean(100 * errors / actual),
    MAPE = mape(actual, predicted),
    sMAPE = smape(actual, predicted),
    MASE = mase(train, actual, predicted)
  )
}

summarize_evaluation <- function(evaluation_table, metric_cols = NULL) {
  if (is.null(metric_cols)) {
    metric_cols <- intersect(c("ME", "RMSE", "MAE", "MPE", "MAPE", "sMAPE", "MASE"), names(evaluation_table))
  }
  data.frame(
    metric = metric_cols,
    mean = vapply(evaluation_table[metric_cols], finite_mean, numeric(1)),
    row.names = NULL
  )
}

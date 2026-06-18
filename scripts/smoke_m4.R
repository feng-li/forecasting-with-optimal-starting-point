source(file.path("R", "pipeline_smoke.R"))

result <- run_smoke_pipeline(base_model = "ets", m = 5L, n = 4L)

if (!identical(dim(result$starts), c(5L, 4L))) {
  stop("Unexpected start-index dimensions.", call. = FALSE)
}
if (!identical(dim(result$predictions), c(5L, 4L, length(make_smoke_future())))) {
  stop("Unexpected forecast dimensions.", call. = FALSE)
}

if (!is.integer(result$label) || result$label < 0L || result$label >= 5L) {
  stop("Invalid OSP interval label.", call. = FALSE)
}

candidate_result <- run_smoke_m4_candidate_workflow(base_model = "ets", m = 5L, n = 4L)
n_series <- length(make_smoke_m4_records())

if (!identical(dim(candidate_result$errors), c(as.integer(n_series), 5L, 4L))) {
  stop("Unexpected M4-shaped candidate error dimensions.", call. = FALSE)
}
if (nrow(candidate_result$labels) != n_series) {
  stop("Unexpected M4-shaped label table.", call. = FALSE)
}
if (!identical(dim(candidate_result$interval_forecasts_mean), c(as.integer(n_series), length(make_smoke_future())))) {
  stop("Unexpected selected-interval forecast dimensions.", call. = FALSE)
}
if (nrow(candidate_result$evaluation_mean) != n_series) {
  stop("Unexpected selected-interval evaluation table.", call. = FALSE)
}
required_metrics <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "sMAPE", "MASE")
if (!all(required_metrics %in% names(candidate_result$evaluation_mean))) {
  stop("Selected-interval evaluation is missing expected metric columns.", call. = FALSE)
}
if (any(!is.finite(candidate_result$evaluation_mean$MASE))) {
  stop("Selected-interval evaluation produced non-finite MASE values.", call. = FALSE)
}
if (is.null(candidate_result$model_evaluation) || !nrow(candidate_result$model_evaluation)) {
  stop("Missing OSP model evaluation table.", call. = FALSE)
}
if (!all(c("model_name", "source_series", "actual_label", "predicted_label", "MASE") %in% names(candidate_result$model_evaluation))) {
  stop("OSP model evaluation is missing expected columns.", call. = FALSE)
}

message("Smoke workflow completed. Best interval label: ", result$label)

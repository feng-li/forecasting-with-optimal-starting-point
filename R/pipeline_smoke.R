# A tiny end-to-end pipeline that exercises the cleaned OSP helpers.

source(file.path("R", "io.R"))
source(file.path("R", "config.R"))
source(file.path("R", "paths.R"))
source(file.path("R", "dependencies.R"))
source(file.path("R", "artifacts.R"))
source(file.path("R", "intervals.R"))
source(file.path("R", "labels.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "forecast_candidates.R"))
source(file.path("R", "features.R"))
source(file.path("R", "osp_models.R"))
source(file.path("R", "m4_workflow.R"))
source(file.path("R", "empirical_workflow.R"))
source(file.path("R", "paper.R"))
source(file.path("R", "pipeline.R"))

make_smoke_series <- function() {
  stats::ts(10 + seq_len(60) * 0.1 + sin(seq_len(60) / 3), frequency = 1)
}

make_smoke_future <- function() {
  stats::ts(10 + (61:66) * 0.1 + sin((61:66) / 3), frequency = 1)
}

make_smoke_m4_records <- function(n_series = 5L) {
  lapply(seq_len(n_series), function(i) {
    list(
      period = "Yearly",
      x = make_smoke_series() + i,
      xx = make_smoke_future() + i,
      h = length(make_smoke_future())
    )
  })
}

run_smoke_pipeline <- function(base_model = "ets", m = 5L, n = 4L) {
  series <- make_smoke_series()
  future <- make_smoke_future()
  grid <- forecast_candidate_grid(series, h = length(future), base_model = base_model, m = m, n = n)
  errors <- apply(grid$predictions, c(1L, 2L), function(pred) mase(series, future, pred))
  label <- label_best_interval(errors, method = "mean")

  list(
    starts = grid$starts,
    predictions = grid$predictions,
    errors = errors,
    label = label,
    summary = apply(errors, 1L, finite_mean)
  )
}

run_smoke_m4_candidate_workflow <- function(base_model = "ets", m = 5L, n = 4L) {
  run_m4_candidate_workflow(
    frequency = "Yearly",
    base_model = base_model,
    m = m,
    n = n,
    train_fraction = 0.6,
    seed = 7L,
    model_engines = "majority",
    model_tasks = "classification",
    m4_data = make_smoke_m4_records()
  )
}

write_smoke_m4_tables <- function(root = "results", base_model = "ets", m = 5L, n = 4L) {
  result <- run_smoke_pipeline(base_model = base_model, m = m, n = n)
  candidate_result <- run_smoke_m4_candidate_workflow(base_model = base_model, m = m, n = n)
  paths <- c(
    summary = result_table_path("smoke_m4_summary", root = root),
    labels = result_table_path("smoke_m4_labels", root = root),
    evaluation = result_table_path("smoke_m4_evaluation", root = root),
    model_evaluation = result_table_path("smoke_m4_model_evaluation", root = root)
  )
  dir.create(dirname(paths[["summary"]]), recursive = TRUE, showWarnings = FALSE)

  summary <- data.frame(
    interval = seq_len(nrow(result$errors)),
    mean_mase = result$summary,
    selected = seq_len(nrow(result$errors)) == result$label + 1L
  )
  write.csv(summary, paths[["summary"]], row.names = FALSE)
  write.csv(candidate_result$labels, paths[["labels"]], row.names = FALSE)
  write.csv(candidate_result$evaluation_mean, paths[["evaluation"]], row.names = FALSE)
  write.csv(candidate_result$model_evaluation, paths[["model_evaluation"]], row.names = FALSE)
  unname(paths)
}

select_empirical_dataset <- function(config, dataset_name) {
  config <- normalize_empirical_config(config)
  names <- vapply(config$datasets, `[[`, character(1), "name")
  hit <- match(dataset_name, names)
  if (is.na(hit)) {
    stop("Empirical dataset not found in config: ", dataset_name, call. = FALSE)
  }
  config$datasets[[hit]]
}

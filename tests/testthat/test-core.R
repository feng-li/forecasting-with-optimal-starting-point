testthat::test_that("start indices cover all intervals for m = 5 and m = 10", {
  starts5 <- make_start_indices(series_length = 100, m = 5, n = 4)
  starts10 <- make_start_indices(series_length = 100, m = 10, n = 4)

  testthat::expect_equal(dim(starts5), c(5L, 4L))
  testthat::expect_equal(dim(starts10), c(10L, 4L))
  testthat::expect_equal(starts5[1, 1], 1L)
  testthat::expect_equal(starts10[1, 1], 1L)
  testthat::expect_equal(length(unique(starts10[, 1])), 10L)
  testthat::expect_true(all(starts10 >= 1L))
  testthat::expect_true(all(starts10 <= 100L))
})

testthat::test_that("interval indices can be normalized and clamped", {
  testthat::expect_equal(as_one_based_interval(c(0, 4), m = 5), c(1L, 5L))
  testthat::expect_equal(as_zero_based_interval(c(1, 5), m = 5, zero_based = FALSE), c(0L, 4L))
  testthat::expect_equal(as_zero_based_interval(c(-2, 9), m = 5, clamp = TRUE), c(0L, 4L))
  testthat::expect_error(as_one_based_interval(5, m = 5), "outside")
})

testthat::test_that("label construction returns zero-based intervals", {
  metrics <- matrix(
    c(
      3, 4, 5,
      2, 2, 2,
      9, 1, 9
    ),
    nrow = 3,
    byrow = TRUE
  )

  testthat::expect_equal(label_best_interval(metrics, "min"), 2L)
  testthat::expect_equal(label_best_interval(metrics, "mean"), 1L)
})

testthat::test_that("multiclass labels are validated as zero-based integers", {
  testthat::expect_equal(encode_multiclass_labels(c(0, 1, 4), m = 5), c(0L, 1L, 4L))
  testthat::expect_error(encode_multiclass_labels(c(1, 5), m = 5), "zero-based")
})

testthat::test_that("OSP model specs and training data are validated before engine calls", {
  specs <- osp_model_specs(label_methods = c("label_min"), engines = "xgboost")
  training_data <- validate_osp_training_data(
    features = matrix(1:6, nrow = 3),
    labels = c(0, 1, 2),
    task = "classification",
    m = 3
  )

  testthat::expect_equal(specs$model_name, c("label_min_xgboost_classification", "label_min_xgboost_regression"))
  testthat::expect_equal(dim(training_data$features), c(3L, 2L))
  testthat::expect_equal(training_data$labels, c(0L, 1L, 2L))
  testthat::expect_error(
    validate_osp_training_data(matrix(1:4, nrow = 2), labels = c(0, 1, 2), task = "classification", m = 3),
    "same number of rows"
  )
  testthat::expect_error(
    train_osp_model_set(matrix(1:4, nrow = 2), data.frame(label_min = c(0, 1)), m = 2, label_methods = "label_mean"),
    "missing required columns"
  )
})

testthat::test_that("OSP model metadata can supply prediction settings", {
  model <- structure(list(), class = "fake")
  model <- set_osp_model_metadata(model, engine = "xgboost", task = "classification", m = 3, nrounds = 5)

  testthat::expect_equal(resolve_osp_model_arg(NULL, model, "osp_engine", osp_engines()), "xgboost")
  testthat::expect_equal(attr(model, "osp_task"), "classification")
  testthat::expect_equal(attr(model, "osp_m"), 3L)
})

testthat::test_that("majority OSP baseline trains and predicts zero-based intervals", {
  features <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  model <- train_osp_model(features, labels = c(1L, 1L, 2L), engine = "majority", task = "classification", m = 3)
  predictions <- predict_osp_interval(model, features, m = 3)

  testthat::expect_equal(predictions, c(1L, 1L, 1L))
  testthat::expect_equal(attr(model, "osp_engine"), "majority")
})

testthat::test_that("xgboost OSP wrapper trains with the installed API", {
  testthat::skip_if_not_installed("xgboost")
  features <- matrix(
    c(
      0, 0,
      0, 1,
      1, 0,
      1, 1,
      2, 1,
      2, 2
    ),
    ncol = 2,
    byrow = TRUE
  )
  labels <- c(0L, 0L, 1L, 1L, 2L, 2L)

  model <- train_osp_model(features, labels = labels, engine = "xgboost", task = "classification", m = 3, nrounds = 2L)
  predictions <- predict_osp_interval(model, features, m = 3)

  testthat::expect_equal(length(predictions), nrow(features))
  testthat::expect_true(all(predictions %in% 0:2))
})

testthat::test_that("model prediction helpers return zero-based intervals", {
  probability_matrix <- matrix(
    c(
      0.1, 0.8, 0.1,
      0.2, 0.3, 0.5
    ),
    nrow = 2,
    byrow = TRUE
  )
  legacy_probability_vector <- as.vector(matrix(
    c(
      0.1, 0.8, 0.1,
      0.2, 0.3, 0.5
    ),
    nrow = 3
  ))

  testthat::expect_equal(decode_multiclass_predictions(probability_matrix, n_obs = 2, m = 3), c(1L, 2L))
  testthat::expect_equal(decode_multiclass_predictions(legacy_probability_vector, n_obs = 2, m = 3), c(1L, 2L))
  testthat::expect_equal(round_interval_predictions(c(-1, 0.4, 4.8, 8), m = 5), c(0L, 0L, 4L, 4L))
})

testthat::test_that("interval forecast selection averages candidate points", {
  predictions <- array(NA_real_, dim = c(2L, 3L, 2L))
  predictions[1, , ] <- matrix(c(1, 10, 3, 30, 3, 30), nrow = 3, byrow = TRUE)
  predictions[2, , ] <- matrix(c(2, 20, 4, 40, 6, 60), nrow = 3, byrow = TRUE)

  testthat::expect_equal(interval_prediction_matrix(predictions, interval = 0), predictions[1, , ])
  testthat::expect_equal(average_interval_predictions(predictions, interval = 0), c(2, 20))
  testthat::expect_equal(average_interval_predictions(predictions, interval = 2, clamp = TRUE), c(4, 40))
})

testthat::test_that("RData artifacts can be loaded without eval(parse())", {
  path <- tempfile(fileext = ".RData")
  datalist <- list(a = 1)
  save(datalist, file = path)

  testthat::expect_equal(load_single_object(path, "datalist"), list(a = 1))
  testthat::expect_equal(load_single_object(path), list(a = 1))
})

testthat::test_that("metric summaries ignore NA and Inf", {
  metrics <- matrix(c(1, NA, Inf, 3, 5, 7), ncol = 2)
  testthat::expect_equal(summarize_accuracy(metrics), c(1, 5))
})

testthat::test_that("forecast accuracy rows include paper metrics", {
  row <- accuracy_row(
    train = c(10, 12, 14, 16),
    actual = c(18, 20),
    predicted = c(17, 22)
  )

  testthat::expect_equal(
    names(row),
    c("ME", "RMSE", "MAE", "MPE", "MAPE", "sMAPE", "MASE")
  )
  testthat::expect_true(is.finite(row$MASE))
  testthat::expect_equal(summarize_evaluation(row)$metric, names(row))
})

testthat::test_that("M4 interval forecast selection averages selected candidate points", {
  predictions <- array(NA_real_, dim = c(2L, 3L, 2L, 4L))
  predictions[1, 1, , ] <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 2, byrow = TRUE)
  predictions[1, 2, , ] <- matrix(c(10, 20, 30, 40, 50, 60, 70, 80), nrow = 2, byrow = TRUE)
  predictions[1, 3, , ] <- matrix(c(100, 200, 300, 400, 500, 600, 700, 800), nrow = 2, byrow = TRUE)
  predictions[2, 1, , ] <- matrix(c(2, 4, 6, 8, 3, 5, 7, 9), nrow = 2, byrow = TRUE)
  predictions[2, 2, , ] <- matrix(c(1, 1, 1, 1, 3, 3, 3, 3), nrow = 2, byrow = TRUE)
  predictions[2, 3, , ] <- matrix(c(4, 4, 4, 4, 6, 6, 6, 6), nrow = 2, byrow = TRUE)

  forecasts <- selected_interval_forecasts(predictions, labels = c(0L, 2L))

  testthat::expect_equal(dim(forecasts), c(2L, 4L))
  testthat::expect_equal(forecasts[1, ], c(3, 4, 5, 6))
  testthat::expect_equal(forecasts[2, ], c(5, 5, 5, 5))
})

testthat::test_that("selected M4 intervals can be evaluated against holdout data", {
  records <- list(
    list(period = "Yearly", x = stats::ts(c(8, 10, 12, 14)), xx = stats::ts(c(16, 18)), h = 2),
    list(period = "Yearly", x = stats::ts(c(3, 5, 7, 9)), xx = stats::ts(c(11, 13)), h = 2)
  )
  predictions <- array(NA_real_, dim = c(2L, 2L, 2L, 2L))
  predictions[1, 1, , ] <- matrix(c(15, 17, 17, 19), nrow = 2, byrow = TRUE)
  predictions[1, 2, , ] <- matrix(c(10, 10, 12, 12), nrow = 2, byrow = TRUE)
  predictions[2, 1, , ] <- matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE)
  predictions[2, 2, , ] <- matrix(c(10, 12, 12, 14), nrow = 2, byrow = TRUE)

  evaluation <- evaluate_selected_intervals(records, predictions, labels = c(0L, 1L), method = "test")

  testthat::expect_equal(nrow(evaluation), 2L)
  testthat::expect_true(all(c("series", "MASE", "label", "method") %in% names(evaluation)))
  testthat::expect_equal(evaluation$label, c(0L, 1L))
  testthat::expect_equal(evaluation$method, rep("test", 2L))
  testthat::expect_true(all(is.finite(evaluation$MASE)))
})

testthat::test_that("artifact paths use the cleaned results layout", {
  testthat::expect_equal(result_table_path("smoke_m4_summary"), file.path("results", "tables", "smoke_m4_summary.csv"))
  testthat::expect_equal(artifact_path("features.rds", type = "intermediate"), file.path("results", "intermediate", "features.rds"))
  testthat::expect_error(artifact_path("../outside.csv", type = "tables"), "relative artifact name")
})

testthat::test_that("dependency manifest helpers report package status", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      package = c("stats", "ospMissingPackageForTest"),
      role = c("core", "full_reproduction"),
      required_for = c("base R smoke check", "missing package test"),
      install_source = c("base", "CRAN"),
      notes = c("", ""),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )

  manifest <- read_dependency_manifest(path)
  status <- dependency_status(manifest)
  missing_full <- missing_dependencies(status, roles = "full_reproduction")
  status_path <- write_dependency_status(status, root = tempfile())

  testthat::expect_equal(manifest$package, c("stats", "ospMissingPackageForTest"))
  testthat::expect_true(status$installed[status$package == "stats"])
  testthat::expect_false(status$installed[status$package == "ospMissingPackageForTest"])
  testthat::expect_equal(missing_full$package, "ospMissingPackageForTest")
  testthat::expect_match(format_missing_dependencies(missing_full), "full_reproduction: ospMissingPackageForTest")
  testthat::expect_true(file.exists(status_path))
})

testthat::test_that("legacy artifact inventory classifies generated files", {
  temp_root <- tempfile()
  empirical_dir <- file.path(temp_root, "Empirical analysis", "Yearly")
  m4_dir <- file.path(temp_root, "OSP-TSP on M4", "Time series 5 equal parts (m=5)", "Yearly")
  dir.create(file.path(empirical_dir, ".ipynb_checkpoints"), recursive = TRUE)
  dir.create(m4_dir, recursive = TRUE)
  write.csv(data.frame(year = 2000:2001, value = 1:2), file.path(empirical_dir, "GDP_worldbank.csv"), row.names = FALSE)
  write.csv(data.frame(MASE = 1), file.path(m4_dir, "ets_Yearly_final_res.csv"), row.names = FALSE)
  writeLines("{}", file.path(empirical_dir, "GDP_M4.ipynb"))
  writeLines("{}", file.path(empirical_dir, ".ipynb_checkpoints", "GDP_M4-checkpoint.ipynb"))
  saveRDS(list(model = 1), file.path(empirical_dir, "model.rds"))

  inventory <- legacy_artifact_inventory(file.path(temp_root, c("OSP-TSP on M4", "Empirical analysis")))
  summary <- legacy_artifact_summary(inventory)
  inventory_path <- write_legacy_artifact_inventory(inventory, root = file.path(temp_root, "results"))
  summary_path <- write_legacy_artifact_summary(summary, root = file.path(temp_root, "results"))

  testthat::expect_true(all(c(
    "empirical_raw_input",
    "legacy_result_table",
    "legacy_notebook",
    "notebook_checkpoint",
    "serialized_model_or_intermediate"
  ) %in% inventory$artifact_type))
  testthat::expect_true(file.exists(inventory_path))
  testthat::expect_true(file.exists(summary_path))
  testthat::expect_equal(sum(summary$count), nrow(inventory))
})

testthat::test_that("M4 frequency loading works with supplied records", {
  records <- list(
    list(period = "Yearly", x = stats::ts(1:20), xx = stats::ts(21:24), h = 4),
    list(period = "Monthly", x = stats::ts(1:30, frequency = 12), xx = stats::ts(31:36, frequency = 12), h = 6)
  )

  yearly <- load_m4_frequency("Yearly", m4_data = records)
  testthat::expect_length(yearly, 1)
  testthat::expect_equal(yearly[[1]]$period, "Yearly")
  testthat::expect_error(load_m4_frequency("Bad", m4_data = records), "arg")
})

testthat::test_that("M4 data can be loaded from local RDS snapshots", {
  records <- list(
    list(period = "Yearly", x = stats::ts(1:20), xx = stats::ts(21:24), h = 4),
    list(period = "Monthly", x = stats::ts(1:30, frequency = 12), xx = stats::ts(31:36, frequency = 12), h = 6)
  )
  path <- tempfile(fileext = ".rds")
  saveRDS(list(M4 = records), path)

  loaded <- load_m4_data(path = path, package_fallback = FALSE)
  yearly <- load_m4_frequency("Yearly", m4_data_path = path)
  status <- m4_data_status(path = path, package_fallback = FALSE)

  testthat::expect_length(loaded, 2)
  testthat::expect_length(yearly, 1)
  testthat::expect_true(status$available)
  testthat::expect_equal(status$source, "local_rds")
  testthat::expect_equal(status$n_records, 2L)
  testthat::expect_match(status$frequencies, "Yearly")
  testthat::expect_error(load_m4_data(path = tempfile(fileext = ".rds"), package_fallback = FALSE), "does not exist")
})

testthat::test_that("M4 records produce model training tables", {
  records <- lapply(seq_len(2), function(i) {
    train <- 10 + seq_len(48) * 0.2 + sin(seq_len(48) / 4) + i
    future <- 10 + (49:52) * 0.2 + i
    list(period = "Yearly", x = stats::ts(train), xx = stats::ts(future), h = length(future))
  })
  features <- m4_record_features(records)
  labels <- data.frame(series = 1:2, label_min = c(0L, 1L), label_mean = c(1L, 0L))
  training_table <- m4_training_table(features, labels)

  testthat::expect_equal(nrow(features), 2L)
  testthat::expect_true(all(c("series", "length") %in% names(features)))
  testthat::expect_true(all(c("label_min", "label_mean") %in% names(training_table)))
})

testthat::test_that("M4 config expands into filtered run grids", {
  config <- read_m4_config(file.path(root, "config", "m4.yml"))
  fallback_config <- normalize_m4_config(read_simple_yaml_config(file.path(root, "config", "m4.yml")))
  grid <- expand_m4_config(config, frequency = "Yearly", base_model = "ets", m = 5, n = 4)
  expected <- m4_expected_artifact_manifest(grid, root = tempfile(), include_model = TRUE)
  grid_path <- write_m4_configured_run_grid(grid, root = tempfile())
  expected_path <- write_m4_expected_artifact_manifest(expected, root = tempfile())

  testthat::expect_equal(nrow(grid), 1L)
  testthat::expect_equal(grid$frequency, "Yearly")
  testthat::expect_equal(grid$base_model, "ets")
  testthat::expect_equal(grid$m, 5L)
  testthat::expect_equal(grid$n, 4L)
  testthat::expect_equal(fallback_config$segmentations[[1L]]$n, 4L)
  testthat::expect_true(all(c("candidate_summary", "labels", "model_evaluation_summary") %in% expected$artifact))
  testthat::expect_true(all(grepl("yearly_ets_m5_n4", expected$path, fixed = TRUE)))
  testthat::expect_true(file.exists(grid_path))
  testthat::expect_true(file.exists(expected_path))
})

testthat::test_that("minimal pipeline config resolves one M4 target spec", {
  m4_config <- normalize_m4_config(list(
    frequencies = "Yearly",
    base_models = "ets",
    segmentations = list(list(m = 5L, n = 4L)),
    seed = 42L,
    train_fraction = 0.75
  ))
  pipeline_config <- list(
    m4_frequency = "Yearly",
    m4_base_model = "ets",
    m4_m = 5L,
    m4_n = 4L,
    m4_max_series = 20L,
    m4_osp_engines = "xgboost",
    m4_osp_tasks = "classification",
    m4_results_root = tempfile()
  )

  spec <- m4_pipeline_spec(pipeline_config, m4_config)

  testthat::expect_equal(nrow(spec$run), 1L)
  testthat::expect_equal(spec$run$frequency, "Yearly")
  testthat::expect_equal(spec$max_series, 20L)
  testthat::expect_equal(spec$model_engines, "xgboost")
  testthat::expect_equal(spec$model_tasks, "classification")
})

testthat::test_that("M4 workflow table writer records expected artifacts", {
  records <- lapply(seq_len(2), function(i) {
    train <- 10 + seq_len(36) * 0.2 + i
    future <- 10 + (37:40) * 0.2 + i
    list(period = "Yearly", x = stats::ts(train), xx = stats::ts(future), h = length(future))
  })
  result <- run_m4_candidate_workflow(
    frequency = "Yearly",
    base_model = "ets",
    m = 2,
    n = 2,
    m4_data = records
  )
  root_dir <- tempfile()

  manifest <- write_m4_workflow_tables(result, root = root_dir)

  testthat::expect_true(all(file.exists(manifest$path)))
  testthat::expect_true(all(c("labels", "evaluation_mean", "evaluation_summary_mean") %in% manifest$artifact))
})

testthat::test_that("M4 OSP model evaluation uses held-out split records", {
  records <- lapply(seq_len(5), function(i) {
    train <- 10 + seq_len(48) * 0.2 + sin(seq_len(48) / 4) + i
    future_index <- 49:52
    future <- 10 + future_index * 0.2 + sin(future_index / 4) + i
    list(period = "Yearly", x = stats::ts(train), xx = stats::ts(future), h = length(future))
  })
  result <- run_m4_candidate_workflow(
    frequency = "Yearly",
    base_model = "ets",
    m = 3,
    n = 2,
    train_fraction = 0.6,
    seed = 7,
    m4_data = records,
    model_engines = "majority",
    model_tasks = "classification",
    model_label_methods = "label_mean"
  )

  testthat::expect_equal(nrow(result$model_evaluation), length(result$split$test))
  testthat::expect_true(all(c("model_name", "source_series", "actual_label", "predicted_label", "MASE") %in% names(result$model_evaluation)))
  testthat::expect_equal(result$model_evaluation$source_series, result$split$test)
  testthat::expect_true(all(result$model_evaluation$engine == "majority"))
  testthat::expect_true(all(result$model_evaluation$task == "classification"))
  testthat::expect_true(all(result$model_evaluation$predicted_label >= 0L & result$model_evaluation$predicted_label < 3L))
  testthat::expect_true(all(c("model_name", "metric", "mean") %in% names(result$model_evaluation_summary)))
})

testthat::test_that("empirical config expands into legacy script manifests", {
  config <- read_empirical_config(file.path(root, "config", "empirical.yml"))
  grid <- expand_empirical_config(config)
  yearly_gdp <- expand_empirical_config(config, dataset = "GDP", method = "M4", m = 5, n = 4)
  manifest_path <- write_empirical_manifest(yearly_gdp, root = tempfile())
  missing_grid <- yearly_gdp
  missing_grid$raw_file_exists <- FALSE
  status <- validate_empirical_inputs(missing_grid, strict = FALSE)

  testthat::expect_equal(nrow(grid), 15L)
  testthat::expect_equal(nrow(yearly_gdp), 1L)
  testthat::expect_true(file.exists(manifest_path))
  testthat::expect_true(yearly_gdp$legacy_script_exists)
  testthat::expect_false(status$complete)
  testthat::expect_equal(status$missing_raw, "data/raw/GDP_worldbank.csv")
})

testthat::test_that("empirical raw data can be prepared from legacy inputs", {
  temp_root <- tempfile()
  legacy_dir <- file.path(temp_root, "Empirical analysis", "Yearly")
  dir.create(legacy_dir, recursive = TRUE)
  write.csv(data.frame(year = 2000:2001, value = c(1, 2)), file.path(legacy_dir, "GDP_worldbank.csv"), row.names = FALSE)

  config <- list(
    datasets = list(list(name = "GDP", frequency = "Yearly", file = "data/raw/GDP_worldbank.csv")),
    segmentations = list(list(m = 5L, n = 4L)),
    target_metric = "MASE"
  )

  dry_run <- prepare_empirical_raw_data(config, root = temp_root, dry_run = TRUE)
  destination <- file.path(temp_root, "data", "raw", "GDP_worldbank.csv")
  testthat::expect_equal(dry_run$status, "dry-run")
  testthat::expect_false(file.exists(destination))

  copied <- prepare_empirical_raw_data(config, root = temp_root)
  manifest_path <- write_empirical_data_manifest(copied, root = file.path(temp_root, "results"))

  testthat::expect_true(file.exists(destination))
  testthat::expect_equal(copied$status, "copied")
  testthat::expect_true(file.exists(manifest_path))
})

testthat::test_that("empirical raw data can be loaded and baseline-evaluated", {
  temp_root <- tempfile()
  raw_dir <- file.path(temp_root, "data", "raw")
  dir.create(raw_dir, recursive = TRUE)
  years <- 1970:2025
  wide <- data.frame(
    year = years,
    series_a = 10 + seq_along(years) * 0.5,
    series_b = 20 + seq_along(years) * 0.25,
    check.names = FALSE
  )
  write.csv(wide, file.path(raw_dir, "toy.csv"), row.names = FALSE)
  dataset <- list(name = "toy", frequency = "Yearly", file = "data/raw/toy.csv")

  records <- empirical_series_records(dataset, root = temp_root, max_series = 2)
  evaluation <- evaluate_empirical_baselines(dataset, models = "thetaf", max_series = 2, root = temp_root)
  path <- write_empirical_baseline_table(evaluation, root = file.path(temp_root, "results"))

  testthat::expect_length(records, 2L)
  testthat::expect_equal(records[[1L]]$h, 6L)
  testthat::expect_equal(nrow(evaluation), 2L)
  testthat::expect_true(all(c("dataset", "baseline_model", "MASE") %in% names(evaluation)))
  testthat::expect_true(all(is.finite(evaluation$MASE)))
  testthat::expect_true(file.exists(path))
})

testthat::test_that("paper table inventory finds labeled result tables", {
  inventory <- paper_table_inventory(file.path(root, "docs", "main.tex"))
  manifest_path <- write_paper_table_inventory(inventory, root = tempfile())

  testthat::expect_true(nrow(inventory) >= 9L)
  testthat::expect_true(all(c("label", "caption", "category", "expected_artifact") %in% names(inventory)))
  testthat::expect_true(all(c("tab3", "tab7", "tab10") %in% inventory$label))
  testthat::expect_true("empirical_results" %in% inventory$category)
  testthat::expect_true(file.exists(manifest_path))
})

testthat::test_that("paper file inventory separates source and generated files", {
  temp_root <- tempfile()
  paper_dir <- file.path(temp_root, "docs")
  dir.create(file.path(paper_dir, ".git"), recursive = TRUE)
  writeLines("\\\\documentclass{article}", file.path(paper_dir, "main.tex"))
  writeLines("log", file.path(paper_dir, "main.log"))
  writeLines("pdf", file.path(paper_dir, "main.pdf"))
  writeLines("git", file.path(paper_dir, ".git", "HEAD"))
  writeLines("archive", file.path(paper_dir, "arxiv.zip"))

  inventory <- paper_file_inventory(paper_dir = paper_dir)
  summary <- paper_file_summary(inventory)
  inventory_path <- write_paper_file_inventory(inventory, root = file.path(temp_root, "results"))
  summary_path <- write_paper_file_summary(summary, root = file.path(temp_root, "results"))

  testthat::expect_true(all(c("paper_source", "latex_generated", "paper_pdf", "nested_git_metadata", "archive") %in% inventory$file_type))
  testthat::expect_true(file.exists(inventory_path))
  testthat::expect_true(file.exists(summary_path))
  testthat::expect_equal(sum(summary$count), nrow(inventory))
})

testthat::test_that("paper table status and available summaries use generated outputs", {
  temp_root <- tempfile()
  dir.create(file.path(temp_root, "tables"), recursive = TRUE)
  write.csv(
    data.frame(model_name = "m", label_method = "label_mean", engine = "majority", task = "classification", MAPE = c(1, 3), MASE = c(2, 4)),
    file.path(temp_root, "tables", "smoke_m4_model_evaluation.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(MAPE = 1, MASE = 2),
    file.path(temp_root, "tables", "smoke_m4_evaluation.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(dataset = "GDP", frequency = "Yearly", baseline_model = "thetaf", MAPE = c(10, 12), MASE = c(3, 5)),
    file.path(temp_root, "tables", "empirical_baseline_evaluation.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(dataset = "GDP"),
    file.path(temp_root, "tables", "empirical_run_manifest.csv"),
    row.names = FALSE
  )
  inventory <- data.frame(
    table_index = 1:2,
    label = c("tab3", "tab7"),
    category = c("m4_results", "empirical_results"),
    expected_artifact = c("paper_table_03_m4_m5_seasonal_results", "paper_table_07_empirical_direct_results")
  )

  status <- paper_table_status(inventory, root = temp_root)
  available <- build_available_paper_tables(root = temp_root)

  testthat::expect_true(all(status$supporting_outputs_exist))
  testthat::expect_true(all(status$status == "supporting outputs available"))
  testthat::expect_true(all(file.exists(available$path)))
  empirical_summary <- read.csv(file.path(temp_root, "tables", "paper_available_empirical_baseline_summary.csv"))
  testthat::expect_equal(empirical_summary$MAPE, 11)
  testthat::expect_equal(empirical_summary$MASE, 4)
})

testthat::test_that("train/test split is deterministic and isolated", {
  set.seed(12)
  before <- runif(1)
  split1 <- split_train_test_indices(10, train_fraction = 0.7, seed = 100)
  after1 <- runif(1)

  set.seed(12)
  testthat::expect_equal(before, runif(1))
  split2 <- split_train_test_indices(10, train_fraction = 0.7, seed = 100)
  after2 <- runif(1)

  testthat::expect_equal(split1, split2)
  testthat::expect_equal(after1, after2)
  testthat::expect_equal(length(split1$assignment), 10)
})

testthat::test_that("M4 candidate workflow creates errors and labels", {
  records <- lapply(seq_len(2), function(i) {
    train <- 10 + seq_len(48) * 0.2 + sin(seq_len(48) / 4) + i
    future_index <- 49:52
    future <- 10 + future_index * 0.2 + sin(future_index / 4) + i
    list(period = "Yearly", x = stats::ts(train), xx = stats::ts(future), h = length(future))
  })

  result <- run_m4_candidate_workflow(
    frequency = "Yearly",
    base_model = "ets",
    m = 3,
    n = 2,
    m4_data = records
  )

  testthat::expect_equal(dim(result$starts), c(2L, 3L, 2L))
  testthat::expect_equal(dim(result$predictions), c(2L, 3L, 2L, 4L))
  testthat::expect_equal(dim(result$errors), c(2L, 3L, 2L))
  testthat::expect_equal(nrow(result$features), 2L)
  testthat::expect_true(all(c("label_min", "label_mean") %in% names(result$training_table)))
  testthat::expect_equal(dim(result$interval_forecasts_mean), c(2L, 4L))
  testthat::expect_equal(nrow(result$evaluation_mean), 2L)
  testthat::expect_true(all(c("series", "MASE", "label", "method") %in% names(result$evaluation_mean)))
  testthat::expect_equal(result$evaluation_mean$method, rep("label_mean", 2L))
  testthat::expect_true(all(is.finite(result$evaluation_mean$MASE)))
  testthat::expect_true(all(c("metric", "mean") %in% names(result$evaluation_summary_mean)))
  testthat::expect_equal(nrow(result$labels), 2L)
  testthat::expect_true(all(result$labels$label_min >= 0L & result$labels$label_min < 3L))
  testthat::expect_true(all(result$labels$label_mean >= 0L & result$labels$label_mean < 3L))
  testthat::expect_equal(length(result$error_summary), 3L)
})

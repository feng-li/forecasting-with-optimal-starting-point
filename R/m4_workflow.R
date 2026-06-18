# M4-shaped workflow helpers for the cleaned OSP pipeline.

m4_frequencies <- function() {
  c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly")
}

validate_m4_frequency <- function(frequency) {
  match.arg(frequency, m4_frequencies())
}

normalize_m4_config <- function(config) {
  if (is.null(config$seed)) {
    config$seed <- 100L
  }
  if (is.null(config$train_fraction)) {
    config$train_fraction <- 0.7
  }
  if (is.null(config$frequencies)) {
    config$frequencies <- m4_frequencies()
  }
  if (is.null(config$base_models)) {
    config$base_models <- c("ets", "thetaf")
  }
  if (is.null(config$segmentations)) {
    config$segmentations <- list(list(m = 5L, n = 4L))
  }

  frequencies <- as.character(unlist(config$frequencies, use.names = FALSE))
  frequencies <- vapply(frequencies, validate_m4_frequency, character(1))
  base_models <- as.character(unlist(config$base_models, use.names = FALSE))
  base_models <- vapply(base_models, match.arg, character(1), choices = c("ets", "thetaf"))
  segmentations <- lapply(config$segmentations, function(segmentation) {
    m <- as.integer(segmentation[["m"]])
    n_value <- if (!is.null(segmentation[["n"]])) {
      segmentation[["n"]]
    } else {
      segmentation[["FALSE"]]
    }
    n <- as.integer(n_value)
    if (length(m) != 1L || is.na(m) || m < 1L ||
        length(n) != 1L || is.na(n) || n < 1L) {
      stop("Each M4 segmentation must define positive integer m and n.", call. = FALSE)
    }
    list(m = m, n = n)
  })

  list(
    seed = as.integer(config$seed),
    train_fraction = as.numeric(config$train_fraction),
    frequencies = unname(frequencies),
    segmentations = segmentations,
    base_models = unname(base_models),
    target_metric = if (is.null(config$target_metric)) "MASE" else as.character(config$target_metric)
  )
}

read_m4_config <- function(path = file.path("config", "m4.yml")) {
  if (!file.exists(path)) {
    stop("M4 config file does not exist: ", path, call. = FALSE)
  }
  config <- read_workflow_config(path)
  normalize_m4_config(config)
}

expand_m4_config <- function(config, frequency = NULL, base_model = NULL, m = NULL, n = NULL) {
  config <- normalize_m4_config(config)
  segmentations <- do.call(rbind, lapply(config$segmentations, as.data.frame))
  grid <- expand.grid(
    frequency = config$frequencies,
    base_model = config$base_models,
    segmentation_id = seq_len(nrow(segmentations)),
    stringsAsFactors = FALSE
  )
  grid$m <- segmentations$m[grid$segmentation_id]
  grid$n <- segmentations$n[grid$segmentation_id]
  grid$seed <- config$seed
  grid$train_fraction <- config$train_fraction
  grid$segmentation_id <- NULL

  if (!is.null(frequency)) {
    frequency <- validate_m4_frequency(frequency)
    grid <- grid[grid$frequency == frequency, , drop = FALSE]
  }
  if (!is.null(base_model)) {
    base_model <- match.arg(base_model, c("ets", "thetaf"))
    grid <- grid[grid$base_model == base_model, , drop = FALSE]
  }
  if (!is.null(m)) {
    grid <- grid[grid$m == as.integer(m), , drop = FALSE]
  }
  if (!is.null(n)) {
    grid <- grid[grid$n == as.integer(n), , drop = FALSE]
  }

  row.names(grid) <- NULL
  grid
}

validate_m4_record <- function(record) {
  required <- c("period", "x", "xx")
  missing <- required[!vapply(required, function(name) !is.null(record[[name]]), logical(1))]
  if (length(missing)) {
    stop("M4 record is missing required fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(record$x) < 2L) {
    stop("M4 record training series must contain at least two observations.", call. = FALSE)
  }
  if (!length(record$xx)) {
    stop("M4 record test series must contain at least one observation.", call. = FALSE)
  }
  invisible(TRUE)
}

m4_data_path_setting <- function(path = NULL) {
  if (!is.null(path) && nzchar(path)) {
    return(list(path = path, explicit = TRUE, source = "argument"))
  }
  env_path <- Sys.getenv("OSP_M4_DATA_RDS")
  if (nzchar(env_path)) {
    return(list(path = env_path, explicit = TRUE, source = "OSP_M4_DATA_RDS"))
  }
  list(path = file.path("data", "raw", "m4_records.rds"), explicit = FALSE, source = "default")
}

normalize_m4_data_object <- function(m4_data) {
  if (is.list(m4_data) && !is.null(m4_data$M4) && is.list(m4_data$M4)) {
    m4_data <- m4_data$M4
  }
  if (is.list(m4_data) && !is.null(m4_data$records) && is.list(m4_data$records)) {
    m4_data <- m4_data$records
  }
  if (!is.list(m4_data) || !length(m4_data)) {
    stop("M4 data must be a non-empty list of records.", call. = FALSE)
  }
  m4_data
}

load_m4_data <- function(path = NULL, package_fallback = TRUE) {
  setting <- m4_data_path_setting(path)
  if (file.exists(setting$path)) {
    return(normalize_m4_data_object(readRDS(setting$path)))
  }
  if (isTRUE(setting$explicit)) {
    stop("M4 data source does not exist: ", setting$path, call. = FALSE)
  }
  if (isTRUE(package_fallback) && requireNamespace("M4comp2018", quietly = TRUE)) {
    return(normalize_m4_data_object(M4comp2018::M4))
  }
  stop(
    "M4 data source is not available. Provide --m4-data=<path>, set OSP_M4_DATA_RDS, ",
    "or create ", setting$path, " with a saveRDS() snapshot shaped like M4comp2018::M4.",
    call. = FALSE
  )
}

m4_record_period <- function(record) {
  if (!is.list(record) || is.null(record$period)) {
    return(NA_character_)
  }
  as.character(record$period)
}

m4_data_status <- function(path = NULL, package_fallback = TRUE) {
  setting <- m4_data_path_setting(path)
  local_exists <- file.exists(setting$path)
  package_available <- requireNamespace("M4comp2018", quietly = TRUE)
  source <- if (local_exists) {
    "local_rds"
  } else if (!setting$explicit && isTRUE(package_fallback) && package_available) {
    "M4comp2018"
  } else {
    "missing"
  }
  status <- data.frame(
    source = source,
    path = setting$path,
    path_source = setting$source,
    local_exists = local_exists,
    package_available = package_available,
    available = source != "missing",
    n_records = NA_integer_,
    frequencies = NA_character_,
    error = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!status$available) {
    return(status)
  }

  loaded <- tryCatch(
    load_m4_data(path = if (local_exists) setting$path else NULL, package_fallback = package_fallback),
    error = function(error) error
  )
  if (inherits(loaded, "error")) {
    status$available <- FALSE
    status$error <- conditionMessage(loaded)
    return(status)
  }
  periods <- vapply(loaded, m4_record_period, character(1))
  periods <- periods[nzchar(periods) & !is.na(periods)]
  status$n_records <- length(loaded)
  status$frequencies <- paste(sort(unique(periods)), collapse = ",")
  status
}

write_m4_data_status <- function(status, root = "results") {
  path <- result_table_path("m4_data_status", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(status, path, row.names = FALSE)
  path
}

load_m4_frequency <- function(frequency, m4_data = NULL, m4_data_path = NULL) {
  frequency <- validate_m4_frequency(frequency)
  if (is.null(m4_data)) {
    m4_data <- load_m4_data(path = m4_data_path)
  }
  m4_data <- normalize_m4_data_object(m4_data)
  records <- Filter(function(record) identical(m4_record_period(record), frequency), m4_data)
  if (!length(records)) {
    stop("No M4 records found for frequency: ", frequency, call. = FALSE)
  }
  lapply(records, validate_m4_record)
  records
}

m4_record_features <- function(records) {
  lapply(records, validate_m4_record)
  features <- extract_ts_features(lapply(records, function(record) record$x))
  cbind(series = seq_along(records), features)
}

m4_training_table <- function(features, labels) {
  if (!"series" %in% names(features) || !"series" %in% names(labels)) {
    stop("features and labels must both contain a series column.", call. = FALSE)
  }
  if (!identical(as.integer(features$series), as.integer(labels$series))) {
    stop("features and labels must be ordered by the same series ids.", call. = FALSE)
  }
  cbind(features, labels[setdiff(names(labels), "series")])
}

m4_feature_matrix <- function(features) {
  feature_cols <- setdiff(names(features), "series")
  validate_feature_matrix(features[feature_cols])
}

split_train_test_indices <- function(n, train_fraction = 0.7, seed = 100L) {
  if (length(n) != 1L || is.na(n) || n < 1L) {
    stop("n must be a single positive integer.", call. = FALSE)
  }
  if (length(train_fraction) != 1L || is.na(train_fraction) ||
      train_fraction <= 0 || train_fraction >= 1) {
    stop("train_fraction must be between 0 and 1.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  assignment <- sample.int(2L, size = n, replace = TRUE, prob = c(train_fraction, 1 - train_fraction))
  list(
    assignment = assignment,
    train = which(assignment == 1L),
    test = which(assignment == 2L)
  )
}

candidate_error_array <- function(records, base_model = c("ets", "thetaf"), m, n) {
  base_model <- match.arg(base_model)
  lapply(records, validate_m4_record)
  h <- length(records[[1L]]$xx)
  n_series <- length(records)
  starts <- array(NA_integer_, dim = c(n_series, m, n))
  predictions <- array(NA_real_, dim = c(n_series, m, n, h))
  errors <- array(NA_real_, dim = c(n_series, m, n))

  for (series_id in seq_along(records)) {
    actual <- as.numeric(records[[series_id]]$xx)
    if (length(actual) != h) {
      stop("All records in one candidate-error array must use the same forecast horizon.", call. = FALSE)
    }
    grid <- forecast_candidate_grid(records[[series_id]]$x, h = h, base_model = base_model, m = m, n = n)
    starts[series_id, , ] <- grid$starts
    predictions[series_id, , , ] <- grid$predictions
    errors[series_id, , ] <- apply(
      grid$predictions,
      c(1L, 2L),
      function(predicted) mase(records[[series_id]]$x, actual, predicted)
    )
  }

  list(starts = starts, predictions = predictions, errors = errors)
}

candidate_label_table <- function(error_array) {
  data.frame(
    series = seq_len(dim(error_array)[[1L]]),
    label_min = label_best_intervals(error_array, method = "min"),
    label_mean = label_best_intervals(error_array, method = "mean")
  )
}

summarize_candidate_errors <- function(error_array) {
  apply(error_array, 2L, finite_mean)
}

m4_result_prefix <- function(frequency, base_model, m, n) {
  paste(tolower(frequency), base_model, paste0("m", as.integer(m)), paste0("n", as.integer(n)), sep = "_")
}

m4_workflow_artifacts <- function(include_model = FALSE) {
  artifacts <- data.frame(
    artifact = c(
      "candidate_summary",
      "labels",
      "evaluation_mean",
      "evaluation_min",
      "evaluation_summary_mean",
      "evaluation_summary_min"
    ),
    suffix = c(
      "_candidate_summary",
      "_labels",
      "_evaluation_label_mean",
      "_evaluation_label_min",
      "_evaluation_summary_label_mean",
      "_evaluation_summary_label_min"
    ),
    stage = c(
      "candidate_evaluation",
      "candidate_evaluation",
      "candidate_evaluation",
      "candidate_evaluation",
      "candidate_evaluation",
      "candidate_evaluation"
    ),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_model)) {
    artifacts <- rbind(
      artifacts,
      data.frame(
        artifact = c("model_evaluation", "model_evaluation_summary"),
        suffix = c("_osp_model_evaluation", "_osp_model_evaluation_summary"),
        stage = c("osp_model_evaluation", "osp_model_evaluation"),
        stringsAsFactors = FALSE
      )
    )
  }
  artifacts
}

m4_expected_artifact_manifest <- function(runs, root = "results", include_model = TRUE) {
  if (!nrow(runs)) {
    return(data.frame(
      run_id = integer(),
      frequency = character(),
      base_model = character(),
      m = integer(),
      n = integer(),
      seed = integer(),
      train_fraction = numeric(),
      prefix = character(),
      artifact = character(),
      stage = character(),
      path = character(),
      exists = logical(),
      row.names = NULL
    ))
  }
  required <- c("frequency", "base_model", "m", "n", "seed", "train_fraction")
  missing <- setdiff(required, names(runs))
  if (length(missing)) {
    stop("M4 run grid is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  artifact_template <- m4_workflow_artifacts(include_model = include_model)
  rows <- vector("list", nrow(runs) * nrow(artifact_template))
  row_id <- 1L
  for (run_id in seq_len(nrow(runs))) {
    run <- runs[run_id, ]
    prefix <- m4_result_prefix(run$frequency, run$base_model, run$m, run$n)
    for (artifact_id in seq_len(nrow(artifact_template))) {
      artifact <- artifact_template[artifact_id, ]
      path <- result_table_path(paste0(prefix, artifact$suffix), root = root)
      rows[[row_id]] <- data.frame(
        run_id = run_id,
        frequency = run$frequency,
        base_model = run$base_model,
        m = as.integer(run$m),
        n = as.integer(run$n),
        seed = as.integer(run$seed),
        train_fraction = as.numeric(run$train_fraction),
        prefix = prefix,
        artifact = artifact$artifact,
        stage = artifact$stage,
        path = path,
        exists = file.exists(path),
        row.names = NULL
      )
      row_id <- row_id + 1L
    }
  }
  do.call(rbind, rows)
}

write_m4_configured_run_grid <- function(runs, root = "results") {
  path <- result_table_path("m4_configured_run_grid", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(runs, path, row.names = FALSE)
  path
}

write_m4_expected_artifact_manifest <- function(manifest, root = "results") {
  path <- result_table_path("m4_expected_artifacts", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(manifest, path, row.names = FALSE)
  path
}

m4_candidate_summary_table <- function(result) {
  interval <- seq_along(result$error_summary)
  mean_counts <- tabulate(result$labels$label_mean + 1L, nbins = length(interval))
  min_counts <- tabulate(result$labels$label_min + 1L, nbins = length(interval))
  data.frame(
    frequency = result$frequency,
    base_model = result$base_model,
    m = result$m,
    n = result$n,
    interval = interval,
    interval_zero_based = interval - 1L,
    mean_mase = as.numeric(result$error_summary),
    selected_label_mean_count = mean_counts,
    selected_label_mean_share = mean_counts / nrow(result$labels),
    selected_label_min_count = min_counts,
    selected_label_min_share = min_counts / nrow(result$labels)
  )
}

with_m4_metadata <- function(result, table) {
  cbind(
    frequency = result$frequency,
    base_model = result$base_model,
    m = result$m,
    n = result$n,
    table
  )
}

write_m4_workflow_tables <- function(result, root = "results",
                                     prefix = m4_result_prefix(result$frequency, result$base_model, result$m, result$n)) {
  paths <- c(
    candidate_summary = result_table_path(paste0(prefix, "_candidate_summary"), root = root),
    labels = result_table_path(paste0(prefix, "_labels"), root = root),
    evaluation_mean = result_table_path(paste0(prefix, "_evaluation_label_mean"), root = root),
    evaluation_min = result_table_path(paste0(prefix, "_evaluation_label_min"), root = root),
    evaluation_summary_mean = result_table_path(paste0(prefix, "_evaluation_summary_label_mean"), root = root),
    evaluation_summary_min = result_table_path(paste0(prefix, "_evaluation_summary_label_min"), root = root)
  )
  dir.create(dirname(paths[[1L]]), recursive = TRUE, showWarnings = FALSE)
  write.csv(m4_candidate_summary_table(result), paths[["candidate_summary"]], row.names = FALSE)
  write.csv(with_m4_metadata(result, result$labels), paths[["labels"]], row.names = FALSE)
  write.csv(with_m4_metadata(result, result$evaluation_mean), paths[["evaluation_mean"]], row.names = FALSE)
  write.csv(with_m4_metadata(result, result$evaluation_min), paths[["evaluation_min"]], row.names = FALSE)
  write.csv(with_m4_metadata(result, result$evaluation_summary_mean), paths[["evaluation_summary_mean"]], row.names = FALSE)
  write.csv(with_m4_metadata(result, result$evaluation_summary_min), paths[["evaluation_summary_min"]], row.names = FALSE)

  if (!is.null(result$model_evaluation)) {
    model_evaluation_path <- result_table_path(paste0(prefix, "_osp_model_evaluation"), root = root)
    write.csv(with_m4_metadata(result, result$model_evaluation), model_evaluation_path, row.names = FALSE)
    paths <- c(paths, model_evaluation = model_evaluation_path)
  }
  if (!is.null(result$model_evaluation_summary)) {
    model_summary_path <- result_table_path(paste0(prefix, "_osp_model_evaluation_summary"), root = root)
    write.csv(with_m4_metadata(result, result$model_evaluation_summary), model_summary_path, row.names = FALSE)
    paths <- c(paths, model_evaluation_summary = model_summary_path)
  }

  data.frame(
    artifact = names(paths),
    path = unname(paths),
    row.names = NULL
  )
}

selected_interval_forecasts <- function(prediction_array, labels, zero_based = TRUE) {
  dims <- dim(prediction_array)
  if (length(dims) != 4L) {
    stop("prediction_array must have dimensions series x interval x point x horizon.", call. = FALSE)
  }
  if (length(labels) != dims[[1L]]) {
    stop("labels must contain one interval per series.", call. = FALSE)
  }
  forecasts <- matrix(NA_real_, nrow = dims[[1L]], ncol = dims[[4L]])
  for (series_id in seq_len(dims[[1L]])) {
    prediction_slice <- prediction_array[series_id, , , , drop = FALSE]
    prediction_slice <- array(prediction_slice, dim = dims[-1L])
    forecasts[series_id, ] <- average_interval_predictions(
      prediction_array = prediction_slice,
      interval = labels[[series_id]],
      zero_based = zero_based,
      clamp = TRUE
    )
  }
  forecasts
}

evaluate_m4_forecasts <- function(records, forecasts, labels = NULL, method = NULL) {
  lapply(records, validate_m4_record)
  forecasts <- as.matrix(forecasts)
  if (nrow(forecasts) != length(records)) {
    stop("forecasts must contain one row per record.", call. = FALSE)
  }
  rows <- vector("list", length(records))
  for (series_id in seq_along(records)) {
    actual <- as.numeric(records[[series_id]]$xx)
    predicted <- as.numeric(forecasts[series_id, ])
    if (length(predicted) != length(actual)) {
      stop("forecast horizon does not match record test length.", call. = FALSE)
    }
    rows[[series_id]] <- accuracy_row(records[[series_id]]$x, actual, predicted)
  }
  out <- do.call(rbind, rows)
  out <- cbind(series = seq_along(records), out)
  if (!is.null(labels)) {
    out$label <- as.integer(labels)
  }
  if (!is.null(method)) {
    out$method <- method
  }
  out
}

evaluate_selected_intervals <- function(records, prediction_array, labels, method = "label_mean") {
  forecasts <- selected_interval_forecasts(prediction_array, labels = labels, zero_based = TRUE)
  evaluate_m4_forecasts(records, forecasts = forecasts, labels = labels, method = method)
}

evaluate_m4_osp_models <- function(result, label_methods = c("label_min", "label_mean"),
                                   engines = osp_engines(), tasks = osp_tasks(), nrounds = 100L) {
  label_methods <- label_methods[label_methods %in% names(result$labels)]
  if (!length(label_methods)) {
    stop("No requested label methods exist in result$labels.", call. = FALSE)
  }
  split <- result$split
  if (!length(split$train) || !length(split$test)) {
    stop("Both training and test splits must contain at least one series.", call. = FALSE)
  }
  features <- m4_feature_matrix(result$features)
  specs <- osp_model_specs(label_methods = label_methods, engines = engines, tasks = tasks)
  rows <- vector("list", nrow(specs))

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    model <- train_osp_model(
      features = features[split$train, , drop = FALSE],
      labels = result$labels[[spec$label_method]][split$train],
      engine = spec$engine,
      task = spec$task,
      m = result$m,
      nrounds = nrounds
    )
    predicted_labels <- predict_osp_interval(
      model,
      features = features[split$test, , drop = FALSE],
      engine = spec$engine,
      task = spec$task,
      m = result$m
    )
    test_prediction_array <- result$predictions[split$test, , , , drop = FALSE]
    evaluation <- evaluate_selected_intervals(
      records = result$records[split$test],
      prediction_array = test_prediction_array,
      labels = predicted_labels,
      method = spec$model_name
    )
    evaluation$model_name <- spec$model_name
    evaluation$label_method <- spec$label_method
    evaluation$engine <- spec$engine
    evaluation$task <- spec$task
    evaluation$source_series <- as.integer(split$test)
    evaluation$actual_label <- as.integer(result$labels[[spec$label_method]][split$test])
    evaluation$predicted_label <- as.integer(predicted_labels)
    rows[[i]] <- evaluation
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

summarize_m4_osp_model_evaluation <- function(model_evaluation) {
  groups <- unique(model_evaluation[c("model_name", "label_method", "engine", "task")])
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    keep <- model_evaluation$model_name == groups$model_name[[i]]
    summary <- summarize_evaluation(model_evaluation[keep, , drop = FALSE])
    metadata <- groups[i, , drop = FALSE]
    metadata <- metadata[rep(1L, nrow(summary)), , drop = FALSE]
    row.names(metadata) <- NULL
    rows[[i]] <- cbind(metadata, summary)
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

run_m4_candidate_workflow <- function(frequency = "Yearly", base_model = "ets", m = 5L, n = 4L,
                                      train_fraction = 0.7, seed = 100L, max_series = NULL,
                                      m4_data = NULL, m4_data_path = NULL, model_engines = NULL,
                                      model_tasks = c("classification", "regression"),
                                      model_label_methods = c("label_min", "label_mean"),
                                      nrounds = 100L) {
  records <- load_m4_frequency(frequency, m4_data = m4_data, m4_data_path = m4_data_path)
  if (!is.null(max_series)) {
    records <- records[seq_len(min(length(records), max_series))]
  }
  split <- split_train_test_indices(length(records), train_fraction = train_fraction, seed = seed)
  candidates <- candidate_error_array(records, base_model = base_model, m = m, n = n)
  labels <- candidate_label_table(candidates$errors)
  features <- m4_record_features(records)
  evaluation_mean <- evaluate_selected_intervals(
    records = records,
    prediction_array = candidates$predictions,
    labels = labels$label_mean,
    method = "label_mean"
  )
  evaluation_min <- evaluate_selected_intervals(
    records = records,
    prediction_array = candidates$predictions,
    labels = labels$label_min,
    method = "label_min"
  )

  out <- list(
    frequency = validate_m4_frequency(frequency),
    base_model = base_model,
    m = as.integer(m),
    n = as.integer(n),
    records = records,
    split = split,
    starts = candidates$starts,
    predictions = candidates$predictions,
    errors = candidates$errors,
    features = features,
    labels = labels,
    training_table = m4_training_table(features, labels),
    interval_forecasts_mean = selected_interval_forecasts(candidates$predictions, labels$label_mean),
    interval_forecasts_min = selected_interval_forecasts(candidates$predictions, labels$label_min),
    evaluation_mean = evaluation_mean,
    evaluation_min = evaluation_min,
    evaluation_summary_mean = summarize_evaluation(evaluation_mean),
    evaluation_summary_min = summarize_evaluation(evaluation_min),
    error_summary = summarize_candidate_errors(candidates$errors)
  )
  if (!is.null(model_engines)) {
    out$model_evaluation <- evaluate_m4_osp_models(
      out,
      label_methods = model_label_methods,
      engines = model_engines,
      tasks = model_tasks,
      nrounds = nrounds
    )
    out$model_evaluation_summary <- summarize_m4_osp_model_evaluation(out$model_evaluation)
  }
  out
}

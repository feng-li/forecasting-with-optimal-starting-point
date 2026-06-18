# Config and manifest helpers for the cleaned empirical workflow.

empirical_methods <- function() {
  c("M4", "Train_by_oneself", "gratis5")
}

empirical_horizon <- function(frequency) {
  switch(
    validate_m4_frequency(frequency),
    Yearly = 6L,
    Quarterly = 8L,
    Monthly = 12L,
    Weekly = 13L,
    Daily = 14L,
    Hourly = 48L
  )
}

empirical_ts_frequency <- function(frequency) {
  switch(
    validate_m4_frequency(frequency),
    Yearly = 1L,
    Quarterly = 4L,
    Monthly = 12L,
    Weekly = 52L,
    Daily = 7L,
    Hourly = 24L
  )
}

legacy_empirical_method_file <- function(dataset_name, frequency, method) {
  directory <- file.path("Empirical analysis", frequency)
  file.path(directory, paste0(dataset_name, "_", method, ".r"))
}

legacy_empirical_raw_file <- function(raw_file, frequency) {
  file.path("Empirical analysis", frequency, basename(raw_file))
}

normalize_empirical_config <- function(config) {
  if (is.null(config$datasets) || !length(config$datasets)) {
    stop("Empirical config must define at least one dataset.", call. = FALSE)
  }
  if (is.null(config$segmentations)) {
    config$segmentations <- list(list(m = 5L, n = 4L))
  }

  datasets <- lapply(config$datasets, function(dataset) {
    required <- c("name", "frequency", "file")
    missing <- required[!vapply(required, function(name) {
      value <- dataset[[name]]
      !is.null(value) && nzchar(as.character(value))
    }, logical(1))]
    if (length(missing)) {
      stop("Empirical dataset is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    list(
      name = as.character(dataset$name),
      frequency = validate_m4_frequency(as.character(dataset$frequency)),
      file = as.character(dataset$file)
    )
  })

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
      stop("Each empirical segmentation must define positive integer m and n.", call. = FALSE)
    }
    list(m = m, n = n)
  })

  list(
    datasets = datasets,
    segmentations = segmentations,
    target_metric = if (is.null(config$target_metric)) "MASE" else as.character(config$target_metric)
  )
}

read_empirical_config <- function(path = file.path("config", "empirical.yml")) {
  normalize_empirical_config(read_workflow_config(path))
}

expand_empirical_config <- function(config, dataset = NULL, method = NULL, m = NULL, n = NULL,
                                    root = project_root()) {
  config <- normalize_empirical_config(config)
  datasets <- do.call(rbind, lapply(config$datasets, as.data.frame))
  segmentations <- do.call(rbind, lapply(config$segmentations, as.data.frame))
  grid <- expand.grid(
    dataset = datasets$name,
    method = empirical_methods(),
    segmentation_id = seq_len(nrow(segmentations)),
    stringsAsFactors = FALSE
  )
  match_id <- match(grid$dataset, datasets$name)
  grid$frequency <- datasets$frequency[match_id]
  grid$raw_file <- datasets$file[match_id]
  grid$m <- segmentations$m[grid$segmentation_id]
  grid$n <- segmentations$n[grid$segmentation_id]
  grid$target_metric <- config$target_metric
  grid$legacy_script <- mapply(
    legacy_empirical_method_file,
    dataset_name = grid$dataset,
    frequency = grid$frequency,
    method = grid$method,
    USE.NAMES = FALSE
  )
  grid$raw_file_exists <- file.exists(file.path(root, grid$raw_file))
  grid$legacy_script_exists <- file.exists(file.path(root, grid$legacy_script))
  grid$segmentation_id <- NULL

  if (!is.null(dataset)) {
    grid <- grid[grid$dataset == dataset, , drop = FALSE]
  }
  if (!is.null(method)) {
    method <- match.arg(method, empirical_methods())
    grid <- grid[grid$method == method, , drop = FALSE]
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

write_empirical_manifest <- function(grid, root = "results") {
  path <- result_table_path("empirical_run_manifest", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(grid, path, row.names = FALSE)
  path
}

validate_empirical_inputs <- function(grid, strict = TRUE) {
  missing_raw <- unique(grid$raw_file[!grid$raw_file_exists])
  missing_scripts <- unique(grid$legacy_script[!grid$legacy_script_exists])
  if (strict && (length(missing_raw) || length(missing_scripts))) {
    pieces <- character()
    if (length(missing_raw)) {
      pieces <- c(pieces, paste0("missing raw files: ", paste(missing_raw, collapse = ", ")))
    }
    if (length(missing_scripts)) {
      pieces <- c(pieces, paste0("missing legacy scripts: ", paste(missing_scripts, collapse = ", ")))
    }
    stop(paste(pieces, collapse = "; "), call. = FALSE)
  }
  list(
    missing_raw = missing_raw,
    missing_scripts = missing_scripts,
    complete = !length(missing_raw) && !length(missing_scripts)
  )
}

date_like_rows <- function(x) {
  grepl("^\\d{4}(-\\d{2})?$", trimws(as.character(x)))
}

numeric_data_frame <- function(data) {
  as.data.frame(lapply(data, function(column) {
    suppressWarnings(as.numeric(trimws(as.character(column))))
  }), check.names = FALSE)
}

clean_empirical_series_names <- function(names, dataset_name) {
  names <- trimws(as.character(names))
  generated <- !nzchar(names) | grepl("^c\\(", names) | grepl("^[.]\\d+$", names)
  names[generated] <- paste0(dataset_name, "_series_", which(generated))
  make.unique(names, sep = "_")
}

industry_cumulative_to_incremental <- function(data) {
  out <- as.matrix(data)
  for (i in seq_len(nrow(out))) {
    if (i %% 4L != 1L) {
      out[i, ] <- out[i, ] - out[i - 1L, ]
    }
  }
  as.data.frame(out, check.names = FALSE)
}

read_empirical_matrix <- function(dataset, root = project_root()) {
  path <- file.path(root, dataset$file)
  if (!file.exists(path)) {
    stop("Empirical raw file does not exist: ", dataset$file, call. = FALSE)
  }
  raw <- read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  if (ncol(raw) < 2L) {
    stop("Empirical raw file must contain a time column and at least one series.", call. = FALSE)
  }
  time <- raw[[1L]]
  keep <- date_like_rows(time)
  if (!any(keep)) {
    stop("No date-like empirical observations found in: ", dataset$file, call. = FALSE)
  }
  values <- numeric_data_frame(raw[keep, -1L, drop = FALSE])
  names(values) <- clean_empirical_series_names(names(values), dataset$name)
  if (identical(dataset$name, "industry")) {
    values <- industry_cumulative_to_incremental(values)
  }
  list(
    dataset = dataset$name,
    frequency = dataset$frequency,
    time = as.character(time[keep]),
    values = values
  )
}

first_non_missing <- function(x) {
  hit <- which(!is.na(x))
  if (!length(hit)) NA_integer_ else hit[[1L]]
}

empirical_good_series <- function(values, min_length = 40L, max_missing = 10L, max_tail_missing = 3L) {
  keep <- logical(ncol(values))
  for (i in seq_len(ncol(values))) {
    x <- values[[i]]
    first <- first_non_missing(x)
    if (is.na(first)) {
      next
    }
    observed_length <- length(x) - first + 1L
    missing_count <- sum(is.na(x[first:length(x)]))
    tail_start <- max(first, length(x) - 11L)
    tail_missing <- sum(is.na(x[tail_start:length(x)]))
    keep[[i]] <- observed_length >= min_length &&
      missing_count <= max_missing &&
      tail_missing <= max_tail_missing
  }
  keep
}

fill_numeric_gaps <- function(x) {
  x <- as.numeric(x)
  observed <- which(!is.na(x))
  if (!length(observed)) {
    stop("Cannot fill a series with no observed values.", call. = FALSE)
  }
  if (length(observed) == 1L) {
    x[is.na(x)] <- x[observed]
    return(x)
  }
  filled <- stats::approx(observed, x[observed], xout = seq_along(x), rule = 2)$y
  as.numeric(filled)
}

empirical_series_records <- function(dataset, root = project_root(), max_series = NULL) {
  matrix_data <- read_empirical_matrix(dataset, root = root)
  values <- matrix_data$values
  keep <- empirical_good_series(values)
  values <- values[keep]
  if (!is.null(max_series)) {
    values <- values[seq_len(min(length(values), as.integer(max_series)))]
  }
  h <- empirical_horizon(dataset$frequency)
  freq <- empirical_ts_frequency(dataset$frequency)
  records <- vector("list", length(values))
  for (i in seq_along(values)) {
    first <- first_non_missing(values[[i]])
    x <- fill_numeric_gaps(values[[i]][first:length(values[[i]])])
    if (length(x) <= h + 1L) {
      stop("Empirical series is too short after filtering for horizon: ", h, call. = FALSE)
    }
    records[[i]] <- list(
      period = dataset$frequency,
      name = names(values)[[i]],
      x = stats::ts(x[seq_len(length(x) - h)], frequency = freq),
      xx = stats::ts(utils::tail(x, h), frequency = freq),
      h = h
    )
  }
  records
}

forecast_baseline <- function(train, h, model = c("ets", "thetaf", "arima", "nnetar")) {
  require_packages("forecast")
  model <- match.arg(model)
  predicted <- switch(
    model,
    ets = forecast::forecast(forecast::ets(train), h = h)$mean,
    thetaf = forecast::thetaf(train, h = h)$mean,
    arima = forecast::forecast(forecast::auto.arima(train), h = h)$mean,
    nnetar = forecast::forecast(forecast::nnetar(train), h = h)$mean
  )
  as.numeric(predicted)
}

evaluate_empirical_baselines <- function(dataset, models = c("ets", "thetaf", "arima"),
                                         max_series = NULL, root = project_root()) {
  records <- empirical_series_records(dataset, max_series = max_series, root = root)
  rows <- list()
  row_id <- 1L
  for (model in models) {
    model <- match.arg(model, c("ets", "thetaf", "arima", "nnetar"))
    for (series_id in seq_along(records)) {
      predicted <- forecast_baseline(records[[series_id]]$x, h = records[[series_id]]$h, model = model)
      row <- accuracy_row(records[[series_id]]$x, records[[series_id]]$xx, predicted)
      rows[[row_id]] <- cbind(
        dataset = dataset$name,
        frequency = dataset$frequency,
        series = series_id,
        series_name = records[[series_id]]$name,
        baseline_model = model,
        row
      )
      row_id <- row_id + 1L
    }
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

write_empirical_baseline_table <- function(table, root = "results") {
  path <- result_table_path("empirical_baseline_evaluation", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(table, path, row.names = FALSE)
  path
}

empirical_raw_data_manifest <- function(config, root = project_root()) {
  config <- normalize_empirical_config(config)
  datasets <- do.call(rbind, lapply(config$datasets, as.data.frame))
  source <- mapply(
    legacy_empirical_raw_file,
    raw_file = datasets$file,
    frequency = datasets$frequency,
    USE.NAMES = FALSE
  )
  source_path <- file.path(root, source)
  destination_path <- file.path(root, datasets$file)
  source_exists <- file.exists(source_path)
  destination_exists <- file.exists(destination_path)
  source_bytes <- rep(NA_real_, length(source_path))
  source_bytes[source_exists] <- file.info(source_path[source_exists])$size

  data.frame(
    dataset = datasets$name,
    frequency = datasets$frequency,
    source = source,
    destination = datasets$file,
    source_exists = source_exists,
    destination_exists = destination_exists,
    source_bytes = source_bytes,
    row.names = NULL
  )
}

prepare_empirical_raw_data <- function(config, root = project_root(), overwrite = FALSE, dry_run = FALSE) {
  manifest <- empirical_raw_data_manifest(config, root = root)
  manifest$copied <- FALSE
  manifest$status <- ifelse(manifest$source_exists, "ready", "missing source")

  for (i in seq_len(nrow(manifest))) {
    source <- file.path(root, manifest$source[[i]])
    destination <- file.path(root, manifest$destination[[i]])
    if (!manifest$source_exists[[i]]) {
      next
    }
    if (file.exists(destination) && !isTRUE(overwrite)) {
      manifest$status[[i]] <- "exists"
      next
    }
    if (isTRUE(dry_run)) {
      manifest$status[[i]] <- "dry-run"
      next
    }
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    manifest$copied[[i]] <- file.copy(source, destination, overwrite = isTRUE(overwrite))
    manifest$status[[i]] <- if (manifest$copied[[i]]) "copied" else "copy failed"
  }

  refreshed <- empirical_raw_data_manifest(config, root = root)
  manifest$destination_exists <- refreshed$destination_exists
  manifest
}

write_empirical_data_manifest <- function(manifest, root = "results") {
  path <- result_table_path("empirical_data_manifest", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(manifest, path, row.names = FALSE)
  path
}

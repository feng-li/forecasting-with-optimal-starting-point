# Minimal OSP model wrappers.

osp_engines <- function() {
  c("xgboost", "lightgbm")
}

osp_all_engines <- function() {
  c(osp_engines(), "majority")
}

osp_tasks <- function() {
  c("classification", "regression")
}

encode_multiclass_labels <- function(labels, m) {
  labels <- as.integer(labels)
  if (anyNA(labels) || any(labels < 0L) || any(labels >= m)) {
    stop("labels must be zero-based integers in [0, m - 1].", call. = FALSE)
  }
  labels
}

validate_feature_matrix <- function(features, labels = NULL) {
  features <- as.matrix(features)
  if (!nrow(features) || !ncol(features)) {
    stop("features must have at least one row and one column.", call. = FALSE)
  }
  storage.mode(features) <- "double"
  if (any(!is.finite(features))) {
    stop("features must contain only finite numeric values.", call. = FALSE)
  }
  if (!is.null(labels) && nrow(features) != length(labels)) {
    stop("features and labels must have the same number of rows.", call. = FALSE)
  }
  features
}

validate_osp_training_data <- function(features, labels, task = c("classification", "regression"), m) {
  task <- match.arg(task)
  features <- validate_feature_matrix(features, labels = labels)
  labels <- if (task == "classification") {
    encode_multiclass_labels(labels, m)
  } else {
    as.numeric(labels)
  }
  if (anyNA(labels) || any(!is.finite(labels))) {
    stop("labels must contain only finite values.", call. = FALSE)
  }
  list(features = features, labels = labels)
}

osp_model_specs <- function(label_methods = c("label_min", "label_mean"),
                            engines = osp_engines(), tasks = osp_tasks()) {
  engines <- vapply(engines, match.arg, character(1), choices = osp_all_engines())
  tasks <- vapply(tasks, match.arg, character(1), choices = osp_tasks())
  specs <- expand.grid(
    label_method = label_methods,
    engine = unname(engines),
    task = unname(tasks),
    stringsAsFactors = FALSE
  )
  specs$model_name <- paste(specs$label_method, specs$engine, specs$task, sep = "_")
  specs
}

set_osp_model_metadata <- function(model, engine, task, m, nrounds) {
  attr(model, "osp_engine") <- engine
  attr(model, "osp_task") <- task
  attr(model, "osp_m") <- as.integer(m)
  attr(model, "osp_nrounds") <- as.integer(nrounds)
  model
}

resolve_osp_model_arg <- function(value, model, attr_name, choices = NULL) {
  if (is.null(value)) {
    value <- attr(model, attr_name, exact = TRUE)
  }
  if (is.null(value)) {
    stop("Missing model setting: ", attr_name, call. = FALSE)
  }
  if (!is.null(choices)) {
    value <- match.arg(value, choices)
  }
  value
}

decode_multiclass_predictions <- function(predictions, n_obs, m) {
  if (length(n_obs) != 1L || is.na(n_obs) || n_obs < 1L) {
    stop("n_obs must be a single positive integer.", call. = FALSE)
  }
  if (is.matrix(predictions)) {
    if (nrow(predictions) == n_obs && ncol(predictions) == m) {
      return(as.integer(max.col(predictions, ties.method = "first") - 1L))
    }
    if (nrow(predictions) == m && ncol(predictions) == n_obs) {
      return(as.integer(max.col(t(predictions), ties.method = "first") - 1L))
    }
  }

  predictions <- as.numeric(predictions)
  if (length(predictions) == n_obs) {
    return(encode_multiclass_labels(round(predictions), m = m))
  }
  if (length(predictions) == n_obs * m) {
    probabilities <- matrix(predictions, nrow = m, ncol = n_obs)
    return(as.integer(max.col(t(probabilities), ties.method = "first") - 1L))
  }

  stop("Cannot decode multiclass predictions with the supplied dimensions.", call. = FALSE)
}

round_interval_predictions <- function(predictions, m, zero_based = TRUE) {
  as_zero_based_interval(predictions, m = m, zero_based = zero_based, clamp = TRUE)
}

train_osp_model <- function(features, labels, engine = c("xgboost", "lightgbm", "majority"),
                            task = c("classification", "regression"), m, nrounds = 100L) {
  engine <- match.arg(engine, osp_all_engines())
  task <- match.arg(task, osp_tasks())
  if (length(nrounds) != 1L || is.na(nrounds) || nrounds < 1L) {
    stop("nrounds must be a single positive integer.", call. = FALSE)
  }
  training_data <- validate_osp_training_data(features, labels, task = task, m = m)
  features <- training_data$features
  labels <- training_data$labels

  if (engine == "majority") {
    value <- if (task == "classification") {
      tab <- tabulate(labels + 1L, nbins = m)
      as.integer(which.max(tab) - 1L)
    } else {
      finite_mean(labels)
    }
    model <- list(value = value)
    class(model) <- "osp_majority_model"
    return(set_osp_model_metadata(model, engine = engine, task = task, m = m, nrounds = nrounds))
  }

  if (engine == "xgboost") {
    require_packages("xgboost")
    dtrain <- xgboost::xgb.DMatrix(data = features, label = labels)
    params <- if (task == "classification") {
      list(objective = "multi:softmax", num_class = m)
    } else {
      list(objective = "reg:squarederror")
    }
    model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
    return(set_osp_model_metadata(model, engine = engine, task = task, m = m, nrounds = nrounds))
  }

  require_packages("lightgbm")
  dtrain <- lightgbm::lgb.Dataset(data = features, label = labels)
  params <- if (task == "classification") {
    list(objective = "multiclass", num_class = m, metric = "multi_logloss")
  } else {
    list(objective = "regression", metric = "l2")
  }
  model <- lightgbm::lgb.train(data = dtrain, nrounds = nrounds, params = params)
  set_osp_model_metadata(model, engine = engine, task = task, m = m, nrounds = nrounds)
}

predict_osp_interval <- function(model, features, engine = NULL, task = NULL, m = NULL) {
  engine <- resolve_osp_model_arg(engine, model, "osp_engine", osp_all_engines())
  task <- resolve_osp_model_arg(task, model, "osp_task", osp_tasks())
  if (is.null(m)) {
    m <- attr(model, "osp_m", exact = TRUE)
  }
  if (is.null(m)) {
    stop("Missing model setting: osp_m", call. = FALSE)
  }
  features <- validate_feature_matrix(features)

  if (engine == "majority") {
    raw_predictions <- rep(model$value, nrow(features))
    if (task == "classification") {
      return(encode_multiclass_labels(raw_predictions, m = m))
    }
    return(round_interval_predictions(raw_predictions, m = m, zero_based = TRUE))
  }

  raw_predictions <- if (engine == "xgboost") {
    require_packages("xgboost")
    predict(model, xgboost::xgb.DMatrix(data = features))
  } else {
    require_packages("lightgbm")
    predict(model, features)
  }

  if (task == "classification") {
    return(decode_multiclass_predictions(raw_predictions, n_obs = nrow(features), m = m))
  }
  round_interval_predictions(raw_predictions, m = m, zero_based = TRUE)
}

train_osp_model_set <- function(features, labels, m, label_methods = c("label_min", "label_mean"),
                                engines = osp_engines(), tasks = osp_tasks(), nrounds = 100L) {
  labels <- as.data.frame(labels)
  missing <- label_methods[!label_methods %in% names(labels)]
  if (length(missing)) {
    stop("labels is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  specs <- osp_model_specs(label_methods = label_methods, engines = engines, tasks = tasks)
  models <- vector("list", nrow(specs))
  names(models) <- specs$model_name
  for (i in seq_len(nrow(specs))) {
    models[[i]] <- train_osp_model(
      features = features,
      labels = labels[[specs$label_method[[i]]]],
      engine = specs$engine[[i]],
      task = specs$task[[i]],
      m = m,
      nrounds = nrounds
    )
  }
  attr(models, "osp_specs") <- specs
  class(models) <- c("osp_model_set", "list")
  models
}

predict_osp_model_set <- function(model_set, features) {
  specs <- attr(model_set, "osp_specs", exact = TRUE)
  if (is.null(specs)) {
    stop("model_set is missing OSP model specifications.", call. = FALSE)
  }
  predictions <- lapply(model_set, predict_osp_interval, features = features)
  out <- as.data.frame(predictions, check.names = FALSE)
  names(out) <- specs$model_name
  out
}

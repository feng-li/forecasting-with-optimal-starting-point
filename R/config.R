# Small config helpers for the cleaned workflow.

parse_config_scalar <- function(value) {
  value <- trimws(value)
  value <- sub("^['\"]", "", sub("['\"]$", "", value))
  if (identical(tolower(value), "true")) {
    return(TRUE)
  }
  if (identical(tolower(value), "false")) {
    return(FALSE)
  }
  if (grepl("^[-+]?[0-9]+$", value)) {
    return(as.integer(value))
  }
  if (grepl("^[-+]?[0-9]*[.][0-9]+$", value)) {
    return(as.numeric(value))
  }
  value
}

parse_config_key <- function(key) {
  as.character(parse_config_scalar(key))
}

read_simple_yaml_config <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- sub("[[:space:]]+#.*$", "", lines)
  lines <- lines[nzchar(trimws(lines))]
  config <- list()
  current_key <- NULL

  for (line in lines) {
    indent <- nchar(line) - nchar(sub("^\\s+", "", line))
    text <- trimws(line)
    if (indent == 0L && grepl("^[^:]+:", text)) {
      key <- parse_config_key(sub(":.*$", "", text))
      value <- trimws(sub("^[^:]+:", "", text))
      if (nzchar(value)) {
        config[[key]] <- parse_config_scalar(value)
        current_key <- NULL
      } else {
        config[[key]] <- list()
        current_key <- key
      }
    } else if (!is.null(current_key) && grepl("^-\\s+", text)) {
      item <- sub("^-\\s+", "", text)
      if (grepl("^[^:]+:", item)) {
        key <- parse_config_key(sub(":.*$", "", item))
        value <- trimws(sub("^[^:]+:", "", item))
        config[[current_key]][[length(config[[current_key]]) + 1L]] <- list()
        config[[current_key]][[length(config[[current_key]])]][[key]] <- parse_config_scalar(value)
      } else {
        config[[current_key]][[length(config[[current_key]]) + 1L]] <- parse_config_scalar(item)
      }
    } else if (!is.null(current_key) && length(config[[current_key]]) && grepl("^[^:]+:", text)) {
      key <- parse_config_key(sub(":.*$", "", text))
      value <- trimws(sub("^[^:]+:", "", text))
      item_id <- length(config[[current_key]])
      if (!is.list(config[[current_key]][[item_id]])) {
        stop("Unsupported config shape near: ", text, call. = FALSE)
      }
      config[[current_key]][[item_id]][[key]] <- parse_config_scalar(value)
    } else {
      stop("Unsupported config line: ", text, call. = FALSE)
    }
  }

  config
}

read_workflow_config <- function(path) {
  if (!file.exists(path)) {
    stop("Config file does not exist: ", path, call. = FALSE)
  }
  if (requireNamespace("yaml", quietly = TRUE)) {
    return(yaml::read_yaml(path))
  }
  read_simple_yaml_config(path)
}

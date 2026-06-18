# Feature extraction wrapper.

extract_ts_features <- function(series_list) {
  require_packages("tsfeatures")
  features <- lapply(series_list, function(series) {
    out <- tsfeatures::tsfeatures(series)
    out$length <- length(series)
    out
  })
  do.call(rbind, features)
}

args <- commandArgs(trailingOnly = TRUE)
tolerance <- 1e-8

default_pairs <- data.frame(
  legacy = c(
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Yearly/ets_Yearly_final_res.csv",
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Yearly/thetaf_Yearly_final_res.csv",
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Quarterly/ets_Quarterly_final_res.csv",
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Quarterly/thetaf_Quarterly_final_res.csv",
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Monthly/ets_Monthly_final_res.csv",
    "OSP-TSP on M4/Time series 5 equal parts (m=5)/Monthly/thetaf_Monthly_final_res.csv"
  ),
  cleaned = c(
    "results/tables/ets_Yearly_final_res.csv",
    "results/tables/thetaf_Yearly_final_res.csv",
    "results/tables/ets_Quarterly_final_res.csv",
    "results/tables/thetaf_Quarterly_final_res.csv",
    "results/tables/ets_Monthly_final_res.csv",
    "results/tables/thetaf_Monthly_final_res.csv"
  ),
  stringsAsFactors = FALSE
)

read_numeric_csv <- function(path) {
  data <- read.csv(path, check.names = FALSE)
  numeric_cols <- vapply(data, is.numeric, logical(1))
  as.matrix(data[, numeric_cols, drop = FALSE])
}

compare_pair <- function(legacy, cleaned) {
  if (!file.exists(legacy)) {
    return(data.frame(legacy = legacy, cleaned = cleaned, status = "missing legacy", max_abs_diff = NA_real_))
  }
  if (!file.exists(cleaned)) {
    return(data.frame(legacy = legacy, cleaned = cleaned, status = "missing cleaned", max_abs_diff = NA_real_))
  }

  legacy_data <- read_numeric_csv(legacy)
  cleaned_data <- read_numeric_csv(cleaned)
  if (!identical(dim(legacy_data), dim(cleaned_data))) {
    return(data.frame(legacy = legacy, cleaned = cleaned, status = "dimension mismatch", max_abs_diff = NA_real_))
  }

  diff <- max(abs(legacy_data - cleaned_data), na.rm = TRUE)
  status <- if (is.finite(diff) && diff <= tolerance) "match" else "different"
  data.frame(legacy = legacy, cleaned = cleaned, status = status, max_abs_diff = diff)
}

if (length(args) == 2L) {
  pairs <- data.frame(legacy = args[[1L]], cleaned = args[[2L]], stringsAsFactors = FALSE)
} else {
  pairs <- default_pairs
}

report <- do.call(
  rbind,
  Map(compare_pair, pairs$legacy, pairs$cleaned)
)

print(report, row.names = FALSE)

if (any(report$status %in% c("different", "dimension mismatch"))) {
  quit(save = "no", status = 1L)
}

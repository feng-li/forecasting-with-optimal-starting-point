# Inventory helpers for legacy generated artifacts.

legacy_artifact_roots <- function() {
  c("OSP-TSP on M4", "Empirical analysis")
}

legacy_artifact_extensions <- function() {
  c("csv", "rdata", "rds", "ipynb")
}

legacy_artifact_type <- function(path) {
  lower_path <- tolower(path)
  extension <- tolower(tools::file_ext(path))

  if (grepl("(^|/)[.]ipynb_checkpoints(/|$)", path)) {
    return("notebook_checkpoint")
  }
  if (identical(extension, "ipynb")) {
    return("legacy_notebook")
  }
  if (extension %in% c("rdata", "rds")) {
    return("serialized_model_or_intermediate")
  }
  if (grepl("features_matrix[.]csv$", lower_path)) {
    return("legacy_feature_matrix")
  }
  if (grepl("final_res[.]csv$", lower_path)) {
    return("legacy_result_table")
  }
  if (identical(extension, "csv") && grepl("(^|/)empirical analysis/", lower_path)) {
    return("empirical_raw_input")
  }
  if (identical(extension, "csv")) {
    return("legacy_csv_artifact")
  }
  "unknown"
}

legacy_artifact_policy <- function(type) {
  switch(
    type,
    empirical_raw_input = "copy to data/raw through scripts/prepare_empirical_data.R; track only with clear provenance",
    legacy_result_table = "keep as legacy reference until cleaned results are compared",
    legacy_feature_matrix = "regenerate under results/intermediate or archive externally",
    serialized_model_or_intermediate = "regenerate under results or archive externally",
    legacy_notebook = "keep only as exploratory record; scripts and targets are canonical",
    notebook_checkpoint = "discard local checkpoint; do not commit",
    legacy_csv_artifact = "classify before tracking; prefer regenerated results/tables output",
    "review manually"
  )
}

legacy_artifact_inventory <- function(roots = legacy_artifact_roots()) {
  roots <- roots[dir.exists(roots)]
  if (!length(roots)) {
    return(data.frame(
      path = character(),
      root = character(),
      directory = character(),
      file = character(),
      extension = character(),
      artifact_type = character(),
      cleanup_policy = character(),
      bytes = numeric(),
      row.names = NULL
    ))
  }

  files <- unlist(lapply(roots, function(root) {
    list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  }), use.names = FALSE)
  files <- files[file.exists(files) & !dir.exists(files)]
  extensions <- tolower(tools::file_ext(files))
  files <- files[extensions %in% legacy_artifact_extensions()]
  files <- sort(files)

  types <- vapply(files, legacy_artifact_type, character(1))
  info <- file.info(files)
  data.frame(
    path = files,
    root = vapply(files, function(path) roots[startsWith(path, roots)][[1L]], character(1)),
    directory = dirname(files),
    file = basename(files),
    extension = tolower(tools::file_ext(files)),
    artifact_type = unname(types),
    cleanup_policy = unname(vapply(types, legacy_artifact_policy, character(1))),
    bytes = as.numeric(info$size),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

legacy_artifact_summary <- function(inventory) {
  if (!nrow(inventory)) {
    return(data.frame(artifact_type = character(), cleanup_policy = character(), count = integer(), bytes = numeric()))
  }
  summary <- aggregate(
    bytes ~ artifact_type + cleanup_policy,
    data = inventory,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  counts <- aggregate(
    path ~ artifact_type + cleanup_policy,
    data = inventory,
    FUN = length
  )
  names(counts)[names(counts) == "path"] <- "count"
  summary <- merge(counts, summary, by = c("artifact_type", "cleanup_policy"), sort = TRUE)
  row.names(summary) <- NULL
  summary
}

write_legacy_artifact_inventory <- function(inventory, root = "results") {
  path <- result_table_path("legacy_artifact_inventory", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(inventory, path, row.names = FALSE)
  path
}

write_legacy_artifact_summary <- function(summary, root = "results") {
  path <- result_table_path("legacy_artifact_summary", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, path, row.names = FALSE)
  path
}

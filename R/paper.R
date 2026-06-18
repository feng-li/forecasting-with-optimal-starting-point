# Helpers for inventorying paper files, paper tables, and future generated-table integration.

paper_source_patterns <- function() {
  c("[.]tex$", "[.]bib$", "[.]cls$", "[.]sty$")
}

paper_figure_patterns <- function() {
  c("[.]pdf$", "[.]jpg$", "[.]jpeg$", "[.]png$")
}

paper_generated_patterns <- function() {
  c("[.]abs$", "[.]aux$", "[.]bbl$", "[.]blg$", "[.]fdb_latexmk$", "[.]fls$", "[.]log$", "[.]out$", "[.]synctex[.]gz$", "[.]xdv$")
}

paper_file_type <- function(path, paper_dir = "docs") {
  normalized <- gsub("\\\\", "/", path)
  rel <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", paper_dir), "/?"), "", normalized)
  lower <- tolower(rel)
  extension <- tolower(tools::file_ext(lower))

  if (grepl("(^|/)[.]git(/|$)", rel)) {
    return("nested_git_metadata")
  }
  if (grepl("(^|/)auto/", lower) || grepl(paste(paper_generated_patterns(), collapse = "|"), lower)) {
    return("latex_generated")
  }
  if (identical(basename(lower), "main.pdf")) {
    return("paper_pdf")
  }
  if (identical(extension, "zip")) {
    return("archive")
  }
  if (grepl(paste(paper_source_patterns(), collapse = "|"), lower)) {
    return("paper_source")
  }
  if (grepl(paste(paper_figure_patterns(), collapse = "|"), lower)) {
    return("paper_asset")
  }
  if (extension %in% c("el", "manifest")) {
    return("editor_or_manifest")
  }
  "other"
}

paper_file_policy <- function(type) {
  switch(
    type,
    paper_source = "track as source when docs is integrated",
    paper_asset = "track as source asset when needed by main.tex",
    paper_pdf = "archive final submitted PDF intentionally, otherwise regenerate",
    latex_generated = "ignore or delete locally; regenerate with latexmk",
    nested_git_metadata = "decide top-level tracked docs versus proper submodule; do not commit nested .git internals",
    archive = "archive intentionally or omit from source tree",
    editor_or_manifest = "review before tracking",
    "review manually"
  )
}

paper_file_inventory <- function(paper_dir = "docs") {
  if (!dir.exists(paper_dir)) {
    stop("Paper directory does not exist: ", paper_dir, call. = FALSE)
  }
  files <- list.files(paper_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- sort(files)
  types <- vapply(files, paper_file_type, character(1), paper_dir = paper_dir)
  info <- file.info(files)
  data.frame(
    path = files,
    directory = dirname(files),
    file = basename(files),
    extension = tolower(tools::file_ext(files)),
    file_type = unname(types),
    cleanup_policy = unname(vapply(types, paper_file_policy, character(1))),
    bytes = as.numeric(info$size),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

paper_file_summary <- function(inventory) {
  if (!nrow(inventory)) {
    return(data.frame(file_type = character(), cleanup_policy = character(), count = integer(), bytes = numeric()))
  }
  summary <- aggregate(
    bytes ~ file_type + cleanup_policy,
    data = inventory,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  counts <- aggregate(
    path ~ file_type + cleanup_policy,
    data = inventory,
    FUN = length
  )
  names(counts)[names(counts) == "path"] <- "count"
  summary <- merge(counts, summary, by = c("file_type", "cleanup_policy"), sort = TRUE)
  row.names(summary) <- NULL
  summary
}

write_paper_file_inventory <- function(inventory, root = "results") {
  path <- result_table_path("paper_file_inventory", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(inventory, path, row.names = FALSE)
  path
}

write_paper_file_summary <- function(summary, root = "results") {
  path <- result_table_path("paper_file_summary", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(summary, path, row.names = FALSE)
  path
}

paper_status <- function(paper_dir = "docs", paper = file.path(paper_dir, "main.tex")) {
  files <- if (dir.exists(paper_dir)) {
    list.files(paper_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  } else {
    character()
  }
  data.frame(
    check = c("paper_source", "latexmk", "nested_git", "source_like_files", "generated_like_files"),
    value = c(
      file.exists(paper),
      nzchar(Sys.which("latexmk")),
      dir.exists(file.path(paper_dir, ".git")),
      sum(grepl(paste(c(paper_source_patterns(), paper_figure_patterns()), collapse = "|"), files)),
      sum(grepl(paste(paper_generated_patterns(), collapse = "|"), files))
    ),
    row.names = NULL
  )
}

strip_latex_comments <- function(lines) {
  sub("(?<!\\\\)%.*$", "", lines, perl = TRUE)
}

extract_latex_braced_value <- function(text, command) {
  pattern <- paste0("\\\\", command, "\\{([^}]*)\\}")
  match <- regexec(pattern, text, perl = TRUE)
  hit <- regmatches(text, match)[[1L]]
  if (length(hit) < 2L) {
    return(NA_character_)
  }
  trimws(hit[[2L]])
}

paper_table_category <- function(label, caption) {
  if (!is.na(label)) {
    if (label %in% c("tab2")) {
      return("m4_metadata")
    }
    if (label %in% c("tab3", "tab4", "tab5", "tab6", "tab10", "tab11", "tab12", "tab13")) {
      return("m4_results")
    }
    if (label %in% c("tab7", "tab8", "tab9")) {
      return("empirical_results")
    }
  }
  text <- tolower(paste(label, caption))
  if (grepl("tab2|number of m4", text)) {
    return("m4_metadata")
  }
  if (grepl("tab3|tab4|tab5|tab6|tab10|m4|weekly|daily|hourly|combined prediction", text)) {
    return("m4_results")
  }
  if (grepl("tab7|tab8|tab9|real world|pre-trained|gratis", text)) {
    return("empirical_results")
  }
  "unknown"
}

paper_table_artifact_name <- function(label) {
  map <- c(
    tab1 = "paper_table_01_method_summary",
    tab2 = "paper_table_02_m4_frequency_domain_counts",
    tab3 = "paper_table_03_m4_m5_seasonal_results",
    tab4 = "paper_table_04_m4_m10_seasonal_results",
    tab5 = "paper_table_05_m4_high_frequency_results",
    tab6 = "paper_table_06_feature_importance",
    tab7 = "paper_table_07_empirical_direct_results",
    tab8 = "paper_table_08_empirical_pretrained_m4_results",
    tab9 = "paper_table_09_empirical_gratis_results",
    tab10 = "paper_table_10_combination_results",
    tab11 = "paper_table_11_nnetar_m4_results",
    tab12 = "paper_table_12_change_point_comparison",
    tab13 = "paper_table_13_fforma_weekly_results"
  )
  if (!is.na(label) && label %in% names(map)) {
    return(unname(map[[label]]))
  }
  paste0("paper_table_", ifelse(is.na(label), "unknown", label))
}

paper_table_inventory <- function(path = file.path("docs", "main.tex")) {
  if (!file.exists(path)) {
    stop("Paper source not found: ", path, call. = FALSE)
  }
  raw_lines <- readLines(path, warn = FALSE)
  lines <- strip_latex_comments(raw_lines)
  starts <- grep("\\\\begin\\{table\\}", lines)
  ends <- grep("\\\\end\\{table\\}", lines)
  if (length(starts) != length(ends)) {
    stop("Mismatched table begin/end blocks in paper source.", call. = FALSE)
  }

  rows <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    block_end <- ends[which(ends > starts[[i]])[[1L]]]
    block <- lines[starts[[i]]:block_end]
    collapsed <- paste(block, collapse = " ")
    label <- extract_latex_braced_value(collapsed, "label")
    caption_text <- gsub("\\\\label\\{[^}]*\\}", "", collapsed)
    caption <- extract_latex_braced_value(caption_text, "caption")
    input_matches <- gregexpr("\\\\input\\{([^}]*)\\}", collapsed, perl = TRUE)
    inputs <- regmatches(collapsed, input_matches)[[1L]]
    inputs <- if (identical(inputs, character(0)) || identical(inputs, -1L)) {
      ""
    } else {
      paste(sub("^\\\\input\\{([^}]*)\\}$", "\\1", inputs), collapse = ";")
    }

    rows[[i]] <- data.frame(
      table_index = i,
      label = label,
      caption = caption,
      category = paper_table_category(label, caption),
      expected_artifact = paper_table_artifact_name(label),
      begin_line = starts[[i]],
      end_line = block_end,
      uses_input = nzchar(inputs),
      input_files = inputs,
      tabular_count = length(grep("\\\\begin\\{tabular\\}", block)),
      uses_resizebox = any(grepl("\\\\resizebox\\{", block)),
      row.names = NULL
    )
  }

  do.call(rbind, rows)
}

write_paper_table_inventory <- function(inventory, root = "results") {
  path <- result_table_path("paper_table_inventory", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(inventory, path, row.names = FALSE)
  path
}

paper_supporting_outputs <- function(label, category) {
  if (identical(category, "m4_results")) {
    return(c("smoke_m4_evaluation.csv", "smoke_m4_model_evaluation.csv"))
  }
  if (identical(category, "empirical_results")) {
    return(c("empirical_baseline_evaluation.csv", "empirical_run_manifest.csv"))
  }
  character()
}

paper_table_status <- function(inventory, root = "results") {
  rows <- vector("list", nrow(inventory))
  for (i in seq_len(nrow(inventory))) {
    expected_path <- result_table_path(inventory$expected_artifact[[i]], root = root)
    support <- paper_supporting_outputs(inventory$label[[i]], inventory$category[[i]])
    support_paths <- if (length(support)) file.path(root, "tables", support) else character()
    final_exists <- file.exists(expected_path)
    support_exists <- if (length(support_paths)) all(file.exists(support_paths)) else FALSE
    rows[[i]] <- data.frame(
      table_index = inventory$table_index[[i]],
      label = inventory$label[[i]],
      category = inventory$category[[i]],
      expected_artifact = inventory$expected_artifact[[i]],
      expected_path = expected_path,
      final_artifact_exists = final_exists,
      supporting_outputs = paste(support, collapse = ";"),
      supporting_outputs_exist = support_exists,
      status = if (final_exists) {
        "final artifact available"
      } else if (support_exists) {
        "supporting outputs available"
      } else {
        "missing generated outputs"
      },
      row.names = NULL
    )
  }
  do.call(rbind, rows)
}

write_paper_table_status <- function(status, root = "results") {
  path <- result_table_path("paper_table_status", root = root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(status, path, row.names = FALSE)
  path
}

finite_column_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  mean(x)
}

summarize_metrics_by <- function(table, groups, metrics = c("MAPE", "MASE")) {
  groups <- groups[groups %in% names(table)]
  metrics <- metrics[metrics %in% names(table)]
  if (!length(groups) || !length(metrics)) {
    stop("summary groups and metrics must exist in the table.", call. = FALSE)
  }
  aggregate(table[metrics], table[groups], finite_column_mean)
}

build_available_paper_tables <- function(root = "results") {
  out <- list()

  smoke_path <- result_table_path("smoke_m4_model_evaluation", root = root)
  if (file.exists(smoke_path)) {
    smoke <- read.csv(smoke_path, check.names = FALSE)
    smoke_summary <- summarize_metrics_by(
      smoke,
      groups = c("model_name", "label_method", "engine", "task"),
      metrics = c("MAPE", "MASE")
    )
    path <- result_table_path("paper_available_m4_smoke_model_summary", root = root)
    write.csv(smoke_summary, path, row.names = FALSE)
    out[[length(out) + 1L]] <- data.frame(artifact = "paper_available_m4_smoke_model_summary", path = path)
  }

  empirical_path <- result_table_path("empirical_baseline_evaluation", root = root)
  if (file.exists(empirical_path)) {
    empirical <- read.csv(empirical_path, check.names = FALSE)
    empirical_summary <- summarize_metrics_by(
      empirical,
      groups = c("dataset", "frequency", "baseline_model"),
      metrics = c("MAPE", "MASE")
    )
    path <- result_table_path("paper_available_empirical_baseline_summary", root = root)
    write.csv(empirical_summary, path, row.names = FALSE)
    out[[length(out) + 1L]] <- data.frame(artifact = "paper_available_empirical_baseline_summary", path = path)
  }

  if (!length(out)) {
    return(data.frame(artifact = character(), path = character()))
  }
  do.call(rbind, out)
}

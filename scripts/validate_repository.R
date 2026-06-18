args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=")
  value <- args[grepl(pattern, args)]
  if (length(value)) {
    return(sub(pattern, "", value[[length(value)]]))
  }
  default
}

source(file.path("R", "paths.R"))

rscript <- file.path(R.home("bin"), "Rscript")

clean_parse_expr <- paste(
  "files <- c(list.files('R', '[.]R$', full.names = TRUE), Sys.glob('scripts/*.R'), '_targets.R');",
  "for (f in files) parse(file = f);",
  "cat('parsed', length(files), 'clean R/script/target files\\n')"
)

legacy_parse_expr <- paste(
  "files <- c(Sys.glob('OSP-TSP on M4/Time series 10 equal parts (m=10)/*/*.r'),",
  "Sys.glob('OSP-TSP on M4/Time series 5 equal parts (m=5)/*/*.r'),",
  "Sys.glob('Empirical analysis/*/*.r'));",
  "files <- unique(files[file.exists(files)]);",
  "for (f in files) parse(file = f);",
  "cat('parsed', length(files), 'legacy files\\n')"
)

known_bug_pattern <- paste0(
  "opt_pre_result1|gratislist_信心指数|num_class=5|num_class = 5|",
  "as\\.matrix\\(as\\.factor\\((train_label(?:_min|_mean)?|train_label1)\\)\\)|",
  "length5\\[k,\\]=c\\(0,1\\*m_l,2\\*m_l,3\\*m_l,4\\*m_l\\)"
)

checks <- list(
  list(name = "environment", command = rscript, args = c("scripts/check_environment.R")),
  list(name = "dependency_install_dry_run", command = rscript, args = c("scripts/install_dependencies.R", "--dry-run", "--role=full_reproduction")),
  list(name = "unit_tests", command = rscript, args = c("scripts/run_tests.R")),
  list(name = "m4_smoke_script", command = rscript, args = c("scripts/smoke_m4.R")),
  list(name = "m4_smoke_command", command = rscript, args = c("scripts/run_m4.R", "--mode=smoke")),
  list(name = "m4_targets", command = rscript, args = c("scripts/run_m4.R", "--mode=targets")),
  list(name = "m4_full_dry_run", command = rscript, args = c("scripts/run_m4.R", "--mode=full", "--dry-run", "--frequency=Yearly", "--base-model=ets", "--m=5", "--n=4")),
  list(name = "m4_data_status", command = rscript, args = c("scripts/check_m4_data.R")),
  list(name = "m4_output_inventory", command = rscript, args = c("scripts/inventory_m4_outputs.R")),
  list(name = "empirical_prepare_dry_run", command = rscript, args = c("scripts/prepare_empirical_data.R", "--dry-run")),
  list(name = "empirical_manifest", command = rscript, args = c("scripts/run_empirical.R")),
  list(name = "empirical_baseline_smoke", command = rscript, args = c("scripts/run_empirical.R", "--baseline", "--dataset=GDP", "--method=M4", "--m=5", "--n=4", "--models=thetaf", "--max-series=2")),
  list(name = "paper_check", command = rscript, args = c("scripts/render_paper.R", "--check")),
  list(name = "paper_file_inventory", command = rscript, args = c("scripts/inventory_paper_files.R")),
  list(name = "paper_table_inventory", command = rscript, args = c("scripts/inventory_paper_tables.R")),
  list(name = "paper_table_build_status", command = rscript, args = c("scripts/build_paper_tables.R")),
  list(name = "legacy_artifact_inventory", command = rscript, args = c("scripts/inventory_legacy_artifacts.R")),
  list(name = "repository_hygiene", command = rscript, args = c("scripts/check_repository_hygiene.R")),
  list(name = "legacy_compare", command = rscript, args = c("scripts/compare_legacy_results.R")),
  list(name = "parse_clean", command = rscript, args = c("-e", clean_parse_expr)),
  list(name = "parse_legacy", command = rscript, args = c("-e", legacy_parse_expr)),
  list(
    name = "unsafe_legacy_load_audit",
    command = "rg",
    args = c("-n", "eval\\s*\\(\\s*parse\\s*\\(", "-g", "*.r", "OSP-TSP on M4", "Empirical analysis"),
    expected_status = c(1L),
    success_note = "no unsafe legacy loads"
  ),
  list(
    name = "known_bug_pattern_audit",
    command = "rg",
    args = c("-n", known_bug_pattern, "-g", "*.r", "-g", "*.R", "-g", "!scripts/validate_repository.R"),
    expected_status = c(1L),
    success_note = "no matches"
  )
)

run_check <- function(check) {
  expected_status <- check$expected_status %||% 0L
  started <- Sys.time()
  shell_args <- if (length(check$args)) shQuote(check$args) else character()
  output <- tryCatch(
    suppressWarnings(system2(check$command, shell_args, stdout = TRUE, stderr = TRUE)),
    error = function(error) {
      structure(conditionMessage(error), status = 127L)
    }
  )
  status <- attr(output, "status", exact = TRUE)
  if (is.null(status)) {
    status <- 0L
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  output_tail <- paste(tail(as.character(output), 8L), collapse = " | ")
  if (nchar(output_tail) > 1000L) {
    output_tail <- paste0(substr(output_tail, 1L, 997L), "...")
  }
  success <- status %in% expected_status
  data.frame(
    check = check$name,
    command = paste(c(check$command, check$args), collapse = " "),
    status = as.integer(status),
    expected_status = paste(expected_status, collapse = ","),
    success = success,
    elapsed_seconds = round(elapsed, 3L),
    note = if (success) (check$success_note %||% "ok") else "failed",
    output_tail = output_tail,
    row.names = NULL
  )
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

summary <- do.call(rbind, lapply(checks, run_check))
print(summary[c("check", "status", "expected_status", "success", "elapsed_seconds", "note")], row.names = FALSE)

path <- result_table_path("validation_summary", root = arg_value("results-root", "results"))
dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
write.csv(summary, path, row.names = FALSE)
message("Validation summary written: ", path)

if (any(!summary$success)) {
  quit(save = "no", status = 1L)
}

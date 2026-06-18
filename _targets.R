if (!requireNamespace("targets", quietly = TRUE)) {
  stop("The targets package is required to run this pipeline.", call. = FALSE)
}

source(file.path("R", "pipeline_smoke.R"))

list(
  targets::tar_target(dependency_manifest, read_dependency_manifest()),
  targets::tar_target(dependency_status_table, dependency_status(dependency_manifest)),
  targets::tar_target(dependency_status_path, write_dependency_status(dependency_status_table), format = "file"),
  targets::tar_target(m4_config, read_m4_config()),
  targets::tar_target(m4_data_status_table, m4_data_status()),
  targets::tar_target(m4_data_status_path, write_m4_data_status(m4_data_status_table), format = "file"),
  targets::tar_target(m4_configured_run_grid, expand_m4_config(m4_config)),
  targets::tar_target(m4_configured_run_grid_path, write_m4_configured_run_grid(m4_configured_run_grid), format = "file"),
  targets::tar_target(m4_expected_artifacts, m4_expected_artifact_manifest(m4_configured_run_grid)),
  targets::tar_target(m4_expected_artifacts_path, write_m4_expected_artifact_manifest(m4_expected_artifacts), format = "file"),
  targets::tar_target(pipeline_config, read_pipeline_config()),
  targets::tar_target(m4_pipeline_spec_target, m4_pipeline_spec(pipeline_config, m4_config)),
  targets::tar_target(m4_pipeline_result, run_m4_pipeline_spec(m4_pipeline_spec_target)),
  targets::tar_target(m4_pipeline_table_paths, write_m4_pipeline_tables(m4_pipeline_result, m4_pipeline_spec_target), format = "file"),
  targets::tar_target(m4_pipeline_manifest_path, write_m4_pipeline_manifest(m4_pipeline_spec_target, m4_pipeline_table_paths), format = "file"),
  targets::tar_target(smoke_pipeline_result, run_smoke_pipeline(base_model = "ets", m = 5L, n = 4L)),
  targets::tar_target(smoke_starts, smoke_pipeline_result$starts),
  targets::tar_target(smoke_label, smoke_pipeline_result$label),
  targets::tar_target(smoke_error_summary, smoke_pipeline_result$summary),
  targets::tar_target(smoke_m4_candidate_result, run_smoke_m4_candidate_workflow(base_model = "ets", m = 5L, n = 4L)),
  targets::tar_target(smoke_m4_features, smoke_m4_candidate_result$features),
  targets::tar_target(smoke_m4_training_table, smoke_m4_candidate_result$training_table),
  targets::tar_target(smoke_m4_candidate_labels, smoke_m4_candidate_result$labels),
  targets::tar_target(smoke_m4_evaluation, smoke_m4_candidate_result$evaluation_mean),
  targets::tar_target(smoke_m4_evaluation_summary, smoke_m4_candidate_result$evaluation_summary_mean),
  targets::tar_target(smoke_m4_model_evaluation, smoke_m4_candidate_result$model_evaluation),
  targets::tar_target(smoke_m4_model_evaluation_summary, smoke_m4_candidate_result$model_evaluation_summary),
  targets::tar_target(smoke_m4_table_paths, write_smoke_m4_tables(root = "results"), format = "file"),
  targets::tar_target(empirical_config, read_empirical_config()),
  targets::tar_target(empirical_grid, expand_empirical_config(empirical_config)),
  targets::tar_target(empirical_manifest_path, write_empirical_manifest(empirical_grid), format = "file"),
  targets::tar_target(empirical_data_manifest, prepare_empirical_raw_data(empirical_config)),
  targets::tar_target(empirical_data_manifest_path, write_empirical_data_manifest(empirical_data_manifest), format = "file"),
  targets::tar_target(empirical_raw_data_paths, empirical_data_manifest$destination[empirical_data_manifest$destination_exists], format = "file"),
  targets::tar_target(empirical_baseline_smoke, {
    empirical_raw_data_paths
    dataset <- select_empirical_dataset(empirical_config, "GDP")
    evaluate_empirical_baselines(dataset, models = "thetaf", max_series = 2L)
  }),
  targets::tar_target(empirical_baseline_path, write_empirical_baseline_table(empirical_baseline_smoke), format = "file"),
  targets::tar_target(paper_file_inventory_target, paper_file_inventory()),
  targets::tar_target(paper_file_inventory_path, write_paper_file_inventory(paper_file_inventory_target), format = "file"),
  targets::tar_target(paper_file_summary_target, paper_file_summary(paper_file_inventory_target)),
  targets::tar_target(paper_file_summary_path, write_paper_file_summary(paper_file_summary_target), format = "file"),
  targets::tar_target(paper_table_inventory_target, paper_table_inventory(file.path("docs", "main.tex"))),
  targets::tar_target(paper_table_inventory_path, write_paper_table_inventory(paper_table_inventory_target), format = "file"),
  targets::tar_target(paper_table_status_target, {
    smoke_m4_table_paths
    empirical_manifest_path
    empirical_baseline_path
    paper_table_status(paper_table_inventory_target)
  }),
  targets::tar_target(paper_table_status_path, write_paper_table_status(paper_table_status_target), format = "file"),
  targets::tar_target(paper_available_table_paths, {
    smoke_m4_table_paths
    empirical_baseline_path
    build_available_paper_tables()$path
  }, format = "file"),
  targets::tar_target(legacy_artifact_inventory_table, legacy_artifact_inventory()),
  targets::tar_target(legacy_artifact_inventory_path, write_legacy_artifact_inventory(legacy_artifact_inventory_table), format = "file"),
  targets::tar_target(legacy_artifact_summary_table, legacy_artifact_summary(legacy_artifact_inventory_table)),
  targets::tar_target(legacy_artifact_summary_path, write_legacy_artifact_summary(legacy_artifact_summary_table), format = "file")
)

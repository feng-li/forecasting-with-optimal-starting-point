# Results

Generated outputs from the cleaned workflow should be written here.

```text
results/tables/        Final CSV tables used by the paper
results/models/        Trained OSP model artifacts
results/intermediate/  Candidate forecasts, labels, and feature matrices
```

Files in this directory should be reproducible from scripts and pipeline configuration. Avoid editing generated result files manually.

The smoke workflow and `targets` smoke/status pipeline write `results/tables/smoke_m4_summary.csv`, `results/tables/smoke_m4_labels.csv`, `results/tables/smoke_m4_evaluation.csv`, and `results/tables/smoke_m4_model_evaluation.csv`. They are ignored by Git because they are generated.

The configured M4 candidate/evaluation stage writes files named like:

```text
results/tables/m4_configured_run_grid.csv
results/tables/m4_data_status.csv
results/tables/m4_expected_artifacts.csv
results/tables/m4_pipeline_manifest.csv
results/tables/yearly_ets_m5_n4_candidate_summary.csv
results/tables/yearly_ets_m5_n4_labels.csv
results/tables/yearly_ets_m5_n4_evaluation_label_mean.csv
results/tables/yearly_ets_m5_n4_evaluation_label_min.csv
results/tables/yearly_ets_m5_n4_osp_model_evaluation.csv
results/tables/yearly_ets_m5_n4_osp_model_evaluation_summary.csv
results/tables/m4_run_manifest.csv
```

The configured run grid, data-source status, and expected-artifacts manifest can be generated without `M4comp2018`; they record the full run matrix from `config/m4.yml`, M4 data availability, expected table paths, and whether those tables already exist locally. The minimal `targets` M4 execution is controlled by `config/pipeline.yml` and records its produced files in `m4_pipeline_manifest.csv`.

The empirical config check, preparation script, and `targets` smoke/status pipeline write:

```text
results/tables/empirical_data_manifest.csv
results/tables/empirical_baseline_evaluation.csv
results/tables/empirical_run_manifest.csv
```

The data manifest records copied raw empirical files and legacy sources. The baseline evaluation table records cleaned holdout accuracy for configured raw empirical datasets and simple forecast baselines. The run manifest records expected raw empirical files, mapped legacy scripts, segmentation settings, and input availability.

The paper table inventory scripts and `targets` smoke/status pipeline write:

```text
results/tables/paper_file_inventory.csv
results/tables/paper_file_summary.csv
results/tables/paper_available_empirical_baseline_summary.csv
results/tables/paper_available_m4_smoke_model_summary.csv
results/tables/paper_table_status.csv
results/tables/paper_table_inventory.csv
```

The file inventory separates paper source files, assets, generated LaTeX files, archived files, final PDFs, and nested-git metadata. The table inventory records each table block in `docs/main.tex`, its label, caption, category, line range, and expected future generated artifact name. The status table records whether each final paper artifact exists or only supporting cleaned outputs are available. The available summary files are not final paper tables; they summarize current smoke and baseline outputs until full reproduction outputs exist.

The validation orchestrator writes:

```text
results/tables/dependency_status.csv
results/tables/legacy_artifact_inventory.csv
results/tables/legacy_artifact_summary.csv
results/tables/validation_summary.csv
```

The dependency status records the package manifest, installed flag, and local version. The legacy artifact inventory records old CSV, `.RData`, `.rds`, notebook, and notebook checkpoint files with a cleanup policy for each class. The validation summary records each standard cleanup check, command, exit status, elapsed time, and the tail of command output.

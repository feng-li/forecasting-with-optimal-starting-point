# Project Cleanup Plan

## Goal

Construct a clean, reproducible repository for the OSP-TSP forecasting project while preserving the ability to reproduce the finished paper in `docs/main.tex`.

The cleanup should prioritize correctness first, then reproducibility, then maintainability and documentation.

## Implementation Status

Started in this cleanup pass:

- Known hard-coded class-count and interval-offset mistakes have been corrected in legacy scripts.
- The copied empirical GRATIS artifact names for imports and exchange rates have been corrected.
- Multiclass labels now use numeric zero-based labels instead of `as.factor()` matrices in the migrated legacy training calls.
- Legacy `.r` script `.RData` artifact loads now use explicit load environments instead of `eval(parse(text = load(...)))`.
- Minimal reusable R helpers now live in `R/`.
- Unit tests now cover interval generation, labels, metric summaries, artifact loading, and label validation.
- A smoke workflow is available at `scripts/smoke_m4.R`.
- The main M4 entry point supports `Rscript scripts/run_m4.R --mode=smoke`.
- Legacy-result comparison scaffolding is available at `scripts/compare_legacy_results.R`.
- Environment checking is available at `scripts/check_environment.R`.
- Dependency expectations are recorded in `config/dependencies.csv`, with status reporting in `results/tables/dependency_status.csv` and dry-run installation support in `scripts/install_dependencies.R`.
- Initial repository documentation and artifact directories have been added.
- `forecast_from_interval()`, interval prediction averaging, model prediction decoding, and artifact path helpers have been added.
- M4-shaped workflow helpers now cover frequency filtering, deterministic train/test split creation, candidate forecast errors, and min/mean interval labels.
- M4-shaped workflow helpers now extract time-series features and assemble feature/label training tables for the OSP model stage.
- M4-shaped workflow helpers now select interval forecasts and compute holdout accuracy metrics for the selected intervals.
- M4-shaped workflow helpers now train OSP interval models on the configured train split and evaluate predicted intervals on the held-out split; the smoke path exercises this with a deterministic `majority` baseline engine, while full runs can request `xgboost` and `lightgbm`.
- OSP model helpers now validate feature/label shapes, enumerate label-method/engine/task model specs, preserve model metadata, and provide optional `xgboost`/`lightgbm` training/prediction wrappers.
- `config/m4.yml` can now be read by the cleaned workflow, expanded into frequency/base-model/segmentation run grids, and inspected with `Rscript scripts/run_m4.R --mode=full --dry-run`.
- `Rscript scripts/run_m4.R --mode=smoke` now writes an interval-level summary to `results/tables/smoke_m4_summary.csv`, a synthetic M4 label table to `results/tables/smoke_m4_labels.csv`, selected-interval oracle evaluation metrics to `results/tables/smoke_m4_evaluation.csv`, and held-out OSP model evaluation metrics to `results/tables/smoke_m4_model_evaluation.csv`.
- `Rscript scripts/run_m4.R --mode=full` now runs the configured M4 candidate/evaluation stage when `M4comp2018` is installed and writes per-run candidate summaries, labels, selected-interval evaluation tables, and `results/tables/m4_run_manifest.csv`.
- `Rscript scripts/run_m4.R --mode=full --osp-engine=xgboost,lightgbm` is wired to train and evaluate OSP models when `M4comp2018`, `xgboost`, and `lightgbm` are installed.
- `Rscript scripts/run_m4.R --mode=full --m4-data=data/raw/m4_records.rds` can now run against a local M4 record snapshot shaped like `M4comp2018::M4`; the command still falls back to `M4comp2018` when available.
- `Rscript scripts/run_m4.R --mode=targets` now runs the current `targets` smoke/status pipeline.
- `Rscript scripts/check_m4_data.R` now reports whether a local M4 RDS snapshot or `M4comp2018` package source is available and writes `results/tables/m4_data_status.csv`.
- `Rscript scripts/inventory_m4_outputs.R` now writes the configured M4 run grid and expected output-table manifest without requiring `M4comp2018`.
- `config/empirical.yml` can now be read by the cleaned workflow, expanded into dataset/method/segmentation run grids, and inspected with `Rscript scripts/run_empirical.R --dry-run`.
- `Rscript scripts/prepare_empirical_data.R` now copies configured empirical CSV inputs from the legacy folders into ignored local `data/raw/` files and writes `results/tables/empirical_data_manifest.csv`.
- `Rscript scripts/run_empirical.R` now writes `results/tables/empirical_run_manifest.csv` and reports missing raw empirical files without executing legacy computations.
- `Rscript scripts/run_empirical.R --baseline` now runs a cleaned empirical baseline evaluation on configured raw datasets and writes `results/tables/empirical_baseline_evaluation.csv`.
- `Rscript scripts/render_paper.R --check` now reports paper-source, `latexmk`, nested-git, source-like-file, and generated-file status without rendering.
- `Rscript scripts/inventory_paper_files.R` now inventories paper source, asset, generated, archive, final-PDF, and nested-git files with cleanup policies.
- `Rscript scripts/inventory_paper_tables.R` now inventories the 12 labeled table blocks in `docs/main.tex` and writes `results/tables/paper_table_inventory.csv` with expected future artifact names.
- `Rscript scripts/build_paper_tables.R` now writes `results/tables/paper_table_status.csv` and available summary tables from current smoke/empirical outputs without pretending they are final paper values.
- `Rscript scripts/check_repository_hygiene.R` now reports nested paper git state, local raw-data CSVs, generated LaTeX files, notebook checkpoints, generated result CSVs, and local `renv/library` state.
- `Rscript scripts/inventory_legacy_artifacts.R` now inventories legacy CSV, `.RData`, `.rds`, notebook, and notebook checkpoint files with cleanup policies.
- `Rscript scripts/validate_repository.R` now runs the standard non-expensive validation checks and writes `results/tables/validation_summary.csv`.
- `Rscript scripts/install_dependencies.R --dry-run --role=full_reproduction` now reports the full-reproduction package install set without making network or library changes.
- The `targets` scaffold now includes synthetic M4 feature, training-table, candidate-label, selected-interval evaluation, dependency-status, M4 output-manifest, empirical-manifest, empirical-baseline smoke, paper-file inventory, paper-table status, and legacy-artifact inventory targets.
- The `targets` pipeline now includes a minimal configurable M4 execution target driven by `config/pipeline.yml`; by default it runs a bounded Yearly/ETS/`m=5,n=4` workflow with `xgboost` classification on 20 M4 series and writes `results/tables/m4_pipeline_manifest.csv`.
- `targets` and `renv` were installed locally; `targets::tar_make()` validates the smoke pipeline.
- `renv.lock` and `renv/` scaffolding have been generated from the installed dependency set that is available in this environment.
- `M4comp2018` is installed locally from the `carlanetto/M4comp2018` GitHub source; `xgboost`, `Ckmeans.1d.dp`, `lightgbm`, and `gratis` are also installed locally.

Still pending:

- Full M4 OSP model training with the paper engines is implemented and can use either `M4comp2018` or an explicit local M4 RDS snapshot; the `targets` pipeline now executes one minimal configured M4 run, while the full paper run matrix and empirical computation logic still need to be migrated into scalable targets.
- `renv` is not auto-activated through `.Rprofile` yet; activation/restore should be revisited before finalizing the reproducibility environment.
- `docs/` still contains a nested `.git` directory and must be intentionally converted to either top-level tracked files or a real submodule.
- Legacy scripts remain in place until refactored outputs are compared against paper results.

## Current State

- The top-level `README.md` now documents the migration status, layout, smoke commands, and paper build command.
- The project still has many duplicated R scripts and notebooks across frequency and segmentation settings.
- Source code, intermediate artifacts, final result CSVs, notebooks, model objects, paper files, and generated LaTeX build files are still mixed together in the legacy directories.
- A dependency lock file now exists for the installed package set that is available in this environment.
- A package manifest now separates core smoke dependencies, workflow tools, environment tooling, and full-reproduction packages.
- There is an expanded `targets` smoke/status scaffold plus one minimal configured M4 execution target, but no full formal paper reproduction pipeline yet.
- The main M4 script now has a config-driven candidate/evaluation/model-evaluation stage and a local M4 snapshot input path, but this is not yet the full paper reproduction pipeline.
- The empirical script now validates configured datasets and writes a manifest, but does not yet reproduce the empirical result tables.
- There are now smoke tests and unit tests for core helpers.
- `docs/` is currently untracked from the top-level repository and also contains its own `.git` directory, so the paper is not cleanly integrated into the project.
- Large generated artifacts such as `.RData`, `.rds`, notebook checkpoints, and LaTeX build outputs are present locally, but most are ignored by `.gitignore`.

## Validation Status

Validated locally on 2026-06-18:

- `Rscript scripts/run_tests.R` passes.
- `Rscript scripts/smoke_m4.R` passes.
- `Rscript scripts/run_m4.R --mode=smoke` passes and writes the smoke summary, label, oracle evaluation, and held-out model evaluation tables.
- `Rscript scripts/run_m4.R --mode=full --dry-run --frequency=Yearly --base-model=ets --m=5 --n=4` passes and prints the filtered run grid without requiring `M4comp2018`.
- `Rscript scripts/check_m4_data.R` passes and writes `results/tables/m4_data_status.csv`; the current status uses the installed `M4comp2018` source with 100,000 records.
- `Rscript scripts/inventory_m4_outputs.R` passes and writes `results/tables/m4_configured_run_grid.csv` and `results/tables/m4_expected_artifacts.csv`.
- `Rscript scripts/run_m4.R --mode=targets` passes for the current smoke/status target scaffold, including held-out OSP model evaluation, dependency-status, M4 output-manifest, empirical-manifest, empirical-baseline smoke, paper-file inventory, paper-table status, and legacy-artifact inventory targets.
- `Rscript -e 'targets::tar_make(names = c(m4_pipeline_manifest_path))'` passes and writes `results/tables/m4_pipeline_manifest.csv` plus candidate, label, selected-interval evaluation, and OSP-model evaluation tables for the minimal configured M4 target.
- `Rscript scripts/run_empirical.R --dry-run --dataset=GDP --method=M4 --m=5 --n=4` passes and prints the filtered empirical run grid.
- `Rscript scripts/prepare_empirical_data.R --dry-run` passes and identifies all five legacy empirical CSV sources.
- `Rscript scripts/prepare_empirical_data.R` passes, writes ignored local raw CSV copies under `data/raw/`, and writes `results/tables/empirical_data_manifest.csv`.
- `Rscript scripts/run_empirical.R --dataset=GDP --method=M4 --m=5 --n=4` passes and writes `results/tables/empirical_run_manifest.csv` with raw GDP input available locally.
- `Rscript scripts/run_empirical.R --baseline --dataset=GDP --method=M4 --m=5 --n=4 --models=thetaf --max-series=2` passes and writes `results/tables/empirical_baseline_evaluation.csv`.
- `Rscript scripts/render_paper.R --check` passes and reports that `docs/main.tex`, `latexmk`, the nested `docs/.git`, source-like files, and generated LaTeX files are present.
- `Rscript scripts/inventory_paper_files.R` passes and writes `results/tables/paper_file_inventory.csv` and `results/tables/paper_file_summary.csv`.
- `Rscript scripts/inventory_paper_tables.R` passes and writes `results/tables/paper_table_inventory.csv`.
- `Rscript scripts/build_paper_tables.R` passes and writes `results/tables/paper_table_status.csv`, `results/tables/paper_available_m4_smoke_model_summary.csv`, and `results/tables/paper_available_empirical_baseline_summary.csv`.
- `Rscript scripts/check_repository_hygiene.R` passes and reports the known local generated/nested artifacts without failing the migration.
- `Rscript scripts/inventory_legacy_artifacts.R` passes and writes `results/tables/legacy_artifact_inventory.csv` and `results/tables/legacy_artifact_summary.csv`.
- `Rscript scripts/validate_repository.R` passes and writes `results/tables/validation_summary.csv`, including the small empirical baseline smoke check and paper-table build/status check.
- `Rscript scripts/check_environment.R` passes and reports all packages in `config/dependencies.csv` as installed in this environment.
- `Rscript scripts/install_dependencies.R --dry-run --role=full_reproduction` passes and lists the full-reproduction install set without installing packages.
- `M4comp2018` was verified locally with 100,000 records across Yearly, Quarterly, Monthly, Weekly, Daily, and Hourly frequencies.
- `Rscript scripts/run_m4.R --mode=full --frequency=Yearly --base-model=ets --m=5 --n=4 --max-series=2 --results-root=/tmp/osp_m4_verify` passes through the installed `M4comp2018` package fallback.
- `Rscript scripts/run_m4.R --mode=full --frequency=Yearly --base-model=ets --m=5 --n=4 --max-series=20 --osp-engine=xgboost --osp-task=classification --results-root=/tmp/osp_xgb_verify` passes through the installed `xgboost` model path.
- `Rscript -e 'targets::tar_make()'` passes for the expanded smoke/status targets.
- `Rscript scripts/compare_legacy_results.R` runs and reports missing cleaned full-result tables as expected.
- Cleaned `R/` and `scripts/` files parse successfully.
- Legacy `.r` scripts under the M4 and empirical directories parse successfully.
- The known-bug pattern audit has no remaining matches for the fixed artifact names, hard-coded class counts, factor-matrix labels, or five-offset interval vectors.
- The unsafe legacy load audit has no remaining `eval(parse())` matches in executable legacy `.r` files under the M4 and empirical script directories.
- Unit tests cover interval normalization, forecast selection from predicted intervals, selected-interval evaluation metrics, M4 feature/training-table construction, held-out M4 OSP model evaluation, M4 config expansion, empirical config expansion/manifests, empirical data loading and baseline evaluation, paper table inventory/status generation, configured result-table writing, OSP model-spec validation, multiclass prediction decoding, artifact paths, and the synthetic M4 candidate-label workflow.

Missing package dependency for full reproduction in this environment, tracked in `config/dependencies.csv`:

- none

## Phase 1: Correct Known Code Mistakes

These fixes should happen before any broad refactor, because they affect result validity.

1. Fix `m = 10` model class counts.
   - In `OSP-TSP on M4/Time series 10 equal parts (m=10)/**/ets_opt_forecast.r` and `thetaf_opt_forecast.r`, replace hard-coded `num_class = 5` with `num_class = m`.
   - Confirm XGBoost and LightGBM labels are encoded as zero-based integers for multiclass objectives.

2. Fix `m = 10` interval offset generation.
   - In `OSP-TSP on M4/Time series 10 equal parts (m=10)/**/**/*final_result.r`, replace hard-coded five-offset vectors such as:
     ```r
     c(0, 1 * m_l, 2 * m_l, 3 * m_l, 4 * m_l)
     ```
     with a general expression such as:
     ```r
     seq.int(0, by = m_l, length.out = m)
     ```
   - Rename `length5` to a neutral name such as `start_offsets`.

3. Fix artifact name mismatches.
   - In `OSP-TSP on M4/Time series 5 equal parts (m=5)/Hourly/Hourly_ETS_thetaf_final_result.r`, change `Hourly_ets_opt_pre_result1.RData` and `Hourly_thetaf_opt_pre_result1.RData` to the artifact names produced by the upstream scripts.
   - In monthly empirical GRATIS scripts, replace copied names such as `gratislist_信心指数_50_12.RData` with dataset-specific names:
     - `gratislist_Confidence_index_50_12.RData`
     - `gratislist_exchange_rate_50_12.RData`
     - `gratislist_Imports_50_12.RData`

4. Fix start-index consistency.
   - Candidate generation uses `round((i * m_l + j * n_l) + 1):y_l`, but evaluation reconstructs with `y_all[start:length(y_all)]`.
   - Define one helper, for example `make_start_indices(series_length, m, n)`, and use it everywhere.
   - Add tests that prove the candidate forecasts and final evaluation use the same starting points.

5. Replace unsafe `eval(parse(text = load(...)))`.
   - Load artifacts into an explicit environment:
     ```r
     env <- new.env(parent = emptyenv())
     load(path, envir = env)
     env$datalist
     ```
   - Better: migrate new artifacts to `saveRDS()` / `readRDS()` where each file contains exactly one object.

## Phase 2: Define a Clean Repository Structure

Target layout:

```text
.
├── README.md
├── Plan.md
├── renv.lock
├── _targets.R
├── R/
│   ├── features.R
│   ├── intervals.R
│   ├── forecast_candidates.R
│   ├── labels.R
│   ├── osp_models.R
│   ├── evaluation.R
│   └── io.R
├── scripts/
│   ├── run_m4.R
│   ├── run_empirical.R
│   └── render_paper.R
├── config/
│   ├── m4.yml
│   ├── empirical.yml
│   └── dependencies.csv
├── data/
│   ├── raw/
│   └── README.md
├── results/
│   ├── tables/
│   ├── models/
│   └── intermediate/
├── notebooks/
├── docs/
└── tests/
```

Rules:

- Keep reusable logic in `R/`.
- Keep runnable commands in `scripts/`.
- Keep configuration in `config/`, not copied into dozens of scripts.
- Keep generated artifacts under `results/`.
- Keep raw local data under `data/raw/`, with clear notes about provenance and whether it is tracked.
- Keep notebooks as exploratory or explanatory material, not the canonical execution path.

## Phase 3: Build a Reproducible Pipeline

Use `targets` for the main computational workflow.

Initial pipeline stages:

1. Load M4 data by frequency.
2. Split train/test indices once per frequency and store them as pipeline artifacts.
3. Extract time-series features.
4. Generate candidate forecasts for each base model, frequency, `m`, and `n`.
5. Construct OSP interval labels from MASE.
6. Train OSP models using XGBoost and LightGBM.
7. Generate interval-based forecasts.
8. Compute evaluation metrics.
9. Write result tables used by `docs/main.tex`.

The pipeline should support two modes:

- `smoke`: tiny subset for CI and quick local verification.
- `full`: full paper reproduction.

## Phase 4: Refactor Duplicated Scripts Into Functions

Create reusable functions before deleting old scripts.

Core functions:

- `extract_ts_features(series_list)`
- `make_start_indices(series_length, m, n)`
- `forecast_candidate_grid(series, future, base_model, m, n)`
- `label_best_interval(metric_array, method = c("min", "mean"))`
- `train_osp_model(features, labels, engine, task, m)`
- `predict_osp_interval(model, features, engine, task, m)`
- `forecast_from_interval(series, interval, base_model, m, n)`
- `summarize_accuracy(forecasts, actuals)`

Migration approach:

1. Implement functions against one small yearly example.
2. Add tests for those functions.
3. Reproduce one existing yearly `m = 5` result.
4. Expand to all M4 frequencies.
5. Expand to empirical datasets.
6. Archive or remove duplicated legacy scripts only after outputs are matched or intentionally updated.

## Phase 5: Improve Documentation

Update `README.md` with:

- Project purpose and paper title.
- Repository layout.
- Required R version.
- Dependency setup using `renv`.
- Quick smoke-run command.
- Full reproduction command.
- Expected runtime and disk usage.
- How to regenerate paper tables.
- How to render `docs/main.tex`.
- Data provenance and download instructions.
- Which artifacts are tracked, ignored, or externally archived.

Add `data/README.md` with:

- M4 data source and package dependency.
- Real-world dataset descriptions.
- Any preprocessing decisions.
- Expected raw file names.

Add `results/README.md` with:

- Explanation of intermediate and final artifacts.
- Naming convention.
- Which files are safe to delete and regenerate.

Add `docs/README.md` with:

- How to build the paper.
- Required TeX engine.
- Which files are source versus generated output.

## Phase 6: Dependency and Environment Management

1. Initialize `renv`.
2. Record package versions for:
   - `M4comp2018`
   - `forecast`
   - `tsfeatures`
   - `dplyr`
   - `xgboost`
   - `lightgbm`
   - `Matrix`
   - `Ckmeans.1d.dp`
   - `imputeTS`
   - `gratis`
   - `feasts`
   - `targets`
   - `testthat`
   - any paper-rendering dependencies
3. Add a short environment check script:
   ```sh
   Rscript scripts/check_environment.R
   ```
4. Consider a Dockerfile only after the R pipeline is stable.

## Phase 7: Tests and Validation

Add `testthat` tests for:

- Start-index generation for `m = 5` and `m = 10`.
- Label construction using minimum and average MASE.
- Multiclass label encoding.
- Forecast matrix dimensions.
- Artifact path construction.
- Metric summarization with `NA`, `Inf`, and zero values.

Add validation scripts:

- `scripts/smoke_m4.R`: runs one tiny end-to-end M4 subset.
- `scripts/compare_legacy_results.R`: compares refactored outputs with selected legacy CSVs.

Acceptance criteria:

- `Rscript scripts/smoke_m4.R` completes from a clean checkout.
- `targets::tar_make()` runs at least the smoke pipeline.
- Tests pass with `testthat`.
- README instructions are enough for a new user to run the smoke workflow.

## Phase 8: Git and Artifact Hygiene

1. Decide how to handle `docs/`.
   - Preferred: remove nested `docs/.git`, track source paper files in the top-level repository, and ignore generated LaTeX outputs.
   - Alternative: make `docs/` a proper Git submodule if it must remain independently versioned.

2. Expand `.gitignore`.
   - Ignore generated LaTeX files such as `.aux`, `.bbl`, `.blg`, `.fdb_latexmk`, `.fls`, `.log`, `.out`, `.synctex.gz`, and `.xdv`.
   - Ignore generated model/result artifacts unless intentionally archived.
   - Keep raw data policy explicit.

3. Do not commit notebook checkpoints.

4. Decide whether existing result CSVs are:
   - paper artifacts to preserve,
   - regenerated outputs under `results/tables/`, or
   - legacy reference files moved to `legacy/`.

## Phase 9: Paper Integration

After the pipeline is stable:

1. Generate paper result tables from `results/tables/`.
2. Replace manually copied table values in `docs/main.tex` where feasible.
3. Add a script to render the paper:
   ```sh
   Rscript scripts/render_paper.R
   ```
4. Keep the final submitted PDF as an optional archived artifact, but do not mix it with generated build products.

## Suggested Implementation Order

1. Add `Plan.md`.
2. Fix immediate code mistakes from Phase 1.
3. Add smoke tests around the corrected logic.
4. Add `renv` and README setup instructions.
5. Create `R/` helpers and migrate one yearly `m = 5` workflow.
6. Add `targets` smoke pipeline.
7. Migrate remaining M4 workflows.
8. Migrate empirical workflows.
9. Clean artifact layout and paper integration.

## Definition of Done

The repository is clean when:

- A fresh clone can restore dependencies and run a smoke workflow.
- The main workflow has one documented command.
- Known correctness bugs are fixed and covered by tests.
- The paper source is tracked cleanly.
- Generated artifacts are either ignored, reproducible, or explicitly archived.
- Legacy scripts are no longer the only source of truth.

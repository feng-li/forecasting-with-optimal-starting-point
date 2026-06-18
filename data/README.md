# Data

This directory is the intended location for local raw data files used by the cleaned workflow.

## M4 Data

The cleaned M4 workflow expects records shaped like the historical `M4comp2018::M4` object. In this environment, `M4comp2018` is installed from the `carlanetto/M4comp2018` GitHub source because it is not available from CRAN for the installed R version.

Supported source order:

1. `--m4-data=/path/to/m4_records.rds`
2. `OSP_M4_DATA_RDS=/path/to/m4_records.rds`
3. `data/raw/m4_records.rds`
4. `M4comp2018::M4`, if the package is installed

The local RDS file should contain either the M4 record list directly or a list with an `M4` or `records` element. Each record must contain at least `period`, `x`, and `xx`.

Check the configured data source with:

```sh
Rscript scripts/check_m4_data.R
```

Run a filtered full workflow against an explicit local snapshot with:

```sh
Rscript scripts/run_m4.R --mode=full --m4-data=data/raw/m4_records.rds --frequency=Yearly --base-model=ets --m=5 --n=4 --max-series=100
```

## Empirical Data

The legacy repository currently contains empirical CSVs under `Empirical analysis/`. During cleanup, copy or move the following raw files here if the workflow is migrated:

- `GDP_worldbank.csv`
- `industry.csv`
- `Confidence_index100.csv`
- `exchange_rate.csv`
- `Imports.csv`

Keep preprocessing decisions in code, not in manually edited intermediate files.

Prepare local copies from the legacy empirical folders with:

```sh
Rscript scripts/prepare_empirical_data.R
```

This writes ignored CSV copies into `data/raw/` and a generated manifest at `results/tables/empirical_data_manifest.csv`.

Check the expected empirical inputs and mapped legacy scripts with:

```sh
Rscript scripts/run_empirical.R --dry-run
```

Write a manifest to `results/tables/empirical_run_manifest.csv` with:

```sh
Rscript scripts/run_empirical.R
```

The run manifest reports whether each raw file exists under `data/raw/` and whether the corresponding legacy script is still present.

## Tracking Policy

Small public CSV inputs may be tracked if their source and license are clear. Generated features, model objects, `.RData`, `.rds`, and large intermediate files should be regenerated into `results/` or archived externally.

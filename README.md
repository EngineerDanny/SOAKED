# SOAKED

This repository contains the SOAKED NeurIPS E&D paper source, experiment runners, and plotting scripts used to analyze same/other cross-validation with downsampling.

## Reviewer Quick Start

The fastest check does not require downloading the hosted benchmark datasets. It runs a small synthetic SOAKED-style experiment and writes a CSV summary.

```bash
# From the repository root
mkdir -p results/simulation
Rscript experiments/generate_figures.R \
  --n-data=80 \
  --n-splits=4 \
  --n-random-seeds=2 \
  --train-sizes=20,40 \
  --cases=same_distribution \
  --output-csv=results/simulation/reviewer_smoke_test.csv
```

Expected output:

```text
Running case: Sample Size Effect
```

The command should create:

```text
results/simulation/reviewer_smoke_test.csv
```

This smoke test exercises the custom `mlr3resampling::ResamplingSameOtherSizesCV` resampling logic, the same/other train/test construction, model fitting with `rpart`, and result aggregation. It is intentionally small; it is not intended to reproduce the full paper figures.

## Setup

The code is written in R. The smoke test and standard `rpart` workflows need these packages:

```r
install.packages(c(
  "data.table", "mlr3", "mlr3learners", "rpart", "ggplot2",
  "batchtools", "mlr3batchmark", "patchwork", "scales",
  "mlr3resampling"
))
```

Optional learner-robustness scripts additionally use `mlr3extralearners`, `torch`, `ranger`, and `glmnet`.

## Repository Layout

- `paper/`: NeurIPS manuscript source, bibliography, template files, and paper figures.
- `experiments/`: experiment runners.
- `figures/`: plotting scripts that read saved results and render figures.
- `results/`: generated CSV summaries and rendered outputs.
- `data/`: benchmark data location after downloading and unpacking the hosted dataset archive.
- `dist/`: local distribution metadata, including Croissant JSON files.

Important scripts:

- `experiments/generate_figures.R`: synthetic simulation runner used for the reviewer smoke test.
- `experiments/soaked_classif.R`: classification benchmark runner.
- `experiments/soaked_regr.R`: regression and simulation benchmark runner.
- `experiments/soaked_collect.R`: collector for SLURM/batchtools registry runs.
- `figures/plot_soaked.R`: renders a SOAKED p-value/downsampling plot from a `results/*_runs.csv` file.
- `figures/plot_soaked_pairs.R`: renders paired classification summary plots from saved run files.
- `figures/plot_soaked_pairs_regr.R`: renders paired regression summary plots from saved run files.

## Running A Small Hosted-Data Example

The two small simulation datasets are included in this repository, so reviewers can run a processed-data example without downloading the full Dataverse archive:

```bash
Rscript experiments/soaked_regr.R --dataset=iid_easy
Rscript figures/plot_soaked.R --dataset=iid_easy
```

Expected outputs:

```text
results/iid_easy_rpart_runs.csv
results/iid_easy_rpart_pvalue_downsample.png
```

The full benchmark CSVs are hosted separately. Download the dataset archive from the submission's Dataverse link and unpack it at the repository root so that paths like this exist:

```text
data/processed/simulation/iid_easy.csv
data/processed/classification/waveform.csv
```

After unpacking the archive, a small classification example is:

```bash
Rscript experiments/soaked_classif.R --dataset=waveform --learner=rpart
Rscript figures/plot_soaked.R --dataset=waveform
```

Expected outputs:

```text
results/waveform_rpart_runs.csv
results/waveform_rpart_pvalue_downsample.png
```

The second included simulation dataset can be run similarly:

```bash
Rscript experiments/soaked_regr.R --dataset=different
Rscript figures/plot_soaked.R --dataset=different
```

Expected outputs:

```text
results/different_rpart_runs.csv
results/different_rpart_pvalue_downsample.png
```

## Full Benchmark Workflow

The full benchmark suite uses the processed data files under `data/processed/`. Some datasets are large, especially `STL10.csv`, so reviewers may prefer the smoke test or `iid_easy` first.

Run local sequential `rpart` experiments:

```bash
Rscript experiments/soaked_classif.R --dataset=nsch_autism --learner=rpart
Rscript experiments/soaked_classif.R --dataset=stl10 --learner=rpart
Rscript experiments/soaked_classif.R --dataset=waveform --learner=rpart

Rscript experiments/soaked_regr.R --dataset=traffic
Rscript experiments/soaked_regr.R --dataset=qsip_dim.control_vs_qme.15
Rscript experiments/soaked_regr.R --dataset=workers_compensation
Rscript experiments/soaked_regr.R --dataset=different
Rscript experiments/soaked_regr.R --dataset=iid_easy
```

Render per-dataset SOAKED plots:

```bash
Rscript figures/plot_soaked.R --dataset=nsch_autism
Rscript figures/plot_soaked.R --dataset=stl10
Rscript figures/plot_soaked.R --dataset=waveform
Rscript figures/plot_soaked.R --dataset=traffic
Rscript figures/plot_soaked.R --dataset=qsip_dim.control_vs_qme.15
Rscript figures/plot_soaked.R --dataset=workers_compensation
Rscript figures/plot_soaked.R --dataset=different
Rscript figures/plot_soaked.R --dataset=iid_easy
```

For SLURM environments, set `SOAKED_USE_SLURM=1` when launching an experiment. This creates a `batchtools` registry under `experiments/<dataset>_<learner>_registry`; after jobs finish, collect results with:

```bash
Rscript experiments/soaked_collect.R --dataset=<dataset> --learner=<learner>
```

## Processed Dataset Summary

The repository includes the small simulation files:

```text
data/raw/simulation/different.csv
data/raw/simulation/iid_easy.csv
data/processed/simulation/different.csv
data/processed/simulation/iid_easy.csv
```

After unpacking the hosted dataset archive, the remaining expected processed benchmark files are:

```text
data/processed/classification/NSCH_autism.csv
data/processed/classification/STL10.csv
data/processed/classification/waveform.csv
data/processed/regression/qsip_dim.control_vs_qme.15.csv
data/processed/regression/traffic.csv
data/processed/regression/workers_compensation.csv
```

`data/dataset_summary.csv` lists the benchmark files and basic sizes.

## Build The Paper

The paper figures used by the submitted manuscript are included under `paper/figures/`.

```bash
cd paper
latexmk -pdf -interaction=nonstopmode main.tex
```

The paper build uses local NeurIPS template files in `paper/`. The repository `.gitignore` excludes generated LaTeX build artifacts and PDFs.

## Notes On Reproducibility

- The quick-start smoke test is synthetic and should finish quickly on a laptop.
- The full processed-data experiments reproduce the code path used for the paper but vary in runtime by dataset size.
- `SOAKED_USE_SLURM=1` is optional and only needed for cluster execution.
- Existing files in `results/` are saved outputs from prior runs; rerunning scripts will overwrite the corresponding output files for the selected dataset and learner.

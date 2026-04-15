# SOAKED

This repository contains the SOAKED NeurIPS E&D paper, the experiment scripts used to generate results, and the plotting scripts used to render final figures.

## Layout

- `paper/`: NeurIPS manuscript source, bibliography, template files, TeX rules, and stable paper figures.
- `R/`: reusable R helpers for SOAKED workflows. This is currently a placeholder for shared code.
- `experiments/`: experiment runners.
- `experiments/generate_figures.R`: current simulation runner for the combined simulation-panel figure.
- `experiments/learner_robustness_real_data.R`: learner-robustness experiment on the local `traffic_aadt` real dataset, with timing output.
- `figures/`: plotting scripts that read saved results and render final figures.
- `results/`: generated CSV summaries and rendered outputs.
- `data/`: local data assets used by experiments.

## Current Files

- Paper source: `paper/main.tex`
- Paper figures: `paper/figures/`
- Simulation runner: `experiments/generate_figures.R`
- Learner robustness runner: `experiments/learner_robustness_real_data.R`
- Simulation plotter: `figures/plot_simulation_panels.R`
- Simulation outputs:
  - `results/simulation_panel_data.csv`
  - `results/combined_simulation_panels.png`

## Requirements

The current experiment and plotting workflow uses R packages including:

- `data.table`
- `mlr3`
- `mlr3verse`
- `mlr3learners`
- `mlr3extralearners`
- `mlr3resampling`
- `rpart`
- `ggplot2`
- `patchwork`

## Simulation Workflow

Generate the simulation summaries:

```bash
Rscript experiments/generate_figures.R
```

Render the combined simulation figure:

```bash
Rscript figures/plot_simulation_panels.R
```

By default, these scripts write:

- `results/simulation_panel_data.csv`
- `results/combined_simulation_panels.png`

The plotting script also refreshes the paper copy of the simulation figure:

- `paper/figures/combined_simulation_panels.png`

## Build The Paper

```bash
cd paper
latexmk -pdf -interaction=nonstopmode main.tex
```

The paper build expects the figure assets under `paper/figures/` and uses the local NeurIPS template files in `paper/`.

## Standard SOAKED Workflow

Prepare qSIP raw data:

```bash
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/data/prepare_qsip_data.R
```

Prepare processed datasets:

```bash
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/data/prepare_data.R
```

Submit jobs one by one:

```bash
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_classif.R --dataset=nsch_autism
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_classif.R --dataset=stl10
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_classif.R --dataset=waveform

SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_regr.R --dataset=traffic
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_regr.R --dataset=qsip_dim.control_vs_qme.15
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_regr.R --dataset=workers_compensation

SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_regr.R --dataset=different
SOAKED_USE_SLURM=1 /projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_regr.R --dataset=iid_easy
```

Collect results:

```bash
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=nsch_autism
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=stl10
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=waveform
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=traffic
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=qsip_dim.control_vs_qme.15
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=workers_compensation
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=different
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/experiments/soaked_collect.R --dataset=iid_easy
```

Render plots:

```bash
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=nsch_autism
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=stl10
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=waveform
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=traffic
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=qsip_dim.control_vs_qme.15
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=workers_compensation
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=different
/projects/genomic-ml/da2343/soak-r/bin/Rscript /projects/genomic-ml/da2343/SOAKED/figures/plot_soaked.R --dataset=iid_easy
```

Current standard SOAKED outputs use the `rpart` tag:

- registries: `experiments/<dataset>_rpart_registry`
- run files: `results/<dataset>_rpart_runs.csv`
- plots: `results/<dataset>_rpart_pvalue_downsample.png`

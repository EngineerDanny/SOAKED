# SOAKED

This repository contains the SOAKED NeurIPS E&D paper, the experiment scripts used to generate results, and the plotting scripts used to render final figures.

## Layout

- `paper/`: NeurIPS manuscript source, bibliography, template files, TeX rules, and stable paper figures.
- `R/`: reusable R helpers for SOAKED workflows. This is currently a placeholder for shared code.
- `experiments/`: experiment runners.
- `experiments/generate_figures.R`: current simulation runner for the combined simulation-panel figure.
- `experiments/learner_robustness_real_data.R`: learner-robustness experiment on real datasets, with timing output.
- `experiments/qsip/`: qSIP analysis scripts and variants.
- `figures/`: plotting scripts that read saved results and render final figures.
- `results/`: generated CSV summaries and rendered outputs.
- `data/`: local data assets used by experiments.

## Current Files

- Paper source: `paper/main.tex`
- Paper figures: `paper/figures/`
- Simulation runner: `experiments/generate_figures.R`
- Learner robustness runner: `experiments/learner_robustness_real_data.R`
- Simulation plotter: `figures/plot_simulation_panels.R`
- qSIP scripts: `experiments/qsip/`
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

# SOAKED

This repository contains the SOAKED NeurIPS E&D paper, the experiment scripts used to generate results, and the plotting scripts used to render final figures.

## Layout

- `paper/`: NeurIPS manuscript source and stable paper figures.
- `R/`: reusable R helpers for SOAKED workflows.
- `experiments/`: dataset- or figure-specific experiment runners.
- `figures/`: plotting scripts that read saved results and render figures.
- `results/`: generated CSV summaries and rendered outputs.
- `data/`: local data assets used by experiments.

## Current Simulation Workflow

Generate the simulation summaries:

```bash
Rscript experiments/simulation/generate_figures.R
```

Render the combined simulation figure:

```bash
Rscript figures/plot_simulation_panels.R
```

This writes:

- `results/simulation/simulation_panel_data.csv`
- `results/simulation/combined_simulation_panels.png`

The paper uses a stable copy of the simulation figure in:

- `paper/figures/combined_simulation_panels.png`

## Build The Paper

```bash
cd paper
latexmk -pdf -interaction=nonstopmode main.tex
```

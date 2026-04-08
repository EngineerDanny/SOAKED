# SOAKED

This repository contains the SOAKED NeurIPS E&D paper, the experiment scripts used to generate results, and the plotting scripts used to render final figures.

## Layout

- `paper/`: NeurIPS manuscript source, bibliography, template files, TeX rules, and stable paper figures.
- `R/`: reusable R helpers for SOAKED workflows. This is currently a placeholder for shared code.
- `experiments/`: experiment runners, including the simulation workflow and qSIP analyses.
- `experiments/qsip/`: qSIP analysis scripts and variants.
- `figures/`: plotting scripts that read saved results and render final figures.
- `results/`: generated CSV summaries and rendered outputs.
- `data/`: local data assets used by experiments.

## Current Files

- Paper source: `paper/main.tex`
- Paper figures: `paper/figures/`
- Simulation runner: `experiments/generate_figures.R`
- Simulation plotter: `figures/plot_simulation_panels.R`
- Simulation outputs:
  - `results/simulation_panel_data.csv`
  - `results/combined_simulation_panels.png`

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

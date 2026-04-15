library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3resampling)
library(batchtools)
library(mlr3batchmark)

learner_id <- "rpart"
output_tag <- "rpart"

# ---- Dataset configs ----
dataset_configs <- list(
  nsch_autism                 = list(metric_id = "classif.auc", metric_col = "classif.auc"),
  stl10                       = list(metric_id = "classif.ce",  metric_col = "classif.ce"),
  waveform                    = list(metric_id = "classif.ce",  metric_col = "classif.ce"),
  traffic                     = list(metric_id = "regr.rmse",   metric_col = "regr.rmse"),
  qsip_dim.control_vs_qme.15  = list(metric_id = "regr.rmse",   metric_col = "regr.rmse"),
  workers_compensation        = list(metric_id = "regr.rmse",   metric_col = "regr.rmse"),
  different                   = list(metric_id = "regr.rmse",   metric_col = "regr.rmse"),
  iid_easy                    = list(metric_id = "regr.rmse",   metric_col = "regr.rmse")
)

# ---- Parse --dataset argument ----
args <- commandArgs(trailingOnly = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
if (length(dataset_arg) == 0L) {
  stop(sprintf(
    "Usage: Rscript soaked_collect.R --dataset=<name>\nAvailable: %s",
    paste(names(dataset_configs), collapse = ", ")
  ))
}
dataset_name <- sub("^--dataset=", "", dataset_arg[[1L]])
if (!dataset_name %in% names(dataset_configs)) {
  stop(sprintf(
    "Unknown dataset '%s'. Available: %s",
    dataset_name, paste(names(dataset_configs), collapse = ", ")
  ))
}
cfg <- dataset_configs[[dataset_name]]

# ---- Paths ----
script_arg  <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root   <- normalizePath(
  file.path(dirname(script_path), ".."), mustWork = TRUE
)

reg_dir    <- file.path(
  repo_root, "experiments", sprintf("%s_%s_registry", dataset_name, output_tag)
)
raw_output <- file.path(
  repo_root, "results", sprintf("%s_%s_runs.csv", dataset_name, output_tag)
)
dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(reg_dir)) {
  stop(sprintf(
    "Registry not found: %s\nRun soaked_classif.R or soaked_regr.R first.",
    reg_dir
  ))
}

# ---- Load registry and collect results ----
message("Loading registry: ", reg_dir)
reg <- batchtools::loadRegistry(reg_dir, writeable = TRUE)

n_jobs <- nrow(batchtools::getJobTable(reg = reg))
n_done <- nrow(batchtools::findDone(reg = reg))
message(sprintf("  %d / %d jobs completed", n_done, n_jobs))

if (n_done < n_jobs) {
  warning(sprintf(
    "%d job(s) not yet done — results will be incomplete.", n_jobs - n_done
  ))
}

bench_result <- mlr3batchmark::reduceResultsBatchmark(reg = reg)
message("Results collected.")

# ---- Score ----
msr_obj <- mlr3::msr(cfg$metric_id)
score_dt <- as.data.table(mlr3resampling::score(bench_result, msr_obj))

raw_dt <- score_dt[, .(
  dataset_id     = dataset_name,
  metric_id      = cfg$metric_id,
  learner        = learner_id,
  test_subset    = as.character(test.subset),
  train_subsets  = train.subsets,
  n_train_groups = n.train.groups,
  fold           = test.fold,
  seed,
  metric_value   = get(cfg$metric_col)
)]

fwrite(raw_dt, raw_output)
message("Saved: ", raw_output)

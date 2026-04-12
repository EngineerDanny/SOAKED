library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3resampling)

# ---- Dataset configs ----
# subset_col   : column used as SOAKED subset label
# target_col   : prediction target column
# drop_cols    : columns to remove before building the task
# log_target   : if TRUE, log-transform the target before fitting
# feature_cols : explicit feature list (NULL = all remaining columns)
dataset_configs <- list(
  traffic = list(
    file         = file.path("processed", "regression", "traffic.csv"),
    subset_col   = "Subset",
    target_col   = "Y",
    fold_col     = "Fold",
    log_target   = FALSE,
    drop_cols    = character(0L),
    feature_cols = NULL
  ),
  qsip_dim.control_vs_qme.15 = list(
    file         = file.path(
      "processed", "regression", "qsip_dim.control_vs_qme.15.csv"
    ),
    subset_col   = "Subset",
    target_col   = "Y",
    fold_col     = "Fold",
    log_target   = FALSE,
    drop_cols    = character(0L),
    feature_cols = NULL
  )
)

# ---- Parse --dataset argument ----
args <- commandArgs(trailingOnly = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
if (length(dataset_arg) == 0L) {
  stop(sprintf(
    "Usage: Rscript soaked_regr.R --dataset=<name>\nAvailable: %s",
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
script_path <- normalizePath(
  sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE
)
repo_root   <- normalizePath(
  file.path(dirname(script_path), ".."), mustWork = TRUE
)

data_path  <- file.path(repo_root, "data", cfg$file)
raw_output <- file.path(
  repo_root, "results", sprintf("%s_glmnet_runs.csv", dataset_name)
)
sum_output <- file.path(
  repo_root, "results", sprintf("%s_glmnet_summary.csv", dataset_name)
)
dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

# ---- Parameters ----
n_folds <- 10L
n_seeds <- 10L

# ---- Load data ----
message(sprintf("Loading %s...", dataset_name))
dt <- fread(data_path)
message(sprintf("  %d rows, %d columns", nrow(dt), ncol(dt)))

if (isTRUE(cfg$log_target)) {
  dt[, (cfg$target_col) := log(get(cfg$log_source))]
}
if (length(cfg$drop_cols) > 0L) {
  dt[, (cfg$drop_cols) := NULL]
}

# ---- Task ----
task <- mlr3::TaskRegr$new(
  id      = dataset_name,
  backend = dt,
  target  = cfg$target_col
)
if (!is.null(cfg$feature_cols)) {
  task$col_roles$feature <- cfg$feature_cols
}
task$set_col_roles(cfg$subset_col, c("subset", "stratum"))
if (!is.null(cfg$fold_col) && cfg$fold_col %in% names(dt)) {
  task$col_roles$fold <- cfg$fold_col
}

# ---- Resampling ----
cv_down <- mlr3resampling::ResamplingSameOtherSizesCV$new()
cv_down$param_set$values$folds <- n_folds
cv_down$param_set$values$seeds <- n_seeds
cv_down$param_set$values$sizes <- 0L

# ---- Learner ----
glmnet_lrn <- mlr3learners::LearnerRegrCVGlmnet$new()

# ---- Benchmark grid ----
bench_grid <- mlr3::benchmark_grid(task, glmnet_lrn, cv_down)

use_slurm <- identical(Sys.getenv("SOAKED_USE_SLURM"), "1")
if (use_slurm && nchar(Sys.which("sbatch")) == 0L) {
  stop("SOAKED_USE_SLURM=1 was set, but sbatch was not found on PATH.")
}

if (use_slurm) {
  # ---- HPC: submit via SLURM job arrays ----
  reg_dir <- file.path(
    repo_root, "experiments",
    sprintf("%s_glmnet_registry", dataset_name)
  )
  message("SLURM detected — submitting via batchtools job arrays...")
  unlink(reg_dir, recursive = TRUE)
  reg <- batchtools::makeExperimentRegistry(
    file.dir = reg_dir,
    seed     = 1L,
    packages = "mlr3verse"
  )
  mlr3batchmark::batchmark(bench_grid, store_models = FALSE, reg = reg)
  job_table <- batchtools::getJobTable(reg = reg)
  chunks    <- data.frame(job_table, chunk = 1)
  batchtools::submitJobs(
    chunks,
    resources = list(
      walltime            = 60L * 60L,
      memory              = 4000L,
      ncpus               = 1L,
      ntasks              = 1L,
      chunks.as.arrayjobs = TRUE
    ),
    reg = reg
  )
  message("Jobs submitted. Registry: ", reg_dir)

} else {
  # ---- Local: run sequentially ----
  message(sprintf(
    "Running sequentially (%d-fold CV, %d seed(s))...", n_folds, n_seeds
  ))
  bench_result <- mlr3::benchmark(bench_grid, store_models = FALSE)
  message("  Done.")

  raw_dt <- as.data.table(
    mlr3resampling::score(bench_result, mlr3::msr("regr.rmse"))
  )[, .(
    dataset_id     = dataset_name,
    metric_id      = "regr.rmse",
    learner        = "cv_glmnet",
    test_subset    = as.character(test.subset),
    train_subsets  = train.subsets,
    n_train_groups = n.train.groups,
    fold           = test.fold,
    seed,
    metric_value   = regr.rmse
  )]

  summary_dt <- raw_dt[, .(
    metric_mean = mean(metric_value),
    metric_sd   = sd(metric_value),
    metric_se   = sd(metric_value) / sqrt(.N),
    n_runs      = .N
  ), by = .(
    dataset_id, metric_id, learner,
    test_subset, train_subsets, n_train_groups
  )]

  fwrite(raw_dt,     raw_output)
  fwrite(summary_dt, sum_output)
  message("Saved: ", raw_output)
  message("Saved: ", sum_output)
}

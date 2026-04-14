library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3resampling)

# ---- Dataset configs ----
# Each entry defines how to load and set up one classification dataset.
# subset_col : column used as SOAKED subset label
# drop_cols  : columns to remove before building the task
# target_col : prediction target column
# positive   : positive class label for binary tasks (NULL for multiclass)
# y_factor   : if TRUE, convert target to factor
dataset_configs <- list(
  nsch_autism = list(
    file       = file.path("processed", "classification", "NSCH_autism.csv"),
    subset_col = "Subset",
    target_col = "Y",
    fold_col   = "Fold",
    positive   = "Yes",
    y_factor   = TRUE,
    y_levels   = c("No", "Yes"),
    drop_cols  = character(0L),
    metric_id  = "classif.auc",
    metric_col = "classif.auc"
  ),
  stl10 = list(
    file       = file.path("processed", "classification", "STL10.csv"),
    subset_col = "Subset",
    target_col = "Y",
    fold_col   = "Fold",
    positive   = NULL,
    y_factor   = TRUE,
    y_levels   = NULL,
    drop_cols  = character(0L),
    metric_id  = "classif.ce",
    metric_col = "classif.ce"
  ),
  waveform = list(
    file       = file.path("processed", "classification", "waveform.csv"),
    subset_col = "Subset",
    target_col = "Y",
    fold_col   = "Fold",
    positive   = NULL,
    y_factor   = TRUE,
    y_levels   = NULL,
    drop_cols  = character(0L),
    metric_id  = "classif.ce",
    metric_col = "classif.ce"
  )
)

# ---- Parse --dataset argument ----
args <- commandArgs(trailingOnly = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
if (length(dataset_arg) == 0L) {
  stop(sprintf(
    "Usage: Rscript soaked_classif.R --dataset=<name>\nAvailable: %s",
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

data_path  <- file.path(repo_root, "data", cfg$file)
raw_output <- file.path(
  repo_root, "results", sprintf("%s_glmnet_runs.csv", dataset_name)
)
dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

# ---- Parameters ----
default_n_folds <- 10L
n_seeds <- 10L
rng_seed <- 42L

# ---- Load data ----
message(sprintf("Loading %s...", dataset_name))
dt <- fread(data_path)
message(sprintf("  %d rows, %d columns", nrow(dt), ncol(dt)))

has_fold_col <- !is.null(cfg$fold_col) && cfg$fold_col %in% names(dt)
n_folds <- if (has_fold_col) {
  uniqueN(dt[[cfg$fold_col]])
} else {
  default_n_folds
}
set.seed(rng_seed)

if (length(cfg$drop_cols) > 0L) {
  dt[, (cfg$drop_cols) := NULL]
}
if (cfg$y_factor) {
  if (!is.null(cfg$y_levels)) {
    dt[, (cfg$target_col) := factor(
      get(cfg$target_col), levels = cfg$y_levels
    )]
  } else {
    dt[, (cfg$target_col) := factor(get(cfg$target_col))]
  }
}

# ---- Task ----
task_args <- list(
  id      = dataset_name,
  backend = dt,
  target  = cfg$target_col
)
if (!is.null(cfg$positive)) {
  task_args$positive <- cfg$positive
}
task <- do.call(mlr3::TaskClassif$new, task_args)
task$set_col_roles(cfg$subset_col, "subset")
if (has_fold_col) {
  task$col_roles$fold <- cfg$fold_col
  task$col_roles$stratum <- c(cfg$fold_col, cfg$target_col)
} else {
  task$col_roles$stratum <- c(cfg$subset_col, cfg$target_col)
}

# ---- Resampling ----
cv_down <- mlr3resampling::ResamplingSameOtherSizesCV$new()
cv_down$param_set$values$folds <- n_folds
cv_down$param_set$values$seeds <- n_seeds
cv_down$param_set$values$sizes <- 0L

# ---- Learner ----
glmnet_lrn <- mlr3learners::LearnerClassifCVGlmnet$new()
glmnet_lrn$predict_type <- "prob"

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
    seed     = 42L,
    packages = "mlr3verse"
  )
  mlr3batchmark::batchmark(bench_grid, store_models = FALSE, reg = reg)
  job_table <- batchtools::getJobTable(reg = reg)
  chunks    <- data.frame(job_table, chunk = 1)
  batchtools::submitJobs(
    chunks,
    resources = list(
      walltime            = 60L * 60L,
      memory              = 16000L,
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
    mlr3resampling::score(bench_result, mlr3::msr(cfg$metric_id))
  )[, .(
    dataset_id     = dataset_name,
    metric_id      = cfg$metric_id,
    learner        = "cv_glmnet",
    test_subset    = as.character(test.subset),
    train_subsets  = train.subsets,
    n_train_groups = n.train.groups,
    fold           = test.fold,
    seed,
    metric_value   = get(cfg$metric_col)
  )]

  fwrite(raw_dt, raw_output)
  message("Saved: ", raw_output)
}

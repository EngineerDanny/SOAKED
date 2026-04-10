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
    file       = "NSCH_autism.csv",
    subset_col = "survey_year",
    target_col = "y",
    positive   = "Yes",
    y_factor   = TRUE,
    y_levels   = c("No", "Yes"),
    drop_cols  = c("Unique_Household_ID", "Selected_Child_Weight")
  ),
  stl10 = list(
    file       = "STL10.csv",
    subset_col = "predefined.set",
    target_col = "y",
    positive   = NULL,
    y_factor   = TRUE,
    y_levels   = NULL,
    drop_cols  = character(0L)
  ),
  waveform = list(
    file       = "waveform.csv",
    subset_col = "predefined.set",
    target_col = "y",
    positive   = NULL,
    y_factor   = TRUE,
    y_levels   = NULL,
    drop_cols  = character(0L)
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
task$set_col_roles(cfg$subset_col, c("subset", "stratum"))

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

on_hpc <- nchar(Sys.which("sbatch")) > 0L

if (on_hpc) {
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
      memory              = 32000L,
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
    mlr3resampling::score(bench_result, mlr3::msr("classif.auc"))
  )[, .(
    dataset_id     = dataset_name,
    metric_id      = "classif.auc",
    learner        = "cv_glmnet",
    test_subset    = as.character(test.subset),
    train_subsets  = train.subsets,
    n_train_groups = n.train.groups,
    fold           = test.fold,
    seed,
    metric_value   = classif.auc
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

library(data.table)
library(ggplot2)
library(grid)
library(mlr3resampling)

# ---- Dataset configs ----
dataset_configs <- list(
  nsch_autism = list(
    metric_label = "Test set AUC = Area Under the ROC Curve (mean\u00b1SD, CV folds)"
  ),
  stl10 = list(
    metric_label = "Test classification error rate (mean\u00b1SD, CV folds)"
  ),
  waveform = list(
    metric_label = "Test classification error rate (mean\u00b1SD, CV folds)"
  ),
  traffic = list(
    metric_label = "Test RMSE on log(AADT) (mean\u00b1SD, CV folds)"
  ),
  qsip_dim.control_vs_qme.15 = list(
    metric_label = "Test RMSE on growth_per_day (mean\u00b1SD, CV folds)"
  ),
  workers_compensation = list(
    metric_label = "Test RMSE on log(UltimateIncurredClaimCost) (mean\u00b1SD, CV folds)"
  ),
  different = list(
    metric_label = "Test RMSE (mean\u00b1SD, CV folds)"
  ),
  iid_easy = list(
    metric_label = "Test RMSE (mean\u00b1SD, CV folds)"
  )
)

# ---- Parse --dataset argument ----
args <- commandArgs(trailingOnly = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
if (length(dataset_arg) == 0L) {
  stop(sprintf(
    "Usage: Rscript plot_soaked.R --dataset=<name>\nAvailable: %s",
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

raw_csv <- file.path(
  repo_root, "results", sprintf("%s_glmnet_runs.csv", dataset_name)
)
if (!file.exists(raw_csv)) {
  stop("Run file not found: ", raw_csv)
}

results_dir <- file.path(repo_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Adapt collected CSV to mlr3resampling score format ----
score_dt <- fread(raw_csv)
if (length(unique(score_dt$metric_id)) != 1L) {
  stop("Expected exactly one metric_id in: ", raw_csv)
}
metric_id <- unique(score_dt$metric_id)[[1L]]

setnames(
  score_dt,
  old = c("test_subset", "train_subsets", "n_train_groups", "fold", "learner"),
  new = c("test.subset", "train.subsets", "n.train.groups", "test.fold", "algorithm")
)
score_dt[, task_id := dataset_id]
score_dt[, (metric_id) := metric_value]
score_dt[, groups := max(n.train.groups), by = .(test.subset, train.subsets)]

# ---- Average over downsampling seeds before p-value computation ----
# This keeps the summary statistics at the fold level and avoids treating
# repeated seed realizations within a fold as independent replicates.
seed_cols <- c(
  "dataset_id", "task_id", "metric_id", "algorithm",
  "test.subset", "train.subsets", "n.train.groups", "test.fold", "groups"
)
score_plot_dt <- score_dt[, .(
  metric_value = mean(metric_value),
  value_for_plot = mean(get(metric_id))
), by = seed_cols]
setnames(score_plot_dt, "value_for_plot", metric_id)

required_cols <- c(
  "test.subset", "train.subsets", "n.train.groups", "test.fold",
  "groups", "algorithm", metric_id
)
missing_cols <- setdiff(required_cols, names(score_plot_dt))
if (length(missing_cols) > 0L) {
  stop("Missing required plotting columns: ", paste(missing_cols, collapse = ", "))
}

# ---- Build package-owned p-value plots ----
test_subsets <- unique(score_plot_dt$test.subset)
plots <- vector("list", length(test_subsets))
names(plots) <- test_subsets

for (i in seq_along(test_subsets)) {
  subset_name <- test_subsets[[i]]
  subset_dt <- score_plot_dt[test.subset == subset_name]
  model_names <- unique(subset_dt$algorithm)
  if (length(model_names) != 1L) {
    stop(
      "Expected one algorithm for subset ", subset_name,
      ", found: ", paste(model_names, collapse = ", ")
    )
  }
  pvalue_args <- names(formals(mlr3resampling::pvalue_downsample))
  if ("value.var" %in% pvalue_args) {
    pvalue_obj <- mlr3resampling::pvalue_downsample(
      subset_dt,
      value.var = metric_id,
      digits = 4
    )
  } else {
    pvalue_obj <- mlr3resampling::pvalue_downsample(
      score_plot_dt,
      subset_name,
      model_names[[1L]]
    )
  }
  plots[[i]] <- plot(pvalue_obj) +
    labs(
      title = sprintf(
        "%s SOAKED analysis, test subset=%s",
        dataset_name, subset_name
      ),
      x = cfg$metric_label
    )
}

# ---- Combine test subsets into one figure ----
out_png <- file.path(
  results_dir, sprintf("%s_glmnet_pvalue_downsample.png", dataset_name)
)

plot_height <- max(4, 4 * length(plots))
png(out_png, width = 12, height = plot_height, units = "in", res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = length(plots), ncol = 1)))
for (i in seq_along(plots)) {
  print(
    plots[[i]],
    vp = viewport(layout.pos.row = i, layout.pos.col = 1)
  )
}
dev.off()

message("Saved: ", out_png)

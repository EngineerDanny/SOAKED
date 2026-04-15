library(data.table)
library(ggplot2)

default_output_tag <- "glmnet"

dataset_configs <- list(
  nsch_autism = list(
    metric_label = "Test set AUC = Area Under the ROC Curve (mean±SD, 10 CV folds)"
  ),
  stl10 = list(
    metric_label = "Test classification error rate (mean±SD, 10 CV folds)"
  ),
  waveform = list(
    metric_label = "Test classification error rate (mean±SD, 10 CV folds)"
  ),
  traffic = list(
    metric_label = "Test RMSE on log(AADT) (mean±SD, 10 CV folds)"
  ),
  qsip_dim.control_vs_qme.15 = list(
    metric_label = "Test RMSE on growth_per_day (mean±SD, 10 CV folds)"
  ),
  workers_compensation = list(
    metric_label = "Test RMSE on log(UltimateIncurredClaimCost) (mean±SD, 10 CV folds)"
  ),
  different = list(
    metric_label = "Test RMSE (mean±SD, 10 CV folds)"
  ),
  iid_easy = list(
    metric_label = "Test RMSE (mean±SD, 10 CV folds)"
  )
)

args <- commandArgs(trailingOnly = TRUE)
pair_args <- grep("^--pair=", args, value = TRUE)
out_arg <- grep("^--out=", args, value = TRUE)
tag_arg <- grep("^--tag=", args, value = TRUE)
xlab_arg <- grep("^--xlab=", args, value = TRUE)
output_tag <- if (length(tag_arg) > 0L) {
  sub("^--tag=", "", tag_arg[[1L]])
} else {
  default_output_tag
}
custom_xlab <- if (length(xlab_arg) > 0L) {
  sub("^--xlab=", "", xlab_arg[[1L]])
} else {
  NULL
}

if (length(pair_args) == 0L) {
  stop(
    paste(
      "Usage:",
      "Rscript plot_soaked_pairs.R",
      "--pair=<dataset:test_subset>",
      "[--pair=<dataset:test_subset> ...]",
      "[--tag=<glmnet|rpart>]",
      "[--xlab=<shared x-axis label>]",
      "[--out=<png path>]"
    )
  )
}

pair_specs <- lapply(pair_args, function(x) {
  pair_text <- sub("^--pair=", "", x)
  split_pos <- regexpr(":", pair_text, fixed = TRUE)[[1L]]
  if (split_pos <= 0L) {
    stop("Each --pair must be of the form <dataset:test_subset>. Got: ", pair_text)
  }
  dataset_name <- substr(pair_text, 1L, split_pos - 1L)
  subset_name <- substr(pair_text, split_pos + 1L, nchar(pair_text))
  if (!dataset_name %in% names(dataset_configs)) {
    stop(
      "Unknown dataset '", dataset_name, "'. Available: ",
      paste(names(dataset_configs), collapse = ", ")
    )
  }
  list(
    dataset = dataset_name,
    test_subset = subset_name,
    pair_label = sprintf("dataset: %s, test_subset: %s", dataset_name, subset_name)
  )
})

script_arg  <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root   <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
results_dir <- file.path(repo_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- if (length(out_arg) > 0L) {
  normalizePath(sub("^--out=", "", out_arg[[1L]]), mustWork = FALSE)
} else {
  file.path(results_dir, sprintf("soaked_pairs_%s.png", output_tag))
}

format_n <- function(x) {
  format(x, trim = TRUE, scientific = FALSE, big.mark = ",")
}

pair_data_list <- lapply(pair_specs, function(spec) {
  raw_csv <- file.path(
    repo_root, "results", sprintf("%s_%s_runs.csv", spec$dataset, output_tag)
  )
  if (!file.exists(raw_csv)) {
    stop("Run file not found: ", raw_csv)
  }

  score_dt <- fread(raw_csv)
  if (length(unique(score_dt$metric_id)) != 1L) {
    stop("Expected exactly one metric_id in: ", raw_csv)
  }
  metric_col <- unique(score_dt$metric_id)[[1L]]

  setnames(
    score_dt,
    old = c("test_subset", "train_subsets", "n_train_groups", "fold", "learner"),
    new = c("test.subset", "train.subsets", "n.train.groups", "test.fold", "algorithm")
  )
  score_dt[, task_id := dataset_id]
  score_dt[, (metric_col) := metric_value]

  seed_cols <- c(
    "dataset_id", "task_id", "metric_id", "algorithm",
    "test.subset", "train.subsets", "n.train.groups", "test.fold"
  )
  score_plot_dt <- score_dt[, .(
    value_for_plot = mean(get(metric_col))
  ), by = seed_cols]
  setnames(score_plot_dt, "value_for_plot", metric_col)

  pair_dt <- score_plot_dt[
    test.subset == spec$test_subset &
      train.subsets %in% c("other", "same")
  ]
  if (nrow(pair_dt) == 0L) {
    stop(
      "No rows found for dataset='", spec$dataset,
      "', test_subset='", spec$test_subset, "'."
    )
  }

  full_n_by_subset <- pair_dt[, .(
    full_n = max(n.train.groups)
  ), by = train.subsets]
  smallest_n <- min(full_n_by_subset$full_n)

  full_dt <- merge(pair_dt, full_n_by_subset, by = "train.subsets", all.x = TRUE)[
    n.train.groups == full_n
  ][, `:=`(
    sample_size = "full",
    sample_size_label = "full"
  )]

  smallest_dt <- pair_dt[n.train.groups == smallest_n][
    , `:=`(
      sample_size = "smallest",
      sample_size_label = sprintf("smallest = %s", format_n(smallest_n))
    )
  ]

  plot_dt <- rbindlist(list(full_dt, smallest_dt), use.names = TRUE, fill = TRUE)
  if (nrow(plot_dt) == 0L) {
    stop(
      "No plotting rows remained for dataset='", spec$dataset,
      "', test_subset='", spec$test_subset, "'."
    )
  }

  plot_dt[, pair_label := spec$pair_label]
  plot_dt[, metric_label := dataset_configs[[spec$dataset]]$metric_label]
  plot_dt[, metric_col_name := metric_col]
  plot_dt
})

plot_dt <- rbindlist(pair_data_list, use.names = TRUE, fill = TRUE)
metric_labels <- unique(plot_dt$metric_label)
if (is.null(custom_xlab) && length(metric_labels) != 1L) {
  stop(
    "Selected pairs do not share one metric label. Got: ",
    paste(metric_labels, collapse = " | "),
    ". Use --xlab=... to provide a shared axis label."
  )
}
x_axis_label <- if (is.null(custom_xlab)) metric_labels[[1L]] else custom_xlab

plot_dt[, pair_label := factor(
  pair_label,
  levels = vapply(pair_specs, `[[`, character(1L), "pair_label")
)]
plot_dt[, sample_size := factor(
  sample_size,
  levels = c("full", "smallest")
)]

summary_dt <- plot_dt[, .(
  mean_value = mean(get(unique(metric_col_name)[[1L]])),
  sd_value = sd(get(unique(metric_col_name)[[1L]])),
  n_train_groups = unique(n.train.groups)[[1L]]
), by = .(pair_label, sample_size, train.subsets)]

summary_dt[, y_label := train.subsets]
summary_dt[y_label %in% c("other", "same"), label := sprintf(
  "%.4f \u00b1 %.4f, N = %s",
  mean_value, sd_value, format_n(n_train_groups)
)]

pvalue_dt <- plot_dt[, .(
  metric_value = mean(get(unique(metric_col_name)[[1L]]))
), by = .(pair_label, sample_size, test.fold, train.subsets)]

pvalue_wide <- dcast(
  pvalue_dt,
  pair_label + sample_size + test.fold ~ train.subsets,
  value.var = "metric_value"
)
pvalue_wide <- pvalue_wide[complete.cases(other, same)]

pvalue_summary <- pvalue_wide[, .(
  p_value = if (.N >= 2L) {
    stats::t.test(other, same, paired = TRUE)$p.value
  } else {
    NA_real_
  },
  x_min = mean(same),
  x_max = mean(other)
), by = .(pair_label, sample_size)]
pvalue_summary[, `:=`(
  y_label = "same-other",
  label = ifelse(is.na(p_value), "P=NA", sprintf("P=%.4f", p_value)),
  x_mid = (x_min + x_max) / 2
)]

plot_anno_dt <- rbindlist(
  list(
    summary_dt[y_label %in% c("other", "same")],
    pvalue_summary
  ),
  use.names = TRUE,
  fill = TRUE
)

x_span <- diff(range(
  c(plot_anno_dt$x_min, plot_anno_dt$x_max, summary_dt$mean_value),
  na.rm = TRUE
))
if (!is.finite(x_span) || x_span <= 0) {
  x_span <- 1
}

pair_plot <- ggplot() +
  geom_segment(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(
      x = mean_value - sd_value,
      xend = mean_value + sd_value,
      y = y_label,
      yend = y_label
    )
  ) +
  geom_point(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(x = mean_value, y = y_label),
    shape = 1
  ) +
  geom_text(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(
      x = mean_value,
      y = y_label,
      label = label
    ),
    vjust = -0.5
  ) +
  geom_segment(
    data = pvalue_summary,
    aes(
      x = x_min,
      xend = x_max,
      y = y_label,
      yend = y_label
    )
  ) +
  geom_text(
    data = pvalue_summary,
    aes(x = x_mid, y = y_label, label = label),
    vjust = -0.5
  ) +
  facet_grid(rows = vars(sample_size), cols = vars(pair_label), scales = "free_x") +
  scale_y_discrete(limits = c("other", "same-other", "same")) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.06))) +
  labs(
    x = x_axis_label,
    y = "Train subsets"
  )

png(
  out_png,
  width = max(8, 5.5 * length(pair_specs)),
  height = 7,
  units = "in",
  res = 300
)
print(pair_plot)
dev.off()

message("Saved: ", out_png)

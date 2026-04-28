library(data.table)
library(ggplot2)

default_output_tag <- "rpart"
default_pair_specs <- c(
  "traffic:Phoenix",
  "qsip_dim.control_vs_qme.15:dim.control",
  "workers_compensation:F"
)
default_xlab <- "Test RMSE (mean±SD, 10 CV folds)"

dataset_configs <- list(
  nsch_autism = list(
    display_name = "nsch_autism",
    metric_label = "Test classification error rate (mean±SD, 10 CV folds)"
  ),
  stl10 = list(
    display_name = "stl10",
    metric_label = "Test classification error rate (mean±SD, 10 CV folds)"
  ),
  waveform = list(
    display_name = "waveform",
    metric_label = "Test classification error rate (mean±SD, 10 CV folds)"
  ),
  traffic = list(
    display_name = "traffic",
    metric_label = "Test RMSE on log(AADT) (mean±SD, 10 CV folds)"
  ),
  qsip_dim.control_vs_qme.15 = list(
    display_name = "qsip",
    metric_label = "Test RMSE on growth_per_day (mean±SD, 10 CV folds)"
  ),
  workers_compensation = list(
    display_name = "workers_compensation",
    metric_label = "Test RMSE on log(UltimateIncurredClaimCost) (mean±SD, 10 CV folds)"
  ),
  different = list(
    display_name = "different",
    metric_label = "Test RMSE (mean±SD, 10 CV folds)"
  ),
  iid_easy = list(
    display_name = "iid_easy",
    metric_label = "Test RMSE (mean±SD, 10 CV folds)"
  )
)

args <- commandArgs(trailingOnly = TRUE)
pair_args <- grep("^--pair=", args, value = TRUE)
if (length(pair_args) == 0L) {
  pair_args <- paste0("--pair=", default_pair_specs)
}
out_arg <- grep("^--out=", args, value = TRUE)
tag_arg <- grep("^--tag=", args, value = TRUE)
xlab_arg <- grep("^--xlab=", args, value = TRUE)
width_arg <- grep("^--width=", args, value = TRUE)
height_arg <- grep("^--height=", args, value = TRUE)
ylab_arg <- grep("^--ylab=", args, value = TRUE)
anno_size_arg <- grep("^--anno-size=", args, value = TRUE)
sample_prefix_arg <- grep("^--sample-prefix=", args, value = TRUE)
expand_left_arg <- grep("^--x-expand-left=", args, value = TRUE)
expand_right_arg <- grep("^--x-expand-right=", args, value = TRUE)
center_labels_arg <- grep("^--center-labels=", args, value = TRUE)
pair_label_break_arg <- grep("^--pair-label-break=", args, value = TRUE)
output_tag <- if (length(tag_arg) > 0L) {
  sub("^--tag=", "", tag_arg[[1L]])
} else {
  default_output_tag
}
custom_xlab <- if (length(xlab_arg) > 0L) {
  sub("^--xlab=", "", xlab_arg[[1L]])
} else {
  default_xlab
}
custom_width <- if (length(width_arg) > 0L) {
  as.numeric(sub("^--width=", "", width_arg[[1L]]))
} else {
  max(5.2, 3.4 * length(pair_args))
}
custom_height <- if (length(height_arg) > 0L) {
  as.numeric(sub("^--height=", "", height_arg[[1L]]))
} else {
  2.6
}
custom_ylab <- if (length(ylab_arg) > 0L) {
  sub("^--ylab=", "", ylab_arg[[1L]])
} else {
  "Train subsets"
}
anno_size <- if (length(anno_size_arg) > 0L) {
  as.numeric(sub("^--anno-size=", "", anno_size_arg[[1L]]))
} else {
  3.3
}
use_sample_prefix <- if (length(sample_prefix_arg) > 0L) {
  tolower(sub("^--sample-prefix=", "", sample_prefix_arg[[1L]])) %in% c("1", "true", "yes")
} else {
  FALSE
}
x_expand_left <- if (length(expand_left_arg) > 0L) {
  as.numeric(sub("^--x-expand-left=", "", expand_left_arg[[1L]]))
} else {
  0
}
x_expand_right <- if (length(expand_right_arg) > 0L) {
  as.numeric(sub("^--x-expand-right=", "", expand_right_arg[[1L]]))
} else {
  0
}
center_labels <- if (length(center_labels_arg) > 0L) {
  tolower(sub("^--center-labels=", "", center_labels_arg[[1L]])) %in% c("1", "true", "yes")
} else {
  TRUE
}
pair_label_break <- if (length(pair_label_break_arg) > 0L) {
  tolower(sub("^--pair-label-break=", "", pair_label_break_arg[[1L]])) %in% c("1", "true", "yes")
} else {
  FALSE
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
    pair_label = sprintf(
      "dataset: %s\ntest_subset: %s",
      dataset_configs[[dataset_name]]$display_name,
      subset_name
    )
  )
})

script_arg  <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root   <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
results_dir <- file.path(repo_root, "results")
paper_figures_dir <- file.path(repo_root, "paper", "figures")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(paper_figures_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- if (length(out_arg) > 0L) {
  normalizePath(sub("^--out=", "", out_arg[[1L]]), mustWork = FALSE)
} else {
  file.path(paper_figures_dir, "fig5-regr-pairs.png")
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
x_axis_label <- sub("(", sprintf("(%s, ", output_tag), x_axis_label, fixed = TRUE)

plot_dt[, pair_label := factor(
  pair_label,
  levels = vapply(pair_specs, `[[`, character(1L), "pair_label")
)]
plot_dt[, sample_size := factor(
  sample_size,
  levels = c("full", "smallest")
)]

if (use_sample_prefix) {
  plot_dt[, sample_size_label := ifelse(
    sample_size == "full",
    "sample\nsize:\nfull",
    "sample\nsize:\nsmallest"
  )]
} else {
  plot_dt[, sample_size_label := ifelse(
    sample_size == "full",
    "sample\nsize:\nfull",
    "sample\nsize:\nsmallest"
  )]
}
plot_dt[, sample_size_label := factor(
  sample_size_label,
  levels = unique(plot_dt[order(sample_size), sample_size_label])
)]

facet_label_dt <- unique(plot_dt[, .(pair_label, sample_size, sample_size_label)])

summary_dt <- plot_dt[, .(
  mean_value = mean(get(unique(metric_col_name)[[1L]])),
  sd_value = sd(get(unique(metric_col_name)[[1L]])),
  n_train_groups = unique(n.train.groups)[[1L]]
), by = .(pair_label, sample_size, train.subsets)]
summary_dt <- merge(
  summary_dt,
  facet_label_dt,
  by = c("pair_label", "sample_size"),
  all.x = TRUE
)

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
  label = fifelse(
    is.na(p_value),
    "P = NA",
    fifelse(
      p_value < 1e-4,
      "P < 0.0001",
      sprintf("P = %.4f", p_value)
    )
  ),
  x_mid = (x_min + x_max) / 2
)]
pvalue_summary <- merge(
  pvalue_summary,
  facet_label_dt,
  by = c("pair_label", "sample_size"),
  all.x = TRUE
)

plot_anno_dt <- rbindlist(
  list(
    summary_dt[y_label %in% c("other", "same")],
    pvalue_summary
  ),
  use.names = TRUE,
  fill = TRUE
)

panel_range_dt <- plot_anno_dt[, .(
  panel_x_min = min(c(x_min, x_max, mean_value), na.rm = TRUE),
  panel_x_max = max(c(x_min, x_max, mean_value), na.rm = TRUE)
), by = .(pair_label, sample_size)]
panel_range_dt[, panel_span := panel_x_max - panel_x_min]
panel_range_dt[!is.finite(panel_span) | panel_span <= 0, panel_span := 1]

summary_dt <- merge(
  summary_dt,
  panel_range_dt,
  by = c("pair_label", "sample_size"),
  all.x = TRUE
)
summary_dt[, `:=`(
  label_x = mean_value,
  label_hjust = 0.5,
  label_half_width = pmax(1.25 * sd_value, 0.08 * panel_span * nchar(label))
)]

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
  geom_blank(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(x = label_x - label_half_width, y = y_label)
  ) +
  geom_blank(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(x = label_x + label_half_width, y = y_label)
  ) +
  geom_text(
    data = summary_dt[y_label %in% c("other", "same")],
    aes(
      x = label_x,
      y = y_label,
      label = label
    ),
    hjust = 0.5,
    vjust = -0.5,
    size = anno_size
  ) +
  geom_segment(
    data = pvalue_summary,
    aes(
      x = x_min,
      xend = x_max,
      y = y_label,
      yend = y_label
    ),
    color = "grey50"
  ) +
  geom_text(
    data = pvalue_summary,
    aes(x = x_mid, y = y_label, label = label),
    vjust = -0.5,
    color = "grey50",
    size = anno_size
  ) +
  facet_grid(rows = vars(sample_size_label), cols = vars(pair_label), scales = "free_x") +
  scale_y_discrete(
    limits = c("other", "same-other", "same"),
    expand = expansion(add = c(0.15, 0.6))
  ) +
  scale_x_continuous(expand = expansion(mult = c(x_expand_left, x_expand_right))) +
  labs(
    x = x_axis_label,
    y = custom_ylab
  ) +
  coord_cartesian(clip = "off") +
  theme_bw(base_size = 11)

png(
  out_png,
  width = custom_width,
  height = custom_height,
  units = "in",
  res = 300
)
print(pair_plot)
dev.off()

message("Saved: ", out_png)

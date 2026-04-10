library(data.table)
library(ggplot2)

# ---- Dataset configs ----
# metric_label : x-axis label
# metric_type  : "auc" (higher = better) or "rmse" (lower = better)
dataset_configs <- list(
  nsch_autism = list(
    metric_label = "Test set AUC = Area Under the ROC Curve (mean\u00b1SD, CV folds)",
    metric_type  = "auc"
  ),
  stl10 = list(
    metric_label = "Test set AUC (mean\u00b1SD, CV folds)",
    metric_type  = "auc"
  ),
  waveform = list(
    metric_label = "Test set AUC (mean\u00b1SD, CV folds)",
    metric_type  = "auc"
  ),
  traffic = list(
    metric_label = "Test RMSE on log(AADT) (mean\u00b1SD, CV folds)",
    metric_type  = "rmse"
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

raw_csv     <- file.path(
  repo_root, "results", sprintf("%s_glmnet_runs.csv", dataset_name)
)
results_dir <- file.path(repo_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load ----
raw_dt <- fread(raw_csv)

# ---- Label full vs matched ----
smallest_dt <- raw_dt[, .(smallest_n = min(n_train_groups)), by = test_subset]
full_dt     <- raw_dt[,
  .(full_n = max(n_train_groups)),
  by = .(test_subset, train_subsets)
]
raw_dt <- merge(raw_dt, smallest_dt, by = "test_subset")
raw_dt <- merge(raw_dt, full_dt,     by = c("test_subset", "train_subsets"))

n0_val <- raw_dt[, unique(smallest_n)]
message(sprintf("n0 = %d", n0_val))

raw_dt <- rbindlist(list(
  raw_dt[n_train_groups == full_n][,
    sample_size := "full"
  ],
  raw_dt[n_train_groups == smallest_n][,
    sample_size := paste0("smallest=", n0_val)
  ]
), use.names = TRUE)

size_levels <- c("full", paste0("smallest=", n0_val))

# ---- Summary stats ----
plot_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_sd   = sd(metric_value),
  n_train     = n_train_groups[1L]
), by = .(test_subset, train_subsets, sample_size)]

# ---- Paired t-test (fold-level averages over seeds) ----
fold_avg <- raw_dt[,
  .(val = mean(metric_value)),
  by = .(test_subset, train_subsets, sample_size, fold)
]

pval_dt <- rbindlist(lapply(unique(fold_avg$test_subset), function(sub) {
  rbindlist(lapply(size_levels, function(sz) {
    sub_dt   <- fold_avg[test_subset == sub & sample_size == sz]
    same_val <- sub_dt[train_subsets == "same", val]
    rbindlist(lapply(c("other", "all"), function(ts) {
      cmp_val <- sub_dt[train_subsets == ts, val]
      if (length(cmp_val) < 2L || length(same_val) < 2L) return(NULL)
      tt <- t.test(cmp_val, same_val, paired = TRUE, alternative = "two.sided")
      data.table(
        test_subset   = sub,
        sample_size   = sz,
        compare       = ts,
        p_value       = tt$p.value
      )
    }))
  }))
}))

message("\nPaired t-test results:")
print(pval_dt)

pval_dt[, p_label := sprintf("P=%.4f", p_value)]

# ---- Y-axis layout: other, other-same, same, all-same, all ----
y_levels <- c("all", "all-same", "same", "other-same", "other")

for (sub in unique(plot_dt$test_subset)) {
  p_plot <- plot_dt[test_subset == sub]
  p_pval <- pval_dt[test_subset == sub]

  means_wide <- dcast(
    p_plot, sample_size ~ train_subsets, value.var = "metric_mean"
  )

  p_plot[, row_label := ifelse(
    sample_size == "full",
    sprintf("%.4f\u00b1%.4f N=%s", metric_mean, metric_sd,
            format(n_train, big.mark = ",")),
    sprintf("%.4f\u00b1%.4f", metric_mean, metric_sd)
  )]
  p_plot[, sample_size := factor(sample_size, levels = size_levels)]
  p_plot[, y_pos := factor(train_subsets, levels = y_levels)]

  pval_other <- merge(
    p_pval[compare == "other"], means_wide, by = "sample_size"
  )[, .(
    sample_size = factor(sample_size, levels = size_levels),
    y_pos       = factor("other-same", levels = y_levels),
    seg_lo      = pmin(same, other),
    seg_hi      = pmax(same, other),
    p_label
  )]
  pval_all <- merge(
    p_pval[compare == "all"], means_wide, by = "sample_size"
  )[, .(
    sample_size = factor(sample_size, levels = size_levels),
    y_pos       = factor("all-same", levels = y_levels),
    seg_lo      = pmin(same, all),
    seg_hi      = pmax(same, all),
    p_label
  )]
  pval_rows <- rbindlist(list(pval_other, pval_all))

  x_min   <- min(p_plot$metric_mean - p_plot$metric_sd)
  x_max   <- max(p_plot$metric_mean + p_plot$metric_sd)
  x_range <- x_max - x_min
  x_lower <- x_min - 0.05 * x_range
  x_upper <- x_max + 0.55 * x_range

  gg <- ggplot() +
    geom_linerange(
      data = p_plot,
      aes(
        y    = y_pos,
        xmin = metric_mean - metric_sd,
        xmax = metric_mean + metric_sd
      ),
      linewidth = 1.0
    ) +
    geom_point(
      data = p_plot,
      aes(y = y_pos, x = metric_mean),
      shape = 1, size = 2, stroke = 0.8
    ) +
    geom_text(
      data = p_plot,
      aes(y = y_pos, x = metric_mean, label = row_label),
      hjust = -0.1, vjust = -0.5, size = 3.2
    ) +
    geom_segment(
      data = pval_rows,
      aes(y = y_pos, yend = y_pos, x = seg_lo, xend = seg_hi),
      color = "gray50", linewidth = 0.5
    ) +
    geom_text(
      data = pval_rows,
      aes(y = y_pos, x = (seg_lo + seg_hi) / 2, label = p_label),
      color = "gray30", size = 3.0, vjust = -0.4
    ) +
    facet_wrap(~sample_size, nrow = 1, labeller = label_both) +
    scale_y_discrete(limits = y_levels) +
    scale_x_continuous(
      limits = c(x_lower, x_upper),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      y     = "Train subsets",
      x     = cfg$metric_label,
      title = sprintf("%s SOAKED analysis, test subset=%s", dataset_name, sub)
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.spacing    = unit(0.8, "lines"),
      strip.background = element_rect(fill = "gray85"),
      strip.text       = element_text(color = "black")
    )

  out_png <- file.path(
    results_dir,
    sprintf("%s_glmnet_%s.png", dataset_name, sub)
  )
  ggsave(out_png, gg, width = 12, height = 4, dpi = 300, bg = "white")
  message("Saved: ", out_png)
}

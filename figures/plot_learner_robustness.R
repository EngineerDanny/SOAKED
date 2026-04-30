library(data.table)
library(ggplot2)
if (!exists("text_size_ratio")) text_size_ratio <- 1

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

raw_csv <- file.path(repo_root, "results", "learner_robustness_runs.csv")
output_png <- file.path(repo_root, "results", "learner_robustness_plot.png")
paper_png <- file.path(repo_root, "paper", "figures", "learner_robustness_plot.png")

add_sample_size_labels <- function(dt) {
  smallest_dt <- dt[, .(smallest_n = min(n_train_groups)), by = test_subset]
  full_dt <- dt[, .(full_n = max(n_train_groups)), by = .(test_subset, train_subsets)]
  dt <- merge(dt, smallest_dt, by = "test_subset")
  dt <- merge(dt, full_dt, by = c("test_subset", "train_subsets"))
  rbindlist(list(
    dt[n_train_groups == smallest_n][, sample_size := "sample size: matched"],
    dt[n_train_groups == full_n][, sample_size := "sample size: full"]
  ), use.names = TRUE)
}

raw_dt <- fread(raw_csv)
raw_dt <- raw_dt[learner != "random_forest"]
raw_dt <- raw_dt[train_subsets %in% c("same", "other")]
raw_dt <- add_sample_size_labels(raw_dt)
raw_dt <- raw_dt[test_subset == "Phoenix"]
plot_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_sd = sd(metric_value)
), by = .(learner, test_subset, sample_size, train_subsets, metric_label)]
panel_dt <- unique(raw_dt[, .(test_subset, sample_size, train_subsets, n_train_groups)])
panel_dt <- dcast(
  panel_dt,
  test_subset + sample_size ~ train_subsets,
  value.var = "n_train_groups"
)

learner_labels <- c(
  featureless = "featureless",
  rpart = "rpart",
  glmnet = "glmnet",
  torch_mlp = "MLP"
)
plot_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
raw_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
plot_dt[, train_subsets := factor(train_subsets, levels = c("same", "other"))]
raw_dt[, train_subsets := factor(train_subsets, levels = c("same", "other"))]
plot_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]
raw_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]
plot_dt[, sample_size := factor(
  sample_size,
  levels = c("sample size: full", "sample size: matched")
)]
raw_dt[, sample_size := factor(
  sample_size,
  levels = c("sample size: full", "sample size: matched")
)]
plot_dt[, text_label := sprintf("%.3f±%.3f", metric_mean, metric_sd)]
plot_dt[, text_hjust := ifelse(
  train_subsets == "other" & learner != "featureless",
  1.25,
  -0.2
)]
panel_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]
panel_dt[, sample_size := factor(
  sample_size,
  levels = c("sample size: full", "sample size: matched")
)]
panel_dt[, n_label := ifelse(
  same == other,
  sprintf("N = %s", format(same, big.mark = ",")),
  sprintf(
    "N[same] = %s\nN[other] = %s",
    format(same, big.mark = ","),
    format(other, big.mark = ",")
  )
)]

range_dt <- plot_dt[, .(
  panel_min = min(metric_mean - metric_sd),
  panel_max = max(metric_mean + metric_sd)
), by = sample_size]
range_dt[, panel_range := panel_max - panel_min]
range_dt[, panel_lower := panel_min - 0.24 * panel_range * text_size_ratio]
range_dt[, panel_upper := panel_max + 0.10 * panel_range * text_size_ratio]

panel_dt <- merge(panel_dt, range_dt, by = "sample_size")
panel_dt[, n_label_y := ifelse(
  sample_size == "sample size: matched",
  0.91,
  0.86
)]

subset_colors <- c(same = "red", other = "blue")
subset_labels <- c(same = "Same", other = "Other")
dodge <- position_dodge2(width = 0.6, reverse = TRUE)

pvalue_dt <- raw_dt[, {
  paired_dt <- dcast(.SD, fold + seed ~ train_subsets, value.var = "metric_value")
  if (!all(c("same", "other") %in% names(paired_dt))) {
    NULL
  } else {
    test_result <- t.test(paired_dt$same, paired_dt$other, paired = TRUE)
    .(p_value = unname(test_result$p.value))
  }
}, by = .(learner, test_subset, sample_size)]

line_dt <- dcast(
  plot_dt,
  learner + test_subset + sample_size ~ train_subsets,
  value.var = "metric_mean"
)
pvalue_dt <- merge(pvalue_dt, line_dt, by = c("learner", "test_subset", "sample_size"))
pvalue_dt <- merge(pvalue_dt, range_dt, by = "sample_size")
pvalue_dt[, learner_num := match(as.character(learner), unname(learner_labels))]
pvalue_dt[, p_label := ifelse(p_value < 1e-4, "P<0.0001", sprintf("P=%.4f", p_value))]
pvalue_dt[, y_start := pmin(same, other)]
pvalue_dt[, y_end := pmax(same, other)]
pvalue_dt[, x_text := learner_num]
pvalue_dt[, y_text := y_start - ifelse(
  as.character(learner) %in% c("glmnet", "rpart"),
  0.03 * panel_range,
  0.02 * panel_range
)]
pvalue_dt[, hjust_text := 1]

figure <- ggplot(
  plot_dt,
  aes(x = learner, y = metric_mean, color = train_subsets)
) +
  geom_linerange(
    aes(
      ymin = metric_mean - metric_sd,
      ymax = metric_mean + metric_sd
    ),
    position = dodge,
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  geom_point(
    position = dodge,
    size = 2,
    shape = 1,
    stroke = 0.8
  ) +
  geom_text(
    aes(label = text_label, hjust = text_hjust),
    position = dodge,
    size = 3.88 * text_size_ratio,
    show.legend = FALSE
  ) +
  geom_segment(
    data = pvalue_dt,
    aes(x = learner_num, xend = learner_num, y = y_start, yend = y_end),
    inherit.aes = FALSE,
    color = "gray35",
    linewidth = 0.4
  ) +
  geom_text(
    data = pvalue_dt,
    aes(x = x_text, y = y_text, label = p_label),
    inherit.aes = FALSE,
    color = "gray20",
    hjust = pvalue_dt$hjust_text,
    vjust = 0.5,
    size = 3.88 * text_size_ratio
  ) +
  geom_text(
    data = panel_dt,
    aes(x = "featureless", y = n_label_y, label = n_label),
    inherit.aes = FALSE,
    color = "gray20",
    hjust = 0,
    vjust = 0,
    size = 3.88 * text_size_ratio
  ) +
  facet_grid(test_subset ~ sample_size, scales = "free_y", space = "free_y") +
  coord_flip() +
  scale_color_manual(
    values = subset_colors,
    breaks = c("same", "other"),
    labels = subset_labels,
    guide = guide_legend(
      override.aes = list(
        shape = 1,
        linewidth = 0,
        size = 2.5,
        stroke = 0.8
      )
    )
  ) +
  scale_y_continuous(
    limits = c(0.85, 1.2),
    breaks = c(0.9, 1.0, 1.1, 1.2),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Test RMSE (mean\u00b1SD, 5 CV folds)",
    color = NULL
  ) +
  theme_bw(base_size = 11 * text_size_ratio) +
  theme(
    axis.text.y = element_text(size = 13 * text_size_ratio),
    legend.position = "inside",
    legend.position.inside = c(0.99, 0.22),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "grey90", color = NA),
    legend.key = element_rect(fill = scales::alpha("white", 0), color = NA),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing = grid::unit(0, "pt"),
    legend.spacing.x = grid::unit(0, "pt"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.key.width = grid::unit(10, "pt"),
    legend.key.height = grid::unit(10, "pt")
  )

ggsave(output_png, figure, width = 12.2, height = 3.2, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

library(data.table)
library(ggplot2)

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
    dt[n_train_groups == smallest_n][, sample_size := "Matched smallest size"],
    dt[n_train_groups == full_n][, sample_size := "Full train sets"]
  ), use.names = TRUE)
}

raw_dt <- fread(raw_csv)
raw_dt <- raw_dt[train_subsets %in% c("same", "other")]
raw_dt <- add_sample_size_labels(raw_dt)
plot_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_se = sd(metric_value) / sqrt(.N)
), by = .(learner, test_subset, sample_size, train_subsets, metric_label)]

learner_labels <- c(
  featureless = "Featureless",
  rpart = "rpart",
  glmnet = "glmnet",
  torch_mlp = "Deep neural network",
  random_forest = "Random forest"
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
  levels = c("Full train sets", "Matched smallest size")
)]
raw_dt[, sample_size := factor(
  sample_size,
  levels = c("Full train sets", "Matched smallest size")
)]
plot_dt[, text_label := sprintf("%.3f±%.3f", metric_mean, metric_se)]

x_min <- min(plot_dt$metric_mean - plot_dt$metric_se)
x_max <- max(plot_dt$metric_mean + plot_dt$metric_se)
x_range <- x_max - x_min
x_lower <- x_min - 0.24 * x_range
x_upper <- x_max + 0.20 * x_range

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
pvalue_dt[, learner_num := match(as.character(learner), unname(learner_labels))]
pvalue_dt[, p_label := ifelse(p_value < 1e-4, "P<0.0001", sprintf("P=%.4f", p_value))]
pvalue_dt[, y_start := pmin(same, other)]
pvalue_dt[, y_end := pmax(same, other)]
pvalue_dt[, x_text := learner_num]
pvalue_dt[, y_text := y_start - 0.02 * x_range]
pvalue_dt[, hjust_text := 1]

figure <- ggplot(
  plot_dt,
  aes(x = learner, y = metric_mean, color = train_subsets)
) +
  geom_linerange(
    aes(
      ymin = metric_mean - metric_se,
      ymax = metric_mean + metric_se
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
    aes(label = text_label),
    position = dodge,
    hjust = -0.2,
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
    vjust = 0.5
  ) +
  facet_grid(test_subset ~ sample_size) +
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
    limits = c(x_lower, x_upper),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Test RMSE on log(AADT), mean ± SE across 5 CV splits",
    color = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray15"),
    strip.text = element_text(color = "white", face = "bold")
  )

ggsave(output_png, figure, width = 11, height = 6.5, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

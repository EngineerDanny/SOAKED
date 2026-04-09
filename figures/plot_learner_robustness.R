library(data.table)
library(ggplot2)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

input_csv <- file.path(repo_root, "results", "learner_robustness_summary.csv")
output_png <- file.path(repo_root, "results", "learner_robustness_plot.png")
paper_png <- file.path(repo_root, "paper", "figures", "learner_robustness_plot.png")

plot_dt <- fread(input_csv)
plot_dt <- plot_dt[train_subsets %in% c("same", "other")]
smallest_dt <- plot_dt[, .(smallest_n = min(n_train_groups)), by = test_subset]
full_dt <- plot_dt[, .(full_n = max(n_train_groups)), by = .(test_subset, train_subsets)]
plot_dt <- merge(plot_dt, smallest_dt, by = "test_subset")
plot_dt <- merge(plot_dt, full_dt, by = c("test_subset", "train_subsets"))
plot_dt <- rbindlist(list(
  plot_dt[n_train_groups == smallest_n][, sample_size := "Matched smallest size"],
  plot_dt[n_train_groups == full_n][, sample_size := "Full train sets"]
), use.names = TRUE)

learner_labels <- c(
  featureless = "Featureless",
  rpart = "rpart",
  glmnet = "glmnet"
)
plot_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
plot_dt[, train_subsets := factor(train_subsets, levels = c("same", "other"))]
plot_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]
plot_dt[, sample_size := factor(
  sample_size,
  levels = c("Full train sets", "Matched smallest size")
)]
plot_dt[, text_label := sprintf("%.3f±%.3f", metric_mean, metric_se)]

x_min <- min(plot_dt$metric_mean - plot_dt$metric_se)
x_max <- max(plot_dt$metric_mean + plot_dt$metric_se)
x_range <- x_max - x_min
x_upper <- x_max + 0.20 * x_range

subset_colors <- c(same = "red", other = "blue")
dodge <- position_dodge(width = 0.6)

figure <- ggplot(
  plot_dt,
  aes(x = learner, y = metric_mean, color = train_subsets)
) +
  geom_errorbar(
    aes(
      ymin = metric_mean - metric_se,
      ymax = metric_mean + metric_se
    ),
    position = dodge,
      width = 0.2,
      linewidth = 0.4
  ) +
  geom_point(position = dodge, size = 2) +
  geom_text(
    aes(label = text_label),
    position = dodge,
    hjust = -0.2,
    size = 3,
    show.legend = FALSE
  ) +
  facet_grid(test_subset ~ sample_size) +
  coord_flip() +
  scale_color_manual(values = subset_colors, breaks = c("same", "other")) +
  scale_y_continuous(
    limits = c(x_min, x_upper),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = unique(plot_dt$metric_label),
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

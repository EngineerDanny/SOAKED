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

learner_labels <- c(
  featureless = "Featureless",
  glmnet = "glmnet"
)
plot_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
plot_dt[, train_subsets := factor(train_subsets, levels = c("same", "other"))]
plot_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]
plot_dt[, train_size_label := factor(
  paste0("Train groups: ", n_train_groups),
  levels = paste0("Train groups: ", sort(unique(n_train_groups)))
)]

x_min <- min(plot_dt$metric_mean - plot_dt$metric_se)
x_max <- max(plot_dt$metric_mean + plot_dt$metric_se)
x_range <- x_max - x_min
x_upper <- x_max + 0.05 * x_range

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
  facet_grid(test_subset ~ train_size_label) +
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

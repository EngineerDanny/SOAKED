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
  rpart = "rpart",
  glmnet = "glmnet",
  random_forest = "Random forest",
  gradient_boosting = "Gradient boosting",
  dnn = "DNN"
)
plot_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
plot_dt[, train_subsets := factor(train_subsets, levels = c("same", "other"))]
plot_dt[, sample_size := factor(
  sample_size,
  levels = c("full", "smallest"),
  labels = c("Full train sets", "Matched smallest size")
)]
plot_dt[, test_subset := factor(
  test_subset,
  levels = c("Baltimore", "Phoenix"),
  labels = c("Test subset: Baltimore", "Test subset: Phoenix")
)]

time_dt <- dcast(
  plot_dt,
  learner + test_subset + sample_size ~ train_subsets,
  value.var = "train_seconds_mean"
)
time_dt[, time_label := sprintf("time: %.02f / %.02f s", same, other)]

x_min <- min(plot_dt$metric_mean - plot_dt$metric_se)
x_max <- max(plot_dt$metric_mean + plot_dt$metric_se)
x_range <- x_max - x_min
text_x <- x_max + 0.12 * x_range
x_upper <- x_max + 0.40 * x_range

plot_dt <- merge(
  plot_dt,
  time_dt[, .(learner, test_subset, sample_size, time_label)],
  by = c("learner", "test_subset", "sample_size"),
  all.x = TRUE
)
label_dt <- unique(plot_dt[, .(learner, test_subset, sample_size, time_label)])

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
    data = label_dt,
    aes(x = learner, y = text_x, label = time_label),
    inherit.aes = FALSE,
    color = "gray35",
    hjust = 0,
    size = 3
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
    color = NULL,
    caption = "Gray labels show mean training seconds for same / other."
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray15"),
    strip.text = element_text(color = "white", face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

ggsave(output_png, figure, width = 11, height = 6.5, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

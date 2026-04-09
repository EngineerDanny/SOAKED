library(data.table)
library(ggplot2)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

input_csv <- file.path(repo_root, "results", "learner_train_time_summary.csv")
output_png <- file.path(repo_root, "results", "learner_train_time_plot.png")
paper_png <- file.path(repo_root, "paper", "figures", "learner_train_time_plot.png")

plot_dt <- fread(input_csv)

learner_labels <- c(
  featureless = "Featureless",
  rpart = "rpart",
  random_forest = "Random forest",
  glmnet = "glmnet"
)
plot_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
plot_dt[, text_label := sprintf("%.3f±%.3f", train_seconds_mean, train_seconds_se)]

x_min <- min(plot_dt$train_seconds_mean - plot_dt$train_seconds_se)
x_max <- max(plot_dt$train_seconds_mean + plot_dt$train_seconds_se)
x_range <- x_max - x_min
x_upper <- x_max + 0.18 * x_range

figure <- ggplot(plot_dt, aes(x = learner, y = train_seconds_mean)) +
  geom_linerange(
    aes(
      ymin = train_seconds_mean - train_seconds_se,
      ymax = train_seconds_mean + train_seconds_se
    ),
    linewidth = 0.4,
    color = "gray35"
  ) +
  geom_point(
    shape = 21,
    fill = "white",
    color = "black",
    size = 2.2,
    stroke = 0.8
  ) +
  geom_text(
    aes(label = text_label),
    hjust = -0.2,
    size = 3,
    color = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(x_min, x_upper),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Train time (s, mean±SE)"
  ) +
  theme_bw()

ggsave(output_png, figure, width = 7.5, height = 3.8, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

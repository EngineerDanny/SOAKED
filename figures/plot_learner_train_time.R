library(data.table)
library(ggplot2)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

input_csv <- file.path(repo_root, "results", "learner_train_time_runs.csv")
output_png <- file.path(repo_root, "results", "learner_train_time_plot.png")
paper_png <- file.path(repo_root, "paper", "figures", "learner_train_time_plot.png")

run_dt <- fread(input_csv)

learner_labels <- c(
  featureless = "Featureless",
  rpart = "rpart",
  glmnet = "glmnet",
  random_forest = "Random forest"
)
run_dt[, learner := factor(learner, levels = names(learner_labels), labels = unname(learner_labels))]
run_dt <- unique(run_dt, by = c("learner", "iteration", "train_seconds"))
run_dt <- run_dt[order(learner, train_seconds)]
run_dt[, learner_num := match(as.character(learner), unname(learner_labels))]
run_dt[, run_rank := seq_len(.N), by = learner]
run_dt[, run_count := .N, by = learner]
run_dt[, y_pos := learner_num + (run_rank - (run_count + 1) / 2) * (0.6 / pmax(run_count, 1))]

mean_dt <- run_dt[, .(
  train_seconds_mean = mean(train_seconds),
  train_seconds_se = sd(train_seconds) / sqrt(.N)
), by = .(learner, learner_num)]
mean_dt[, text_label := sprintf("%.3f±%.3f", train_seconds_mean, train_seconds_se)]

x_min <- 0
x_max <- max(run_dt$train_seconds)
x_range <- x_max - x_min
x_upper <- x_max + 0.18 * x_range

figure <- ggplot() +
  geom_segment(
    data = run_dt,
    aes(x = 0, xend = train_seconds, y = y_pos, yend = y_pos),
    linewidth = 0.35,
    color = "gray40",
    alpha = 0.5
  ) +
  geom_point(
    data = mean_dt,
    aes(x = train_seconds_mean, y = learner_num),
    shape = 21,
    fill = "white",
    color = "black",
    size = 2.2,
    stroke = 0.8
  ) +
  geom_text(
    data = mean_dt,
    aes(x = train_seconds_mean, y = learner_num, label = text_label),
    hjust = -0.2,
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(x_min, x_upper),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq_along(learner_labels),
    labels = unname(learner_labels),
    limits = c(0.5, length(learner_labels) + 0.5)
  ) +
  labs(
    x = "Train time (s)",
    y = NULL
  ) +
  theme_bw()

ggsave(output_png, figure, width = 7.5, height = 3.8, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

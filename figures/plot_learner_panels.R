library(patchwork)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

robustness_script <- file.path(repo_root, "figures", "plot_learner_robustness.R")
time_script <- file.path(repo_root, "figures", "plot_learner_train_time.R")
output_png <- file.path(repo_root, "results", "learner_panels.png")
paper_png <- file.path(repo_root, "paper", "figures", "learner_panels.png")

robustness_env <- new.env(parent = globalenv())
sys.source(robustness_script, envir = robustness_env)
robustness_plot <- robustness_env$figure +
  labs(title = "A) Learner Robustness Across Train-Subset Conditions") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

time_env <- new.env(parent = globalenv())
sys.source(time_script, envir = time_env)
time_plot <- time_env$figure +
  labs(title = "B) Per-Run Training Time Across Learners") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0)
  )

combined_plot <- (robustness_plot / time_plot) +
  plot_layout(heights = c(2.0, 1.4), guides = "collect") &
  theme(
    legend.position = "bottom"
  )

ggsave(output_png, combined_plot, width = 15, height = 7.1, dpi = 300, bg = "white")
file.copy(output_png, paper_png, overwrite = TRUE)

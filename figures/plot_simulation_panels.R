library(data.table)
library(ggplot2)
library(patchwork)
library(grid)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

arg_list <- list()
for (arg in commandArgs(trailingOnly = TRUE)) {
  if (!startsWith(arg, "--")) {
    stop(sprintf("Unsupported argument format: %s", arg))
  }
  kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1L]]
  key <- kv[[1L]]
  value <- if (length(kv) >= 2L) paste(kv[-1L], collapse = "=") else "TRUE"
  arg_list[[key]] <- value
}

input_csv <- arg_list[["input-csv"]]
if (is.null(input_csv)) {
  input_csv <- file.path(repo_root, "results", "simulation", "simulation_panel_data.csv")
}

output_png <- arg_list[["output-png"]]
if (is.null(output_png)) {
  output_png <- file.path(repo_root, "results", "simulation", "combined_simulation_panels.png")
}
paper_png <- file.path(repo_root, "paper", "figures", "combined_simulation_panels.png")

figure_width <- arg_list[["width"]]
if (is.null(figure_width)) {
  figure_width <- 11.5
} else {
  figure_width <- as.numeric(figure_width)
}

figure_height <- arg_list[["height"]]
if (is.null(figure_height)) {
  figure_height <- 3.9
} else {
  figure_height <- as.numeric(figure_height)
}

plot_dt <- fread(input_csv)
case_order <- c("same_distribution", "mean_shift", "variance_shift")
case_label_map <- c(
  same_distribution = "Sample Size Effect",
  mean_shift = "Subset Effect",
  variance_shift = "Combined Effect"
)
case_ids_present <- unique(plot_dt$case_id)
case_ids_present <- case_order[case_order %in% case_ids_present]
case_levels <- unname(case_label_map[case_ids_present])
plot_dt[, case_label := factor(case_label, levels = case_levels)]

rmse_dt <- plot_dt[panel_type == "rmse"]
scatter_dt <- plot_dt[panel_type == "scatter"]
category_colors <- c(same = "red", other = "blue", all = "green")
legend_order <- c("same", "other", "all")
scatter_colors <- c(other = "blue", same = "red")

scatter_plots <- vector("list", length(case_levels))
rmse_plots <- vector("list", length(case_levels))
column_plots <- vector("list", length(case_levels))
middle_case_i <- ceiling(length(case_levels) / 2)

for (case_i in seq_along(case_levels)) {
  current_case_label <- case_levels[[case_i]]
  case_rmse_dt <- rmse_dt[case_label == current_case_label][order(train_size)]
  case_scatter_dt <- scatter_dt[case_label == current_case_label]
  has_multiple_train_sizes <- uniqueN(case_rmse_dt$train_size) > 1L

  p_scatter <- ggplot(case_scatter_dt, aes(x = x, y = y, color = category)) +
    geom_point(shape = 1, alpha = 0.9) +
    scale_color_manual(values = scatter_colors, guide = "none") +
    labs(
      x = "feature",
      y = "target",
      color = NULL
    ) +
    theme_bw()
  if (case_i != 1L) {
    p_scatter <- p_scatter + theme(axis.title.y = element_blank())
  }
  if (case_i != middle_case_i) {
    p_scatter <- p_scatter + theme(axis.title.x = element_blank())
  }
  scatter_plots[[case_i]] <- p_scatter

  p_rmse <- ggplot(case_rmse_dt, aes(
    x = x,
    y = y,
    color = category,
    fill = category,
    group = category
  ))
  if (has_multiple_train_sizes) {
    p_rmse <- p_rmse + geom_line()
  }
  p_rmse <- p_rmse +
    geom_point() +
    geom_ribbon(aes(
      ymin = ymin,
      ymax = ymax
    ), alpha = 0.2, color = NA) +
    scale_color_manual(values = category_colors, breaks = legend_order) +
    scale_fill_manual(values = category_colors, breaks = legend_order) +
    guides(fill = "none") +
    labs(
      x = "Train Size",
      y = "RMSE (mean ± SE)",
      color = NULL,
      fill = NULL
    ) +
    theme_bw()
  if (case_i != 1L) {
    p_rmse <- p_rmse + theme(axis.title.y = element_blank())
  }
  if (case_i != middle_case_i) {
    p_rmse <- p_rmse + theme(axis.title.x = element_blank())
  }
  rmse_plots[[case_i]] <- p_rmse

  title_plot <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = current_case_label, fontface = "bold") +
    xlim(0, 1) +
    ylim(0, 1)

  column_plots[[case_i]] <- title_plot / p_scatter / p_rmse +
    plot_layout(heights = c(0.12, 1, 1))
}

combined_plot <- wrap_plots(
  column_plots,
  nrow = 1,
  guides = "collect"
) & theme(legend.position = "right")

ggsave(output_png, combined_plot, width = figure_width, height = figure_height, dpi = 300, bg = "white")
if (!identical(normalizePath(output_png, mustWork = FALSE), normalizePath(paper_png, mustWork = FALSE))) {
  file.copy(output_png, paper_png, overwrite = TRUE)
}

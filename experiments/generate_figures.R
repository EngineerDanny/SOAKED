library(data.table)
library(mlr3)
library(mlr3resampling)
library(rpart)

set.seed(0)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)

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

parse_int_arg <- function(name, default) {
  value <- arg_list[[name]]
  if (is.null(value)) {
    return(default)
  }
  as.integer(value)
}

# ---- 1) Parameters ----
n_data <- parse_int_arg("n-data", 500L)
n_splits <- parse_int_arg("n-splits", 10L)
n_random_seeds <- parse_int_arg("n-random-seeds", 10L)
max_train_size <- (n_data * (n_splits - 1L)) %/% n_splits
train_size_list <- arg_list[["train-sizes"]]
if (is.null(train_size_list)) {
  train_size_list <- seq.int(20L, max_train_size - 1L, by = 20L)
} else {
  train_size_list <- as.integer(strsplit(train_size_list, ",", fixed = TRUE)[[1L]])
}
output_csv <- arg_list[["output-csv"]]
if (is.null(output_csv)) {
  output_csv <- file.path(repo_root, "results", "simulation", "simulation_panel_data.csv")
}

# ---- 2) Create synthetic datasets ----
create_subset <- function(n, noise_sd, offset) {
  x <- runif(n, -5, 5)
  y <- offset + sin(x) + rnorm(n, 0, noise_sd)
  data.table(x = x, y = y)
}

set_a <- create_subset(n_data, 1, 0)
set_a2 <- create_subset(n_data, 1, 0)
set_b <- create_subset(n_data, 3, 0)
set_c <- create_subset(n_data, 1, 3)

# ---- 3) Define comparison cases ----
cases <- list(
  list(
    case_id = "same_distribution",
    case_label = "Sample Size Effect",
    other_name = "A2",
    same_name = "A",
    other_df = set_a2,
    same_df = set_a
  ),
  list(
    case_id = "mean_shift",
    case_label = "Subset Effect",
    other_name = "C",
    same_name = "A",
    other_df = set_c,
    same_df = set_a
  ),
  list(
    case_id = "variance_shift",
    case_label = "Variance Effect",
    other_name = "B",
    same_name = "A",
    other_df = set_b,
    same_df = set_a
  )
)

case_filter <- arg_list[["cases"]]
if (is.null(case_filter)) {
  case_filter <- vapply(cases, `[[`, "", "case_id")
} else {
  case_filter <- strsplit(case_filter, ",", fixed = TRUE)[[1L]]
}
cases <- cases[vapply(cases, function(case) case$case_id %in% case_filter, logical(1))]
if (!length(cases)) {
  stop("No cases selected after applying --cases filter.")
}

# ---- 4) Use SOAK API only for outer fold construction ----
task <- mlr3::TaskRegr$new("same_only", set_a, target = "y")
resampling <- mlr3resampling::ResamplingSameOtherSizesCV$new()
resampling$param_set$values$folds <- n_splits
resampling$param_set$values$seeds <- 1L
resampling$param_set$values$sizes <- -1L
resampling$param_set$values$ignore_subset <- TRUE
resampling$param_set$values$subsets <- "S"
resampling$instantiate(task)
outer_folds <- unique(resampling$instance$iteration.dt[, .(
  test.fold,
  train = train,
  test = test
)], by = "test.fold")[order(test.fold)]

# ---- 5) Tree fitting ----
fit_tree_default <- function(df) {
  rpart::rpart(
    y ~ x,
    data = df,
    method = "anova"
  )
}

predict_tree <- function(model, df) {
  predict(model, newdata = df)
}

sample_rows <- function(df, size) {
  df[sample.int(nrow(df), size = size, replace = FALSE)]
}

# ---- 6) Main experiment ----
all_results <- list()
result_i <- 1L
for (case in cases) {
  message(sprintf("Running case: %s", case$case_label))
  for (train_size in train_size_list) {
    results <- vector("list", nrow(outer_folds) * n_random_seeds * 3L)
    score_i <- 1L
    for (fold_i in seq_len(nrow(outer_folds))) {
      fold_row <- outer_folds[fold_i]
      train_idx <- fold_row$train[[1L]]
      test_idx <- fold_row$test[[1L]]
      test_fold <- case$same_df[test_idx]
      remaining_same <- case$same_df[train_idx]
      remaining_other <- case$other_df[train_idx]
      for (seed in 0:(n_random_seeds - 1L)) {
        set.seed(seed)
        train_same <- sample_rows(remaining_same, train_size)
        model_same <- fit_tree_default(train_same)
        pred_same <- predict_tree(model_same, test_fold)
        results[[score_i]] <- data.table(
          category = "same",
          fold = fold_row$test.fold,
          seed = seed,
          rmse = sqrt(mean((test_fold$y - pred_same)^2))
        )
        score_i <- score_i + 1L

        set.seed(seed)
        train_other <- sample_rows(remaining_other, train_size)
        model_other <- fit_tree_default(train_other)
        pred_other <- predict_tree(model_other, test_fold)
        results[[score_i]] <- data.table(
          category = "other",
          fold = fold_row$test.fold,
          seed = seed,
          rmse = sqrt(mean((test_fold$y - pred_other)^2))
        )
        score_i <- score_i + 1L

        set.seed(seed)
        half_same <- train_size %/% 2L
        half_other <- train_size - half_same
        train_all <- rbind(
          sample_rows(remaining_same, half_same),
          sample_rows(remaining_other, half_other)
        )
        model_all <- fit_tree_default(train_all)
        pred_all <- predict_tree(model_all, test_fold)
        results[[score_i]] <- data.table(
          category = "all",
          fold = fold_row$test.fold,
          seed = seed,
          rmse = sqrt(mean((test_fold$y - pred_all)^2))
        )
        score_i <- score_i + 1L
      }
    }
    summary_dt <- rbindlist(results)[, .(
      rmse_mean = mean(rmse),
      rmse_se = sd(rmse) / sqrt(.N),
      n_obs = .N
    ), by = category]
    summary_dt[, `:=`(
      case_id = case$case_id,
      case_label = case$case_label,
      other_name = case$other_name,
      same_name = case$same_name,
      train_size = train_size
    )]
    all_results[[result_i]] <- summary_dt
    result_i <- result_i + 1L
  }
}

final_dt <- rbindlist(all_results, fill = TRUE)
setorder(final_dt, case_label, category, train_size)

# ---- 7) Write plotting data ----
category_colors <- c(same = "red", other = "blue", all = "green")
shift_vals <- c(same = -5, all = 0, other = 5)
final_dt[, shifted_size := train_size + shift_vals[category]]
rmse_plot_dt <- final_dt[, .(
  panel_type = "rmse",
  case_id,
  case_label,
  x = shifted_size,
  y = rmse_mean,
  ymin = rmse_mean - rmse_se,
  ymax = rmse_mean + rmse_se,
  category,
  train_size,
  n_splits,
  n_random_seeds
)]

scatter_rows <- list()
scatter_i <- 1L
for (case in cases) {
  scatter_rows[[scatter_i]] <- data.table(
    panel_type = "scatter",
    case_id = case$case_id,
    case_label = case$case_label,
    x = case$other_df$x,
    y = case$other_df$y,
    ymin = NA_real_,
    ymax = NA_real_,
    category = "other",
    train_size = NA_integer_,
    n_splits = n_splits,
    n_random_seeds = n_random_seeds
  )
  scatter_i <- scatter_i + 1L
  scatter_rows[[scatter_i]] <- data.table(
    panel_type = "scatter",
    case_id = case$case_id,
    case_label = case$case_label,
    x = case$same_df$x,
    y = case$same_df$y,
    ymin = NA_real_,
    ymax = NA_real_,
    category = "same",
    train_size = NA_integer_,
    n_splits = n_splits,
    n_random_seeds = n_random_seeds
  )
  scatter_i <- scatter_i + 1L
}

plot_dt <- rbindlist(c(list(rmse_plot_dt), scatter_rows), fill = TRUE)
fwrite(plot_dt, output_csv)

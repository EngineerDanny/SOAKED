library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3resampling)

set.seed(1)

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

parse_int_arg <- function(name, default) {
  value <- arg_list[[name]]
  if (is.null(value)) {
    return(default)
  }
  as.integer(value)
}

parse_double_arg <- function(name, default) {
  value <- arg_list[[name]]
  if (is.null(value)) {
    return(default)
  }
  as.numeric(value)
}

download_if_missing <- function(url, dest) {
  if (file.exists(dest)) {
    return(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  download.file(url, destfile = dest, quiet = TRUE)
  dest
}

sample_preserve_subset_ratio <- function(dt, subset_col, max_rows_largest_subset) {
  if (is.infinite(max_rows_largest_subset)) {
    return(copy(dt))
  }
  subset_sizes <- dt[, .N, by = subset_col]
  largest_subset_n <- max(subset_sizes$N)
  sample_fraction <- min(1, max_rows_largest_subset / largest_subset_n)
  dt[, .SD[sample.int(.N, max(1L, floor(.N * sample_fraction)))], by = subset_col]
}

make_traffic_task <- function(max_rows_largest_subset) {
  dt <- fread(file.path(repo_root, "data", "traffic.csv"))
  dt <- dt[city_key %in% c("Phoenix", "Baltimore")]
  dt <- sample_preserve_subset_ratio(dt, "city_key", max_rows_largest_subset)
  dt[, subset_id := factor(city_key)]
  task_dt <- dt[, .(
    AADT,
    NTL,
    POP,
    ROAD_DENS,
    S2_R,
    S2_G,
    S2_B,
    S2_NIR,
    subset_id
  )]
  task <- TaskRegr$new("traffic_aadt", task_dt, target = "AADT")
  task$set_col_roles("subset_id", c("subset"))
  list(
    dataset_id = "traffic_aadt",
    dataset_label = "Traffic AADT",
    task_type = "regr",
    subset_col = "city_key",
    target = "AADT",
    metric_id = "regr.rmse",
    metric_label = "RMSE",
    task = task
  )
}

make_dataset_specs <- function(dataset_ids, max_rows_largest_subset) {
  loader_map <- list(
    traffic_aadt = make_traffic_task
  )
  if (!all(dataset_ids %in% names(loader_map))) {
    unknown_ids <- setdiff(dataset_ids, names(loader_map))
    stop("Unknown dataset ids: ", paste(unknown_ids, collapse = ", "))
  }
  setNames(
    lapply(dataset_ids, function(dataset_id) loader_map[[dataset_id]](max_rows_largest_subset)),
    dataset_ids
  )
}

make_learner_spec <- function(task_type, learner_name) {
  if (task_type == "regr") {
    if (learner_name == "rpart") {
      return(list(label = "rpart", learner = lrn("regr.rpart")))
    }
    if (learner_name == "glmnet") {
      learner <- lrn("regr.cv_glmnet")
      learner$param_set$values <- list(nfolds = 5L)
      return(list(label = "glmnet", learner = learner))
    }
    if (learner_name == "random_forest") {
      learner <- lrn("regr.ranger")
      learner$param_set$values <- list(num.trees = 200L)
      return(list(label = "random_forest", learner = learner))
    }
    if (learner_name == "gradient_boosting") {
      learner <- lrn("regr.gbm")
      learner$param_set$values <- list(
        n.trees = 200L,
        interaction.depth = 3L,
        shrinkage = 0.05,
        bag.fraction = 1
      )
      return(list(label = "gradient_boosting", learner = learner))
    }
    if (learner_name == "dnn") {
      learner <- lrn("regr.nnet")
      learner$param_set$values <- list(
        size = 10L,
        decay = 1e-4,
        maxit = 200L,
        trace = FALSE,
        MaxNWts = 10000L
      )
      return(list(label = "dnn", learner = learner))
    }
  }
  if (task_type == "classif") {
    if (learner_name == "rpart") {
      return(list(label = "rpart", learner = lrn("classif.rpart")))
    }
    if (learner_name == "glmnet") {
      learner <- lrn("classif.cv_glmnet")
      learner$param_set$values <- list(nfolds = 5L)
      return(list(label = "glmnet", learner = learner))
    }
    if (learner_name == "random_forest") {
      learner <- lrn("classif.ranger")
      learner$param_set$values <- list(num.trees = 200L)
      return(list(label = "random_forest", learner = learner))
    }
    if (learner_name == "gradient_boosting") {
      learner <- lrn("classif.gbm")
      learner$param_set$values <- list(
        n.trees = 200L,
        interaction.depth = 3L,
        shrinkage = 0.05,
        bag.fraction = 1
      )
      return(list(label = "gradient_boosting", learner = learner))
    }
    if (learner_name == "dnn") {
      learner <- lrn("classif.nnet")
      learner$param_set$values <- list(
        size = 10L,
        decay = 1e-4,
        maxit = 200L,
        trace = FALSE,
        MaxNWts = 10000L
      )
      return(list(label = "dnn", learner = learner))
    }
  }
  stop(sprintf("Unsupported learner '%s' for task type '%s'.", learner_name, task_type))
}

dataset_filter <- arg_list[["datasets"]]
if (is.null(dataset_filter)) {
  dataset_filter <- "traffic_aadt"
} else {
  dataset_filter <- strsplit(dataset_filter, ",", fixed = TRUE)[[1L]]
}
learner_filter <- arg_list[["learners"]]
if (is.null(learner_filter)) {
  learner_filter <- c("rpart", "glmnet", "random_forest", "gradient_boosting", "dnn")
} else {
  learner_filter <- strsplit(learner_filter, ",", fixed = TRUE)[[1L]]
}
folds <- parse_int_arg("folds", 5L)
seeds <- parse_int_arg("seeds", 1L)
max_rows_largest_subset <- parse_int_arg("max-rows-largest-subset", 5000L)
raw_output <- arg_list[["output-raw"]]
if (is.null(raw_output)) {
  raw_output <- file.path(repo_root, "results", "learner_robustness_runs.csv")
}
summary_output <- arg_list[["output-summary"]]
if (is.null(summary_output)) {
  summary_output <- file.path(repo_root, "results", "learner_robustness_summary.csv")
}

dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

dataset_specs <- make_dataset_specs(dataset_filter, max_rows_largest_subset)

all_raw <- list()
raw_i <- 1L
for (dataset_name in names(dataset_specs)) {
  dataset_spec <- dataset_specs[[dataset_name]]
  message(sprintf("Running dataset: %s", dataset_spec$dataset_label))
  task <- dataset_spec$task$clone(deep = TRUE)
  metric <- msr(dataset_spec$metric_id)

  resampling <- ResamplingSameOtherSizesCV$new()
  resampling$param_set$values$folds <- folds
  resampling$param_set$values$seeds <- seeds
  resampling$param_set$values$sizes <- 0L
  resampling$instantiate(task)

  iteration_dt <- copy(resampling$instance$iteration.dt)
  smallest_dt <- iteration_dt[, .(smallest_n = min(n.train.groups)), by = test.subset]
  full_dt <- iteration_dt[, .(full_n = max(n.train.groups)), by = .(test.subset, train.subsets)]
  iteration_dt <- merge(iteration_dt, smallest_dt, by = "test.subset")
  iteration_dt <- merge(iteration_dt, full_dt, by = c("test.subset", "train.subsets"))
  full_rows <- copy(iteration_dt[n.train.groups == full_n])
  full_rows[, sample_size := "full"]
  smallest_rows <- copy(iteration_dt[n.train.groups == smallest_n])
  smallest_rows[, sample_size := "smallest"]
  iteration_dt <- rbindlist(list(full_rows, smallest_rows), use.names = TRUE)

  for (learner_name in learner_filter) {
    learner_spec <- make_learner_spec(dataset_spec$task_type, learner_name)
    message(sprintf("  learner: %s", learner_spec$label))
    for (iter_i in seq_len(nrow(iteration_dt))) {
      iter_row <- iteration_dt[iter_i]
      learner <- learner_spec$learner$clone(deep = TRUE)
      train_seconds <- system.time(
        learner$train(task, row_ids = iter_row$train[[1L]])
      )[["elapsed"]]
      prediction <- learner$predict(task, row_ids = iter_row$test[[1L]])
      metric_value <- prediction$score(metric)
      all_raw[[raw_i]] <- data.table(
        dataset_id = dataset_spec$dataset_id,
        dataset_label = dataset_spec$dataset_label,
        task_type = dataset_spec$task_type,
        metric_id = dataset_spec$metric_id,
        metric_label = dataset_spec$metric_label,
        learner = learner_spec$label,
        test_subset = as.character(iter_row$test.subset),
        train_subsets = iter_row$train.subsets,
        sample_size = iter_row$sample_size,
        n_train_groups = iter_row$n.train.groups,
        fold = iter_row$test.fold,
        seed = iter_row$seed,
        iteration = iter_row$iteration,
        metric_value = metric_value,
        train_seconds = train_seconds
      )
      raw_i <- raw_i + 1L
    }
  }
}

raw_dt <- rbindlist(all_raw, fill = TRUE)
summary_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_se = sd(metric_value) / sqrt(.N),
  train_seconds_mean = mean(train_seconds),
  train_seconds_se = sd(train_seconds) / sqrt(.N),
  train_seconds_total = sum(train_seconds),
  n_runs = .N,
  n_train_groups = unique(n_train_groups)
), by = .(
  dataset_id,
  dataset_label,
  task_type,
  metric_id,
  metric_label,
  learner,
  test_subset,
  train_subsets,
  sample_size
)]

fwrite(raw_dt, raw_output)
fwrite(summary_dt, summary_output)

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

make_adult_task <- function(max_rows_largest_subset) {
  cache_dir <- file.path(repo_root, "data", "external")
  train_path <- file.path(cache_dir, "adult.data")
  test_path <- file.path(cache_dir, "adult.test")
  download_if_missing(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
    train_path
  )
  download_if_missing(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",
    test_path
  )
  col_names <- c(
    "age", "workclass", "fnlwgt", "education", "education_num",
    "marital_status", "occupation", "relationship", "sex", "capital_gain",
    "capital_loss", "hours_per_week", "native_country", "income"
  )
  train_dt <- fread(train_path, header = FALSE, strip.white = TRUE, col.names = col_names)
  test_dt <- fread(test_path, header = FALSE, skip = 1L, strip.white = TRUE, col.names = col_names)
  dt <- rbindlist(list(train_dt, test_dt), use.names = TRUE)
  dt[, income := gsub("\\.$", "", income)]
  dt <- dt[!Reduce(`|`, lapply(.SD, function(x) x == "?")), .SDcols = names(dt)]
  dt <- dt[sex %in% c("Male", "Female")]
  dt <- sample_preserve_subset_ratio(dt, "sex", max_rows_largest_subset)
  dt[, subset_id := factor(sex)]
  dt[, income := factor(income)]
  dt[, sex := NULL]
  task <- TaskClassif$new("adult_income", dt, target = "income")
  task$set_col_roles("subset_id", c("subset"))
  list(
    dataset_id = "adult_income",
    dataset_label = "Adult Income",
    task_type = "classif",
    subset_col = "sex",
    target = "income",
    metric_id = "classif.ce",
    metric_label = "Classification Error",
    task = task
  )
}

make_wine_task <- function(max_rows_largest_subset) {
  cache_dir <- file.path(repo_root, "data", "external")
  red_path <- file.path(cache_dir, "winequality-red.csv")
  white_path <- file.path(cache_dir, "winequality-white.csv")
  download_if_missing(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
    red_path
  )
  download_if_missing(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
    white_path
  )
  red_dt <- fread(red_path, sep = ";")
  white_dt <- fread(white_path, sep = ";")
  red_dt[, wine_type := "red"]
  white_dt[, wine_type := "white"]
  dt <- rbindlist(list(red_dt, white_dt), use.names = TRUE)
  dt <- sample_preserve_subset_ratio(dt, "wine_type", max_rows_largest_subset)
  dt[, subset_id := factor(wine_type)]
  dt[, wine_type := NULL]
  task <- TaskRegr$new("wine_quality", dt, target = "quality")
  task$set_col_roles("subset_id", c("subset"))
  list(
    dataset_id = "wine_quality",
    dataset_label = "Wine Quality",
    task_type = "regr",
    subset_col = "wine_type",
    target = "quality",
    metric_id = "regr.rmse",
    metric_label = "RMSE",
    task = task
  )
}

make_dataset_specs <- function(dataset_ids, max_rows_largest_subset) {
  loader_map <- list(
    traffic_aadt = make_traffic_task,
    adult_income = make_adult_task,
    wine_quality = make_wine_task
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

classify_conclusion <- function(metric_dt, minimize, sample_ratio_cutoff, persist_ratio_cutoff) {
  full_dt <- metric_dt[sample_size == "full"]
  smallest_dt <- metric_dt[sample_size == "smallest"]
  if (nrow(full_dt) != 3L || nrow(smallest_dt) != 3L) {
    return("incomplete")
  }
  if (minimize) {
    full_dt[, score := -metric_mean]
    smallest_dt[, score := -metric_mean]
  } else {
    full_dt[, score := metric_mean]
    smallest_dt[, score := metric_mean]
  }
  full_gap <- max(full_dt$score) - min(full_dt$score)
  smallest_gap <- max(smallest_dt$score) - min(smallest_dt$score)
  if (full_gap <= .Machine$double.eps) {
    return("unclear")
  }
  winner_full <- full_dt[which.max(score)]$train_subsets[[1L]]
  winner_smallest <- smallest_dt[which.max(score)]$train_subsets[[1L]]
  gap_ratio <- smallest_gap / full_gap
  if (winner_full == "all" && gap_ratio <= sample_ratio_cutoff) {
    return("sample_size")
  }
  if (winner_smallest == winner_full && gap_ratio >= persist_ratio_cutoff) {
    return("subset")
  }
  "combined"
}

dataset_filter <- arg_list[["datasets"]]
if (is.null(dataset_filter)) {
  dataset_filter <- c("traffic_aadt", "adult_income", "wine_quality")
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
sample_ratio_cutoff <- parse_double_arg("sample-ratio-cutoff", 0.33)
persist_ratio_cutoff <- parse_double_arg("persist-ratio-cutoff", 0.67)
raw_output <- arg_list[["output-raw"]]
if (is.null(raw_output)) {
  raw_output <- file.path(repo_root, "results", "learner_robustness_runs.csv")
}
summary_output <- arg_list[["output-summary"]]
if (is.null(summary_output)) {
  summary_output <- file.path(repo_root, "results", "learner_robustness_summary.csv")
}
conclusion_output <- arg_list[["output-conclusions"]]
if (is.null(conclusion_output)) {
  conclusion_output <- file.path(repo_root, "results", "learner_robustness_conclusions.csv")
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
  minimize <- metric$minimize

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

conclusion_dt <- summary_dt[, {
  metric_view <- .SD[, .(sample_size, train_subsets, metric_mean)]
  conclusion <- classify_conclusion(
    metric_view,
    minimize = unique(metric_id) == "regr.rmse" || unique(metric_id) == "classif.ce",
    sample_ratio_cutoff = sample_ratio_cutoff,
    persist_ratio_cutoff = persist_ratio_cutoff
  )
  full_runtime <- sum(train_seconds_total[sample_size == "full"])
  smallest_runtime <- sum(train_seconds_total[sample_size == "smallest"])
  list(
    conclusion = conclusion,
    full_runtime_seconds = full_runtime,
    smallest_runtime_seconds = smallest_runtime
  )
}, by = .(dataset_id, dataset_label, learner, test_subset, metric_id, metric_label)]

fwrite(raw_dt, raw_output)
fwrite(summary_dt, summary_output)
fwrite(conclusion_dt, conclusion_output)

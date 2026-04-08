library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3resampling)
library(mlr3tuning)
library(paradox)

set.seed(1)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

sample_preserve_subset_ratio <- function(dt, subset_col, max_rows_largest_subset) {
  if (is.infinite(max_rows_largest_subset)) {
    return(copy(dt))
  }
  subset_sizes <- dt[, .N, by = subset_col]
  largest_subset_n <- max(subset_sizes$N)
  sample_fraction <- min(1, max_rows_largest_subset / largest_subset_n)
  dt[, .SD[sample.int(.N, max(1L, floor(.N * sample_fraction)))], by = subset_col]
}

make_learner_spec <- function(learner_name) {
  switch(
    learner_name,
    featureless = list(label = "featureless", learner = lrn("regr.featureless")),
    rpart = {
      learner <- lrn("regr.rpart")
      learner <- auto_tuner(
        tuner = tnr("grid_search", resolution = 3L),
        learner = learner,
        resampling = rsmp("cv", folds = 3L),
        measure = msr("regr.rmse"),
        terminator = trm("evals", n_evals = 3L),
        search_space = ps(
          cp = p_dbl(lower = 1e-4, upper = 1e-2, logscale = TRUE)
        )
      )
      list(label = "rpart", learner = learner)
    },
    glmnet = {
      learner <- lrn("regr.cv_glmnet")
      learner$param_set$values <- list(nfolds = 5L)
      list(label = "glmnet", learner = learner)
    },
    random_forest = {
      learner <- lrn("regr.ranger")
      learner$param_set$values <- list(num.trees = 200L)
      list(label = "random_forest", learner = learner)
    },
    dnn = {
      learner <- lrn("regr.nnet")
      learner$param_set$values <- list(
        size = 10L,
        decay = 1e-4,
        maxit = 200L,
        trace = FALSE,
        MaxNWts = 10000L
      )
      list(label = "dnn", learner = learner)
    },
    stop(sprintf("Unsupported regression learner '%s'.", learner_name))
  )
}

learner_filter <- c("featureless", "rpart", "glmnet", "random_forest", "dnn")
folds <- 5L
seeds <- 1L
max_rows_largest_subset <- 5000L
raw_output <- file.path(repo_root, "results", "learner_robustness_runs.csv")
summary_output <- file.path(repo_root, "results", "learner_robustness_summary.csv")

dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

traffic_dt <- fread(file.path(repo_root, "data", "traffic.csv"))
traffic_dt <- traffic_dt[city_key %in% c("Phoenix", "Baltimore")]
traffic_dt <- sample_preserve_subset_ratio(traffic_dt, "city_key", max_rows_largest_subset)
traffic_dt[, subset_id := factor(city_key)]
traffic_dt[, log_AADT := log(AADT)]

task <- TaskRegr$new(
  "traffic_aadt",
  traffic_dt[, .(
    log_AADT,
    NTL,
    POP,
    ROAD_DENS,
    S2_R,
    S2_G,
    S2_B,
    S2_NIR,
    subset_id
  )],
  target = "log_AADT"
)
task$set_col_roles("subset_id", c("subset"))
metric <- msr("regr.rmse")

all_raw <- list()
raw_i <- 1L
message("Running dataset: Traffic AADT")

resampling <- ResamplingSameOtherSizesCV$new()
resampling$param_set$values$folds <- folds
resampling$param_set$values$seeds <- seeds
resampling$param_set$values$sizes <- 0L
resampling$instantiate(task)

iteration_dt <- copy(resampling$instance$iteration.dt)

for (learner_name in learner_filter) {
  learner_spec <- make_learner_spec(learner_name)
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
      dataset_id = "traffic_aadt",
      dataset_label = "Traffic AADT",
      metric_id = "regr.rmse",
      metric_label = "RMSE on log(AADT)",
      learner = learner_spec$label,
      test_subset = as.character(iter_row$test.subset),
      train_subsets = iter_row$train.subsets,
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

raw_dt <- rbindlist(all_raw, fill = TRUE)
summary_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_se = sd(metric_value) / sqrt(.N),
  train_seconds_mean = mean(train_seconds),
  train_seconds_se = sd(train_seconds) / sqrt(.N),
  train_seconds_total = sum(train_seconds),
  n_runs = .N
), by = .(
  dataset_id,
  dataset_label,
  metric_id,
  metric_label,
  learner,
  test_subset,
  train_subsets,
  n_train_groups
)]

fwrite(raw_dt, raw_output)
fwrite(summary_dt, summary_output)

library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3resampling)
library(torch)

set.seed(1)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(repo_root, "R", "torch_mlp_benchmark.R"))

folds <- 5L
seeds <- 1L
raw_output <- file.path(repo_root, "results", "learner_robustness_runs.csv")
summary_output <- file.path(repo_root, "results", "learner_robustness_summary.csv")
time_runs_output <- file.path(repo_root, "results", "learner_train_time_runs.csv")
time_output <- file.path(repo_root, "results", "learner_train_time_summary.csv")

dir.create(dirname(raw_output), recursive = TRUE, showWarnings = FALSE)

traffic_dt <- fread(file.path(repo_root, "data", "traffic.csv"))
traffic_dt <- traffic_dt[city_key %in% c("Phoenix", "Baltimore")]
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
task$col_roles$feature <- c("NTL", "POP", "ROAD_DENS", "S2_R", "S2_G", "S2_B", "S2_NIR")
task$col_roles$subset <- "subset_id"
task$col_roles$stratum <- "subset_id"
metric <- msr("regr.rmse")
message("Running dataset: Traffic AADT")

soak <- ResamplingSameOtherSizesCV$new()
soak$param_set$values$folds <- folds
soak$param_set$values$seeds <- seeds
soak$param_set$values$sizes <- 0L
soak$instantiate(task)

featureless <- mlr3::LearnerRegrFeatureless$new()
rpart <- mlr3::lrn("regr.rpart")
random_forest <- mlr3::lrn("regr.ranger", num.trees = 10L)
glmnet <- mlr3learners::LearnerRegrCVGlmnet$new()
glmnet$param_set$values$nfolds <- 5L
reg.learner.list <- list(
  featureless = featureless,
  rpart = rpart,
  glmnet = glmnet,
  random_forest = random_forest
)

bench.grid <- mlr3::benchmark_grid(task, reg.learner.list, soak)
bench.result <- mlr3::benchmark(bench.grid, store_models = TRUE)
iteration_dt <- as.data.table(soak$instance$iteration.dt)
timing_dt <- as.data.table(bench.result)[, .(
  learner = fcase(
    learner_id == "regr.cv_glmnet", "glmnet",
    learner_id == "regr.ranger", "random_forest",
    default = sub("^regr\\.", "", learner_id)
  ),
  iteration,
  train_seconds = vapply(learner, function(x) x$timings[["train"]], numeric(1))
)]
raw_dt <- as.data.table(mlr3resampling::score(bench.result, metric))[, .(
  dataset_id = "traffic_aadt",
  dataset_label = "Traffic AADT",
  metric_id = "regr.rmse",
  metric_label = "RMSE on log(AADT)",
  learner = fcase(
    algorithm == "cv_glmnet", "glmnet",
    algorithm == "ranger", "random_forest",
    default = algorithm
  ),
  test_subset = as.character(test.subset),
  train_subsets = train.subsets,
  n_train_groups = n.train.groups,
  fold = test.fold,
  seed,
  iteration,
  metric_value = regr.rmse
)]
raw_dt <- merge(raw_dt, timing_dt, by = c("learner", "iteration"))
feature_names <- c("NTL", "POP", "ROAD_DENS", "S2_R", "S2_G", "S2_B", "S2_NIR")
torch_result <- run_torch_mlp_benchmark(
  dt = traffic_dt,
  feature_names = feature_names,
  target_name = "log_AADT",
  iteration_dt = iteration_dt
)
raw_dt <- rbindlist(list(raw_dt, torch_result$raw_dt), use.names = TRUE)
timing_dt <- rbindlist(list(timing_dt, torch_result$timing_dt), use.names = TRUE)
summary_dt <- raw_dt[, .(
  metric_mean = mean(metric_value),
  metric_se = sd(metric_value) / sqrt(.N),
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
time_dt <- raw_dt[, .(
  train_seconds_mean = mean(train_seconds),
  train_seconds_se = sd(train_seconds) / sqrt(.N),
  n_runs = .N
), by = .(learner)]

fwrite(raw_dt, raw_output)
fwrite(summary_dt, summary_output)
fwrite(timing_dt, time_runs_output)
fwrite(time_dt, time_output)

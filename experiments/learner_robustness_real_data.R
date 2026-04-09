library(data.table)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3resampling)

set.seed(1)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

folds <- 5L
seeds <- 1L
raw_output <- file.path(repo_root, "results", "learner_robustness_runs.csv")
summary_output <- file.path(repo_root, "results", "learner_robustness_summary.csv")

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

featureless <- mlr3::LearnerRegrFeatureless$new()
rpart <- mlr3::lrn("regr.rpart")
random_forest <- mlr3::lrn("regr.ranger", num.trees = 10L)
glmnet <- mlr3learners::LearnerRegrCVGlmnet$new()
glmnet$param_set$values$nfolds <- 5L
reg.learner.list <- list(
  featureless = featureless,
  rpart = rpart,
  random_forest = random_forest,
  glmnet = glmnet
)

bench.grid <- mlr3::benchmark_grid(task, reg.learner.list, soak)
bench.result <- mlr3::benchmark(bench.grid)
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

fwrite(raw_dt, raw_output)
fwrite(summary_dt, summary_output)

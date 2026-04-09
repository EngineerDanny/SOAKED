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
x_mat <- as.matrix(traffic_dt[, ..feature_names])
y_vec <- traffic_dt$log_AADT
torch_device_name <- if (torch::backends_mps_is_available()) "mps" else "cpu"
torch_device_obj <- torch::torch_device(torch_device_name)

torch_mlp <- nn_module(
  initialize = function(input_dim) {
    self$net <- nn_sequential(
      nn_linear(input_dim, 128),
      nn_relu(),
      nn_dropout(0.10),
      nn_linear(128, 64),
      nn_relu(),
      nn_dropout(0.10),
      nn_linear(64, 32),
      nn_relu(),
      nn_linear(32, 1)
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

torch_raw_list <- vector("list", nrow(iteration_dt))
torch_timing_list <- vector("list", nrow(iteration_dt))

for (iter_i in seq_len(nrow(iteration_dt))) {
  message(sprintf("  torch iteration %d/%d", iter_i, nrow(iteration_dt)))
  iter_row <- iteration_dt[iter_i]
  train_ids <- iter_row$train[[1L]]
  test_ids <- iter_row$test[[1L]]
  inner_train_ids <- train_ids
  val_n <- max(256L, floor(length(train_ids) * 0.1))
  if (val_n >= length(train_ids)) {
    val_n <- max(1L, length(train_ids) - 1L)
  }
  train_perm <- sample.int(length(train_ids))
  val_ids <- train_ids[train_perm[seq_len(val_n)]]
  if (val_n < length(train_ids)) {
    inner_train_ids <- train_ids[train_perm[-seq_len(val_n)]]
  }

  x_center <- colMeans(x_mat[inner_train_ids, , drop = FALSE])
  x_scale <- apply(x_mat[inner_train_ids, , drop = FALSE], 2, sd)
  x_scale[is.na(x_scale) | x_scale == 0] <- 1
  y_center <- mean(y_vec[inner_train_ids])
  y_scale <- sd(y_vec[inner_train_ids])
  if (is.na(y_scale) || y_scale == 0) {
    y_scale <- 1
  }

  x_train_scaled <- scale(x_mat[inner_train_ids, , drop = FALSE], center = x_center, scale = x_scale)
  x_val_scaled <- scale(x_mat[val_ids, , drop = FALSE], center = x_center, scale = x_scale)
  x_test_scaled <- scale(x_mat[test_ids, , drop = FALSE], center = x_center, scale = x_scale)

  x_train <- torch_tensor(
    x_train_scaled,
    dtype = torch_float(),
    device = torch_device_obj
  )
  y_train <- torch_tensor(
    matrix((y_vec[inner_train_ids] - y_center) / y_scale, ncol = 1L),
    dtype = torch_float(),
    device = torch_device_obj
  )
  x_val <- torch_tensor(
    x_val_scaled,
    dtype = torch_float(),
    device = torch_device_obj
  )
  y_val <- torch_tensor(
    matrix((y_vec[val_ids] - y_center) / y_scale, ncol = 1L),
    dtype = torch_float(),
    device = torch_device_obj
  )
  x_test <- torch_tensor(
    x_test_scaled,
    dtype = torch_float(),
    device = torch_device_obj
  )

  model <- torch_mlp(input_dim = ncol(x_mat))
  model$to(device = torch_device_obj)
  optimizer <- optim_adamw(model$parameters, lr = 1e-3, weight_decay = 1e-4)
  best_val <- Inf
  best_state <- NULL
  patience <- 0L

  train_seconds <- system.time({
    for (epoch in seq_len(80L)) {
      model$train()
      optimizer$zero_grad()
      pred <- model(x_train)
      loss <- nn_smooth_l1_loss()(pred, y_train)
      loss$backward()
      optimizer$step()
      model$eval()
      with_no_grad({
        val_loss <- as.numeric(nn_mse_loss()(model(x_val), y_val)$to(device = "cpu"))
      })
      if (val_loss + 1e-8 < best_val) {
        best_val <- val_loss
        best_state <- lapply(model$state_dict(), function(x) x$clone()$detach())
        patience <- 0L
      } else {
        patience <- patience + 1L
      }
      if (patience >= 8L) {
        break
      }
    }
  })[["elapsed"]]

  model$load_state_dict(best_state)
  model$eval()
  pred_vec <- with_no_grad({
    as.numeric(model(x_test)$squeeze(2)$to(device = "cpu"))
  })
  pred_vec <- pred_vec * y_scale + y_center
  metric_value <- sqrt(mean((pred_vec - y_vec[test_ids])^2))

  torch_raw_list[[iter_i]] <- data.table(
    learner = "torch_mlp",
    iteration = iter_row$iteration,
    dataset_id = "traffic_aadt",
    dataset_label = "Traffic AADT",
    metric_id = "regr.rmse",
    metric_label = "RMSE on log(AADT)",
    test_subset = as.character(iter_row$test.subset),
    train_subsets = iter_row$train.subsets,
    n_train_groups = iter_row$n.train.groups,
    fold = iter_row$test.fold,
    seed = iter_row$seed,
    metric_value = metric_value,
    train_seconds = train_seconds
  )
  torch_timing_list[[iter_i]] <- data.table(
    learner = "torch_mlp",
    iteration = iter_row$iteration,
    train_seconds = train_seconds
  )
}

torch_raw_dt <- rbindlist(torch_raw_list, use.names = TRUE)
torch_timing_dt <- rbindlist(torch_timing_list, use.names = TRUE)
raw_dt <- rbindlist(list(raw_dt, torch_raw_dt), use.names = TRUE)
timing_dt <- rbindlist(list(timing_dt, torch_timing_dt), use.names = TRUE)
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

run_torch_mlp_benchmark <- function(dt, feature_names, target_name, iteration_dt) {
  x_mat <- as.matrix(dt[, ..feature_names])
  y_vec <- dt[[target_name]]
  learning_rates <- c(3e-2, 1e-2, 5e-3, 3e-3, 1e-3, 5e-4, 3e-4, 1e-4)
  torch_device_name <- if (torch::backends_mps_is_available()) "mps" else "cpu"
  torch_device_obj <- torch::torch_device(torch_device_name)

  torch_mlp <- nn_module(
    initialize = function(input_dim) {
      self$net <- nn_sequential(
        nn_linear(input_dim, 32),
        nn_relu(),
        nn_linear(32, 16),
        nn_relu(),
        nn_linear(16, 1)
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

    x_train <- torch_tensor(x_train_scaled, dtype = torch_float(), device = torch_device_obj)
    y_train <- torch_tensor(
      matrix((y_vec[inner_train_ids] - y_center) / y_scale, ncol = 1L),
      dtype = torch_float(),
      device = torch_device_obj
    )
    x_val <- torch_tensor(x_val_scaled, dtype = torch_float(), device = torch_device_obj)
    y_val <- torch_tensor(
      matrix((y_vec[val_ids] - y_center) / y_scale, ncol = 1L),
      dtype = torch_float(),
      device = torch_device_obj
    )
    x_test <- torch_tensor(x_test_scaled, dtype = torch_float(), device = torch_device_obj)

    model <- torch_mlp(input_dim = ncol(x_mat))
    best_val <- Inf
    best_state <- NULL
    best_lr <- NA_real_

    train_seconds <- system.time({
      for (lr in learning_rates) {
        model <- torch_mlp(input_dim = ncol(x_mat))
        model$to(device = torch_device_obj)
        optimizer <- optim_adamw(model$parameters, lr = lr, weight_decay = 1e-4)
        candidate_best_val <- Inf
        candidate_best_state <- NULL
        patience <- 0L

        for (epoch in seq_len(40L)) {
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
          if (val_loss + 1e-8 < candidate_best_val) {
            candidate_best_val <- val_loss
            candidate_best_state <- lapply(model$state_dict(), function(x) x$clone()$detach())
            patience <- 0L
          } else {
            patience <- patience + 1L
          }
          if (patience >= 5L) {
            break
          }
        }

        if (candidate_best_val + 1e-8 < best_val) {
          best_val <- candidate_best_val
          best_state <- candidate_best_state
          best_lr <- lr
        }
      }
    })[["elapsed"]]

    model <- torch_mlp(input_dim = ncol(x_mat))
    model$to(device = torch_device_obj)
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

  list(
    raw_dt = rbindlist(torch_raw_list, use.names = TRUE),
    timing_dt = rbindlist(torch_timing_list, use.names = TRUE)
  )
}

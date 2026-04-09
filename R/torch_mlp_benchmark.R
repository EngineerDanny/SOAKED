run_torch_mlp_benchmark <- function(dt, feature_names, target_name, iteration_dt) {
  x_mat <- as.matrix(dt[, ..feature_names])
  y_vec <- dt[[target_name]]
  learning_rates <- c(1e-2, 3e-3, 1e-3)
  torch_device_name <- if (torch::backends_mps_is_available()) "mps" else "cpu"
  torch_device_obj <- torch::torch_device(torch_device_name)
  batch_size <- 16384L
  max_epochs <- 120L
  early_stop_patience <- 15L
  make_scaled_tensors <- function(row_ids, x_center, x_scale, y_center = NULL, y_scale = NULL) {
    x_scaled <- scale(x_mat[row_ids, , drop = FALSE], center = x_center, scale = x_scale)
    x_tensor <- torch_tensor(x_scaled, dtype = torch_float(), device = torch_device_obj)
    if (is.null(y_center) || is.null(y_scale)) {
      return(list(x = x_tensor))
    }
    y_tensor <- torch_tensor(
      matrix((y_vec[row_ids] - y_center) / y_scale, ncol = 1L),
      dtype = torch_float(),
      device = torch_device_obj
    )
    list(x = x_tensor, y = y_tensor)
  }
  train_epoch <- function(model, optimizer, x_tensor, y_tensor) {
    model$train()
    n_obs <- as.integer(x_tensor$size(1))
    shuffled_idx <- sample.int(n_obs)
    batch_starts <- seq.int(1L, n_obs, by = batch_size)
    for (start_idx in batch_starts) {
      end_idx <- min(start_idx + batch_size - 1L, n_obs)
      batch_idx <- shuffled_idx[start_idx:end_idx]
      optimizer$zero_grad()
      pred <- model(x_tensor[batch_idx, ])
      loss <- nn_smooth_l1_loss()(pred, y_tensor[batch_idx, ])
      loss$backward()
      optimizer$step()
    }
  }

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
    torch::torch_manual_seed(1000L + iter_i)
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

    train_tensors <- make_scaled_tensors(inner_train_ids, x_center, x_scale, y_center, y_scale)
    val_tensors <- make_scaled_tensors(val_ids, x_center, x_scale, y_center, y_scale)

    best_val <- Inf
    best_lr <- NA_real_
    best_epoch <- NA_integer_
    final_model <- NULL
    final_scaling <- NULL

    train_seconds <- system.time({
      for (lr in learning_rates) {
        model <- torch_mlp(input_dim = ncol(x_mat))
        model$to(device = torch_device_obj)
        optimizer <- optim_adamw(model$parameters, lr = lr, weight_decay = 1e-4)
        candidate_best_val <- Inf
        candidate_best_epoch <- 1L
        patience <- 0L

        for (epoch in seq_len(max_epochs)) {
          train_epoch(model, optimizer, train_tensors$x, train_tensors$y)

          model$eval()
          with_no_grad({
            val_loss <- as.numeric(nn_mse_loss()(model(val_tensors$x), val_tensors$y)$to(device = "cpu"))
          })
          if (val_loss + 1e-8 < candidate_best_val) {
            candidate_best_val <- val_loss
            candidate_best_epoch <- epoch
            patience <- 0L
          } else {
            patience <- patience + 1L
          }
          if (patience >= early_stop_patience) {
            break
          }
        }

        if (candidate_best_val + 1e-8 < best_val) {
          best_val <- candidate_best_val
          best_lr <- lr
          best_epoch <- candidate_best_epoch
        }
      }

      x_center_full <- colMeans(x_mat[train_ids, , drop = FALSE])
      x_scale_full <- apply(x_mat[train_ids, , drop = FALSE], 2, sd)
      x_scale_full[is.na(x_scale_full) | x_scale_full == 0] <- 1
      y_center_full <- mean(y_vec[train_ids])
      y_scale_full <- sd(y_vec[train_ids])
      if (is.na(y_scale_full) || y_scale_full == 0) {
        y_scale_full <- 1
      }

      full_train_tensors <- make_scaled_tensors(train_ids, x_center_full, x_scale_full, y_center_full, y_scale_full)
      final_model <- torch_mlp(input_dim = ncol(x_mat))
      final_model$to(device = torch_device_obj)
      final_optimizer <- optim_adamw(final_model$parameters, lr = best_lr, weight_decay = 1e-4)

      for (epoch in seq_len(best_epoch)) {
        train_epoch(final_model, final_optimizer, full_train_tensors$x, full_train_tensors$y)
      }

      final_scaling <- list(
        x_center = x_center_full,
        x_scale = x_scale_full,
        y_center = y_center_full,
        y_scale = y_scale_full
      )
    })[["elapsed"]]

    test_tensors <- make_scaled_tensors(
      test_ids,
      final_scaling$x_center,
      final_scaling$x_scale
    )
    final_model$eval()
    pred_vec <- with_no_grad({
      as.numeric(final_model(test_tensors$x)$squeeze(2)$to(device = "cpu"))
    })
    pred_vec <- pred_vec * final_scaling$y_scale + final_scaling$y_center
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

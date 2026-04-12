library(data.table)
library(mlr3)
library(mlr3resampling)

# ---- Config ----
n_folds <- 10L

dataset_configs <- list(

  # Classification
  list(
    in_path      = "raw/classification/NSCH_autism.csv",
    out_path     = "processed/classification/NSCH_autism.csv",
    type         = "classif",
    subset_col   = "survey_year",
    target_col   = "y",
    drop_cols    = c("Unique_Household_ID", "Selected_Child_Weight"),
    log_target   = FALSE,
    feature_cols = NULL,
    y_levels     = c("No", "Yes")
  ),
  list(
    in_path      = "raw/classification/STL10.csv",
    out_path     = "processed/classification/STL10.csv",
    type         = "classif",
    subset_col   = "predefined.set",
    target_col   = "y",
    drop_cols    = character(0L),
    log_target   = FALSE,
    feature_cols = NULL,
    y_levels     = NULL
  ),
  list(
    in_path      = "raw/classification/waveform.csv",
    out_path     = "processed/classification/waveform.csv",
    type         = "classif",
    subset_col   = "predefined.set",
    target_col   = "y",
    drop_cols    = character(0L),
    log_target   = FALSE,
    feature_cols = NULL,
    y_levels     = NULL
  ),

  # Regression
  list(
    in_path      = "raw/regression/workers_compensation.csv",
    out_path     = "processed/regression/workers_compensation.csv",
    type         = "regr",
    subset_col   = "Gender",
    target_col   = "UltimateIncurredClaimCost",
    drop_cols    = character(0L),
    log_target   = TRUE,
    feature_cols = NULL
  ),
  list(
    in_path      = "raw/regression/traffic.csv",
    out_path     = "processed/regression/traffic.csv",
    type         = "regr",
    subset_col   = "city_key",
    target_col   = "AADT",
    drop_cols    = character(0L),
    log_target   = TRUE,
    feature_cols = c(
      "NTL", "POP", "ROAD_DENS", "S2_R", "S2_G", "S2_B", "S2_NIR"
    )
  ),
  list(
    in_path         = "raw/regression/qsip_dim.control_vs_qme.15.csv",
    out_path        = "processed/regression/qsip_dim.control_vs_qme.15.csv",
    type            = "regr",
    subset_col      = "subset_id",
    target_col      = "growth_per_day",
    drop_cols       = character(0L),
    log_target      = FALSE,
    feature_cols    = NULL,
    feature_pattern = "^K"
  ),

  # Simulation
  list(
    in_path      = "raw/simulation/different.csv",
    out_path     = "processed/simulation/different.csv",
    type         = "regr",
    subset_col   = "Subset",
    target_col   = "y",
    drop_cols    = character(0L),
    log_target   = FALSE,
    feature_cols = NULL
  ),
  list(
    in_path      = "raw/simulation/iid_easy.csv",
    out_path     = "processed/simulation/iid_easy.csv",
    type         = "regr",
    subset_col   = "Subset",
    target_col   = "y",
    drop_cols    = character(0L),
    log_target   = FALSE,
    feature_cols = NULL
  )
)

# ---- Paths relative to this script ----
script_arg  <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(
  sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE
)
data_dir <- dirname(script_path)

args <- commandArgs(trailingOnly = TRUE)
dataset_arg <- grep("^--dataset=", args, value = TRUE)
selected_out_paths <- NULL
if (length(dataset_arg) > 0L) {
  selected_out_paths <- strsplit(
    sub("^--dataset=", "", dataset_arg[[1L]]),
    ",",
    fixed = TRUE
  )[[1L]]
  selected_out_paths <- trimws(selected_out_paths)
  selected_out_paths <- selected_out_paths[nzchar(selected_out_paths)]
  dataset_configs <- Filter(
    function(cfg) tools::file_path_sans_ext(basename(cfg$out_path)) %in% selected_out_paths,
    dataset_configs
  )
  if (length(dataset_configs) == 0L) {
    stop("No dataset configs matched --dataset=", sub("^--dataset=", "", dataset_arg[[1L]]))
  }
}

# ---- Process each dataset ----
for (cfg in dataset_configs) {
  in_path  <- file.path(data_dir, cfg$in_path)
  out_path <- file.path(data_dir, cfg$out_path)

  message(sprintf("Processing: %s", cfg$in_path))

  dt <- fread(in_path)
  message(sprintf("  Read %d rows x %d cols", nrow(dt), ncol(dt)))

  # Drop unwanted columns
  if (length(cfg$drop_cols) > 0L) {
    dt[, (cfg$drop_cols) := NULL]
  }

  # Log-transform target
  target_col <- cfg$target_col
  if (isTRUE(cfg$log_target)) {
    target_col <- paste0("log_", cfg$target_col)
    dt[, (target_col) := log(get(cfg$target_col))]
    dt[, (cfg$target_col) := NULL]
  }

  # Convert target to factor for classification
  if (cfg$type == "classif") {
    if (!is.null(cfg$y_levels)) {
      dt[, (target_col) := factor(get(target_col), levels = cfg$y_levels)]
    } else {
      dt[, (target_col) := factor(get(target_col))]
    }
  }

  # Determine feature columns
  feat_cols <- if (!is.null(cfg$feature_cols)) {
    cfg$feature_cols
  } else if (!is.null(cfg$feature_pattern)) {
    grep(cfg$feature_pattern, names(dt), value = TRUE)
  } else {
    setdiff(names(dt), c(cfg$subset_col, target_col))
  }

  # Assign folds (stratified by subset+target for classif, subset for regr)
  strat_cols <- if (cfg$type == "classif") {
    c(cfg$subset_col, target_col)
  } else {
    cfg$subset_col
  }
  dt[, Fold := rep(seq_len(n_folds), length.out = .N), by = strat_cols]

  # ---- Build mlr3 task to validate structure and col_roles ----
  if (cfg$type == "classif") {
    task <- mlr3::TaskClassif$new(
      id      = tools::file_path_sans_ext(basename(cfg$out_path)),
      backend = dt,
      target  = target_col
    )
  } else {
    task <- mlr3::TaskRegr$new(
      id      = tools::file_path_sans_ext(basename(cfg$out_path)),
      backend = dt,
      target  = target_col
    )
  }
  task$col_roles$feature <- feat_cols
  task$set_col_roles(cfg$subset_col, "subset")
  task$col_roles$fold <- "Fold"
  if (cfg$type == "classif") {
    task$col_roles$stratum <- c("Fold", target_col)
  } else {
    task$col_roles$stratum <- "Fold"
  }

  message(sprintf(
    "  Task: %d rows, target=%s, subsets=%s, folds=%s",
    task$nrow,
    task$target_names,
    paste(unique(task$data(cols = cfg$subset_col)[[1L]]), collapse = "/"),
    paste(sort(unique(task$data(cols = "Fold")$Fold)), collapse = " ")
  ))

  # ---- Save task data with standardized column names ----
  out_dt <- task$data(cols = c("Fold", cfg$subset_col, target_col, feat_cols))
  setnames(out_dt, old = c(cfg$subset_col, target_col), new = c("Subset", "Y"))

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  fwrite(out_dt, out_path)
  message(sprintf(
    "  Saved: %s (%d rows x %d cols)",
    cfg$out_path, nrow(out_dt), ncol(out_dt)
  ))
}

message("Done.")

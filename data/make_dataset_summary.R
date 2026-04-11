library(data.table)

dataset_meta <- list(
  list(
    path = "processed/classification/NSCH_autism.csv",
    dataset = "NSCH autism",
    task = "Classification",
    target = "Autism diagnosis"
  ),
  list(
    path = "processed/classification/STL10.csv",
    dataset = "STL10",
    task = "Classification",
    target = "Image class"
  ),
  list(
    path = "processed/classification/waveform.csv",
    dataset = "Waveform",
    task = "Classification",
    target = "Waveform class"
  ),
  list(
    path = "processed/regression/traffic.csv",
    dataset = "Traffic",
    task = "Regression",
    target = "log(AADT)"
  ),
  list(
    path = "processed/regression/workers_compensation.csv",
    dataset = "Workers compensation",
    task = "Regression",
    target = "log(Ultimate incurred claim cost)"
  ),
  list(
    path = "processed/simulation/different.csv",
    dataset = "Different",
    task = "Regression",
    target = "y"
  ),
  list(
    path = "processed/simulation/iid_easy.csv",
    dataset = "IID easy",
    task = "Regression",
    target = "y"
  )
)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- normalizePath(
  sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE
)
data_dir <- dirname(script_path)

summary_list <- vector("list", length(dataset_meta))

for (meta_i in seq_along(dataset_meta)) {
  meta <- dataset_meta[[meta_i]]
  in_path <- file.path(data_dir, meta$path)
  dt <- fread(in_path)
  subset_levels <- unique(dt$Subset)
  subset_counts <- dt[, .N, by = Subset][match(subset_levels, Subset)]

  summary_list[[meta_i]] <- data.table(
    Dataset = meta$dataset,
    Task = meta$task,
    `Subset levels` = paste(subset_counts$Subset, collapse = "/"),
    Rows = nrow(dt),
    Features = ncol(dt) - 3L,
    `Subset imb.` = round(max(subset_counts$N) / min(subset_counts$N), 2),
    Target = meta$target
  )
}

summary_dt <- rbindlist(summary_list)
out_path <- file.path(data_dir, "dataset_summary.csv")
fwrite(summary_dt, out_path)
message(sprintf("Saved: %s", out_path))

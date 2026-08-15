# Reproducible timing benchmark for improved workflow
#
# Benchmarked workflow corresponds to scripts/ldmppr_paper_code_full.R
# lines 111-186 (improved fit workflow):
#   1) estimate_process_parameters
#   2) train_mark_model
#   3) check_model_fit
#
# Outputs (under timing_outputs/ by default):
#   - improved_workflow_timing_runs.csv
#   - improved_workflow_timing_summary.csv

infer_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
  }
  NULL
}

is_project_root <- function(path) {
  file.exists(file.path(path, "ldmppr_supplement.Rmd")) ||
    file.exists(file.path(path, "ldmppr.Rmd"))
}

script_path <- infer_script_path()
script_dir <- if (is.null(script_path)) normalizePath(getwd(), mustWork = TRUE) else dirname(script_path)

proj_candidates <- unique(c(
  normalizePath(getwd(), mustWork = TRUE),
  script_dir,
  normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
))

proj_hits <- proj_candidates[vapply(proj_candidates, is_project_root, logical(1))]
proj_path <- if (length(proj_hits)) normalizePath(proj_hits[1], mustWork = TRUE) else normalizePath(getwd(), mustWork = TRUE)

if (!requireNamespace("ldmppr", quietly = TRUE)) {
  stop("Package 'ldmppr' is required. Install it before running this script.", call. = FALSE)
}
library(ldmppr)

library(readr)
library(dplyr)
library(tibble)
library(terra)

parse_logical <- function(x, default = TRUE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(trimws(x)) %in% c("1", "true", "yes", "y", "t")
}

seed <- as.integer(Sys.getenv("LDMPPR_BENCH_SEED", "90210"))
n_reps <- as.integer(Sys.getenv("LDMPPR_BENCH_REPS", "5"))
parallel_train <- parse_logical(Sys.getenv("LDMPPR_BENCH_PARALLEL_TRAIN", "true"), TRUE)

if (!is.finite(seed)) stop("LDMPPR_BENCH_SEED must be numeric.", call. = FALSE)
if (!is.finite(n_reps) || n_reps < 1L) stop("LDMPPR_BENCH_REPS must be >= 1.", call. = FALSE)

out_dir <- file.path(proj_path, Sys.getenv("LDMPPR_BENCH_OUTDIR", "timing_outputs"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

runs_path <- file.path(out_dir, "improved_workflow_timing_runs.csv")
summary_path <- file.path(out_dir, "improved_workflow_timing_summary.csv")

set.seed(seed)
data("medium_example_data")
parameter_estimation_data <- medium_example_data
upper_bounds <- c(1, 50, 50)

raster_paths <- list.files(system.file("extdata", package = "ldmppr"), pattern = "[.]tif$", full.names = TRUE)
raster_paths <- raster_paths[grepl("_med[.]tif$", raster_paths)]
rasters <- lapply(raster_paths, terra::rast)
scaled_rasters <- scale_rasters(rasters)

run_one <- function(rep_id) {
  starts <- list(global = 5, local = 3, jitter_sd = 0.15, seed = seed + rep_id)

  t_est <- proc.time()[["elapsed"]]
  estimated_sc_update <- estimate_process_parameters(
    data = parameter_estimation_data,
    process = "self_correcting",
    grids = ldmppr_grids(
      upper_bounds = upper_bounds,
      levels = list(c(10, 10, 10), c(12, 12, 12), c(16, 16, 16))
    ),
    budgets = ldmppr_budgets(
      global_options = list(maxeval = 1000, ftol_rel = 1e-6, xtol_rel = 1e-6),
      local_budget_first_level = list(maxeval = 2000, ftol_rel = 1e-8, xtol_rel = 1e-8),
      local_budget_refinement_levels = list(maxeval = 3000, ftol_rel = 1e-10, xtol_rel = 1e-10)
    ),
    delta = 1,
    parallel = FALSE,
    strategy = "multires_global_local",
    global_algorithm = "NLOPT_GN_CRS2_LM",
    local_algorithm = "NLOPT_LN_BOBYQA",
    starts = starts,
    verbose = FALSE
  )
  estimate_sec <- proc.time()[["elapsed"]] - t_est

  t_train <- proc.time()[["elapsed"]]
  improved_example_trained_mark_model <- train_mark_model(
    data = estimated_sc_update,
    raster_list = scaled_rasters,
    scaled_rasters = TRUE,
    model_type = "xgboost",
    parallel = parallel_train,
    include_comp_inds = TRUE,
    competition_radius = 10,
    edge_correction = "none",
    selection_metric = "mae",
    cv_folds = 10,
    tuning_grid_size = 150,
    seed = seed + rep_id,
    verbose = FALSE
  )
  train_mark_sec <- proc.time()[["elapsed"]] - t_train

  t_check <- proc.time()[["elapsed"]]
  improved_example_model_fit <- check_model_fit(
    process = "self_correcting",
    process_fit = estimated_sc_update,
    mark_model = improved_example_trained_mark_model,
    include_comp_inds = TRUE,
    thinning = TRUE,
    edge_correction = "none",
    competition_radius = 10,
    n_sim = 2500,
    save_sims = FALSE,
    seed = seed + rep_id,
    verbose = FALSE,
    parallel = FALSE,
    fg_correction = "km"
  )
  check_sec <- proc.time()[["elapsed"]] - t_check

  tibble(
    rep = rep_id,
    seed = seed + rep_id,
    parallel_train = parallel_train,
    estimate_sec = as.numeric(estimate_sec),
    train_mark_sec = as.numeric(train_mark_sec),
    check_sec = as.numeric(check_sec),
    total_sec = as.numeric(estimate_sec + train_mark_sec + check_sec),
    p_combined = as.numeric(summary(improved_example_model_fit)$p_combined),
    error = NA_character_
  )
}

runs <- vector("list", n_reps)

for (r in seq_len(n_reps)) {
  message("Benchmark run ", r, " / ", n_reps)
  row <- tryCatch(
    run_one(r),
    error = function(e) {
      tibble(
        rep = r,
        seed = seed + r,
        parallel_train = parallel_train,
        estimate_sec = NA_real_,
        train_mark_sec = NA_real_,
        check_sec = NA_real_,
        total_sec = NA_real_,
        p_combined = NA_real_,
        error = conditionMessage(e)
      )
    }
  )
  runs[[r]] <- row
  readr::write_csv(bind_rows(runs), runs_path)
}

runs_tbl <- bind_rows(runs)

summary_tbl <- tibble(
  n_reps = n_reps,
  failures = sum(!is.na(runs_tbl$error)),
  estimate_mean = mean(runs_tbl$estimate_sec, na.rm = TRUE),
  estimate_sd = stats::sd(runs_tbl$estimate_sec, na.rm = TRUE),
  estimate_median = stats::median(runs_tbl$estimate_sec, na.rm = TRUE),
  train_mark_mean = mean(runs_tbl$train_mark_sec, na.rm = TRUE),
  train_mark_sd = stats::sd(runs_tbl$train_mark_sec, na.rm = TRUE),
  train_mark_median = stats::median(runs_tbl$train_mark_sec, na.rm = TRUE),
  check_mean = mean(runs_tbl$check_sec, na.rm = TRUE),
  check_sd = stats::sd(runs_tbl$check_sec, na.rm = TRUE),
  check_median = stats::median(runs_tbl$check_sec, na.rm = TRUE),
  total_mean = mean(runs_tbl$total_sec, na.rm = TRUE),
  total_sd = stats::sd(runs_tbl$total_sec, na.rm = TRUE),
  total_median = stats::median(runs_tbl$total_sec, na.rm = TRUE)
 )

readr::write_csv(runs_tbl, runs_path)
readr::write_csv(summary_tbl, summary_path)

cat("\nImproved workflow timing summary:\n")
print(summary_tbl)
cat("\nSaved run-level timings to: ", runs_path, "\n", sep = "")
cat("Saved summary timings to: ", summary_path, "\n", sep = "")

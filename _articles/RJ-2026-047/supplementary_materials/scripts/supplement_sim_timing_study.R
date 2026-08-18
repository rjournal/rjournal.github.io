# Lightweight simulation timing study for supplement
#
# Purpose:
# - quantify check_model_fit runtime under manuscript-like settings
# - provide concrete timing evidence with mean/sd summaries
# - keep runtime short enough for same-day iteration

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

library(dplyr)
library(readr)
library(tibble)
library(terra)

parse_int_vec <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  out <- as.integer(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
  out <- out[is.finite(out) & !is.na(out) & out > 0L]
  if (!length(out)) default else unique(out)
}

mode <- tolower(Sys.getenv("LDMPPR_TIMING_MODE", "full"))
if (!mode %in% c("quick", "full")) {
  stop("LDMPPR_TIMING_MODE must be one of: quick, full", call. = FALSE)
}

default_nsim <- if (identical(mode, "full")) "200,500,1000,2500" else "200,500"
default_reps <- if (identical(mode, "full")) "10" else "1"
default_cores <- if (identical(mode, "full")) "1,5" else "1,2"

n_sim_grid <- parse_int_vec(Sys.getenv("LDMPPR_TIMING_N_SIM", default_nsim), c(200L, 500L, 1000L, 2500L))
n_reps <- as.integer(Sys.getenv("LDMPPR_TIMING_REPS", default_reps))
cores_grid <- parse_int_vec(Sys.getenv("LDMPPR_TIMING_CORES", default_cores), c(1L, 5L))

if (!is.finite(n_reps) || n_reps < 1L) stop("LDMPPR_TIMING_REPS must be >= 1", call. = FALSE)

out_dir <- file.path(proj_path, "timing_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

runs_path <- file.path(out_dir, paste0("sim_timing_runs_", mode, ".csv"))
summary_path <- file.path(out_dir, paste0("sim_timing_summary_", mode, ".csv"))

data("medium_example_data")
reference_data <- generate_mpp(
  locations = medium_example_data[, c("x", "y")],
  marks = medium_example_data$size,
  xy_bounds = c(0, 50, 0, 50)
)

process_fit <- readRDS(file.path(proj_path, "data", "estimate_demo_par.rds"))

raster_paths <- list.files(system.file("extdata", package = "ldmppr"), pattern = "[.]tif$", full.names = TRUE)
raster_paths <- raster_paths[grepl("_med[.]tif$", raster_paths)]
rasters <- lapply(raster_paths, terra::rast)
scaled_rasters <- scale_rasters(rasters)

message("Training mark model once for timing study...")
mark_model <- train_mark_model(
  data = process_fit,
  raster_list = scaled_rasters,
  scaled_rasters = TRUE,
  model_type = "xgboost",
  parallel = FALSE,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  selection_metric = "mae",
  cv_folds = 5,
  tuning_grid_size = 50,
  seed = 90210,
  verbose = FALSE
)

run_one <- function(n_sim, cores, rep_id) {
  parallel_flag <- cores > 1L
  seed_val <- 32000L + 1000L * n_sim + 100L * cores + rep_id

  t0 <- proc.time()[["elapsed"]]
  fit <- check_model_fit(
    reference_data = reference_data,
    process = "self_correcting",
    process_fit = process_fit,
    mark_model = mark_model,
    include_comp_inds = TRUE,
    thinning = TRUE,
    edge_correction = "none",
    competition_radius = 10,
    n_sim = n_sim,
    save_sims = FALSE,
    verbose = FALSE,
    seed = seed_val,
    parallel = parallel_flag,
    num_cores = cores,
    set_future_plan = parallel_flag,
    fg_correction = "km"
  )
  elapsed <- proc.time()[["elapsed"]] - t0

  tibble(
    mode = mode,
    n_sim = n_sim,
    cores = cores,
    parallel = parallel_flag,
    rep = rep_id,
    elapsed_sec = as.numeric(elapsed),
    p_combined = as.numeric(summary(fit)$p_combined),
    seed = seed_val,
    error = NA_character_
  )
}

runs <- list()
rid <- 0L

for (n_sim in n_sim_grid) {
  for (cores in cores_grid) {
    for (r in seq_len(n_reps)) {
      message("Timing run n_sim=", n_sim, " cores=", cores, " rep=", r)
      row <- tryCatch(
        run_one(n_sim = n_sim, cores = cores, rep_id = r),
        error = function(e) {
          tibble(
            mode = mode,
            n_sim = n_sim,
            cores = cores,
            parallel = cores > 1L,
            rep = r,
            elapsed_sec = NA_real_,
            p_combined = NA_real_,
            seed = 32000L + 1000L * n_sim + 100L * cores + r,
            error = conditionMessage(e)
          )
        }
      )
      rid <- rid + 1L
      runs[[rid]] <- row
      readr::write_csv(bind_rows(runs), runs_path)
    }
  }
}

runs_tbl <- bind_rows(runs)

summary_tbl <- runs_tbl |>
  group_by(n_sim, cores, parallel) |>
  summarise(
    runs = n(),
    failures = sum(!is.na(error)),
    elapsed_mean = mean(elapsed_sec, na.rm = TRUE),
    elapsed_sd = stats::sd(elapsed_sec, na.rm = TRUE),
    p_combined_mean = mean(p_combined, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(n_sim, cores)

readr::write_csv(summary_tbl, summary_path)

cat("\nSimulation timing summary:\n")
print(summary_tbl)
cat("\nSaved runs to: ", runs_path, "\n", sep = "")
cat("Saved summary to: ", summary_path, "\n", sep = "")

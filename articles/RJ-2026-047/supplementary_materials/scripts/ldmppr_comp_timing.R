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
library(terra)
library(tibble)

n_sim <- as.integer(Sys.getenv("LDMPPR_COMP_N_SIM", "2500"))
parallel_flag <- tolower(Sys.getenv("LDMPPR_COMP_PARALLEL", "true")) %in% c("1", "true", "yes", "y")
seed <- as.integer(Sys.getenv("LDMPPR_COMP_SEED", "90210"))

if (!is.finite(n_sim) || n_sim < 99L) stop("LDMPPR_COMP_N_SIM must be >= 99", call. = FALSE)

data("medium_example_data")

raster_paths <- list.files(system.file("extdata", package = "ldmppr"), pattern = "[.]tif$", full.names = TRUE)
raster_paths <- raster_paths[grepl("_med[.]tif$", raster_paths)]
rasters <- lapply(raster_paths, terra::rast)
scaled_rasters <- scale_rasters(rasters)

t0 <- proc.time()[["elapsed"]]
fit <- estimate_process_parameters(
  data = medium_example_data,
  process = "self_correcting",
  grids = ldmppr_grids(
    upper_bounds = c(1, 50, 50),
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
  starts = list(global = 5L, local = 3L, jitter_sd = 0.15, seed = seed),
  verbose = FALSE
)
fit_sec <- proc.time()[["elapsed"]] - t0

t1 <- proc.time()[["elapsed"]]
mm <- train_mark_model(
  data = fit,
  raster_list = scaled_rasters,
  scaled_rasters = TRUE,
  model_type = "xgboost",
  parallel = parallel_flag,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  selection_metric = "mae",
  cv_folds = 10,
  tuning_grid_size = 150,
  seed = seed,
  verbose = FALSE
)
train_sec <- proc.time()[["elapsed"]] - t1

reference_data <- generate_mpp(
  locations = medium_example_data[, c("x", "y")],
  marks = medium_example_data$size,
  xy_bounds = c(0, 50, 0, 50)
)

t2 <- proc.time()[["elapsed"]]
ck <- check_model_fit(
  reference_data = reference_data,
  process = "self_correcting",
  process_fit = fit,
  mark_model = mm,
  include_comp_inds = TRUE,
  thinning = TRUE,
  edge_correction = "none",
  competition_radius = 10,
  n_sim = n_sim,
  save_sims = FALSE,
  verbose = FALSE,
  seed = seed,
  parallel = FALSE,
  fg_correction = "km"
)
check_sec <- proc.time()[["elapsed"]] - t2

sim_obj <- simulate_mpp(
  process = "self_correcting",
  process_fit = fit,
  t_min = 0,
  t_max = 1,
  mark_model = mm,
  raster_list = scaled_rasters,
  scaled_rasters = TRUE,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  thinning = TRUE,
  seed = seed
)

out_dir <- file.path(proj_path, "timing_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summary_tbl <- tibble(
  method = "ldmppr",
  n_points = nrow(medium_example_data),
  n_sim = n_sim,
  fit_sec = as.numeric(fit_sec),
  fit_mark_sec = as.numeric(train_sec),
  sim_sec = NA_real_,
  check_sec = as.numeric(check_sec),
  total_sec = as.numeric(fit_sec + train_sec + check_sec),
  p_combined = as.numeric(summary(ck)$p_combined)
)

p_by_stat <- tibble(
  stat = c("L", "F", "G", "J", "E", "V"),
  p_value = c(
    as.numeric(attr(ck$envs$L, "p")),
    as.numeric(attr(ck$envs$F, "p")),
    as.numeric(attr(ck$envs$G, "p")),
    as.numeric(attr(ck$envs$J, "p")),
    as.numeric(attr(ck$envs$E, "p")),
    as.numeric(attr(ck$envs$V, "p"))
  )
)

readr::write_csv(summary_tbl, file.path(out_dir, "ldmppr_comp_summary.csv"))
readr::write_csv(p_by_stat, file.path(out_dir, "ldmppr_comp_p_by_stat.csv"))
saveRDS(list(check = ck, sim = sim_obj, reference = reference_data), file.path(out_dir, "ldmppr_comp_artifacts.rds"))

cat("Saved ldmppr comparison outputs to ", out_dir, "\n", sep = "")

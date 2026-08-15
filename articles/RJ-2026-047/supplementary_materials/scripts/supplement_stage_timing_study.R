# Stage timing study for supplement
#
# Benchmarks estimate_process_parameters() and train_mark_model()
# on:
#   - medium_example_data
#   - one contiguous larger window near 200 points
#   - one contiguous larger window near 400 points
#
# Outputs:
#   timing_outputs/stage_timing_runs_<profile>.csv
#   timing_outputs/stage_timing_summary_<profile>.csv
#   timing_outputs/stage_timing_datasets_<profile>.csv

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

parse_chr_vec <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  out <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  out <- out[nzchar(out)]
  if (!length(out)) default else unique(out)
}

set.seed(20260220)

profile <- tolower(Sys.getenv("LDMPPR_STAGE_PROFILE", "quick"))
if (!profile %in% c("quick", "full")) {
  stop("LDMPPR_STAGE_PROFILE must be one of: quick, full", call. = FALSE)
}

default_reps <- if (identical(profile, "full")) "10" else "1"
default_cores <- if (identical(profile, "full")) "1,5" else "1"
default_datasets <- "medium,win200,win400"

n_reps <- as.integer(Sys.getenv("LDMPPR_STAGE_REPS", default_reps))
cores_grid <- parse_int_vec(Sys.getenv("LDMPPR_STAGE_CORES", default_cores), c(1L))
dataset_ids <- parse_chr_vec(Sys.getenv("LDMPPR_STAGE_DATASETS", default_datasets), c("medium", "win200", "win400"))

if (!is.finite(n_reps) || n_reps < 1L) stop("LDMPPR_STAGE_REPS must be >= 1", call. = FALSE)

out_dir <- file.path(proj_path, "timing_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

runs_path <- file.path(out_dir, paste0("stage_timing_runs_", profile, ".csv"))
summary_path <- file.path(out_dir, paste0("stage_timing_summary_", profile, ".csv"))
datasets_path <- file.path(out_dir, paste0("stage_timing_datasets_", profile, ".csv"))

cfg <- if (identical(profile, "full")) {
  list(
    grid_levels = list(c(10, 10, 10), c(14, 14, 14), c(18, 18, 18)),
    global_opts = list(maxeval = 1000, ftol_rel = 1e-6, xtol_rel = 1e-6, population = 0),
    local1_opts = list(maxeval = 1200, ftol_rel = 1e-8, xtol_rel = 1e-8),
    localr_opts = list(maxeval = 1200, ftol_rel = 1e-8, xtol_rel = 1e-8),
    starts = list(global = 4L, local = 2L, jitter_sd = 0.20),
    cv_folds = 5L,
    tuning_grid_size = 50L
  )
} else {
  list(
    grid_levels = list(c(10, 10, 10), c(12, 12, 12)),
    global_opts = list(maxeval = 500, ftol_rel = 1e-5, xtol_rel = 1e-5, population = 0),
    local1_opts = list(maxeval = 700, ftol_rel = 1e-7, xtol_rel = 1e-7),
    localr_opts = list(maxeval = 700, ftol_rel = 1e-7, xtol_rel = 1e-7),
    starts = list(global = 2L, local = 1L, jitter_sd = 0.20),
    cv_folds = 3L,
    tuning_grid_size = 25L
  )
}

data("medium_example_data")

medium_raster_paths <- list.files(system.file("extdata", package = "ldmppr"), pattern = "[.]tif$", full.names = TRUE)
medium_raster_paths <- medium_raster_paths[grepl("_med[.]tif$", medium_raster_paths)]
medium_rasters <- lapply(medium_raster_paths, terra::rast)
medium_rasters <- scale_rasters(medium_rasters)

full_data <- readr::read_csv(
  "https://data.ess-dive.lbl.gov/catalog/d1/mn/v2/object/ess-dive-3120c69b6a46352-20240513T174234713",
  show_col_types = FALSE
) |>
  transmute(x = XTOP, y = YTOP, size = CANVOL2015) |>
  filter(is.finite(x), is.finite(y), is.finite(size), size > 0)

xmin <- min(full_data$x)
ymin <- min(full_data$y)
data_local <- full_data |>
  transmute(x = x - xmin, y = y - ymin, size = size)

domain_x <- range(data_local$x)
domain_y <- range(data_local$y)

raster_dir <- Sys.getenv("LDMPPR_RASTER_DIR", unset = file.path(proj_path, "data", "ess_dive_rasters"))
raster_files <- c(
  "Snodgrass_aspect_southness_1m.tif",
  "Snodgrass_DEM_1m.tif",
  "Snodgrass_slope_1m.tif",
  "Snodgrass_wetness_index_1m.tif"
)
raster_object_ids <- c(
  Snodgrass_aspect_southness_1m.tif = "ess-dive-c3c46ff25d50885-20240513T173925432",
  Snodgrass_DEM_1m.tif = "ess-dive-f6d46b0898ecb21-20240513T173925433",
  Snodgrass_slope_1m.tif = "ess-dive-8a59960f4ffd550-20240513T173925429",
  Snodgrass_wetness_index_1m.tif = "ess-dive-dffdeec81023d23-20240513T173925427"
)

ensure_ess_dive_rasters <- function(files, object_ids, dir_path) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(dir_path, files)
  missing <- which(!file.exists(out))
  if (!length(missing)) return(out)

  message("Downloading missing ESS-DIVE raster files to: ", dir_path)
  for (i in missing) {
    f <- files[i]
    oid <- object_ids[[f]]
    if (is.null(oid) || !nzchar(oid)) {
      stop("Missing ESS-DIVE object id mapping for: ", f, call. = FALSE)
    }
    url <- sprintf("https://data.ess-dive.lbl.gov/catalog/d1/mn/v2/object/%s", oid)
    utils::download.file(url = url, destfile = out[i], mode = "wb", quiet = FALSE)
  }

  still_missing <- out[!file.exists(out)]
  if (length(still_missing)) {
    stop("Failed to download required raster file(s): ", paste(still_missing, collapse = ", "), call. = FALSE)
  }

  out
}
raster_paths <- ensure_ess_dive_rasters(raster_files, raster_object_ids, raster_dir)

build_rasters_for_window <- function(x0, y0, side) {
  ext_global <- terra::ext(x0 + xmin, x0 + xmin + side, y0 + ymin, y0 + ymin + side)
  ras <- lapply(raster_paths, function(path) {
    r <- terra::rast(path)
    rc <- terra::crop(r, ext_global, snap = "out")
    terra::ext(rc) <- terra::ext(0, side, 0, side)
    rc
  })
  scale_rasters(ras)
}

find_window_near_target <- function(target_n, side_candidates = c(50, 75, 100), n_trials = 4000L) {
  best <- NULL

  for (side in side_candidates) {
    if (side > diff(domain_x) || side > diff(domain_y)) next

    for (k in seq_len(n_trials)) {
      x0 <- stats::runif(1, min = domain_x[1], max = domain_x[2] - side)
      y0 <- stats::runif(1, min = domain_y[1], max = domain_y[2] - side)

      in_win <- data_local |>
        filter(x >= x0, x <= x0 + side, y >= y0, y <= y0 + side)

      n_obs <- nrow(in_win)
      if (n_obs < 20L) next

      score <- abs(n_obs - target_n)
      cand <- list(x0 = x0, y0 = y0, side = side, n_obs = n_obs, score = score, data = in_win)

      if (is.null(best) || cand$score < best$score) best <- cand
      if (!is.null(best) && best$score <= max(8, 0.05 * target_n)) return(best)
    }
  }

  if (is.null(best)) stop("Could not locate a valid contiguous window.", call. = FALSE)
  best
}

win200 <- find_window_near_target(target_n = 200L, side_candidates = c(75, 50, 100))
win400 <- find_window_near_target(target_n = 400L, side_candidates = c(100, 75))

dataset_bank <- list(
  medium = list(
    id = "medium",
    data = medium_example_data,
    side = 50,
    rasters = medium_rasters,
    source = "medium_example_data"
  ),
  win200 = list(
    id = "win200",
    data = win200$data |> transmute(x = x - win200$x0, y = y - win200$y0, size = size),
    side = win200$side,
    rasters = build_rasters_for_window(win200$x0, win200$y0, win200$side),
    source = "full_data_contiguous_window"
  ),
  win400 = list(
    id = "win400",
    data = win400$data |> transmute(x = x - win400$x0, y = y - win400$y0, size = size),
    side = win400$side,
    rasters = build_rasters_for_window(win400$x0, win400$y0, win400$side),
    source = "full_data_contiguous_window"
  )
)

dataset_ids <- intersect(dataset_ids, names(dataset_bank))
if (!length(dataset_ids)) stop("No valid dataset IDs selected.", call. = FALSE)

dataset_manifest <- bind_rows(lapply(dataset_ids, function(id) {
  d <- dataset_bank[[id]]
  tibble(
    dataset_id = id,
    source = d$source,
    n_points = nrow(d$data),
    window_side = as.numeric(d$side)
  )
}))
readr::write_csv(dataset_manifest, datasets_path)

run_one <- function(dataset_id, rep_id, cores) {
  d <- dataset_bank[[dataset_id]]
  dat <- d$data
  side <- d$side
  xy_bounds <- c(0, side, 0, side)

  parallel_flag <- cores > 1L
  seed_base <- 41000L + 1000L * match(dataset_id, names(dataset_bank)) + 100L * cores + rep_id

  t0 <- proc.time()[["elapsed"]]
  fit <- estimate_process_parameters(
    data = dat,
    process = "self_correcting",
    grids = ldmppr_grids(upper_bounds = c(1, side, side), levels = cfg$grid_levels),
    budgets = ldmppr_budgets(
      global_options = cfg$global_opts,
      local_budget_first_level = cfg$local1_opts,
      local_budget_refinement_levels = cfg$localr_opts
    ),
    delta = 1,
    strategy = "multires_global_local",
    global_algorithm = "NLOPT_GN_CRS2_LM",
    local_algorithm = "NLOPT_LN_BOBYQA",
    starts = list(
      global = cfg$starts$global,
      local = cfg$starts$local,
      jitter_sd = cfg$starts$jitter_sd,
      seed = seed_base
    ),
    rescore_control = list(enabled = TRUE, top = 5L, objective_tol = 1e-6, param_tol = 0.1),
    parallel = parallel_flag,
    num_cores = cores,
    set_future_plan = parallel_flag,
    verbose = FALSE
  )
  estimate_sec <- proc.time()[["elapsed"]] - t0

  t1 <- proc.time()[["elapsed"]]
  mm <- train_mark_model(
    data = fit,
    raster_list = d$rasters,
    scaled_rasters = TRUE,
    model_type = "xgboost",
    xy_bounds = xy_bounds,
    parallel = parallel_flag,
    num_cores = cores,
    include_comp_inds = TRUE,
    competition_radius = 10,
    edge_correction = "none",
    selection_metric = "mae",
    cv_folds = cfg$cv_folds,
    tuning_grid_size = cfg$tuning_grid_size,
    seed = seed_base + 1L,
    verbose = FALSE
  )
  train_sec <- proc.time()[["elapsed"]] - t1

  tibble(
    profile = profile,
    dataset_id = dataset_id,
    n_points = nrow(dat),
    window_side = side,
    cores = cores,
    parallel = parallel_flag,
    rep = rep_id,
    estimate_sec = as.numeric(estimate_sec),
    train_mark_sec = as.numeric(train_sec),
    total_sec = as.numeric(estimate_sec + train_sec),
    objective = as.numeric(fit$fit$objective),
    model_engine = as.character(mm$engine),
    seed_estimate = seed_base,
    seed_train = seed_base + 1L,
    error = NA_character_
  )
}

runs <- list()
rid <- 0L

for (dataset_id in dataset_ids) {
  for (cores in cores_grid) {
    for (r in seq_len(n_reps)) {
      message("Stage timing dataset=", dataset_id, " cores=", cores, " rep=", r)
      row <- tryCatch(
        run_one(dataset_id = dataset_id, rep_id = r, cores = cores),
        error = function(e) {
          d <- dataset_bank[[dataset_id]]
          tibble(
            profile = profile,
            dataset_id = dataset_id,
            n_points = nrow(d$data),
            window_side = as.numeric(d$side),
            cores = cores,
            parallel = cores > 1L,
            rep = r,
            estimate_sec = NA_real_,
            train_mark_sec = NA_real_,
            total_sec = NA_real_,
            objective = NA_real_,
            model_engine = NA_character_,
            seed_estimate = 41000L + 1000L * match(dataset_id, names(dataset_bank)) + 100L * cores + r,
            seed_train = 41001L + 1000L * match(dataset_id, names(dataset_bank)) + 100L * cores + r,
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
  group_by(dataset_id, n_points, window_side, cores, parallel) |>
  summarise(
    runs = n(),
    failures = sum(!is.na(error)),
    estimate_mean = mean(estimate_sec, na.rm = TRUE),
    estimate_sd = stats::sd(estimate_sec, na.rm = TRUE),
    train_mean = mean(train_mark_sec, na.rm = TRUE),
    train_sd = stats::sd(train_mark_sec, na.rm = TRUE),
    total_mean = mean(total_sec, na.rm = TRUE),
    total_sd = stats::sd(total_sec, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(n_points, cores)

readr::write_csv(summary_tbl, summary_path)

cat("\nStage timing summary:\n")
print(summary_tbl)
cat("\nSaved dataset manifest to: ", datasets_path, "\n", sep = "")
cat("Saved run-level timings to: ", runs_path, "\n", sep = "")
cat("Saved summary timings to: ", summary_path, "\n", sep = "")

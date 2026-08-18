# ============================================================
# DROP-IN BASELINE (UPDATED): spatstat two-stage (location + mark)
# + OPTIONAL competition indices (spatial-only)
# + XGBoost mark model training (tidymodels/parsnip) matched to ldmppr::train_mark_model
# Mirrors ldmppr::check_model_fit() LGFJEV + combined GET rank test
# ============================================================

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

suppressPackageStartupMessages({
  library(spatstat.geom)
  library(spatstat.model)
  library(spatstat.explore)
  library(terra)
  library(GET)
  # library(ldmppr)      # example data + helpers (toroidal dist, scale_rasters)
  library(ldmppr)
  library(progress)
  
  # tidymodels stack (only needed for xgboost mark model)
  library(recipes)
  library(parsnip)
  library(workflows)
  library(rsample)
  library(tune)
  library(dials)
  library(yardstick)
  library(doParallel)
  library(foreach)
  library(readr)
  library(dplyr)
})

# ------------------------------------------------------------
# Runtime/config controls (override by env vars)
# ------------------------------------------------------------
cfg_n_sim <- as.integer(Sys.getenv("LDMPPR_SPATSTAT_N_SIM", "2500"))
cfg_cv_folds <- as.integer(Sys.getenv("LDMPPR_SPATSTAT_CV_FOLDS", "10"))
cfg_tuning_grid <- as.integer(Sys.getenv("LDMPPR_SPATSTAT_TUNING_GRID", "150"))
cfg_parallel <- tolower(Sys.getenv("LDMPPR_SPATSTAT_PARALLEL", "false")) %in% c("1", "true", "yes", "y")
cfg_num_cores <- as.integer(Sys.getenv("LDMPPR_SPATSTAT_NUM_CORES", "7"))
cfg_seed <- as.integer(Sys.getenv("LDMPPR_SPATSTAT_SEED", "90210"))

if (!is.finite(cfg_n_sim) || cfg_n_sim < 99L) stop("LDMPPR_SPATSTAT_N_SIM must be >= 99", call. = FALSE)
if (!is.finite(cfg_cv_folds) || cfg_cv_folds < 2L) stop("LDMPPR_SPATSTAT_CV_FOLDS must be >= 2", call. = FALSE)
if (!is.finite(cfg_tuning_grid) || cfg_tuning_grid < 1L) stop("LDMPPR_SPATSTAT_TUNING_GRID must be >= 1", call. = FALSE)
if (!is.finite(cfg_num_cores) || cfg_num_cores < 1L) cfg_num_cores <- 1L

out_dir <- file.path(proj_path, "timing_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# helper: competition indices (mirrors train_mark_model logic)
# - expects df with columns x, y
# - adds spatial-only: near_nbr_dist, near_nbr_num, avg_nbr_dist
# - edge_correction: none / toroidal
# NOTE: truncation is handled outside this helper (same as ldmppr)
# ------------------------------------------------------------
add_comp_inds_df <- function(df,
                             xy_bounds,
                             competition_radius = 15,
                             edge_correction = c("none", "toroidal", "truncation"),
                             verbose = FALSE) {
  edge_correction <- match.arg(edge_correction)
  stopifnot(is.data.frame(df))
  stopifnot(all(c("x", "y") %in% names(df)))
  stopifnot(is.numeric(xy_bounds), length(xy_bounds) == 4)
  
  s <- as.matrix(df[, c("x", "y"), drop = FALSE])
  
  if (edge_correction %in% c("none", "truncation")) {
    distance_matrix <- as.matrix(stats::dist(s, method = "euclidean"))
  } else {
    distance_matrix <- ldmppr:::toroidal_dist_matrix_optimized(
      s,
      xy_bounds[2] - xy_bounds[1],
      xy_bounds[4] - xy_bounds[3]
    )
  }
  
  df$near_nbr_dist <- NA_real_
  df$near_nbr_num <- NA_real_
  df$avg_nbr_dist <- NA_real_
  
  n_pts <- nrow(df)
  for (i in seq_len(n_pts)) {
    close_points <- unique(which(distance_matrix[i, ] < competition_radius & distance_matrix[i, ] != 0))
    
    # min distance to any other point
    df$near_nbr_dist[i] <- min(distance_matrix[i, ][-i])
    
    df$near_nbr_num[i] <- length(close_points)
    df$avg_nbr_dist[i] <- if (length(close_points)) mean(distance_matrix[i, close_points]) else min(distance_matrix[i, ][-i])
    
    nn_idx <- unique(which(distance_matrix[i, ] == df$near_nbr_dist[i]))
  }
  
  df
}

# ------------------------------------------------------------
# XGBoost mark model training (baseline) to match ldmppr::train_mark_model
# - input df_xy_size_time must contain x,y,size
# - extracts raster covars, adds x,y, optionally adds competition indices
# - edge_correction="truncation" drops points within 15m boundary (matches your function)
# - tuning: do_tuning if cv_folds>=2 and nrows>=cv_folds; space-filling grid size tuning_grid_size
# - selection_metric: rmse / mae / rsq (rsq also computes rmse,mae,rsq)
# returns list: wf_fit, model_data, resid_vec, feature_names
# ------------------------------------------------------------
train_mark_model_baseline_xgb <- function(df_xy_size_time,
                                          cov_stack,
                                          xy_bounds,
                                          include_comp_inds = FALSE,
                                          competition_radius = 15,
                                          edge_correction = c("none", "toroidal", "truncation"),
                                          selection_metric = c("rmse", "mae", "rsq"),
                                          cv_folds = 5,
                                          tuning_grid_size = 200,
                                          parallel = TRUE,
                                          num_cores = NULL,
                                          verbose = TRUE) {
  
  edge_correction <- match.arg(edge_correction)
  selection_metric <- match.arg(selection_metric)
  
  .vcat <- function(..., .indent = 0L) {
    if (!isTRUE(verbose)) return(invisible(NULL))
    indent <- if (.indent > 0L) paste(rep("  ", .indent), collapse = "") else ""
    message("[spatstat_comp::train_mark_model_baseline_xgb] ", indent, paste0(..., collapse = ""))
    invisible(NULL)
  }
  .step_header <- function(i, n, label) .vcat(sprintf("Step %d/%d: %s", i, n, label))
  .elapsed_sec <- function(t0) as.numeric((proc.time() - t0)[3])
  .fmt_time <- function(x) {
    if (!is.finite(x)) return("NA")
    if (x < 60) return(sprintf("%.1fs", x))
    if (x < 3600) return(sprintf("%.1fm", x / 60))
    sprintf("%.2fh", x / 3600)
  }
  
  stopifnot(is.data.frame(df_xy_size_time))
  stopifnot(all(c("x", "y", "size") %in% names(df_xy_size_time)))
  stopifnot(inherits(cov_stack, "SpatRaster"))
  stopifnot(is.numeric(xy_bounds), length(xy_bounds) == 4)

  .vcat("Training mark model baseline")
  .vcat("Selection metric: ", selection_metric, .indent = 1L)
  .vcat("CV folds: ", cv_folds, ", tuning grid size: ", tuning_grid_size, .indent = 1L)
  .vcat("Include competition indices: ", include_comp_inds, ", edge correction: ", edge_correction, .indent = 1L)
  
  # ---- parallel backend (PSOCK + foreach) ----
  .step_header(1, 4, "Configuring parallel backend")
  step_t <- proc.time()
  cl <- NULL
  n_workers <- 1L
  if (isTRUE(parallel)) {
    if (!is.null(num_cores)) {
      if (!is.numeric(num_cores) || num_cores < 1) stop("Provide num_cores >= 1.", call. = FALSE)
      n_workers <- as.integer(num_cores)
    } else {
      n_workers <- max(1L, floor(parallel::detectCores() / 2))
    }
    cl <- parallel::makePSOCKcluster(n_workers)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    }, add = TRUE)
    .vcat("Parallel: on (PSOCK workers = ", n_workers, ")", .indent = 1L)
  } else {
    foreach::registerDoSEQ()
    .vcat("Parallel: off", .indent = 1L)
  }
  
  # avoid nested parallelism during tuning
  engine_threads <- if (isTRUE(parallel)) 1L else max(1L, parallel::detectCores() - 1L)
  .vcat("Model engine threads: ", engine_threads, .indent = 1L)
  .vcat("Done in ", .fmt_time(.elapsed_sec(step_t)), ".", .indent = 1L)
  
  # ---- raster covariates ----
  .step_header(2, 4, "Extracting raster covariates")
  step_t <- proc.time()
  pts <- terra::vect(df_xy_size_time[, c("x", "y")], geom = c("x", "y"), crs = terra::crs(cov_stack))
  X <- terra::extract(cov_stack, pts)
  if ("ID" %in% names(X)) X <- X[, names(X) != "ID", drop = FALSE]
  names(X) <- make.unique(names(X), sep = "__")
  
  model_data <- data.frame(
    size = df_xy_size_time$size,
    X,
    x = df_xy_size_time$x,
    y = df_xy_size_time$y,
    check.names = FALSE
  )
  model_data <- model_data[complete.cases(model_data), , drop = FALSE]
  .vcat("Extracted ", ncol(model_data) - 1L, " feature(s) incl x,y.", .indent = 1L)
  .vcat("Done in ", .fmt_time(.elapsed_sec(step_t)), ".", .indent = 1L)
  
  # ---- competition indices (optional) ----
  .step_header(3, 4, "Building feature matrix")
  if (isTRUE(include_comp_inds)) {
    step_t <- proc.time()
    .vcat("Computing competition indices (radius=", competition_radius, ", edge=", edge_correction, ") ...", .indent = 1L)
    model_data <- add_comp_inds_df(
      df = model_data,
      xy_bounds = xy_bounds,
      competition_radius = competition_radius,
      edge_correction = edge_correction,
      verbose = verbose
    )
    model_data <- model_data[complete.cases(model_data), , drop = FALSE]
    .vcat("Done in ", .fmt_time(.elapsed_sec(step_t)), ".", .indent = 1L)
  }
  
  # ---- truncation (optional) ----
  if (edge_correction == "truncation") {
    ax <- xy_bounds[1]; bx <- xy_bounds[2]
    ay <- xy_bounds[3]; by <- xy_bounds[4]
    before_n <- nrow(model_data)
    model_data <- model_data[
      model_data$x > (ax + 15) & model_data$x < (bx - 15) &
        model_data$y > (ay + 15) & model_data$y < (by - 15),
      , drop = FALSE
    ]
    .vcat("Truncation kept ", nrow(model_data), "/", before_n, " rows.", .indent = 1L)
  }
  
  if (nrow(model_data) < 2) stop("Not enough rows to train mark model.", call. = FALSE)
  
  # ---- tune/fit XGBoost (matched to ldmppr defaults) ----
  .step_header(4, 4, "Fitting XGBoost")
  step_t <- proc.time()
  
  metric_set <- if (selection_metric == "rsq") {
    yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)
  } else {
    yardstick::metric_set(yardstick::rmse, yardstick::mae)
  }
  
  ctrl <- tune::control_grid(
    verbose = FALSE,
    parallel_over = if (isTRUE(parallel)) "resamples" else NULL
  )
  
  recipe_spec <- recipes::recipe(size ~ ., data = model_data)
  
  cv_folds <- as.integer(cv_folds)
  tuning_grid_size <- as.integer(tuning_grid_size)
  if (is.na(cv_folds) || cv_folds < 1L) stop("cv_folds must be >= 1.", call. = FALSE)
  if (is.na(tuning_grid_size) || tuning_grid_size < 1L) stop("tuning_grid_size must be >= 1.", call. = FALSE)
  
  do_tuning <- (cv_folds >= 2L) && (nrow(model_data) >= cv_folds)
  
  spec <- parsnip::boost_tree(
    mode = "regression",
    trees = if (do_tuning) hardhat::tune() else 500,
    min_n = if (do_tuning) hardhat::tune() else 5,
    tree_depth = if (do_tuning) hardhat::tune() else 6,
    learn_rate = if (do_tuning) hardhat::tune() else 0.05,
    loss_reduction = if (do_tuning) hardhat::tune() else 0
  ) %>%
    parsnip::set_engine(
      "xgboost",
      objective = "reg:squarederror",
      nthread = engine_threads,
      verbose = 0
    )
  
  wf <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_recipe(recipe_spec)
  
  if (isTRUE(do_tuning)) {
    .vcat("Tuning enabled: ", cv_folds, "-fold CV, grid size ", tuning_grid_size,
          " (", cv_folds * tuning_grid_size, " fits).", .indent = 1L)
    
    folds <- rsample::vfold_cv(model_data, v = cv_folds)
    
    params <- dials::parameters(
      dials::trees(),
      dials::min_n(),
      dials::tree_depth(),
      dials::learn_rate(),
      dials::loss_reduction()
    )
    grid <- dials::grid_space_filling(params, size = tuning_grid_size)
    
    tuned <- tune::tune_grid(
      object = wf,
      resamples = folds,
      grid = grid,
      metrics = metric_set,
      control = ctrl
    )
    
    best <- tune::select_best(tuned, metric = selection_metric)
    wf_final <- tune::finalize_workflow(wf, best)
    wf_fit <- parsnip::fit(wf_final, data = model_data)
  } else {
    .vcat("Tuning skipped: fitting one model with defaults.", .indent = 1L)
    wf_fit <- parsnip::fit(wf, data = model_data)
  }
  
  .vcat("Done in ", .fmt_time(.elapsed_sec(step_t)), ".", .indent = 1L)
  .vcat("Training complete.")
  
  # residuals for bootstrap noise
  pred_obs <- predict(wf_fit, new_data = model_data)$.pred
  resid_vec <- model_data$size - pred_obs
  
  feature_names <- setdiff(names(model_data), "size")
  
  list(
    workflow_fit = wf_fit,
    model_data = model_data,
    resid_vec = resid_vec,
    feature_names = feature_names,
    settings = list(
      include_comp_inds = include_comp_inds,
      competition_radius = competition_radius,
      edge_correction = edge_correction,
      selection_metric = selection_metric,
      cv_folds = cv_folds,
      tuning_grid_size = tuning_grid_size,
      parallel = parallel,
      num_cores = num_cores
    )
  )
}

# ------------------------------------------------------------
# 0) Data + covariate rasters (unchanged from your workflow)
# ------------------------------------------------------------
full_data <- readr::read_csv(
  "https://data.ess-dive.lbl.gov/catalog/d1/mn/v2/object/ess-dive-3120c69b6a46352-20240513T174234713",
  show_col_types = FALSE
) |>
  transmute(x = XTOP, y = YTOP, size = CANVOL2015) |>
  filter(is.finite(x), is.finite(y), is.finite(size), size > 0)

# Selected window from search (window_id = 2)
x0 <- 327308.19109940575
y0 <- 4311057.818756809
L <- 50

medium_example_data <- full_data |>
  filter(x >= x0, x <= x0 + L, y >= y0, y <= y0 + L) |>
  transmute(
    x = x - x0,
    y = y - y0,
    size = size
  )


# Window-specific rasters, relabeled to local [0, L] coordinates
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

window_extent_global <- terra::ext(x0, x0 + L, y0, y0 + L)
raster_list_window <- lapply(raster_paths, function(path) {
  r <- terra::rast(path)
  rc <- terra::crop(r, window_extent_global, snap = "out")
  terra::ext(rc) <- terra::ext(0, L, 0, L)
  rc
})
cov_list <- scale_rasters(raster_list_window)

df <- medium_example_data[, c("x", "y", "size")]
df <- df[complete.cases(df), ]

# raster_paths <- list.files(system.file("extdata", package = "ldmppr"),
#                            pattern = "\\.tif$", full.names = TRUE)
# raster_paths <- raster_paths[grepl("_med\\.tif$", raster_paths)]
# rasters <- lapply(raster_paths, terra::rast)
# 
# cov_list <- ldmppr::scale_rasters(rasters)

clean_name <- function(x) {
  nm <- names(x)[1]
  if (!is.null(nm) && nzchar(nm)) return(nm)
  tools::file_path_sans_ext(basename(sources(x)[1]))
}
names(cov_list) <- vapply(cov_list, clean_name, character(1))

cov_stack <- terra::rast(cov_list)
cov_names <- names(cov_stack)

win_ext <- terra::ext(cov_stack)
W <- owin(xrange = c(win_ext$xmin, win_ext$xmax),
          yrange = c(win_ext$ymin, win_ext$ymax))

inside <- df$x >= W$xrange[1] & df$x <= W$xrange[2] &
  df$y >= W$yrange[1] & df$y <= W$yrange[2]
df <- df[inside, ]

X_obs <- ppp(df$x, df$y, window = W, marks = df$size)
X_loc <- unmark(X_obs)

xy_bounds <- c(W$xrange[1], W$xrange[2], W$yrange[1], W$yrange[2])

# ------------------------------------------------------------
# 1) Fit location model (ppm): inhibition + covariates (unchanged)
# ------------------------------------------------------------
terra_to_im <- function(r) {
  stopifnot(inherits(r, "SpatRaster"), nlyr(r) == 1)
  m <- as.matrix(r, wide = TRUE)
  ex <- terra::ext(r)
  m <- m[nrow(m):1, , drop = FALSE]
  im(mat = m,
     xcol = seq(ex$xmin, ex$xmax, length.out = ncol(m)),
     yrow = seq(ex$ymin, ex$ymax, length.out = nrow(m)))
}
cov_im <- setNames(lapply(seq_len(nlyr(cov_stack)), function(j) terra_to_im(cov_stack[[j]])),
                   cov_names)

r_guess <- median(nndist(X_loc))
t_fit_loc <- proc.time()[["elapsed"]]
fit_loc <- ppm(
  X_loc,
  trend = as.formula(paste("~", paste(cov_names, collapse = " + "))),
  interaction = Strauss(r = r_guess),
  covariates = cov_im
)
fit_loc_sec <- proc.time()[["elapsed"]] - t_fit_loc

# ------------------------------------------------------------
# 2) Fit mark model (XGBoost): location + covariates + spatial competition indices
# - no synthetic time covariate is used
# - include_comp_inds is supported and will be used for both training + prediction
# ------------------------------------------------------------
include_comp_inds <- TRUE
competition_radius <- 10
edge_correction <- "none"   # choose "toroidal" to match toroidal distances
fg_correction <- tolower(Sys.getenv("LDMPPR_SPATSTAT_FG_CORRECTION", "km"))
if (!fg_correction %in% c("km", "rs")) {
  stop("LDMPPR_SPATSTAT_FG_CORRECTION must be one of: km, rs", call. = FALSE)
}

df_xy_size <- data.frame(
  x = df$x,
  y = df$y,
  size = df$size
)

t_fit_mark <- proc.time()[["elapsed"]]
mm <- train_mark_model_baseline_xgb(
  df_xy_size_time = df_xy_size,
  cov_stack = cov_stack,
  xy_bounds = xy_bounds,
  include_comp_inds = include_comp_inds,
  competition_radius = competition_radius,
  edge_correction = edge_correction,
  selection_metric = "mae",
  cv_folds = cfg_cv_folds,
  tuning_grid_size = cfg_tuning_grid,
  parallel = cfg_parallel,
  num_cores = cfg_num_cores,
  verbose = TRUE
)
fit_mark_sec <- proc.time()[["elapsed"]] - t_fit_mark

fit_mark <- mm$workflow_fit
mark_resid <- mm$resid_vec

# ------------------------------------------------------------
# 3) Predict marks for a simulated pattern (no synthetic time covariate)
# ------------------------------------------------------------
predict_marks_two_stage_xgb <- function(Xi_ppp,
                                        cov_stack,
                                        xy_bounds,
                                        fit_mark,
                                        resid_vec,
                                        include_comp_inds = FALSE,
                                        competition_radius = 15,
                                        edge_correction = c("none", "toroidal", "truncation"),
                                        enforce_positive = TRUE) {
  edge_correction <- match.arg(edge_correction)
  
  if (Xi_ppp$n == 0L) return(list(size = numeric(0)))
  
  # raster covariates at simulated points
  pts <- terra::vect(as.data.frame(spatstat.geom::coords(Xi_ppp)),
                     geom = c("x", "y"),
                     crs = terra::crs(cov_stack))
  cov_at <- terra::extract(cov_stack, pts)
  if ("ID" %in% names(cov_at)) cov_at <- cov_at[, setdiff(names(cov_at), "ID"), drop = FALSE]
  names(cov_at) <- make.unique(names(cov_at), sep = "__")
  
  newdat <- as.data.frame(cov_at)
  newdat$x <- spatstat.geom::coords(Xi_ppp)[, 1]
  newdat$y <- spatstat.geom::coords(Xi_ppp)[, 2]
  
  # optional competition indices are spatial-only in this baseline
  if (isTRUE(include_comp_inds)) {
    newdat <- add_comp_inds_df(
      df = newdat,
      xy_bounds = xy_bounds,
      competition_radius = competition_radius,
      edge_correction = edge_correction,
      verbose = FALSE
    )
  }
  
  mu <- as.numeric(predict(fit_mark, new_data = newdat)$.pred)
  
  # residual bootstrap noise
  mu <- mu + sample(resid_vec, size = length(mu), replace = TRUE)
  if (enforce_positive) mu <- pmax(mu, 0)
  
  list(size = mu)
}

simulate_two_stage_marked <- function(nsim,
                                      fit_loc,
                                      cov_stack,
                                      xy_bounds,
                                      fit_mark,
                                      resid_vec,
                                      include_comp_inds = FALSE,
                                      competition_radius = 15,
                                      edge_correction = c("none", "toroidal", "truncation"),
                                      seed = NULL) {
  edge_correction <- match.arg(edge_correction)
  if (!is.null(seed)) set.seed(seed)
  
  loc_sims <- simulate(fit_loc, nsim = nsim, drop = TRUE)
  
  out <- vector("list", nsim)
  for (i in seq_len(nsim)) {
    Xi <- loc_sims[[i]]
    if (Xi$n == 0L) {
      out[[i]] <- list(pp = Xi)
      next
    }
    
    mk <- predict_marks_two_stage_xgb(
      Xi_ppp = Xi,
      cov_stack = cov_stack,
      xy_bounds = xy_bounds,
      fit_mark = fit_mark,
      resid_vec = resid_vec,
      include_comp_inds = include_comp_inds,
      competition_radius = competition_radius,
      edge_correction = edge_correction
    )
    
    out[[i]] <- list(
      pp = setmarks(Xi, mk$size)
    )
  }
  
  out
}

# ------------------------------------------------------------
# 4) Baseline model-check: LGFJEV + combined GET rank test
# (your code, with one correction: J_scale should be per-r, not scalar)
# ------------------------------------------------------------
check_model_fit_spatstat_twostage <- function(reference_data,
                                              sim_list,
                                              n_sim,
                                              include_comp_inds,
                                              competition_radius,
                                              edge_correction,
                                              fg_correction = c("km", "rs"),
                                              seed = 0,
                                              verbose = TRUE) {
  fg_correction <- match.arg(fg_correction)
  set.seed(seed)
  .vmsg <- function(..., .indent = 0L) {
    if (!isTRUE(verbose)) return(invisible(NULL))
    indent <- if (.indent > 0L) paste(rep("  ", .indent), collapse = "") else ""
    message("[spatstat_comp::check_model_fit] ", indent, paste0(..., collapse = ""))
    invisible(NULL)
  }
  .step_header <- function(i, n, label) .vmsg(sprintf("Step %d/%d: %s", i, n, label))

  .vmsg("Checking baseline model fit")
  .vmsg("n_sim=", n_sim, ", fg_correction=", fg_correction, .indent = 1L)
  .step_header(1, 3, "Preparing reference-driven r-grids")

  ref_un <- spatstat.geom::unmark(reference_data)

  # Full grid from K (for L/E/V)
  K_ref <- spatstat.explore::Kest(ref_un)
  d_L <- K_ref$r
  dL_len <- length(d_L)

  # Reference-driven FGJ truncation (matches ldmppr::check_model_fit)
  F_ref_full <- spatstat.explore::Fest(ref_un, correction = fg_correction, r = d_L)[[fg_correction]]
  G_ref_full <- spatstat.explore::Gest(ref_un, correction = fg_correction, r = d_L)[[fg_correction]]
  ok_FG <- is.finite(F_ref_full) & is.finite(G_ref_full) & (F_ref_full < 1) & (G_ref_full < 1)

  if (!any(ok_FG)) {
    first_ok <- which(is.finite(F_ref_full) & is.finite(G_ref_full))
    if (!length(first_ok)) {
      stop("Reference F/G are non-finite for all r; cannot determine FGJ range.", call. = FALSE)
    }
    FGJ_max_idx <- first_ok[1]
  } else {
    FGJ_max_idx <- max(which(ok_FG))
  }
  FGJ_max_idx <- max(1L, FGJ_max_idx)
  d_FGJ <- d_L[1:FGJ_max_idx]
  dFGJ_len <- length(d_FGJ)

  .vmsg("Using FGJ r-grid from reference: 1:", FGJ_max_idx,
        " (max r=", signif(max(d_FGJ), 4), "), correction=", fg_correction, .indent = 1L)

  K_PP <- matrix(NA_real_, nrow = dL_len, ncol = n_sim)
  F_PP <- matrix(NA_real_, nrow = dFGJ_len, ncol = n_sim)
  G_PP <- matrix(NA_real_, nrow = dFGJ_len, ncol = n_sim)
  J_PP <- matrix(NA_real_, nrow = dFGJ_len, ncol = n_sim)
  E_PP <- matrix(NA_real_, nrow = dL_len, ncol = n_sim)
  V_PP <- matrix(NA_real_, nrow = dL_len, ncol = n_sim)
  n_real <- numeric(n_sim)
  
  .step_header(2, 3, "Generating simulations")
  if (isTRUE(verbose)) {
    pb <- progress::progress_bar$new(
      format = "spatstat_comp::check_model_fit sims: [:bar] :percent in :elapsed, ETA: :eta",
      total = n_sim, clear = FALSE, width = 80
    )
    pb$tick(0)
  }
  
  for (j in seq_len(n_sim)) {
    PP_xy <- sim_list[[j]]$pp
    n_real[j] <- PP_xy$n
    
    if (PP_xy$n < 1L) {
      K_PP[, j] <- rep(NA_real_, dL_len)
      F_PP[, j] <- rep(NA_real_, dFGJ_len)
      G_PP[, j] <- rep(NA_real_, dFGJ_len)
      J_PP[, j] <- rep(NA_real_, dFGJ_len)
      E_PP[, j] <- rep(NA_real_, dL_len)
      V_PP[, j] <- rep(NA_real_, dL_len)
    } else {
      K_PP[, j] <- spatstat.explore::Kest(spatstat.geom::unmark(PP_xy),
                                          correction = "isotropic", r = d_L)$iso
      F_PP[, j] <- spatstat.explore::Fest(spatstat.geom::unmark(PP_xy),
                                          correction = fg_correction, r = d_FGJ)[[fg_correction]]
      G_PP[, j] <- spatstat.explore::Gest(spatstat.geom::unmark(PP_xy),
                                          correction = fg_correction, r = d_FGJ)[[fg_correction]]
      J_PP[, j] <- spatstat.explore::Jest(spatstat.geom::unmark(PP_xy),
                                          correction = fg_correction, r = d_FGJ)[[fg_correction]] - 1
      E_PP[, j] <- spatstat.explore::Emark(PP_xy, correction = "isotropic", r = d_L)$iso
      V_PP[, j] <- spatstat.explore::Vmark(PP_xy, correction = "isotropic", r = d_L)$iso
    }
    
    if (isTRUE(verbose)) pb$tick()
  }
  
  accepted_cols <- which(
    colSums(!is.finite(K_PP)) == 0 &
      colSums(!is.finite(F_PP)) == 0 &
      colSums(!is.finite(G_PP)) == 0 &
      colSums(!is.finite(J_PP)) == 0 &
      colSums(!is.finite(E_PP)) == 0 &
      colSums(!is.finite(V_PP)) == 0
  )

  if (!length(accepted_cols)) {
    stop("No fully finite simulations available for envelope construction.", call. = FALSE)
  }

  if (length(accepted_cols) < n_sim) {
    warning("Using ", length(accepted_cols), "/", n_sim,
            " simulations after finite-value filtering.")
  }
  .vmsg("Accepted simulations after filtering: ", length(accepted_cols), "/", n_sim, .indent = 1L)

  .step_header(3, 3, "Computing envelope tests")
  K_use <- K_PP[, accepted_cols, drop = FALSE]
  F_use <- F_PP[, accepted_cols, drop = FALSE]
  G_use <- G_PP[, accepted_cols, drop = FALSE]
  J_use <- J_PP[, accepted_cols, drop = FALSE]
  E_use <- E_PP[, accepted_cols, drop = FALSE]
  V_use <- V_PP[, accepted_cols, drop = FALSE]
  n_real_use <- n_real[accepted_cols]

  C_ref_L <- GET::create_curve_set(list(
    r = d_L,
    obs = sqrt(K_ref$iso / pi) - d_L,
    theo = sqrt(K_ref$theo / pi) - d_L,
    sim_m = sqrt(K_use / pi) - d_L
  ))
  r_envL <- GET::global_envelope_test(C_ref_L, type = "rank")

  F_ref_use <- spatstat.explore::Fest(ref_un, correction = fg_correction, r = d_FGJ)[[fg_correction]]
  F_theo_use <- spatstat.explore::Fest(ref_un, correction = fg_correction, r = d_FGJ)$theo
  C_ref_F <- GET::create_curve_set(list(
    r = d_FGJ,
    obs = F_ref_use,
    theo = F_theo_use,
    sim_m = F_use
  ))
  r_envF <- GET::global_envelope_test(C_ref_F, type = "rank")

  G_ref_use <- spatstat.explore::Gest(ref_un, correction = fg_correction, r = d_FGJ)[[fg_correction]]
  G_theo_use <- spatstat.explore::Gest(ref_un, correction = fg_correction, r = d_FGJ)$theo
  C_ref_G <- GET::create_curve_set(list(
    r = d_FGJ,
    obs = G_ref_use,
    theo = G_theo_use,
    sim_m = G_use
  ))
  r_envG <- GET::global_envelope_test(C_ref_G, type = "rank")

  J_ref_use <- spatstat.explore::Jest(ref_un, correction = fg_correction, r = d_FGJ)[[fg_correction]] - 1
  J_theo_use <- spatstat.explore::Jest(ref_un, correction = fg_correction, r = d_FGJ)$theo - 1

  J_raw <- J_use
  # Match ldmppr::check_model_fit() scaling for J:
  # single global positive max over simulated J values.
  J_scale <- max(J_raw, na.rm = TRUE)
  if (!is.finite(J_scale) || J_scale <= 0) J_scale <- 1

  if (any(is.infinite(J_raw) | is.na(J_raw))) {
    warning("J_PP contains Inf or NA values in the range used for envelopes.")
  }
  
  C_ref_J <- GET::create_curve_set(list(
    r = d_FGJ,
    obs  = J_ref_use / J_scale,
    theo = J_theo_use / J_scale,
    sim_m = J_raw / J_scale
  ))
  r_envJ <- GET::global_envelope_test(C_ref_J, type = "rank")
  
  C_ref_E <- GET::create_curve_set(list(
    r = d_L,
    obs = spatstat.explore::Emark(reference_data, correction = "isotropic", r = d_L)$iso,
    theo = spatstat.explore::Emark(reference_data, correction = "isotropic", r = d_L)$theo,
    sim_m = E_use
  ))
  r_envE <- GET::global_envelope_test(C_ref_E, type = "rank")
  
  C_ref_V <- GET::create_curve_set(list(
    r = d_L,
    obs = spatstat.explore::Vmark(reference_data, correction = "isotropic", r = d_L)$iso,
    theo = spatstat.explore::Vmark(reference_data, correction = "isotropic", r = d_L)$theo,
    sim_m = V_use
  ))
  r_envV <- GET::global_envelope_test(C_ref_V, type = "rank")
  
  r_envComb <- GET::global_envelope_test(
    curve_sets = list(L = C_ref_L, F = C_ref_F, G = C_ref_G, J = C_ref_J, E = C_ref_E, V = C_ref_V),
    type = "rank"
  )
  
  out <- list(
    combined_env = r_envComb,
    envs = list(L = r_envL, F = r_envF, G = r_envG, J = r_envJ, E = r_envE, V = r_envV),
    curve_sets = list(L = C_ref_L, F = C_ref_F, G = C_ref_G, J = C_ref_J, E = C_ref_E, V = C_ref_V),
    sim_metrics = list(Ksim = K_use, Fsim = F_use, Gsim = G_use, Jsim = J_use, Esim = E_use, Vsim = V_use, n_per = n_real_use),
    settings = list(n_sim = n_sim, n_sim_used = length(accepted_cols), seed = seed,
                    include_comp_inds = include_comp_inds,
                    competition_radius = competition_radius,
                    edge_correction = edge_correction,
                    fg_correction = fg_correction,
                    FGJ_max_idx = FGJ_max_idx)
  )
  .vmsg("Model check complete.")
  out
}

# ------------------------------------------------------------
# 5) Run
# ------------------------------------------------------------
set.seed(cfg_seed)
n_sim <- cfg_n_sim

t_start <- proc.time()[["elapsed"]]
t0 <- proc.time()[["elapsed"]]

sim_list <- simulate_two_stage_marked(
  nsim = n_sim,
  fit_loc = fit_loc,
  cov_stack = cov_stack,
  xy_bounds = xy_bounds,
  fit_mark = fit_mark,
  resid_vec = mark_resid,
  include_comp_inds = include_comp_inds,
  competition_radius = competition_radius,
  edge_correction = edge_correction,
  seed = cfg_seed
)
sim_sec <- proc.time()[["elapsed"]] - t0

t1 <- proc.time()[["elapsed"]]
out <- check_model_fit_spatstat_twostage(
  reference_data = X_obs,
  sim_list = sim_list,
  n_sim = n_sim,
  include_comp_inds = include_comp_inds,
  competition_radius = competition_radius,
  edge_correction = edge_correction,
  fg_correction = fg_correction,
  seed = 1,
  verbose = TRUE
)
check_sec <- proc.time()[["elapsed"]] - t1
total_sec <- proc.time()[["elapsed"]] - t_start

combined_p <- as.numeric(attr(out$combined_env, "p"))
p_by_stat <- tibble(
  stat = c("L", "F", "G", "J", "E", "V"),
  p_value = c(
    as.numeric(attr(out$envs$L, "p")),
    as.numeric(attr(out$envs$F, "p")),
    as.numeric(attr(out$envs$G, "p")),
    as.numeric(attr(out$envs$J, "p")),
    as.numeric(attr(out$envs$E, "p")),
    as.numeric(attr(out$envs$V, "p"))
  )
)

summary_tbl <- tibble(
  method = "spatstat_two_stage",
  n_points = npoints(X_obs),
  n_sim = n_sim,
  fg_correction = fg_correction,
  include_comp_inds = include_comp_inds,
  competition_radius = competition_radius,
  edge_correction = edge_correction,
  cv_folds = cfg_cv_folds,
  tuning_grid_size = cfg_tuning_grid,
  parallel = cfg_parallel,
  num_cores = cfg_num_cores,
  fit_loc_sec = as.numeric(fit_loc_sec),
  fit_mark_sec = as.numeric(fit_mark_sec),
  sim_sec = as.numeric(sim_sec),
  check_sec = as.numeric(check_sec),
  total_sec = as.numeric(total_sec),
  p_combined = combined_p
)

readr::write_csv(summary_tbl, file.path(out_dir, "spatstat_comp_summary.csv"))
readr::write_csv(p_by_stat, file.path(out_dir, "spatstat_comp_p_by_stat.csv"))
saveRDS(
  list(
    check = out,
    sim_one = sim_list[[1]]$pp,
    reference = X_obs
  ),
  file.path(out_dir, "spatstat_comp_artifacts.rds")
)

cat("\nspatstat comparison summary:\n")
print(summary_tbl)
cat("\nSaved summary to: ", file.path(out_dir, "spatstat_comp_summary.csv"), "\n", sep = "")
cat("Saved stat p-values to: ", file.path(out_dir, "spatstat_comp_p_by_stat.csv"), "\n", sep = "")
cat("Saved artifacts to: ", file.path(out_dir, "spatstat_comp_artifacts.rds"), "\n", sep = "")

plot(out$combined_env)

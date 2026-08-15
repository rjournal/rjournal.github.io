#############################################################
##### This file contains the code for the ldmppr paper. #####
#############################################################

# Load necessary libraries
library("ldmppr")
library("patchwork")
library("ggplot2")
library("GET")
library("dplyr")
library("terra")

# Set a seed for reproducibility
seed <- 90210
set.seed(seed)

# ------------------------------------------------------------------
# Initial fit workflow
# ------------------------------------------------------------------

# Load the example data
data("medium_example_data")
parameter_estimation_data <- medium_example_data

# Initial optimization setup
upper_bounds <- c(1, 50, 50)

grids <- ldmppr_grids(
  upper_bounds = upper_bounds,
  levels = list(c(10, 10, 10))
)

budgets <- ldmppr_budgets(
  global_options = list(
    maxeval = 500,
    ftol_rel = 1e-3,
    xtol_rel = 1e-3
  ),
  local_budget_first_level = list(
    maxeval = 1000,
    ftol_rel = 1e-4,
    xtol_rel = 1e-4
  )
)

starts <- list(global = 1, local = 1, jitter_sd = 0.35, seed = seed)

estimated_sc <- estimate_process_parameters(
  data = parameter_estimation_data,
  process = "self_correcting",
  grids = grids,
  budgets = budgets,
  delta = 1,
  parallel = FALSE,
  strategy = "global_local",
  global_algorithm = "NLOPT_GN_CRS2_LM",
  local_algorithm = "NLOPT_LN_BOBYQA",
  starts = starts,
  verbose = TRUE
)

summary(estimated_sc)

# Load and scale the example rasters
raster_paths <- list.files(system.file("extdata", package = "ldmppr"),
                           pattern = "[.]tif$", full.names = TRUE)
raster_paths <- raster_paths[grepl("_med[.]tif$", raster_paths)]
rasters <- lapply(raster_paths, terra::rast)
scaled_rasters <- scale_rasters(rasters)

# Train initial mark model
example_trained_mark_model <- train_mark_model(
  data = estimated_sc,
  raster_list = scaled_rasters,
  scaled_rasters = TRUE,
  model_type = "xgboost",
  parallel = TRUE,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  selection_metric = "mae",
  cv_folds = 5,
  tuning_grid_size = 50,
  seed = seed,
  verbose = TRUE
)

# Initial model check
example_model_fit <- check_model_fit(
  process = "self_correcting",
  process_fit = estimated_sc,
  mark_model = example_trained_mark_model,
  include_comp_inds = TRUE,
  thinning = TRUE,
  edge_correction = "none",
  competition_radius = 10,
  n_sim = 499,
  save_sims = FALSE,
  seed = seed,
  verbose = TRUE,
  parallel = FALSE,
  fg_correction = "km"
)

plot(example_model_fit)

# ------------------------------------------------------------------
# Improved fit workflow
# ------------------------------------------------------------------

grids <- ldmppr_grids(
  upper_bounds = upper_bounds,
  levels = list(c(10, 10, 10),
                c(12, 12, 12),
                c(16, 16, 16))
)

budgets <- ldmppr_budgets(
  global_options = list(
    maxeval = 1000,
    ftol_rel = 1e-6,
    xtol_rel = 1e-6
  ),
  local_budget_first_level = list(
    maxeval = 2000,
    ftol_rel = 1e-8,
    xtol_rel = 1e-8
  ),
  local_budget_refinement_levels = list(
    maxeval = 3000,
    ftol_rel = 1e-10,
    xtol_rel = 1e-10
  )
)

starts <- list(global = 5, local = 3, jitter_sd = 0.15, seed = seed)

estimated_sc_update <- estimate_process_parameters(
  data = parameter_estimation_data,
  process = "self_correcting",
  grids = grids,
  budgets = budgets,
  delta = 1,
  parallel = FALSE,
  strategy = "multires_global_local",
  global_algorithm = "NLOPT_GN_CRS2_LM",
  local_algorithm = "NLOPT_LN_BOBYQA",
  starts = starts,
  verbose = TRUE
)

summary(estimated_sc_update)

improved_example_trained_mark_model <- train_mark_model(
  data = estimated_sc_update,
  raster_list = scaled_rasters,
  scaled_rasters = TRUE,
  model_type = "xgboost",
  parallel = TRUE,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  selection_metric = "mae",
  cv_folds = 10,
  tuning_grid_size = 150,
  seed = seed,
  verbose = TRUE
)

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
  seed = seed,
  verbose = TRUE,
  parallel = FALSE,
  fg_correction = "km"
)

plot(improved_example_model_fit)

# ------------------------------------------------------------------
# Simulation and comparison plot
# ------------------------------------------------------------------

improved_simulated_mpp <- simulate_mpp(
  process = "self_correcting",
  process_fit = estimated_sc_update,
  mark_model = improved_example_trained_mark_model,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  thinning = TRUE,
  seed = seed
)

initial_simulated_mpp <- simulate_mpp(
  process = "self_correcting",
  process_fit = estimated_sc,
  mark_model = example_trained_mark_model,
  include_comp_inds = TRUE,
  competition_radius = 10,
  edge_correction = "none",
  thinning = TRUE,
  seed = seed
)

reference_data <- generate_mpp(
  locations = medium_example_data[, c("x", "y")],
  marks = medium_example_data$size,
  xy_bounds = c(0, 50, 0, 50)
)

ref_plot <- plot_mpp(mpp_data = reference_data, pattern_type = "reference")
improved_sim_plot <- plot(improved_simulated_mpp, pattern_type = "simulated")
initial_sim_plot <- plot(initial_simulated_mpp, pattern_type = "simulated")

hist_data <- data.frame(mark_data = c(reference_data$marks,
                                      improved_simulated_mpp$mpp$marks,
                                      initial_simulated_mpp$mpp$marks),
                        pattern_type = c(rep("Reference Mark Distribution", reference_data$n),
                                         rep("Improved Simulated Mark Distribution", improved_simulated_mpp$mpp$n),
                                         rep("Initial Simulated Mark Distribution", initial_simulated_mpp$mpp$n))) |>
  dplyr::mutate(pattern_type = factor(pattern_type,
                                      levels = c("Reference Mark Distribution",
                                                 "Improved Simulated Mark Distribution",
                                                 "Initial Simulated Mark Distribution")))

hist_plot <- ggplot(hist_data, aes(x = mark_data)) +
  geom_histogram(binwidth = 100, position = "identity",
                 alpha = 0.75, color = "black", linewidth = .25) +
  facet_wrap(~pattern_type, nrow = 1) +
  labs(x = "Mark", y = "Count") +
  theme(strip.text = element_text(size = 12))

plot_design <- c("
                 111122223333
                 111122223333
                 111122223333
                 111122223333
                 444444444444
                 555555555555
                 555555555555
                 555555555555
                 555555555555")

data_plot <- (ref_plot +
                ggplot2::scale_size(breaks = c(0, 200, 400, 600, 800, 1000, 1200)) +
                ggplot2::theme(legend.position = "bottom")) +
  (improved_sim_plot +
     ggplot2::scale_size(breaks = c(0, 200, 400, 600, 800, 1000, 1200)) +
     ggplot2::theme(legend.position = "none") +
     ggplot2::labs(title = "Improved Simulated Data")) +
  (initial_sim_plot +
     ggplot2::scale_size(breaks = c(0, 200, 400, 600, 800, 1000, 1200)) +
     ggplot2::theme(legend.position = "none") +
     ggplot2::labs(title = "Initial Simulated Data")) +
  guide_area() +
  hist_plot +
  plot_layout(design = plot_design, guides = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

data_plot

# ------------------------------------------------------------------
# Optional: refresh precomputed objects used by ldmppr.Rmd
# ------------------------------------------------------------------

# Toggle to FALSE before submission if you do not want to overwrite
# precomputed artifacts when running this script.
SAVE_PRECOMPUTED_OBJECTS <- TRUE

if (isTRUE(SAVE_PRECOMPUTED_OBJECTS)) {
  saveRDS(estimated_sc, "./data/estimate_demo_par_initial.rds")
  save_mark_model(example_trained_mark_model, "./data/example_trained_mark_model_initial.rds")
  saveRDS(example_model_fit, "./data/example_model_fit_initial.rds")

  saveRDS(estimated_sc_update, "./data/estimate_demo_par.rds")
  save_mark_model(improved_example_trained_mark_model, "./data/example_trained_mark_model_updated.rds")
  saveRDS(improved_example_model_fit, "./data/example_model_fit_updated.rds")

  saveRDS(initial_simulated_mpp, "./data/initial_simulated_mpp.rds")
  saveRDS(improved_simulated_mpp, "./data/improved_simulated_mpp.rds")
}

sessionInfo()

#=========================================================#
# GeoThinneR: An R Package for Efficient Spatial Thinning
# of Species Occurrences and Point Data
#
# Replication script for functionalities and plots 
# and small benchmark code
#
# Jorge Mestre-Tomás, 2026
#
# Execution time: ~5 mins
# Output: Plots saved to ../figures
#=========================================================#

start_time <- Sys.time()
if (!dir.exists("../figures")) dir.create("../figures")

#=========================================================#
# 1. Load required packages ----
#=========================================================#
library("GeoThinneR")
library("enmSdmX")
library("ggforce")
library("ggpubr")
library("ggrepel")
library("peakRAM")
library("sf")
library("spThin")
library("terra")
library("tidyverse")

#=========================================================#
# 2. "Using GeoThinneR" manuscript section ----
#=========================================================#

# * Simulate data ----
set.seed(2547)
sim_data <- data.frame(
  lon = runif(100, 0, 1),
  lat = runif(100, 0, 1),
  group = sample(c("species_1", "species_2"), 100, replace = TRUE)
)

# Run thinning methods
# * Distance-based thinning ----
dist_thin <- thin_points(
  data = sim_data,
  method = "distance",
  thin_dist = 10,
  seed = 8237
)

# * Grid-based thinning ----
grid_thin <- thin_points(
  data = sim_data,
  method = "grid",
  resolution = 0.1,
  seed = 9137
)

# Using raster layer
rast_obj <- rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.1)
grid_thin <- thin_points(
  data = sim_data,
  method = "grid",
  raster_obj = rast_obj,
  seed = 54898
)

# * Precision-based thinning ----
prec_thin <- thin_points(
  data = sim_data,
  method = "precision",
  precision = 1,
  seed = 5674
)

# * Inspect the object ----
class(dist_thin)
print(dist_thin)
summary(dist_thin)
plot(dist_thin)
dist_thin$params
dist_thin$method
length(dist_thin$retained)
head(dist_thin$original_data)
head(largest(dist_thin))
head(get_trial(dist_thin, 1))

# * Thinning by group ----
precision_thin_group <- thin_points(
  data = sim_data,
  method = "precision",
  precision = 1,  # rounding precision
  group_col = "group",  # grouping variable
  trials = 50,
  seed = 2948
)

# * Thinning target points ----
target_thin <- thin_points(
  data = sim_data,
  target_points = 20,
  thin_dist = 10,
  all_trials = FALSE,
  seed = 675
)

# * Thinning priority ----
sim_data$priority <- runif(100, 1, 5)
rast_obj <- rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.2)
priority_thin <- thin_points(
  data = sim_data,
  method = "grid",
  raster_obj = rast_obj,
  priority = sim_data$priority,
  seed = 3455
)

# * Small use case: loggerhead turtle in the Mediterranean Sea ----
# Load data
data("caretta", package = "GeoThinneR")
head(caretta)
summary(caretta)

# Distance-based thinning - 50 km
real_thin <- thin_points(
  data = caretta,
  lon_col = "decimalLongitude",
  lat_col = "decimalLatitude",
  method = "distance",
  thin_dist = 50,
  trials = 5,
  all_trials = TRUE,
  seed = 6758
)

print(real_thin)
summary(real_thin)
plot(real_thin)

#=========================================================#
# 3. Minimal benchmark (complete benchmark in GeoThinneR_replication_large.R) ----
#=========================================================#

# * Simulate Complete Spatial Random (CSR) distribution ----
set.seed(8362)
thin_dist <- 10
trials <- 1
n_points <- 10000
csr_data <- data.frame(lon = runif(n_points, 0, 1), lat = runif(n_points, 0, 1), group = "csr")

# * Compare GeoThinneR neighbor-search methods ----
mat <- as.matrix(csr_data[, c("lon", "lat")])
brute_ns <- peakRAM(compute_neighbors_brute(mat, thin_dist))
kd_tree_ns <- peakRAM(compute_neighbors_kdtree(mat, thin_dist))
local_kd_tree_ns <- peakRAM(compute_neighbors_local_kdtree(mat, thin_dist))
k_estimation_ns <- peakRAM(function() {
  k_max <- estimate_k_max(mat, thin_dist)
  compute_neighbors_kdtree(mat, thin_dist, k_max)
})

ns_results <- data.frame(
  method = c("brute", "kd_tree", "local_kd_tree", "k_estimation"),
  time_sec = c(brute_ns$Elapsed_Time_sec, kd_tree_ns$Elapsed_Time_sec, local_kd_tree_ns$Elapsed_Time_sec, k_estimation_ns$Elapsed_Time_sec),
  total_RAM_MB = c(brute_ns$Total_RAM_Used_MiB, kd_tree_ns$Total_RAM_Used_MiB, local_kd_tree_ns$Total_RAM_Used_MiB, k_estimation_ns$Total_RAM_Used_MiB),
  peak_RAM_MB = c(brute_ns$Peak_RAM_Used_MiB, kd_tree_ns$Peak_RAM_Used_MiB, local_kd_tree_ns$Peak_RAM_Used_MiB, k_estimation_ns$Peak_RAM_Used_MiB)
)
print(ns_results)

# * Benchmark GeoThinneR, spThin and enmSdmX thinning ----
brute_thin <- peakRAM(thin_points(csr_data, method = "distance", search_type = "brute", thin_dist = thin_dist, trials = trials))
kd_tree_thin <- peakRAM(thin_points(csr_data, method = "distance", search_type = "kd_tree", thin_dist = thin_dist, trials = trials))
local_kd_tree_thin <- peakRAM(thin_points(csr_data, method = "distance", search_type = "local_kd_tree", thin_dist = thin_dist, trials = trials))
k_estimation_thin <- peakRAM(thin_points(csr_data, method = "distance", search_type = "k_estimation", thin_dist = thin_dist, trials = trials))
spThin_thin <- peakRAM(spThin::thin(csr_data, lat.col = "lat", long.col = "lon", spec.col = "group",
                                    thin.par = thin_dist, reps = trials, locs.thinned.list.return = TRUE,
                                    write.files = FALSE, write.log.file = FALSE, verbose = FALSE))
enmSdmX_thin <- peakRAM(enmSdmX::geoThin(csr_data, minDist = thin_dist*1000, longLat = c("lon", "lat"), method = "complete"))

thinning_results <- data.frame(
  method = c("brute", "kd_tree", "local_kd_tree", "k_estimation", "spThin", "enmSdmX"),
  time_sec = c(brute_thin$Elapsed_Time_sec, kd_tree_thin$Elapsed_Time_sec, local_kd_tree_thin$Elapsed_Time_sec, k_estimation_thin$Elapsed_Time_sec, spThin_thin$Elapsed_Time_sec, enmSdmX_thin$Elapsed_Time_sec),
  total_RAM_MB = c(brute_thin$Total_RAM_Used_MiB, kd_tree_thin$Total_RAM_Used_MiB, local_kd_tree_thin$Total_RAM_Used_MiB, k_estimation_thin$Total_RAM_Used_MiB, spThin_thin$Total_RAM_Used_MiB, enmSdmX_thin$Total_RAM_Used_MiB),
  peak_RAM_MB = c(brute_thin$Peak_RAM_Used_MiB, kd_tree_thin$Peak_RAM_Used_MiB, local_kd_tree_thin$Peak_RAM_Used_MiB, k_estimation_thin$Peak_RAM_Used_MiB, spThin_thin$Peak_RAM_Used_MiB, enmSdmX_thin$Peak_RAM_Used_MiB)
)
print(thinning_results)

#=========================================================#
# 4. Replication code for figures ----
#=========================================================#

# Fig. 1. Multiple thinning methods ----
p1a <- ggplot() +
  geom_point(data = sim_data[!dist_thin$retained[[1]], ], aes(x = lon, y = lat), color = "#EB714B", size = 2, alpha = 0.5) +
  geom_point(data = largest(dist_thin), aes(x = lon, y = lat), color = "#5183B3", size = 2) +
  geom_circle(data = largest(dist_thin), aes(x0 = lon, y0 = lat, r = 10 / 111), color = "grey", alpha = 0.2) +
  labs(
    title = "Distance-based thinning",
    subtitle = "Thinning distance = 10 km",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

rast_obj <- rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.1)
rast_obj <- terra::as.polygons(rast_obj)
rast_obj <- sf::st_as_sf(rast_obj)
p1b <- ggplot() +
  geom_sf(data = rast_obj, color = "#353839") +
  geom_point(data = sim_data[!grid_thin$retained[[1]], ], aes(x = lon, y = lat), color = "#EB714B", size = 2, alpha = 0.5) +
  geom_point(data = largest(grid_thin), aes(x = lon, y = lat), color = "#5183B3", size = 2) +
  labs(
    title = "Grid-based thinning",
    subtitle = "Grid resolution = 0.1",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

p1c <- sim_data %>%
  mutate(
    keep = replace(rep(FALSE, 100), as.integer(rownames(largest(prec_thin))), TRUE),
    label = ifelse(lon < 0.25 & lat < 0.135, paste0("[", round(lon, 3), ",", round(lat, 3), "]"), NA)
  ) %>%
  ggplot() +
  geom_point(aes(x = lon, y = lat, color = keep, alpha = keep), size = 2) +
  geom_text_repel(aes(x = lon, y = lat, label = label), size = 3) +
  scale_color_manual(values = c("#5183B3", "#EB714B"), guide = "none") +
  scale_alpha_manual(values = c(1, 0.5), guide = "none") +
  labs(
    title = "Precision-based thinning",
    subtitle = "Decimal precision = 1",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

p1 <- suppressWarnings(ggarrange(p1a, p1b, p1c, ncol = 3, labels = c("A", "B", "C")))
print(p1)
ggsave(filename = "../figures/p_thin_methods.pdf", p1, height = 3.5, width = 12)

# Fig. 2. Additional features ----
p2a <- ggplot() +
  geom_point(data = sim_data[!precision_thin_group$retained[[1]], ], aes(x = lon, y = lat, shape = group), color = "#EB714B", size = 2, alpha = 0.5) +
  geom_point(data = largest(precision_thin_group), aes(x = lon, y = lat, shape = group), color = "#5183B3", size = 2) +
  scale_shape_discrete(name = "Group", labels = c("Species 1", "Species 2")) +
  theme_minimal() +
  labs(
    title = "Thinning by group",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom")

p2b <- ggplot() +
  geom_point(data = sim_data[!target_thin$retained[[1]], ], aes(x = lon, y = lat), color = "#EB714B", size = 2, alpha = 0.5) +
  geom_point(data = largest(target_thin), aes(x = lon, y = lat), color = "#5183B3", size = 2) +
  labs(
    title = "Target points",
    x = "Longitude",
    y = "Latitude",
    caption = "20 target points"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1)

rast_obj <- rast(xmin = 0, xmax = 1, ymin = 0, ymax = 1, res = 0.2)
rast_obj <- terra::as.polygons(rast_obj)
rast_obj <- sf::st_as_sf(rast_obj)
p2c <- ggplot() +
  geom_sf(data = rast_obj, color = "#353839") +
  geom_point(data = sim_data[!priority_thin$retained[[1]], ], aes(x = lon, y = lat, size = priority), color = "#EB714B", alpha = 0.5) +
  geom_point(data = largest(priority_thin), aes(x = lon, y = lat, size = priority), color = "#5183B3") +
  theme_minimal() +
  labs(
    title = "Thinning by priority",
    x = "Longitude",
    y = "Latitude",
    size = "Priority"
  ) +
  theme_minimal() +
  theme(aspect.ratio = 1) +
  theme(legend.position = "bottom")

p2 <- ggarrange(p2a, p2b, p2c, ncol = 3, labels = c("A", "B", "C"))
print(p2)
ggsave(filename = "../figures/p_add_feat.pdf", p2, height = 3.5, width = 12)

#=========================================================#
# 5. Session Info ----
#=========================================================#

sessionInfo()

end_time <- Sys.time()
cat("Total execution time:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")

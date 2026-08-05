#=========================================================#
# GeoThinneR: An R Package for Efficient Spatial Thinning
# of Species Occurrences and Point Data
#
# Large replication script for benchmark results
#
# Jorge Mestre-Tomás, 2026
#
# Execution time: ~1-48 hours depending on n_points and reps
# Output: .RData results written to ../data and plots to ../figures
#=========================================================#

start_time <- Sys.time()
if (!dir.exists("../data")) dir.create("../data")
if (!dir.exists("../figures")) dir.create("../figures")

# ========================================================= #
# 0. Benchmark options ----
# ========================================================= #
# MODIFY OPTIONS FOR DIFFERENT BENCHMARK TESTS

# Number of points to simulate
n_points <- c(1000, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000)
# Neighbor search methods to test
neighbor_methods <- c("brute", "kd_tree", "local_kd_tree", "k_estimation")
# Thinning methods to test
thinning_methods <- c("brute", "kd_tree", "local_kd_tree", "k_estimation", "spThin", "enmSdmX")
# Data types to test: csr (complete spatial random), cluster (Matern clustered point process), mix (mixed)
data_types <- c("csr", "cluster", "mix")
# Thinning distance to test (in km)
thinning_distances <- c(1, 10)
# Number of repetitions for each test
reps <- 3
# Number of jobs to run in parallel
n_cores <- c(1, 2, 4, 6)
run_parallel <- TRUE
# Run real-world data benchmark
caretta_benchmark <- TRUE
thunnus_benchmark <- TRUE

# Seed for reproducibility
set.seed(8362)

#=========================================================#
# 1. Load required packages ----
#=========================================================#
library("GeoThinneR")
library("enmSdmX")
library("ggpubr")
library("peakRAM")
library("sf")
library("spatstat")
library("spThin")
library("terra")
library("tidyverse")

#=========================================================#
# 2. Simulate datasets ----
#=========================================================#

# * Complete Spatial Random (CSR) distribution ----
csr_data <- lapply(n_points, function(n) {
  data.frame(lon = runif(n, 0, 1), lat = runif(n, 0, 1), group = "csr")
})

# * Matern Cluster Process simulation ----
w <- as.owin(c(0,1,0,1))
clust_data <- lapply(n_points, function(n) {
  sim_pp <- rMatClust(10, 0.15, n, win = w)
  sel <- sample(1:sim_pp$n, n) # select exactly n points
  data.frame(lon = sim_pp$x[sel], lat = sim_pp$y[sel], group = "cluster")
})

# * Mixed data: CSR + Clustered ----
mix_data <- lapply(seq_along(n_points), function(i) {
  n <- n_points[i]
  sel <- sample(1:(2*n), n) # select exactly n points
  tmp_data <- rbind(csr_data[[i]], clust_data[[i]])[sel, ]
  tmp_data$group <- "mix"
  return(tmp_data)
})

# * Plot data ----
n_plots <- min(c(4, length(n_points)))
par(mfrow = c(3, n_plots), pty = "s")
lapply(csr_data[1:n_plots], function(x) plot(x[, 1:2], main = "CSR"))
lapply(clust_data[1:n_plots], function(x) plot(x[, 1:2], main = "cluster"))
lapply(mix_data[1:n_plots], function(x) plot(x[, 1:2], main = "Mixed"))

#=========================================================#
# 3. Load real data ----
#=========================================================#

# * Small real-world dataset ----
data("caretta", package = "GeoThinneR")

# * Large real-world dataset ----
data("thunnus", package = "GeoThinneR")

#=========================================================#
# 4. Compare GeoThinneR neighbor-search methods ----
#=========================================================#

# * Function to run benchmark ----
benchmark_neighbors <- function(data, thinning_distances, methods, reps) {
  data <- as.matrix(data[, 1:2])
  results <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(results) <- c("method", "thin_dist", "n_points", "rep", "time_sec", "total_RAM_MiB", "peak_RAM_MiB")

  if (nrow(data) > 50000) { # Memory overhead on my pc
    methods <- methods[!methods %in% c("brute", "kd_tree")]
  }

  for (thin_dist in thinning_distances){
    for (i in seq_len(reps)) {
      for (meth in methods){
        message("-------------------------------------------------------------------")
        message(paste0("Method: ", meth, " | Trial: ", i, " | N: ", nrow(data), " | ThinDist: ", thin_dist))
        if (meth == "brute"){
          tmp_res <- peakRAM(compute_neighbors_brute(data, thin_dist))
        } else if (meth == "kd_tree"){
          tmp_res <- peakRAM(compute_neighbors_kdtree(data, thin_dist))
        } else if (meth == "local_kd_tree") {
          tmp_res <- peakRAM(compute_neighbors_local_kdtree(data, thin_dist))
        } else if (meth == "k_estimation"){
          tmp_res <- peakRAM(function() {
            k_max <- estimate_k_max(data, thin_dist)
            compute_neighbors_kdtree(data, thin_dist, k_max)
          })
        }

        print(tmp_res)
        results <- rbind(
          results,
          data.frame(
            method = meth,
            thin_dist = thin_dist,
            n_points = nrow(data),
            rep = i,
            time_sec = tmp_res$Elapsed_Time_sec,
            total_RAM_MB = tmp_res$Total_RAM_Used_MiB,
            peak_RAM_MB = tmp_res$Peak_RAM_Used_MiB
          )
        )
      }
    }
  }

  return(results)
}

benchmark_multithread <- function(data, thinning_distances, n_cores, reps) {
  data <- as.matrix(data[, 1:2])
  results <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(results) <- c("method", "thin_dist", "n_cores", "n_points", "rep", "time_sec", "total_RAM_MiB", "peak_RAM_MiB")

  for (thin_dist in thinning_distances){
    for (i in seq_len(reps)) {
      for (j in n_cores){
        message("-------------------------------------------------------------------")
        message(paste0("N cores: ", j, " | Trial: ", i, " | N: ", nrow(data), " | ThinDist: ", thin_dist))

        tmp_res <- peakRAM(compute_neighbors_local_kdtree(data, thin_dist, n_cores = j))

        print(tmp_res)
        results <- rbind(
          results,
          data.frame(
            method = "local_kd_tree",
            thin_dist = thin_dist,
            n_cores = as.character(j),
            n_points = nrow(data),
            rep = i,
            time_sec = tmp_res$Elapsed_Time_sec,
            total_RAM_MB = tmp_res$Total_RAM_Used_MiB,
            peak_RAM_MB = tmp_res$Peak_RAM_Used_MiB
          )
        )
      }
    }
  }

  return(results)
}

# * Run neighbor search benchmarks for each data type ----

# Run benchmark for CSR data
if ("csr" %in% data_types) {
  csr_neighbor_results <- bind_rows(lapply(csr_data, benchmark_neighbors, thinning_distances = thinning_distances, methods = neighbor_methods, reps = reps))
  save(csr_neighbor_results, file = "../data/csr_neighbor_results.RData")
}

# Run benchmark for clustered data
if ("cluster" %in% data_types) {
  cluster_neighbor_results <- bind_rows(lapply(clust_data, benchmark_neighbors, thinning_distances = thinning_distances, methods = neighbor_methods, reps = reps))
  save(cluster_neighbor_results, file = "../data/cluster_neighbor_results.RData")
}

# Run benchmark for mixed data
if ("mix" %in% data_types) {
  mix_neighbor_results <- bind_rows(lapply(mix_data, benchmark_neighbors, thinning_distances = thinning_distances, methods = neighbor_methods, reps = reps))
  save(mix_neighbor_results, file = "../data/mix_neighbor_results.RData")
}

# Run multithreading benchmark
if ("csr" %in% data_types & run_parallel == TRUE){
  multithread_neighbor_results <- benchmark_multithread(csr_data[[length(csr_data)]], thinning_distances = c(0.5, 1), n_cores = n_cores, reps = reps)
  save(multithread_neighbor_results, file = "../data/multithread_neighbor_results.RData")
}

#=========================================================#
# 5. Benchmark GeoThinneR, spThin and enmSdmX thinning ----
#=========================================================#

# * Function to run benchmark ----
benchmark_thinning <- function(data, thinning_distances, methods, reps) {
  results <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(results) <- c("method", "thin_dist", "n_points", "rep", "time_sec", "total_RAM_MiB", "peak_RAM_MiB")

  if (nrow(data) > 50000) { # Memory overhead on my pc
    methods <- methods[!methods %in% c("brute", "kd_tree", "spThin", "enmSdmX")]
  }

  trials <- 1 # Number of thinning trials to maximize retained points - Just one trial for the benchmark
  for (thin_dist in thinning_distances){
    for (i in seq_len(reps)) {
      for (meth in methods){
        message("-------------------------------------------------------------------")
        message(paste0("Method: ", meth, " | Trial: ", i, " | N: ", nrow(data), " | ThinDist: ", thin_dist))
        if (meth == "brute"){
          tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "brute", thin_dist = thin_dist, trials = trials))
        } else if (meth == "kd_tree"){
          tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "kd_tree", thin_dist = thin_dist, trials = trials))
        } else if (meth == "local_kd_tree") {
          tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "local_kd_tree", thin_dist = thin_dist, trials = trials))
        } else if (meth == "k_estimation"){
          tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "k_estimation", thin_dist = thin_dist, trials = trials))
        } else if (meth == "spThin"){
          tmp_res <- peakRAM(spThin::thin(data, lat.col = "lat", long.col = "lon", spec.col = "group",
                                          thin.par = thin_dist, reps = trials, locs.thinned.list.return = TRUE,
                                          write.files = FALSE, write.log.file = FALSE, verbose = FALSE))
        } else if (meth == "enmSdmX"){
          tmp_res <- peakRAM(enmSdmX::geoThin(data, minDist = thin_dist*1000, longLat = c("lon", "lat"), method = "complete"))
        }

        print(tmp_res)
        results <- rbind(
          results,
          data.frame(
            method = meth,
            thin_dist = thin_dist,
            n_points = nrow(data),
            rep = i,
            time_sec = tmp_res$Elapsed_Time_sec,
            total_RAM_MB = tmp_res$Total_RAM_Used_MiB,
            peak_RAM_MB = tmp_res$Peak_RAM_Used_MiB
          )
        )
      }
    }
  }

  return(results)
}

# * Run thinning benchmarks for each data type ----

# Run benchmark for CSR data
if ("csr" %in% data_types) {
  csr_thinning_results <- bind_rows(lapply(csr_data, benchmark_thinning, thinning_distances = thinning_distances, methods = thinning_methods, reps = reps))
  save(csr_thinning_results, file = "../data/csr_thinning_results.RData")
}

# Run benchmark for clustered data
if ("cluster" %in% data_types) {
  cluster_thinning_results <- bind_rows(lapply(clust_data, benchmark_thinning, thinning_distances = thinning_distances, methods = thinning_methods, reps = reps))
  save(cluster_thinning_results, file = "../data/cluster_thinning_results.RData")
}

# Run benchmark for mixed data
if ("mix" %in% data_types) {
  mix_thinning_results <- bind_rows(lapply(mix_data, benchmark_thinning, thinning_distances = thinning_distances, methods = thinning_methods, reps = reps))
  save(mix_thinning_results, file = "../data/mix_thinning_results.RData")
}

#=========================================================#
# 6. Performance on real data ----
#=========================================================#

# * Function to run benchmark ----
benchmark_real <- function(thin_dist, data, methods, reps) {
  results <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(results) <- c("method", "thin_dist", "n_points", "rep", "time_sec", "total_RAM_MiB", "peak_RAM_MiB")

  if (nrow(data) > 50000) { # Memory overhead on my pc
    methods <- methods[!methods %in% c("brute", "kd_tree", "spThin", "enmSdmX")]
  }

  trials <- 1
  for (i in seq_len(reps)) {
    for (meth in methods){
      message("-------------------------------------------------------------------")
      message(paste0("Method: ", meth, " | Trial: ", i, " | N: ", nrow(data), " | ThinDist: ", thin_dist))
      if (meth == "local_kd_tree") {
        tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "local_kd_tree", thin_dist = thin_dist, trials = trials, n_cores = 6, lon_col = "decimalLongitude", lat_col = "decimalLatitude"))
      } else if (meth == "k_estimation"){
        tmp_res <- peakRAM(thin_points(data, method = "distance", search_type = "k_estimation", thin_dist = thin_dist, trials = trials, lon_col = "decimalLongitude", lat_col = "decimalLatitude"))
      } else if (meth == "spThin"){
        data$group <- "group"
        tmp_res <- peakRAM(spThin::thin(data, lat.col = "decimalLatitude", long.col = "decimalLongitude", spec.col = "group",
                                        thin.par = thin_dist, reps = trials, locs.thinned.list.return = TRUE,
                                        write.files = FALSE, write.log.file = FALSE, verbose = FALSE))
      }

      print(tmp_res)
      results <- rbind(
        results,
        data.frame(
          method = meth,
          thin_dist = thin_dist,
          n_points = nrow(data),
          rep = i,
          time_sec = tmp_res$Elapsed_Time_sec,
          total_RAM_MB = tmp_res$Total_RAM_Used_MiB,
          peak_RAM_MB = tmp_res$Peak_RAM_Used_MiB
        )
      )
    }
  }

  return(results)
}

# * Run real data benchmarks for each data type ----
# Run benchmark for small dataset
if (caretta_benchmark) {
  caretta_benchmark_results <- bind_rows(lapply(c(10, 25, 50), benchmark_real, data = caretta, methods = c("k_estimation", "local_kd_tree", "spThin"), reps = reps))
  save(caretta_benchmark_results, file = "../data/caretta_benchmark_results.RData")
}

# Run benchmark for clustered data
if (thunnus_benchmark) {
  thunnus_benchmark_results <- bind_rows(lapply(c(10, 25, 50), benchmark_real, data = thunnus, methods = c("k_estimation", "local_kd_tree", "spThin"), reps = reps))
  save(thunnus_benchmark_results, file = "../data/thunnus_benchmark_results.RData")
}

#=========================================================#
# 7. Replication code for figures ----
#=========================================================#
# Silence summarise info
options(dplyr.summarise.inform = FALSE)

# Fig. 4. Benchmark for neighbor searches ----
palette <- c("#cecece", "#a559aa", "#59a89c", "#f0c571", "#e02b35", "#082a54")
names(palette) <- c("kd-tree", "Brute-force", "local kd-trees", "k_max estimation", "spThin", "enmSdmX")

# Neighbor search results
load("../data/csr_neighbor_results.RData")
load("../data/cluster_neighbor_results.RData")
load("../data/mix_neighbor_results.RData")
load("../data/multithread_neighbor_results.RData")
csr_neighbor_results$data_type <- "CSR"
cluster_neighbor_results$data_type <- "Clustered"
mix_neighbor_results$data_type <- "Mix"

plot_results <- function(data, metric = "time_sec", y_label = "Execution Time (sec)"){
  data %>%
    group_by(method, thin_dist, n_points, data_type) %>%
    summarise(
      mean = mean(.data[[metric]]),
      ci_lower = quantile(.data[[metric]], 0.025),
      ci_upper = quantile(.data[[metric]], 0.975)
    ) %>%
    mutate(
      data_type = factor(data_type, levels = c("CSR", "Clustered", "Mix")),
      method = factor(method, levels = c("brute", "kd_tree", "local_kd_tree", "k_estimation", "spThin", "enmSdmX"),
                      labels = c("Brute-force", "kd-tree", "local kd-trees", "k_max estimation", "spThin", "enmSdmX")),
      thin_dist = factor(thin_dist, levels = c(1, 10))
    ) %>%
    ggplot(aes(x = n_points, y = mean, color = method, shape = thin_dist)) +
    geom_line() +
    geom_point(size = 3) +
    facet_grid(~ data_type, scales = "free") +
    scale_color_manual(values = palette) +
    xlab("Number of points (dataset size)") +
    ylab(y_label) +
    labs(shape="Thinning distance (km)", colour="Method") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = "black"),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2, size = 15),
          axis.title.x = element_text(vjust = -0.2, size = 15),
          axis.text = element_text(size = 10),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "inside",
          legend.position.inside = c(0.1, 0.6),
          legend.key.size= unit(0.2, "cm"),
          legend.title = element_text(face="italic"),
          legend.text = element_text(size = 10),
          legend.box.background = element_rect(colour ="black"),
          legend.box.margin = margin(6, 6, 6, 6),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="black",fill="#f0f0f0"),
          strip.text = element_text(face="bold", size = 15)
    )
}

p4a <- plot_results(bind_rows(csr_neighbor_results, cluster_neighbor_results, mix_neighbor_results), "peak_RAM_MB", "Peak RAM (MB)")
p4b <- plot_results(bind_rows(csr_neighbor_results, cluster_neighbor_results, mix_neighbor_results), "time_sec", "Execution time (sec)")

p4c <- multithread_neighbor_results %>%
  group_by(n_points, n_cores, thin_dist) %>%
  summarise(
    mean = mean(time_sec),
    ci_lower = quantile(time_sec, 0.025),
    ci_upper = quantile(time_sec, 0.975)
  ) %>%
  ggplot(aes(x = n_cores, y = mean, fill = as.factor(thin_dist))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#e34049", "#203f65")) +
  xlab("Number of threads") +
  ylab("Execution time (sec)") +
  labs(fill="Thinning distance (km)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2, size = 15),
        axis.title.x = element_text(vjust = -0.2, size = 15),
        axis.text = element_text(size = 10),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.key.size= unit(0.2, "cm"),
        legend.title = element_text(face="italic"),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="black",fill="#f0f0f0"),
        strip.text = element_text(face="bold")
  )
blank <- ggplot() + theme_void()
p4c <- ggarrange(blank, p4c, blank, ncol = 3, widths = c(0.5, 2, 0.5))

p4 <- ggarrange(
  p4a,
  p4b,
  p4c,
  nrow = 3,
  heights = c(1, 1, 0.75),
  labels = c("A", "B", "C")
)

print(p4)
ggsave(filename = "../figures/p_benchmark_search_type.pdf", p4, height = 10, width = 10)

# Fig. 5. Benchmark thinning R packages ----
load("../data/csr_thinning_results.RData")
load("../data/cluster_thinning_results.RData")
load("../data/mix_thinning_results.RData")
csr_thinning_results$data_type <- "CSR"
cluster_thinning_results$data_type <- "Clustered"
mix_thinning_results$data_type <- "Mix"

p5a <- plot_results(bind_rows(csr_thinning_results, cluster_thinning_results, mix_thinning_results), "peak_RAM_MB", "Peak RAM (MB)")
p5b <- plot_results(bind_rows(csr_thinning_results, cluster_thinning_results, mix_thinning_results), "time_sec", "Execution Time (sec)")

p5 <- ggarrange(
  p5a,
  p5b,
  nrow = 2,
  labels = c("A", "B")
)

print(p5)
ggsave(filename = "../figures/p_benchmark_thinning.pdf", p5, height = 7, width = 10)

# Fig. 6. Benchmark on real data ----
load("../data/caretta_benchmark_results.RData")
load("../data/thunnus_benchmark_results.RData")

caretta_10 <- GeoThinneR::thin_points(caretta, thin_dist = 10, lon_col = "decimalLongitude", lat_col = "decimalLatitude", method = "distance", search_type = "k_estimation", trials = 10, all_trials = FALSE, seed = 786)
caretta_25 <- GeoThinneR::thin_points(caretta, thin_dist = 25, lon_col = "decimalLongitude", lat_col = "decimalLatitude", method = "distance", search_type = "k_estimation", trials = 10, all_trials = FALSE, seed = 244)
caretta_50 <- GeoThinneR::thin_points(caretta, thin_dist = 50, lon_col = "decimalLongitude", lat_col = "decimalLatitude", method = "distance", search_type = "k_estimation", trials = 10, all_trials = FALSE, seed = 764)

sf::sf_use_s2(FALSE)
medit <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_as_sfc() %>%
  sf::st_crop(ext(-10, 38, 30, 46))

pa1 <- ggplot() +
  geom_sf(data = medit, fill = "#DADADA", color = "white") +
  geom_point(data = largest(caretta_10), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.75, size = 2, stroke = 0) +
  theme_void()+
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pa2 <- ggplot() +
  geom_sf(data = medit, fill = "#DADADA", color = "white") +
  geom_point(data = largest(caretta_25), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.75, size = 2, stroke = 0) +
  theme_void()+
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pa3 <- ggplot() +
  geom_sf(data = medit, fill = "#DADADA", color = "white") +
  geom_point(data = largest(caretta_50), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.75, size = 2, stroke = 0) +
  theme_void()+
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pa4 <- caretta_benchmark_results %>%
  group_by(method, thin_dist) %>%
  summarise(
    time_sec = mean(time_sec),
    peak_RAM_MB = mean(peak_RAM_MB)
  ) %>%
  mutate(
    method = factor(method, levels = c("brute", "kd_tree", "local_kd_tree", "k_estimation", "spThin"),
                    labels = c("Brute-force", "kd-tree", "local kd-trees", "k_max estimation", "spThin")),
    thin_dist = factor(thin_dist, levels = c(10, 25, 50), labels = c("10 km", "25 km", "50 km"))
  ) %>%
  ggplot(aes(x = method)) +
  geom_bar(aes(y = time_sec, fill = method), stat = "identity", position = "dodge") +
  geom_point(aes(y = peak_RAM_MB / max(peak_RAM_MB) * max(time_sec)), color = "#082a54", size = 3) +
  facet_grid(~ thin_dist) +
  scale_fill_manual(values = palette, guide = "none") +
  scale_y_continuous(
    name = "Execution time (sec)",
    sec.axis = sec_axis(~ . * max(caretta_benchmark_results$peak_RAM_MB) / max(caretta_benchmark_results$time_sec), name = "Peak RAM (MB)")
  ) +
  ylab("Peak RAM usage (MB)") +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = "black"),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = -0.2, size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(hjust = 1, size = 12,  face = "bold"),
    axis.line.x = element_line(colour="black"),
    axis.line.y = element_line(colour="black"),
    axis.ticks = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(10,5,5,5), "mm"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

print(pa4)

ggsave(pa1, filename = "../figures/caretta_10.png", width = 10, height = 8)
ggsave(pa2, filename = "../figures/caretta_25.png", width = 10, height = 8)
ggsave(pa3, filename = "../figures/caretta_50.png", width = 10, height = 8)
ggsave(pa4, filename = "../figures/p_caretta_benchmark.pdf", height = 2, width = 10)

thunnus_10 <- GeoThinneR::thin_points(thunnus, lon_col = "decimalLongitude", lat_col = "decimalLatitude", thin_dist = 10, method = "distance", search_type = "local_kd_tree", trials = 1, all_trials = FALSE, n_cores = 6, seed = 563)
thunnus_25 <- GeoThinneR::thin_points(thunnus, lon_col = "decimalLongitude", lat_col = "decimalLatitude", thin_dist = 25, method = "distance", search_type = "local_kd_tree", trials = 1, all_trials = FALSE, n_cores = 6, seed = 345)
thunnus_50 <- GeoThinneR::thin_points(thunnus, lon_col = "decimalLongitude", lat_col = "decimalLatitude", thin_dist = 50, method = "distance", search_type = "local_kd_tree", trials = 1, all_trials = FALSE, n_cores = 6, seed = 367)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_as_sfc()

pb1 <- ggplot() +
  geom_sf(data = world, fill = "#DADADA", color = "white") +
  geom_point(data = largest(thunnus_10), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.5, size = 1, stroke = 0) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pb2 <- ggplot() +
  geom_sf(data = world, fill = "#DADADA", color = "white") +
  geom_point(data = largest(thunnus_25), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.5, size = 1, stroke = 0) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pb3 <- ggplot() +
  geom_sf(data = world, fill = "#DADADA", color = "white") +
  geom_point(data = largest(thunnus_50), aes(x = decimalLongitude, y = decimalLatitude), color = "#203f65", alpha = 0.5, size = 1, stroke = 0) +
  theme_void()+
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )

pb4 <- thunnus_benchmark_results %>%
  group_by(method, thin_dist) %>%
  summarise(
    time_sec = mean(time_sec),
    peak_RAM_MB = mean(peak_RAM_MB)
  ) %>%
  mutate(
    method = factor(method, levels = c("brute", "kd_tree", "local_kd_tree", "k_estimation", "spThin"),
                    labels = c("Brute-force", "kd-tree", "local kd-trees", "k_max estimation", "spThin")),
    thin_dist = factor(thin_dist, levels = c(10, 25, 50), labels = c("10 km", "25 km", "50 km"))
  ) %>%
  ggplot(aes(x = method)) +
  geom_bar(aes(y = time_sec, fill = method), stat = "identity", position = "dodge") +
  geom_point(aes(y = peak_RAM_MB / max(peak_RAM_MB) * max(time_sec)), color = "#082a54", size = 3) +
  facet_grid(~ thin_dist) +
  scale_fill_manual(values = palette, guide = "none") +
  scale_y_continuous(
    name = "Execution time (sec)",
    sec.axis = sec_axis(~ . * max(caretta_benchmark_results$peak_RAM_MB) / max(caretta_benchmark_results$time_sec), name = "Peak RAM (MB)", labels = scales::label_number(scale = 1e-3, suffix = "K"))
  ) +
  ylab("Peak RAM usage (MB)") +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = "black"),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = -0.2, size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(hjust = 1, size = 12,  face = "bold"),
    axis.line.x = element_line(colour="black"),
    axis.line.y = element_line(colour="black"),
    axis.ticks = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(10,5,5,5), "mm"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

print(pb4)

ggsave(pb1, filename = "../figures/thunnus_10.png", width = 10, height = 8)
ggsave(pb2, filename = "../figures/thunnus_25.png", width = 10, height = 8)
ggsave(pb3, filename = "../figures/thunnus_50.png", width = 10, height = 8)
ggsave(pb4, filename = "../figures/p_thunnus_benchmark.pdf", height = 1.85, width = 10)

#=========================================================#
# 5. Session Info ----
#=========================================================#

sessionInfo()

end_time <- Sys.time()
cat("Total execution time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

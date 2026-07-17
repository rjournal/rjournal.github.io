# Reproduce results for section 3.2 of the manuscript.
# This script compares clustering methods (SFD, Kmedians, Gmedian) for design of experiments.
# It generates clustering error plots and computational time comparisons for different dimensions.

rm(list = ls()) # Clear workspace

# Load required libraries -----------------------------------------------------
library(Kmedians)
library(Gmedian)
library(SFDesign)
library(parallel)
library(doParallel)
library(ggplot2)
library(patchwork)

# Plotting utilities ---------------------------------------------------------
# my_theme: custom ggplot theme for consistent appearance
my_theme <- function() {
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.75, 0.85),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
}

# my_plot: summarizes clustering error across replicates and plots with quantile ribbon
# - method_list: list of matrices (replicates x sample-sizes) for each method
# - size_list: list of sample sizes for each method
# - factor_f: function to set factor levels for plotting
# - custom_shape_order, custom_color_order: manual style controls
# - upper/lower: quantiles for uncertainty ribbon
# - log_y: use log scale for y-axis
my_plot <- function(method_list, size_list, factor_f = NULL,
                    custom_shape_order = NULL, custom_color_order = NULL,
                    upper = 0.9, lower = 0.1, title = "", ylab = "y", log_y = TRUE) {
  n_method <- length(method_list)
  obj_summary <- c()
  n_list <- c()
  size_each_method <- c()
  # Compute quantiles and medians for each method
  for (i in 1:n_method) {
    obj_summary <- rbind(obj_summary, cbind(
      apply(method_list[[i]], 2, quantile, probs = lower, na.rm = TRUE),
      apply(method_list[[i]], 2, median, na.rm = TRUE),
      apply(method_list[[i]], 2, quantile, probs = upper, na.rm = TRUE)
    ))
    n_list <- c(n_list, size_list[[i]])
    size_each_method[i] <- length(size_list[[i]])
  }
  # Prepare data for plotting
  data.plot <- data.frame(obj_summary,
    n = n_list,
    group = rep(names(method_list), times = size_each_method)
  )
  if (!is.null(factor_f)) {
    data.plot$group <- factor_f(data.plot$group)
  }
  colnames(data.plot)[1:3] <- c("low", "mu", "high")
  # Build plot with median line and quantile ribbon
  pp <- ggplot(data = data.plot, aes(x = n, y = mu, color = group, fill = group)) +
    labs(y = ylab, title = title) +
    geom_line() +
    geom_point(aes(shape = group), size = 1) +
    scale_shape_manual(values = 1:n_method) +
    geom_ribbon(aes(x = n, ymin = low, ymax = high), color = NA, alpha = 0.2) +
    theme_bw() +
    my_theme()
  if (!is.null(custom_color_order)) {
    pp <- pp + scale_color_manual(values = custom_color_order) +
      scale_fill_manual(values = custom_color_order)
  }
  if (!is.null(custom_shape_order)) {
    pp <- pp + scale_shape_manual(values = custom_shape_order)
  }
  if (log_y) {
    pp <- pp + scale_y_log10()
  }
  return(pp)
}

# Comparison utilities -------------------------------------------------------
# compare: runs clustering methods on Sobol sequence and calculates clustering error
# - n_rep: number of replicates
# - nlist: list of sample sizes
# - p: dimension of the problem
# - N: number of points in Sobol sequence
# - cores: number of cores for parallel computation
# - iter.max: maximum number of iterations for clustering algorithms
compare <- function(n_rep = 10, nlist, p, N = 1e5,
                    cores = min(n_rep, 5), iter.max = 20) {
  set.seed(1)
  registerDoParallel(cores) # Register parallel backend
  result <- foreach(
    iter = 1:n_rep, .packages = c("Kmedians", "Gmedian"),
    .errorhandling = "pass", .options.RNG = 1
  ) %dopar% {
    Kmedians <- Gmedian <- SFD <- c()
    X <- randtoolbox::sobol(N + 1, p)
    for (n in nlist) {
      # SFD
      result_SFD <- SFDesign::clustering.design(n, p, X,
        Lloyd.iter.max = iter.max,
        Lloyd.tol = 1e-6
      )
      # Kmedians
      result_Kmedians <- Kmedians(X,
        nclust = n, niter = iter.max,
        ninit = 1, method = "Offline", init = FALSE
      )
      # Gmedian
      result_Gmedian <- kGmedian(X,
        ncenters = n, gamma = 1, alpha = 0.75,
        nstart = 1, nstartkmeans = 1,
        iter.max = 1000
      )

      SFD <- c(SFD, SFDesign::cluster.error(result_SFD$design, X))
      Kmedians <- c(Kmedians, SFDesign::cluster.error(result_Kmedians$bestresult$centers, X))
      Gmedian <- c(Gmedian, SFDesign::cluster.error(result_Gmedian$centers, X))
    }
    list(nlist = nlist, SFD = SFD, Kmedians = Kmedians, Gmedian = Gmedian)
  }
  stopImplicitCluster() # Stop parallel backend
  return(result)
}

# Generate plots for different dimensions -------------------------------------
# Figure 6 (a) ------------------------------
# p = 2 ------------------------------
p <- 2
N <- 1e5
nlist <- seq(5, 85, by = 16)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# result = compare(10, nlist, p, N)
# save(result, file=paste0('../result/cluster_result/p=', p, '.Rdata'))
load(paste0("../result/cluster_result/p=", p, ".Rdata")) # Load pre-computed result
result_list <- c("SFD", "Kmedians", "Gmedian")
factor_f <- function(column) {
  return(factor(column, levels = result_list))
}
custom_shape_order <- c(1, 2, 3)
custom_color_order <- c("#F8766D", "#7CAE00", "#00BFC4")
compare_result <- list()
n_rep <- 10
for (method_idx in 1:(length(result_list))) {
  method <- result_list[method_idx]
  compare_result[[method]] <- c()
  for (idx in 1:n_rep) {
    compare_result[[method]] <- rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list <- rep(list(result[[1]]$nlist), 3)
pp1 <- my_plot(compare_result, size_list, factor_f, custom_shape_order,
  custom_color_order,
  title = "p = 2", ylab = "cluster error", upper = 0.9, lower = 0.1
)
pp1

# p = 5 ------------------------------
p <- 5
N <- 1e5
nlist <- seq(10, 100, by = 18)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# result = compare(10, nlist, p, N, iter.max=50)
# save(result, file=paste0('../result/cluster_result/p=', p, '.Rdata'))
load(paste0("../result/cluster_result/p=", p, ".Rdata")) # Load pre-computed result
result_list <- c("SFD", "Kmedians", "Gmedian")
factor_f <- function(column) {
  return(factor(column, levels = result_list))
}
custom_shape_order <- c(1, 2, 3)
custom_color_order <- c("#F8766D", "#7CAE00", "#00BFC4")
compare_result <- list()
n_rep <- 10
for (method_idx in 1:(length(result_list))) {
  method <- result_list[method_idx]
  compare_result[[method]] <- c()
  for (idx in 1:n_rep) {
    compare_result[[method]] <- rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list <- rep(list(result[[1]]$nlist), 3)
pp2 <- my_plot(compare_result, size_list, factor_f, custom_shape_order,
  custom_color_order,
  title = "p = 5", ylab = "cluster error", upper = 0.9, lower = 0.1
)
pp2

# p = 10 ------------------------------
p <- 10
N <- 1e5
nlist <- seq(30, 150, by = 24)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# result = compare(10, nlist, p, N, iter.max=100)
# save(result, file=paste0('../result/cluster_result/p=', p, '.Rdata'))
load(paste0("../result/cluster_result/p=", p, ".Rdata")) # Load pre-computed result
result_list <- c("SFD", "Kmedians", "Gmedian")
factor_f <- function(column) {
  return(factor(column, levels = result_list))
}
custom_shape_order <- c(1, 2, 3)
custom_color_order <- c("#F8766D", "#7CAE00", "#00BFC4")
compare_result <- list()
n_rep <- 10
for (method_idx in 1:(length(result_list))) {
  method <- result_list[method_idx]
  compare_result[[method]] <- c()
  for (idx in 1:n_rep) {
    compare_result[[method]] <- rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list <- rep(list(result[[1]]$nlist), 3)
pp3 <- my_plot(compare_result, size_list, factor_f, custom_shape_order,
  custom_color_order,
  title = "p = 10", ylab = "cluster error", upper = 0.9, lower = 0.1
)
pp3

# Combine plots for different dimensions
pp <- pp1 + pp2 + pp3
ggsave("./figure/clustering_compare.pdf", plot = pp, width = 12, height = 4, units = "in")

# Compare computational time -------------------------------------------------
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# set.seed(2025) # Set seed for reproducibility
# Kmedians.time <- SFD <- c()
# n <- 50
# p <- 5
# X <- randtoolbox::sobol(1e5, p) # Generate Sobol sequence
# iter.max <- 50
# for (i in 1:20) {
#   # SFD
#   start_time <- proc.time()
#   result_SFD <- SFDesign::clustering.design(n, p, X, Lloyd.iter.max = iter.max, Lloyd.tol = 1e-6)
#   end_time <- proc.time()
#   SFD <- c(SFD, (end_time - start_time)[3])
#   # Kmedians
#   start_time <- proc.time()
#   result_Kmedians <- Kmedians(X, nclust = n, niter = iter.max, ninit = 1, method = "Offline", init = FALSE)
#   end_time <- proc.time()
#   Kmedians.time <- c(Kmedians.time, (end_time - start_time)[3])
# }
# df <- data.frame(time = c(SFD, Kmedians), package = c(rep("SFD", 10), rep("Kmedians", 10)))
# save(df, file=paste0('../result/cluster_result/time_comparison', '.Rdata'))
load(paste0("../result/cluster_result/time_comparison", ".Rdata")) # Load pre-computed result
pp4 <- ggplot(data = df, aes(x = package, y = time)) +
  geom_boxplot() +
  labs(title = "computational time", x = "package", y = "time (s)") +
  theme_bw() +
  my_theme()
pp4
ggsave("./figure/clustering_time.pdf", plot = pp4, width = 4, height = 4, units = "in")

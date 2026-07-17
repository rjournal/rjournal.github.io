# Reproduce results for section 3.1. Maximin design Figure 1
# This script compares maximin design results and records for 2D and 3D cases.
# It generates plots for maximin criterion and visualizes optimal/seven-point designs.

rm(list = ls()) # Clear workspace

# Load required libraries -----------------------------------------------------
library(parallel)
library(doParallel)
library(SFDesign)
library(ggplot2)
library(ggforce)
library(patchwork)

# Design generation utility ---------------------------------------------------
# design_gen: generates maximin designs for a list of sizes nlist and dimension p
# Uses parallel computation for efficiency
# Returns a list of optimized designs
# - nlist: vector of design sizes
# - p: dimension
# - cores: number of parallel workers
# Each design is optimized using simulated annealing (sa=TRUE)
design_gen <- function(nlist, p,
                       cores = min(length(nlist), 5)) {
  set.seed(1)
  registerDoParallel(cores)
  result <- foreach(
    idx = 1:length(nlist), .packages = c("SLHD"),
    .errorhandling = "pass", .options.RNG = 1
  ) %dopar% {
    n <- nlist[idx]
    # SFD
    D_SFD <- SFDesign::maximinLHD(n, p)$design
    D_SFD <- SFDesign::maximin.optim(D_SFD, sa = TRUE)
    D_SFD
  }
  stopImplicitCluster()
  return(result)
}

# Plotting utilities ---------------------------------------------------------
# my_theme: custom ggplot theme for consistent appearance
my_theme <- function() {
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.85, 0.85)
  )
}

# --- Figure 1 (b): Maximin criterion comparison for 2D and 3D designs --------
# 2D case --------------------------------------------------------------------
p <- 2
nlist <- seq(5, 100, by = 5)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# result = design_gen(nlist, p)
# save(result, file='../result/maximin_result/compare_record_p=2.Rdata')
load("../result/maximin_result/compare_record_p=2.Rdata")
SFD_result <- matrix(nrow = length(nlist), ncol = 2)
for (idx in 1:length(nlist)) {
  SFD_result[idx, 1] <- nlist[idx] # design size
  SFD_result[idx, 2] <- SFDesign::maximin.crit(result[[idx]]) # maximin criterion
}
record <- as.matrix(read.csv("../result/maximin_record/Maximin_2d_record.csv")[, 1:2])
colnames(record)[2] <- "distance"

# Plot: compare SFD and record for 2D
pp_2d <- ggplot() +
  geom_line(data = data.frame("n" = record[, 1], "Mmdist" = record[, 2]), aes(x = n, y = Mmdist, color = "record")) +
  geom_point(data = data.frame("n" = SFD_result[, 1], "Mmdist" = SFD_result[, 2]), aes(x = n, y = Mmdist, color = "SFD"), shape = 13, size = 3) +
  scale_color_manual(values = c("record" = "black", "SFD" = "red")) +
  labs(title = "p = 2", y = "maximin distance") +
  theme_bw() +
  my_theme()
pp_2d

# 3D case --------------------------------------------------------------------
p <- 3
nlist <- seq(5, 100, by = 5)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.
# result = design_gen(nlist, p)
# save(result, file=paste0('../result/maximin_result/compare_record_p=3.Rdata'))
load("../result/maximin_result/compare_record_p=3.Rdata")
SFD_result <- matrix(nrow = length(nlist), ncol = 2)
for (idx in 1:length(nlist)) {
  SFD_result[idx, 1] <- nlist[idx]
  SFD_result[idx, 2] <- SFDesign::maximin.crit(result[[idx]])
}
record <- as.matrix(read.csv("../result/maximin_record/Maximin_3d_record.csv")[, 1:2])
colnames(record)[2] <- "distance"

# Plot: compare SFD and record for 3D
pp_3d <- ggplot() +
  geom_line(data = data.frame("n" = record[, 1], "Mmdist" = record[, 2]), aes(x = n, y = Mmdist, color = "record")) +
  geom_point(data = data.frame("n" = SFD_result[, 1], "Mmdist" = SFD_result[, 2]), aes(x = n, y = Mmdist, color = "SFD"), shape = 13, size = 3) +
  scale_color_manual(values = c("record" = "black", "SFD" = "red")) +
  labs(title = "p = 3", y = "maximin distance") +
  theme_bw() +
  my_theme()
pp_3d

# Combine and save both plots
pp <- pp_2d + pp_3d
print(pp)
ggsave("./figure/Maximin_compare_record.pdf", plot = pp, width = 8, height = 4, units = "in")

# --- Figure 1 (a): Visualize seven-point optimal and SFD design in 2D --------
# Custom theme for this plot
my_theme <- function() {
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
}

# Generate and plot n=7, p=2 designs -----------------------------------------
set.seed(42)
n <- 7
p <- 2
# SFD design
D_SFD <- SFDesign::maximinLHD(n, p)$design
D_SFD <- SFDesign::maximin.optim(D_SFD, sa = TRUE)$design
plot(D_SFD, pch = 16)
# Known optimal configuration for n=7, p=2
r <- 1 / (2 + sqrt(3))
optimal <- matrix(c(
  0, 0,
  0, 2 * r,
  2 * r, 0,
  2 * r, 2 * r,
  1, r,
  r, 1,
  1, 1
), nrow = 7, byrow = TRUE)
points(optimal)
SFDesign::maximin.crit(D_SFD)
# Prepare data for ggplot
df <- data.frame(rbind(D_SFD, optimal), method = rep(c("SFD", "optimal"), each = n))
pp <- ggplot(data = df, aes(x = X1, y = X2)) +
  geom_point(aes(shape = method, color = method), size = 3) +
  geom_circle(data = data.frame(D_SFD), aes(x0 = X1, y0 = X2, r = r), color = "red") +
  scale_shape_manual(values = c("SFD" = 16, "optimal" = 11)) +
  scale_color_manual(values = c("SFD" = "red", "optimal" = "black")) +
  theme_bw() +
  my_theme() +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = TRUE,
    default = FALSE,
    clip = "on"
  ) +
  coord_fixed(ratio = 1) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
    fill = NA, color = "black", linetype = "dashed"
  ) +
  labs(x = expression(x[1]), y = expression(x[2]))
pp
ggsave("./figure/Maximin_seven_points.pdf", plot = pp, width = 5, height = 4, units = "in")

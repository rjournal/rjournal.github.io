# Reproduce results for section 4.
# This script reproduces figures and analyses from Section 4 of the manuscript.
# It compares design methods (MaxPro, maximin, SFDesign variations) using surrogate
# modeling and uncertainty quantification (UQ) experiments.

rm(list = ls())

# Load required packages -----------------------------------------------------
library(SFDesign)
library(rkriging)
library(MaxPro)
library(ggplot2)
library(patchwork)

# Plot utilities -------------------------------------------------------------
# Small helpers to standardize plot appearance across figures in the paper.
my_theme <- function() {
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.3, 0.8),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
}

# my_plot: summarizes multiple replicates of performance metrics and creates a
# ggplot with median and an uncertainty ribbon between specified quantiles.
# Parameters:
# - method_list: list of matrices (replicates x sample-sizes) with metric values
# - size_list: list of vectors containing the sample sizes corresponding to columns
# - factor_f: optional function to convert group names to factors for ordering
# - custom_*_order: optional manual ordering/styles for shape, color, line types
# - upper/lower: quantiles used for the uncertainty ribbon (defaults to 0.1/0.9)
# - log_y: whether to use a log10 y-scale
my_plot <- function(method_list, size_list, factor_f = NULL,
                    custom_shape_order = NULL, custom_color_order = NULL, custom_line_order = NULL,
                    upper = 0.9, lower = 0.1, title = "", ylab = "y", log_y = FALSE) {
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

  # Build plot
  pp <- ggplot(data = data.plot, aes(x = n, y = mu, color = group, fill = group)) +
    labs(y = ylab, title = title) +
    geom_line(aes(linetype = group)) +
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
  if (!is.null(custom_line_order)) {
    pp <- pp + scale_linetype_manual(values = custom_line_order)
  }
  if (log_y) {
    pp <- pp + scale_y_log10()
  }
  return(pp)
}

# Reproduce section 4.1: surrogate modeling for an operational transconductance
# amplifier (OTL) circuit example. The function below maps normalized inputs in
# [0,1]^6 to physical component values and returns the simulated output Vm.
otlcircuit <- function(xx) {
  # Map normalized inputs to physical ranges
  Rb1 <- xx[1] * 100 + 50
  Rb2 <- xx[2] * 50 + 25
  Rf <- xx[3] * 2.5 + 0.5
  Rc1 <- xx[4] * 1.3 + 1.2
  Rc2 <- xx[5] * 0.95 + 0.25
  beta <- xx[6] * 250 + 50

  # Formulas derived from circuit model to compute the output Vm
  Vb1 <- 12 * Rb2 / (Rb1 + Rb2)
  term1a <- (Vb1 + 0.74) * beta * (Rc2 + 9)
  term1b <- beta * (Rc2 + 9) + Rf
  term1 <- term1a / term1b

  term2a <- 11.35 * Rf
  term2b <- beta * (Rc2 + 9) + Rf
  term2 <- term2a / term2b

  term3a <- 0.74 * Rf * beta * (Rc2 + 9)
  term3b <- (beta * (Rc2 + 9) + Rf) * Rc1
  term3 <- term3a / term3b

  Vm <- term1 + term2 + term3
  return(Vm)
}

set.seed(1)
# Design and experiment settings used for this section
n <- 60 # number of design points
p <- 12 # input dimension used for some comparisons
n.rep <- 50
f <- otlcircuit

# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are saved in '../result/case_result/' and loaded below. To re-run the
# experiments from scratch, uncomment the block between lines indicated in the
# original script.
# maxpro.rmse = sfd.rmse = maximin.rmse = c()
# maxpro.crit = sfd.crit = c()
# for (i in 1:n.rep){
#   x.test = spacefillr::generate_sobol_set(1e6, p, i)
#   y.test = apply(x.test, 1, f)
#
#   D = maxproLHD(n, p)$design
#   y = apply(D, 1, f)
#   model = Fit.Kriging(D, y, kernel.parameters=list(type="Gaussian"))
#   y.pred = Predict.Kriging(model, x.test)$mean
#   sfd.rmse = c(sfd.rmse, mean((y.test-y.pred)^2))
#   sfd.crit = c(sfd.crit, maxpro.crit(D))
#
#   D = MaxProLHD(n, p)$Design
#   y = apply(D, 1, f)
#   model = Fit.Kriging(D, y, kernel.parameters=list(type="Gaussian"))
#   y.pred = Predict.Kriging(model, x.test)$mean
#   maxpro.rmse = c(maxpro.rmse, mean((y.test-y.pred)^2))
#   maxpro.crit = c(maxpro.crit, maxpro.crit(D))
#
#   D = maximinLHD(n, p)$design
#   y = apply(D, 1, f)
#   model = Fit.Kriging(D, y, kernel.parameters=list(type="Gaussian"))
#   y.pred = Predict.Kriging(model, x.test)$mean
#   maximin.rmse = c(maximin.rmse, mean((y.test-y.pred)^2))
# }
#
# rmse.df = data.frame(rmse=c(maximin.rmse, maxpro.rmse, sfd.rmse),
#                      func=c(rep('SFDesign::\n maximinLHD', n.rep),
#                             rep('MaxPro::\n MaxProLHD', n.rep),
#                             rep('SFDesign::\n maxproLHD', n.rep)) )
# rmse.df$func = factor(rmse.df$func, levels=c('SFDesign::\n maximinLHD',
#                                             'MaxPro::\n MaxProLHD', 'SFDesign::\n maxproLHD'))
# crit.df = data.frame(crit=c(maxpro.crit, sfd.crit),
#                      func=c(rep('MaxPro::\n MaxProLHD', n.rep),
#                             rep('SFDesign::\n maxproLHD', n.rep)) )
# crit.df$func = factor(crit.df$func, levels=c('MaxPro::\n MaxProLHD', 'SFDesign::\n maxproLHD'))

# save(rmse.df, file=paste0('../result/case_result/surrogate_rmse.Rdata'))
# save(crit.df, file=paste0('../result/case_result/surrogate_crit.Rdata'))
# Load precomputed results (uncomment above to rerun)
load(paste0("../result/case_result/surrogate_rmse.Rdata"))
load(paste0("../result/case_result/surrogate_crit.Rdata"))

# Boxplot: MaxPro criterion
pp1 <- ggplot(data = crit.df, aes(x = func, y = crit)) + # compare design criteria
  geom_boxplot() +
  geom_point(col = "gray") +
  labs(title, x = "", y = "MaxPro criterion") +
  theme_bw() +
  my_theme() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
pp1

# Boxplot: RMSE
pp2 <- ggplot(data = rmse.df, aes(x = func, y = rmse)) + # compare surrogate RMSE
  geom_boxplot() +
  geom_point(col = "gray") +
  labs(title, x = "", y = "RMSE") +
  theme_bw() +
  my_theme() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# Combine and save
pp <- pp1 + pp2
pp
ggsave("./figure/case_surrogate.pdf", plot = pp, width = 8, height = 4.5, units = "in")

# Figure 7: visualize a design (pairs plot for first 6 dimensions)
n <- 60
p <- 10
D <- maxproLHD(n, p)$design
pdf("./figure/surrogate_design.pdf", width = 4, height = 4)
pairs(D[, 1:6], labels = c(
  expression(R[b1]), expression(R[b2]), expression(R[f]),
  expression(R[c1]), expression(R[c2]), expression(beta)
), cex = 0.6)
dev.off()


# Reproduce section 4.2: uncertainty quantification (UQ) on the piston function.
# The piston function maps normalized inputs to the natural frequency C.
piston <- function(xx) {
  # Map normalized inputs to physical ranges
  M <- xx[1] * 30 + 30
  S <- xx[2] * (0.02 - 0.005) + 0.005
  V0 <- xx[3] * (0.01 - 0.002) + 0.002
  k <- xx[4] * (5000 - 1000) + 1000
  P0 <- xx[5] * (110000 - 90000) + 90000
  Ta <- xx[6] * 6 + 290
  T0 <- xx[7] * 20 + 340

  # Compute derived quantities and final output C (natural frequency)
  Aterm1 <- P0 * S
  Aterm2 <- 19.62 * M
  Aterm3 <- -k * V0 / S
  A <- Aterm1 + Aterm2 + Aterm3

  Vfact1 <- S / (2 * k)
  Vfact2 <- sqrt(A^2 + 4 * k * (P0 * V0 / T0) * Ta)
  V <- Vfact1 * (Vfact2 - A)

  fact1 <- M
  fact2 <- k + (S^2) * (P0 * V0 / T0) * (Ta / (V^2))

  C <- 2 * pi * sqrt(fact1 / fact2)
  return(C)
}

set.seed(1)
n <- 50
p <- 7
f <- piston
# Generate a large Sobol set for ground-truth estimates of mean/variance
x.test <- spacefillr::generate_sobol_set(1e6, p)
y.test <- apply(x.test, 1, f)
mean.response <- mean(y.test)
var.response <- var(y.test)

# NOTE: The UQ experiment loop is also computationally heavy and pre-computed
# results are loaded below. See the commented block in the original file if you
# need to re-run.
# dice.mean = sfd.mean = c()
# dice.var = sfd.var = c()
# dice.crit = sfd.crit = c()
# for (i in 1:n.rep){
#   D.ini = randomLHD(n, p)
#   D.sfd = uniformLHD(n, p, D.ini)$design
#   D.dice = DiceDesign::discrepSA_LHS(D.ini, criterion='W2')$design
#
#   sfd.crit = c(sfd.crit, uniform.crit(D.sfd))
#   dice.crit = c(dice.crit, uniform.crit(D.dice))
#
#   y.sfd = apply(D.sfd, 1, f)
#   sfd.mean = c(sfd.mean, mean(y.sfd))
#   sfd.var = c(sfd.var, var(y.sfd))
#   y.dice = apply(D.dice, 1, f)
#   dice.mean = c(dice.mean, mean(y.dice))
#   dice.var = c(dice.var, var(y.dice))
# }
#
# mean.df = data.frame(mean=c(dice.mean, sfd.mean),
#                      func= c(rep('DiceDesign::\n discrepSA_LHS', n.rep),
#                      rep('SFDesign::\n uniformLHD', n.rep)) )
# mean.df$func = factor(mean.df$func, levels = c('DiceDesign::\n discrepSA_LHS',
#                                                 'SFDesign::\n uniformLHD'))
# var.df = data.frame(var=c(dice.var, sfd.var),
#                     func= c(rep('DiceDesign::\n discrepSA_LHS', n.rep),
#                             rep('SFDesign::\n uniformLHD', n.rep)) )
# var.df$func = factor(var.df$func, levels = c('DiceDesign::\n discrepSA_LHS',
#                                                 'SFDesign::\n uniformLHD'))
# crit.df = data.frame(crit=c(dice.crit, sfd.crit),
#                      func= c(rep('DiceDesign::\n discrepSA_LHS', n.rep),
#                              rep('SFDesign::\n uniformLHD', n.rep)) )
# crit.df$func = factor(crit.df$func, levels = c('DiceDesign::\n discrepSA_LHS',
#                                                 'SFDesign::\n uniformLHD'))
#
# save(mean.df, file=('../result/case_result/uq_mean.Rdata'))
# save(var.df, file=('../result/case_result/uq_var.Rdata'))
# save(crit.df, file=('../result/case_result/uq_crit.Rdata'))
load("../result/case_result/uq_mean.Rdata")
load("../result/case_result/uq_var.Rdata")
load("../result/case_result/uq_crit.Rdata")

# Plot warp-around discrepancy comparison
pp1 <- ggplot(data = crit.df, aes(x = func, y = crit)) +
  geom_boxplot() +
  geom_point(col = "gray") +
  labs(title, x = "", y = "warp-around discrepancy") +
  theme_bw() +
  my_theme() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
pp1

# Plot estimated means from designs compared with true mean (red line)
pp2 <- ggplot(data = mean.df, aes(x = func, y = mean)) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = mean.response, color = "red", linewidth = 1.5) +
  geom_point(col = "gray") +
  labs(title, x = "", y = "mean") +
  theme_bw() +
  my_theme() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
pp2

# Plot estimated variances from designs compared with true variance (red line)
pp3 <- ggplot(data = var.df, aes(x = func, y = var)) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = var.response, color = "red", linewidth = 1.5) +
  geom_point(col = "gray") +
  labs(title, x = "", y = "var") +
  theme_bw() +
  my_theme() +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
pp3

# Combine and save the UQ comparison figure
pp <- pp1 + pp2 + pp3
pp

ggsave("./figure/case_uq.pdf", plot = pp, width = 12, height = 4.5, units = "in")

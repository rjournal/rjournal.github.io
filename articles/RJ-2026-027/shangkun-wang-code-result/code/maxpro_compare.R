# Reproduce results for section 3.1. Maxpro design Figure 4
# This script compares MaxPro and SFDesign algorithms for space-filling design.
# It generates MaxPro criterion plots for different dimensions and design sizes.

rm(list=ls())  # Clear workspace

# Load required libraries -----------------------------------------------------
library(MaxPro)
library(parallel)
library(doParallel)
library(SFDesign)
library(ggplot2)
library(patchwork)

# Plotting utilities ---------------------------------------------------------
# my_theme: custom ggplot theme for consistent appearance
my_theme = function(){
  theme(
    plot.title = element_text(size=14,hjust = 0.5,face="bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    legend.position = "inside", legend.position.inside =  c(0.3, 0.8),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
}

# my_plot: summarizes performance metrics across replicates and plots with quantile ribbon
# - method_list: list of matrices (replicates x sample-sizes) for each method
# - size_list: list of sample sizes for each method
# - factor_f: function to set factor levels for plotting
# - custom_shape_order, custom_color_order, custom_line_order: manual style controls
# - upper/lower: quantiles for uncertainty ribbon
# - log_y: use log scale for y-axis
my_plot = function (method_list, size_list, factor_f=NULL,
                    custom_shape_order=NULL, custom_color_order=NULL, custom_line_order=NULL,
                    upper=0.9, lower=0.1, title='', ylab='y', log_y=FALSE){
  # each method in method_list is n_rep x n_size matrix, where n_size is the number of design sizes we considered
  n_method = length(method_list)
  obj_summary = c()
  n_list = c()
  size_each_method = c()
  # Compute quantiles and medians for each method
  for (i in 1:n_method){
    obj_summary = rbind(obj_summary, cbind(apply(method_list[[i]],2,quantile,probs=lower,na.rm=TRUE),
                                           apply(method_list[[i]],2,median,na.rm=TRUE),
                                           apply(method_list[[i]],2,quantile,probs=upper,na.rm=TRUE)))
    n_list = c(n_list, size_list[[i]])
    size_each_method[i] = length(size_list[[i]])
  }
  # Prepare data for plotting
  data.plot = data.frame(obj_summary, n=n_list,
                         group=rep(names(method_list), times=size_each_method))
  if (!is.null(factor_f)){
    data.plot$group = factor_f(data.plot$group)
  }
  colnames(data.plot)[1:3] <- c('low', 'mu', 'high')
  # Build plot with median line and quantile ribbon
  pp = ggplot(data=data.plot, aes(x=n, y=mu, color=group, fill=group))+
    labs(y=ylab, title=title)+
    geom_line(aes(linetype=group))+
    geom_point(aes(shape=group), size=2)+
    scale_shape_manual(values=1:n_method)+
    geom_ribbon(aes(x=n, ymin=low, ymax=high), color=NA, alpha=0.2)+
    theme_bw()+my_theme()
  if(!is.null(custom_color_order)){
    pp = pp + scale_color_manual(values = custom_color_order) +
      scale_fill_manual(values = custom_color_order)
  }
  if (!is.null(custom_shape_order)){
    pp = pp + scale_shape_manual(values = custom_shape_order)
  }
  if (!is.null(custom_line_order)){
    pp = pp + scale_linetype_manual(values=custom_line_order)
  }
  if (log_y){
    pp = pp + scale_y_log10()
  }
  return (pp)
}

# Compare utility -------------------------------------------------------------
# compare: runs several design algorithms and computes MaxPro criterion for each
# - n_rep: number of replicates
# - nlist: list of design sizes
# - p: dimension
# - cores: number of parallel workers
# Returns a list of results for each method and replicate
compare = function(n_rep=10, nlist, p,
                   cores=min(n_rep, 5)){
  set.seed(1)
  registerDoParallel(cores)
  result = foreach(iter = 1:n_rep,
                   .errorhandling = 'pass', .options.RNG=1) %dopar% {
     MaxPro_LHD_result <- MaxPro_result <- SFD_LHD_result <- SFD_result <- c()
     for (n in nlist){
       # MaxProLHD
       D_MaxPro_LHD = MaxPro::MaxProLHD(n, p)$Design
       # MaxPro
       D_MaxPro = MaxPro::MaxPro(D_MaxPro_LHD)$Design
       # Det_LHD
       D_SFD_LHD = SFDesign::maxproLHD(n, p)$design
       # SFD
       D_SFD = SFDesign::maxpro.optim(D_SFD_LHD)$design
       # Compute MaxPro criterion for each design
       MaxPro_LHD_result = c(MaxPro_LHD_result,  SFDesign::maxpro.crit(D_MaxPro_LHD))
       MaxPro_result = c(MaxPro_result, SFDesign::maxpro.crit(D_MaxPro))
       SFD_LHD_result = c(SFD_LHD_result, SFDesign::maxpro.crit(D_SFD_LHD))
       SFD_result = c(SFD_result, SFDesign::maxpro.crit(D_SFD))
     }
     list(nlist=nlist, MaxPro.LHD=MaxPro_LHD_result, MaxPro=MaxPro_result,
          SFD.LHD=SFD_LHD_result, SFD=SFD_result)
   }
  stopImplicitCluster()
  return (result)
}

# --- Figure 3: MaxPro criterion trajectory for a single run ------------------
# Plot the criterion history for a single MaxProLHD run
n = 50
p = 5
result = maxproLHD(n, p, method='full')
pp = ggplot(data=data.frame(n=1:length(result$crit.hist), criteria=result$crit.hist)) +
  geom_line(aes(x=n, y=criteria)) +
  theme_bw()+my_theme()
pp
ggsave("./figure/MaxPro_traj.pdf", plot = pp, width = 8, height = 4, units = "in")

# --- Figure 4: MaxPro criterion comparison for p = 2, 5, 10 ------------------
# For each dimension, load precomputed results and plot MaxPro criterion for all methods
# p = 2 ----------------------------------------------------------------------
p = 2
nlist = seq(5, 85, by=16)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maxpro_result/p=', p, '.Rdata'))

load(paste0('../result/maxpro_result/p=', p, '.Rdata'))
result_list = c('MaxPro.LHD', 'SFD.LHD',  'MaxPro', 'SFD')
factor_f = function(column){
  # Ensure consistent factor levels for plotting
  return (factor(column, levels=result_list))
}
custom_shape_order = c(2, 1, 2, 1)
custom_line_order = c('solid', 'solid', 'dashed', 'dashed')
custom_color_order = c("#00BFC4", "#F8766D", "#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
# Aggregate results for each method across replicates
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  compare_result[[method]] = c()
  for (idx in 1:n_rep){
    compare_result[[method]] = rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 4)
# Plot MaxPro criterion for p = 2
pp1 = my_plot(compare_result, size_list, factor_f,
              custom_shape_order, custom_color_order, custom_line_order,
              title='p = 2', ylab='MaxPro criterion', upper=0.9, lower=0.1)
pp1

# p = 5 ----------------------------------------------------------------------
p = 5
nlist = seq(10, 100, by=18)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maxpro_result/p=', p, '.Rdata'))

load(paste0('../result/maxpro_result/p=', p, '.Rdata'))
result_list = c('MaxPro.LHD', 'SFD.LHD',  'MaxPro', 'SFD')
factor_f = function(column){
  return (factor(column, levels=result_list))
}
custom_shape_order = c(2, 1, 2, 1)
custom_line_order = c('solid', 'solid', 'dashed', 'dashed')
custom_color_order = c("#00BFC4", "#F8766D", "#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  compare_result[[method]] = c()
  for (idx in 1:n_rep){
    compare_result[[method]] = rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 4)
pp2 = my_plot(compare_result, size_list, factor_f,
              custom_shape_order, custom_color_order, custom_line_order,
              title='p = 5', ylab='MaxPro criterion', upper=0.9, lower=0.1)
pp2

# p = 10 ---------------------------------------------------------------------
p = 10
nlist = seq(30, 150, by=24)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maxpro_result/p=', p, '.Rdata'))

load(paste0('../result/maxpro_result/p=', p, '.Rdata'))
result_list = c('MaxPro.LHD', 'SFD.LHD',  'MaxPro', 'SFD')
factor_f = function(column){
  return (factor(column, levels=result_list))
}
custom_shape_order = c(2, 1, 2, 1)
custom_line_order = c('solid', 'solid', 'dashed', 'dashed')
custom_color_order = c("#00BFC4", "#F8766D", "#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  compare_result[[method]] = c()
  for (idx in 1:n_rep){
    compare_result[[method]] = rbind(compare_result[[method]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 4)
pp3 = my_plot(compare_result, size_list, factor_f,
              custom_shape_order, custom_color_order, custom_line_order,
              title='p = 10', ylab='MaxPro criterion', upper=0.9, lower=0.1)
pp3

# Combine and save all plots -------------------------------------------------
pp = pp1 + pp2 + pp3
print(pp)
ggsave("./figure/MaxPro_compare.pdf", plot = pp, width = 12, height = 4, units = "in")

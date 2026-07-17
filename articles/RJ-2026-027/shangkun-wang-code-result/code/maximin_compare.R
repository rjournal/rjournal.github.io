# Reproduce results for section 3.1. Maximin design Figure 2
# This script compares several maximin design algorithms and visualizes their performance.
# It generates reciprocal distance plots for different dimensions and design sizes.

rm(list=ls())  # Clear workspace

# Load required libraries -----------------------------------------------------
library(parallel)
library(doParallel)
library(SFDesign)
library(SLHD)
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
    legend.position = "inside", legend.position.inside =  c(0.2, 0.8),
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
# compare: runs several design algorithms and computes reciprocal distance for each
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
       SLHD_rec_result <- SFD.LHD_rec_result <- SFD_rec_result <- maximin_rec_result <- c()
       for (n in nlist){
         # SLHD
         D_SLHD = SLHD::maximinSLHD(1, n, p)$StandDesign
         # Det_LHD
         D_SFD_LHD = SFDesign::maximinLHD(n, p)$design
         # SFD
         D_SFD = SFDesign::maximin.optim(D_SFD_LHD)$design
         # maximin
         D_maximin = maximin::maximin(n-1, p, T=10*n, 
                                      Xorig=t(runif(p)))$Xf
         # Compute reciprocal distance for each design
         SLHD_rec_result = c(SLHD_rec_result,  SFDesign::maximin.crit(D_SLHD, surrogate=TRUE))
         SFD.LHD_rec_result = c(SFD.LHD_rec_result, SFDesign::maximin.crit(D_SFD_LHD, surrogate=TRUE))
         SFD_rec_result = c(SFD_rec_result, SFDesign::maximin.crit(D_SFD, surrogate=TRUE))
         maximin_rec_result = c(maximin_rec_result, SFDesign::maximin.crit(D_maximin, surrogate=TRUE))
       }
       list(nlist=nlist, SLHD_rec=SLHD_rec_result, SFD.LHD_rec=SFD.LHD_rec_result,
            SFD_rec=SFD_rec_result, maximin_rec=maximin_rec_result)
     }
  stopImplicitCluster()
  return (result)
}

# --- Figure 2: Reciprocal distance comparison for p = 2, 5, 10 ---------------
# For each dimension, load precomputed results and plot reciprocal distance for all methods
# p = 2 ----------------------------------------------------------------------
p = 2
nlist = seq(5, 85, by=16)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maximin_result/p=', p, '.Rdata'))

load(paste0('../result/maximin_result/p=', p, '.Rdata'))
result_list = c('SLHD_rec', 'SFD.LHD_rec')
result_list_colname = c('SLHD', 'SFD.LHD')
factor_f = function(column){
  # Ensure consistent factor levels for plotting
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(2, 1)
custom_line_order = c('solid', 'solid')
custom_color_order = c("#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
# Aggregate results for each method across replicates
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 2)
# Plot reciprocal distance for SLHD and SFD.LHD
pp11 = my_plot(compare_result, size_list, factor_f,
              custom_shape_order, custom_color_order, custom_line_order,
              title='p = 2', ylab='reciprocal distance', upper=0.9, lower=0.1) + ylim(1, 5)
pp11

# Compare maximin and SFD designs for p = 2 ----------------------------------
result_list = c('maximin_rec', 'SFD_rec')
result_list_colname = c('maximin', 'SFD')
factor_f = function(column){
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(3, 1)
custom_line_order = c('dashed', 'dashed')
custom_color_order = c("#C77CFF", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 5)
pp12 = my_plot(compare_result, size_list, factor_f,
              custom_shape_order, custom_color_order, custom_line_order,
              title='p = 2', ylab='reciprocal distance', upper=0.9, lower=0.1) + ylim(1, 5)
pp12
pp11 + pp12

# p = 5 ----------------------------------------------------------------------
p = 5
nlist = seq(10, 100, by=18)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maximin_result/p=', p, '.Rdata'))

load(paste0('../result/maximin_result/p=', p, '.Rdata'))
result_list = c('SLHD_rec', 'SFD.LHD_rec')
result_list_colname = c('SLHD', 'SFD.LHD')
factor_f = function(column){
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(2, 1)
custom_line_order = c('solid', 'solid')
custom_color_order = c("#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 2)
pp21 = my_plot(compare_result, size_list, factor_f,
               custom_shape_order, custom_color_order, custom_line_order,
               title='p = 5', ylab='reciprocal distance', upper=0.9, lower=0.1) + ylim(0.5, 2)
pp21

result_list = c('maximin_rec', 'SFD_rec')
result_list_colname = c('maximin', 'SFD')
factor_f = function(column){
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(3, 1)
custom_line_order = c('dashed', 'dashed')
custom_color_order = c("#C77CFF", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 5)
pp22 = my_plot(compare_result, size_list, factor_f,
               custom_shape_order, custom_color_order, custom_line_order,
               title='p = 5', ylab='reciprocal distance', upper=0.9, lower=0.1) + ylim(0.5, 2)
pp22

# p = 10 ---------------------------------------------------------------------
p = 10
nlist = seq(30, 150, by=24)
# NOTE: The full simulation loop is computationally expensive. Pre-computed
# results are loaded below. To re-run the experiments from scratch, uncomment
# the block between lines indicated in the original script.

# result = compare(10, nlist, p)
# save(result, file=paste0('../result/maximin_result/p=', p, '.Rdata'))

load(paste0('../result/maximin_result/p=', p, '.Rdata'))
result_list = c('SLHD_rec', 'SFD.LHD_rec')
result_list_colname = c('SLHD', 'SFD.LHD')
factor_f = function(column){
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(2, 1)
custom_line_order = c('solid', 'solid')
custom_color_order = c("#00BFC4", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 2)
pp31 = my_plot(compare_result, size_list, factor_f,
               custom_shape_order, custom_color_order, custom_line_order,
               title='p = 10', ylab='reciprocal distance', upper=0.9, lower=0.1) +
  theme(legend.position.inside =  c(0.8, 0.55)) + ylim(0.5, 1)
pp31

result_list = c('maximin_rec', 'SFD_rec')
result_list_colname = c('maximin', 'SFD')
factor_f = function(column){
  return (factor(column, levels=result_list_colname))
}
custom_shape_order = c(3, 1)
custom_line_order = c('dashed', 'dashed')
custom_color_order = c("#C77CFF", "#F8766D")
compare_result = list()
n_rep = 10
for (method_idx in 1:length(result_list)){
  method = result_list[method_idx]
  method_colname = result_list_colname[method_idx]
  compare_result[[method_colname]] = c()
  for (idx in 1:n_rep){
    compare_result[[method_colname]] = rbind(compare_result[[method_colname]], result[[idx]][[method]])
  }
}
size_list = rep(list(result[[1]]$nlist), 5)
pp32 = my_plot(compare_result, size_list, factor_f,
               custom_shape_order, custom_color_order, custom_line_order,
               title='p = 10', ylab='reciprocal distance', upper=0.9, lower=0.1) + ylim(0.5, 1)
pp32

# Arrange all plots in a grid layout -----------------------------------------
layout = "
ABC
DEF
"
pp = pp11 + pp21 + pp31 + pp12 + pp22 + pp32 +
  plot_layout(design = layout)
print(pp)
ggsave("./figure/Maximin_compare.pdf", plot = pp, width = 12, height = 8, units = "in")

##################################################################
##                    load required packages                    ##
##################################################################
library(dplyr)
library(RGMMBench)
num_cores <- parallel::detectCores() - 1 # set it to 1 if parallel implementation is unwanted


# prepare the overall setting
relevant_mixture_functions <- list(
  "RGMMBench" = list(name_fonction = emnmix_univariate, list_params = list()),
  "Rmixmod" = list(name_fonction = em_Rmixmod_univariate, list_params = list()),
  "mixtools" = list(name_fonction = em_mixtools_univariate, list_params = list()),
  "bgmm" = list(name_fonction = em_bgmm_univariate, list_params = list()),
  "mclust" = list(name_fonction = em_mclust_univariate, list_params = list(prior = NULL)),
  "EMCluster" = list(name_fonction = em_EMCluster_univariate, list_params = list()),
  "GMKMcharlie" = list(name_fonction = em_GMKMcharlie_univariate, list_params = list()),
  "flexmix" = list(name_fonction = em_flexmix_univariate, list_params = list())
)

sigma_values <- list("null OVL" = rep(0.3, 4), "average OVL" = rep(1, 4), "high OVL" = rep(2, 4))
mean_values <- list(c(0, 4, 8, 12))
proportions <- list("balanced" = rep(1 / 4, 4), "small unbalance" = c(0.2, 0.4, 0.2, 0.2), "highly unbalanced" = c(0.1, 0.7, 0.1, 0.1))
univariate_initialisation_algorithms <- c("kmeans", "quantiles", "random", "hc", "rebmix", "small em")


#################################################################
##   benchmark the performance of the benchmarked packages,    ##
##                   in the univariate setting                 ##
#################################################################
RNGkind("L'Ecuyer-CMRG")
set.seed(20)

univariate_distribution_parameters <- benchmark_univariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions,
  initialisation_algorithms = univariate_initialisation_algorithms,
  sigma_values = sigma_values, mean_values = mean_values, proportions = proportions,
  prop_outliers = c(0), cores = num_cores,
  Nbootstrap = 200, nobservations = 200
)

saveRDS(
  univariate_distribution_parameters$distributions,
  file.path("tables", "univariate", "univariate_distributions.rds")
)

saveRDS(
  univariate_distribution_parameters$local_scores,
  file.path("tables", "univariate", "univariate_local_scores.rds")
)


###################################################################
##  estimate the initialisation and EM packages running times,   ##
##                   in the univariate setting                   ##
###################################################################

RNGkind("L'Ecuyer-CMRG")
set.seed(20)

univariate_time_computations <- compute_microbenchmark_univariate(
  mixture_functions = relevant_mixture_functions,
  initialisation_algorithms = univariate_initialisation_algorithms,
  sigma_values = sigma_values, mean_values = mean_values, proportions = proportions,
  prop_outliers = c(0), cores = num_cores,
  Nbootstrap = 200, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)

saveRDS(
  univariate_time_computations$time_data,
  file.path("tables", "univariate", "univariate_time_computation.rds")
)

saveRDS(
  univariate_time_computations$init_time_data,
  file.path("tables", "univariate", "univariate_initialisation_time_computation.rds")
)

#################################################################
##   benchmark the performance of the benchmarked packages,    ##
##           in the univariate setting with outliers           ##
#################################################################
outliers_mixture_functions <- c(list("otrimle" = list(name_fonction = em_otrimle, list_params = list())), relevant_mixture_functions)

RNGkind("L'Ecuyer-CMRG")
set.seed(20)

outliers_distribution_parameters <- benchmark_univariate_GMM_estimation(
  mixture_functions = outliers_mixture_functions,
  initialisation_algorithms = c("kmeans", "quantiles", "random", "hc", "rebmix"),
  initialisation_algorithms = univariate_initialisation_algorithms,
  sigma_values = c(0.3, 0.3), mean_values = c(0, 2), proportions = c(0.5, 0.5),
  prop_outliers = c(0.02, 0.04), cores = num_cores,
  Nbootstrap = 200, nobservations = 2000
)

saveRDS(
  outliers_distribution_parameters$distributions,
  file.path("tables", "univariate", "univariate_outliers_distribution_parameters.rds")
)

###################################################################
##  estimate the initialisation and EM packages running times,   ##
##             in the univariate setting with outliers           ##
###################################################################

RNGkind("L'Ecuyer-CMRG")
set.seed(20)
outliers_time_computations <- compute_microbenchmark_univariate(
  mixture_functions = outliers_mixture_functions,
  initialisation_algorithms = c("kmeans", "quantiles", "random", "hc", "rebmix"),
  initialisation_algorithms = univariate_initialisation_algorithms,
  sigma_values = c(0.3, 0.3), mean_values = c(0, 2), proportions = c(0.5, 0.5),
  prop_outliers = c(0.02, 0.04), cores = num_cores,
  Nbootstrap = 200, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)

saveRDS(
  outliers_time_computations$time_data,
  file.path("tables", "univariate", "univariate_outliers_time_computations.rds")
)

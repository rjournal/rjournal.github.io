##################################################################
##                    load required packages                    ##
##################################################################
library(dplyr)
library(RGMMBench)
num_cores <- parallel::detectCores() - 1 # set it to 1 if parallel implementation is unwanted


# prepare the overall setting
relevant_mixture_functions <- list(
  "RGMMBench" = list(name_fonction = emnmix_bivariate, list_params = list()),
  "Rmixmod" = list(name_fonction = em_Rmixmod_bivariate, list_params = list()),
  "mixtools" = list(name_fonction = em_mixtools_bivariate, list_params = list()),
  "bgmm" = list(name_fonction = em_bgmm_bivariate, list_params = list()),
  "mclust" = list(name_fonction = em_mclust_bivariate, list_params = list(prior = NULL)),
  "EMCluster" = list(name_fonction = em_EMCluster_bivariate, list_params = list()),
  "GMKMcharlie" = list(name_fonction = em_GMKMcharlie_bivariate, list_params = list()),
  "flexmix" = list(name_fonction = em_flexmix_bivariate, list_params = list())
)

sigma_values <- list()
for (corr_1 in c(-0.8, 0.8)) {
  for (corr_2 in c(-0.8, 0.8)) {
    sigma_values[[glue::glue("comp_1_corr_{corr_1}_comp_2_{corr_2}")]] <-
      array(c(1, corr_1, corr_1, 1, 1, corr_2, corr_2, 1), dim = c(2, 2, 2))
  }
}
sigma_values[["well-separated"]] <- array(rep(c(1, 0, 0, 1), 2), dim = c(2, 2, 2))
mean_values <- list(
  "high OVL" = matrix(c(0, 2, 2, 0), nrow = 2, ncol = 2),
  "small OVL" = matrix(c(0, 20, 20, 0), nrow = 2, ncol = 2)
)
proportions <- list("balanced" = c(0.5, 0.5), "highly unbalanced" = c(0.9, 0.1))



#################################################################
##   benchmark the performance of the benchmarked packages,    ##
##                   in the bivariate setting               ##
#################################################################
RNGkind("L'Ecuyer-CMRG")
set.seed(20)

bivariate_distribution_parameters <- benchmark_bivariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions,
  initialisation_algorithms = bivariate_initialisation_algorithms,
  sigma_values = sigma_values, mean_values = mean_values, proportions = proportions,
  prop_outliers = c(0), cores = num_cores,
  Nbootstrap = 200, nobservations = 500
)

saveRDS(
  bivariate_distribution_parameters$distributions,
  file.path("tables", "bivariate", "bivariate_distributions.rds")
)

saveRDS(
  bivariate_distribution_parameters$local_scores,
  file.path("tables", "bivariate", "bivariate_local_scores.rds")
)


###################################################################
##  estimate the initialisation and EM packages running times,   ##
##                   in the bivariate setting                 ##
###################################################################

RNGkind("L'Ecuyer-CMRG")
set.seed(20)

bivariate_time_computations <- compute_microbenchmark_multivariate(
  mixture_functions = relevant_mixture_functions,
  initialisation_algorithms = "kmeans",
  sigma_values = sigma_values, mean_values = mean_values, proportions = proportions,
  prop_outliers = c(0), cores = num_cores,
  Nbootstrap = 200, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)

saveRDS(
  bivariate_time_computations$time_data,
  file.path("tables", "bivariate", "bivariate_time_computation.rds")
)

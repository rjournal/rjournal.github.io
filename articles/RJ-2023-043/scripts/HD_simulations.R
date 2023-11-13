##################################################################
##                    load required packages                    ##
##################################################################

library(RGMMBench)
library(dplyr)
relevant_mixture_functions <- list(
  "EMMIXmfa" = list(name_fonction = em_EMMIXmfa_multivariate, list_params = list()),
  "HDclassif" = list(name_fonction = em_HDclassif_multivariate, list_params = list()),
  "em R" = list(name_fonction = emnmix_multivariate, list_params = list()),
  "Rmixmod" = list(name_fonction = RGMMBench::em_Rmixmod_multivariate, list_params = list()),
  "mixtools" = list(name_fonction = em_mixtools_multivariate, list_params = list()),
  "bgmm" = list(name_fonction = em_bgmm_multivariate, list_params = list()),
  "mclust" = list(name_fonction = em_mclust_multivariate, list_params = list(prior = NULL)),
  "EMCluster" = list(name_fonction = em_EMCluster_multivariate, list_params = list()),
  "GMKMcharlie" = list(name_fonction = em_GMKMcharlie_multivariate, list_params = list()),
  "flexmix" = list(name_fonction = em_flexmix_multivariate, list_params = list())
)


###########################################################################
###########################################################################
###                                                                     ###
###                BENCHMARK PERFORMANCE IN A HD SETTING                ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                         low overlap                         ##
#################################################################

RNGkind("L'Ecuyer-CMRG")
set.seed(20)

###  low OVL, circular
theta_low_OVL_balanced_circular <- MixSim::MixSim(
  BarOmega = 10^-4,
  K = 2, p = 10, sph = TRUE, hom = FALSE,
  ecc = 0.9, PiLow = 1, int = c(0.0, 2.0)
)
theta_low_OVL_balanced_circular_formatted <- list(
  p = theta_low_OVL_balanced_circular$Pi,
  mu = t(theta_low_OVL_balanced_circular$Mu),
  sigma = theta_low_OVL_balanced_circular$S
)
HD_low_OVL_balanced_circular_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  id_scenario = 1,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_balanced_circular_formatted$sigma),
  mean_values = list(theta_low_OVL_balanced_circular_formatted$mu),
  proportions = list(theta_low_OVL_balanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_low_OVL_balanced_circular_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 1,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_balanced_circular_formatted$sigma),
  mean_values = list(theta_low_OVL_balanced_circular_formatted$mu),
  proportions = list(theta_low_OVL_balanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


theta_low_OVL_unbalanced_circular <- MixSim::MixSim(
  BarOmega = 10^-4,
  K = 2, p = 10, sph = TRUE, hom = FALSE,
  ecc = 0.90, PiLow = 0.1, int = c(0.0, 2.0)
)
theta_low_OVL_unbalanced_circular_formatted <- list(
  p = theta_low_OVL_unbalanced_circular$Pi,
  mu = t(theta_low_OVL_unbalanced_circular$Mu),
  sigma = theta_low_OVL_unbalanced_circular$S
)
HD_low_OVL_unbalanced_circular_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  id_scenario = 2,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_unbalanced_circular_formatted$sigma),
  mean_values = list(theta_low_OVL_unbalanced_circular_formatted$mu),
  proportions = list(theta_low_OVL_unbalanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_low_OVL_unbalanced_circular_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 2,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_unbalanced_circular_formatted$sigma),
  mean_values = list(theta_low_OVL_unbalanced_circular_formatted$mu),
  proportions = list(theta_low_OVL_unbalanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


###  low OVL, full covariance
theta_low_OVL_balanced_eccentric <- MixSim::MixSim(
  BarOmega = 10^-4,
  K = 2, p = 10, sph = FALSE, hom = FALSE,
  ecc = 0.90, PiLow = 1.0, int = c(0.0, 2.0)
)
theta_low_OVL_balanced_eccentric_formatted <- list(
  p = theta_low_OVL_balanced_eccentric$Pi,
  mu = t(theta_low_OVL_balanced_eccentric$Mu),
  sigma = theta_low_OVL_balanced_eccentric$S
)
HD_low_OVL_balanced_eccentric_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  mixture_functions = relevant_mixture_functions, id_scenario = 3,
  sigma_values = list(theta_low_OVL_balanced_eccentric_formatted$sigma),
  mean_values = list(theta_low_OVL_balanced_eccentric_formatted$mu),
  proportions = list(theta_low_OVL_balanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_low_OVL_balanced_eccentric_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 3,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_balanced_eccentric_formatted$sigma),
  mean_values = list(theta_low_OVL_balanced_eccentric_formatted$mu),
  proportions = list(theta_low_OVL_balanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)



theta_low_OVL_unbalanced_eccentric <- MixSim::MixSim(
  BarOmega = 10^-4,
  K = 2, p = 10, sph = FALSE, hom = FALSE,
  ecc = 0.90, PiLow = 0.1, int = c(0.0, 2.0)
)
theta_low_OVL_unbalanced_eccentric_formatted <- list(
  p = theta_low_OVL_unbalanced_eccentric$Pi,
  mu = t(theta_low_OVL_unbalanced_eccentric$Mu),
  sigma = theta_low_OVL_unbalanced_eccentric$S
)
HD_low_OVL_unbalanced_eccentric_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  id_scenario = 4, initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_unbalanced_eccentric_formatted$sigma),
  mean_values = list(theta_low_OVL_unbalanced_eccentric_formatted$mu),
  proportions = list(theta_low_OVL_unbalanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_low_OVL_unbalanced_eccentric_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 4,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_low_OVL_unbalanced_eccentric_formatted$sigma),
  mean_values = list(theta_low_OVL_unbalanced_eccentric_formatted$mu),
  proportions = list(theta_low_OVL_unbalanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)



##################################################################
##                         high overlap                         ##
##################################################################

###  high OVL, circular
theta_high_OVL_balanced_circular <- MixSim::MixSim(
  BarOmega = 0.2,
  K = 2, p = 10, sph = TRUE, hom = FALSE,
  ecc = 0.9, PiLow = 1, int = c(0.0, 2.0)
)
theta_high_OVL_balanced_circular_formatted <- list(
  p = theta_high_OVL_balanced_circular$Pi,
  mu = t(theta_high_OVL_balanced_circular$Mu),
  sigma = theta_high_OVL_balanced_circular$S
)
HD_high_OVL_balanced_circular_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions, id_scenario = 5,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  sigma_values = list(theta_high_OVL_balanced_circular_formatted$sigma),
  mean_values = list(theta_high_OVL_balanced_circular_formatted$mu),
  proportions = list(theta_high_OVL_balanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_high_OVL_balanced_circular_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 5,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_high_OVL_balanced_circular_formatted$sigma),
  mean_values = list(theta_high_OVL_balanced_circular_formatted$mu),
  proportions = list(theta_high_OVL_balanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


theta_high_OVL_unbalanced_circular <- MixSim::MixSim(
  BarOmega = 0.2,
  K = 2, p = 10, sph = TRUE, hom = FALSE,
  ecc = 0.90, PiLow = 0.1, int = c(0.0, 2.0)
)
theta_high_OVL_unbalanced_circular_formatted <- list(
  p = theta_high_OVL_unbalanced_circular$Pi,
  mu = t(theta_high_OVL_unbalanced_circular$Mu),
  sigma = theta_high_OVL_unbalanced_circular$S
)
HD_high_OVL_unbalanced_circular_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions, id_scenario = 6,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  sigma_values = list(theta_high_OVL_unbalanced_circular_formatted$sigma),
  mean_values = list(theta_high_OVL_unbalanced_circular_formatted$mu),
  proportions = list(theta_high_OVL_unbalanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_high_OVL_unbalanced_circular_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 6,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_high_OVL_unbalanced_circular_formatted$sigma),
  mean_values = list(theta_high_OVL_unbalanced_circular_formatted$mu),
  proportions = list(theta_high_OVL_unbalanced_circular_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


###  high OVL, full covariance
theta_high_OVL_balanced_eccentric <- MixSim::MixSim(
  BarOmega = 0.2,
  K = 2, p = 10, sph = FALSE, hom = FALSE,
  ecc = 0.90, PiLow = 1, int = c(0.0, 2.0)
)
theta_high_OVL_balanced_eccentric_formatted <- list(
  p = theta_high_OVL_balanced_eccentric$Pi,
  mu = t(theta_high_OVL_balanced_eccentric$Mu),
  sigma = theta_high_OVL_balanced_eccentric$S
)
HD_high_OVL_balanced_eccentric_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions, id_scenario = 7,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  sigma_values = list(theta_high_OVL_balanced_eccentric_formatted$sigma),
  mean_values = list(theta_high_OVL_balanced_eccentric_formatted$mu),
  proportions = list(theta_high_OVL_balanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_high_OVL_balanced_eccentric_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 7,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_high_OVL_balanced_eccentric_formatted$sigma),
  mean_values = list(theta_high_OVL_balanced_eccentric_formatted$mu),
  proportions = list(theta_high_OVL_balanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


theta_high_OVL_unbalanced_eccentric <- MixSim::MixSim(
  BarOmega = 0.2,
  K = 2, p = 10, sph = FALSE, hom = FALSE,
  ecc = 0.90, PiLow = 0.1, int = c(0.0, 2.0)
)
theta_high_OVL_unbalanced_eccentric_formatted <- list(
  p = theta_high_OVL_unbalanced_eccentric$Pi,
  mu = t(theta_high_OVL_unbalanced_eccentric$Mu),
  sigma = theta_high_OVL_unbalanced_eccentric$S
)
HD_high_OVL_unbalanced_eccentric_distribution_parameters <- benchmark_multivariate_GMM_estimation(
  mixture_functions = relevant_mixture_functions, id_scenario = 8,
  initialisation_algorithms = c("hc", "kmeans", "rebmix"),
  sigma_values = list(theta_high_OVL_unbalanced_eccentric_formatted$sigma),
  mean_values = list(theta_high_OVL_unbalanced_eccentric_formatted$mu),
  proportions = list(theta_high_OVL_unbalanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(200, 2000)
)
HD_high_OVL_unbalanced_eccentric_time_computations <- compute_microbenchmark_multivariate(
  initialisation_algorithms = c("kmeans"), id_scenario = 8,
  mixture_functions = relevant_mixture_functions,
  sigma_values = list(theta_high_OVL_unbalanced_eccentric_formatted$sigma),
  mean_values = list(theta_high_OVL_unbalanced_eccentric_formatted$mu),
  proportions = list(theta_high_OVL_unbalanced_eccentric_formatted$p),
  Nbootstrap = 100, nobservations = c(100, 200, 500, 1000, 2000, 5000, 10000)
)


HD_distribution_parameters <- c(
  HD_low_OVL_balanced_circular_distribution_parameters, HD_low_OVL_unbalanced_circular_distribution_parameters,
  HD_low_OVL_balanced_eccentric_distribution_parameters, HD_low_OVL_unbalanced_eccentric_distribution_parameters,
  HD_high_OVL_balanced_circular_distribution_parameters, HD_high_OVL_unbalanced_circular_distribution_parameters,
  HD_high_OVL_balanced_eccentric_distribution_parameters, HD_high_OVL_unbalanced_eccentric_distribution_parameters
)
HD_time_computations <- c(
  HD_low_OVL_balanced_circular_time_computations, HD_low_OVL_unbalanced_circular_time_computations,
  HD_low_OVL_balanced_eccentric_time_computations, HD_low_OVL_unbalanced_eccentric_time_computations,
  HD_high_OVL_balanced_circular_time_computations, HD_high_OVL_unbalanced_circular_time_computations,
  HD_high_OVL_balanced_eccentric_time_computations, HD_high_OVL_unbalanced_eccentric_time_computations
)

saveRDS(
  purrr::map_dfr(HD_distribution_parameters, "distributions"),
  file.path("tables", "HD", "HD_distributions.rds")
)
saveRDS(
  purrr::map_dfr(HD_distribution_parameters, "local_scores"),
  file.path("tables", "HD", "HD_local_scores.rds")
)
saveRDS(
  purrr::map_dfr(HD_distribution_parameters, "config"),
  file.path("tables", "HD", "HD_configuration_scenario.rds")
)
saveRDS(
  purrr::map_dfr(HD_time_computations, "time_data"),
  file.path("tables", "HD", "HD_time_computation.rds")
)

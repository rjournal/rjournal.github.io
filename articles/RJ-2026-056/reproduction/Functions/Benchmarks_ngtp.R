

library(BayesianLasso)

# Directory of THIS script (works when sourced)
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))


# reproduction/ folder is script_dir (assuming Benchmarks.R lives in reproduction/)
results_dir <- file.path(script_dir, "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

helpers_dir <- file.path(script_dir, "helpers")






helper_files <- c(
  "benchmark_blasso_bayeslm.R",
  "benchmark_blasso_bayesreg.R",
  "benchmark_blasso_monomvn.R",
  "benchmark_blasso_rstan.R",
  "mcmc_stats.R"
)


helper_paths <- file.path(helpers_dir, helper_files)
missing <- helper_paths[!file.exists(helper_paths)]
if (length(missing) > 0) {
  stop("Missing helper file(s):\n", paste(missing, collapse = "\n"))
}

invisible(lapply(helper_paths, source, local = globalenv()))

# ===========================
# ===========================
## Effective sample size
# ===========================
# ===========================


effective_sample_size <- function(samples) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message(
      "Package 'posterior' is required to compute effective sample size (ESS).\n",
      "Install it with install.packages('posterior') to enable ESS diagnostics."
    )
    return(NA_real_)
  }
  
  as.numeric(posterior::ess_bulk(as.numeric(samples)))
}




# ===========================
# ===========================
#   Simulated Example
# ===========================
# ===========================

if(dataset_name == "simulated"){

  # Simulate data
  set.seed(123)
  Ns <- 2000
  ns <- 100
  ps <- 10
  X <- matrix(rnorm(ns * ps), nrow = ns)
  beta <- c(rep(2, 3), rep(0, ps - 3))
  y <- X %*% beta + rnorm(ns)
  
  
  vtime_val_Hans = c()
  results_Hans <- NULL 

  N_iter = 5
  # Run the modified Hans sampler
  for(i in 1:N_iter){
    time_val <- system.time({
      res_Hans <- Modified_Hans_Gibbs(
        X = X, y = y, beta_init= rep(1,10), a1=2, b1=1, u1=2, v1=1,
        nsamples=Ns, lambda_init=1, sigma2_init=1, verbose=100, tune_lambda2 = TRUE,
        rao_blackwellization = FALSE)
    })[3]
    
    vtime_val_Hans[i] <- time_val
    
    if (i == 1) {
      output_Hans <- list(
        mBeta = res_Hans$mBeta,
        vsigma2 = res_Hans$vsigma2,
        vlambda2 = res_Hans$vlambda2
      )  # raw draws from a single run
    }
    
    # Initialize accumulators after first run
    if (is.null(results_Hans)) {
      results_Hans <- list(
        mBeta = res_Hans$mBeta,
        vsigma2 = res_Hans$vsigma2,
        vlambda2 = res_Hans$vlambda2
      )
    } else {
      results_Hans$mBeta    <- results_Hans$mBeta + res_Hans$mBeta
      results_Hans$vsigma2  <- results_Hans$vsigma2 + res_Hans$vsigma2
      results_Hans$vlambda2 <- results_Hans$vlambda2 + res_Hans$vlambda2
    }
  }
  

  # Take averages
  mBeta = (results_Hans$mBeta)/N_iter
  vsigma2 = (results_Hans$vsigma2)/N_iter
  vlambda2 = (results_Hans$vlambda2)/N_iter
  time_val_Hans = mean(vtime_val_Hans)
  

  vESS <- c()
  for(j in 1:ps) {
    vESS[j] <- effective_sample_size(results_Hans$mBeta[200:Ns,j])
  }
  Ef_Hans = median(vESS)/time_val_Hans
  
  ESS_sigma2_Hans  = effective_sample_size(as.vector(results_Hans$vsigma2[200:Ns]))
  Ef_sigma2_Hans = ESS_sigma2_Hans/time_val_Hans
  
  ESS_lambda2_Hans  = effective_sample_size(as.vector(results_Hans$vlambda2[200:Ns]))
  Ef_lambda2_Hans = ESS_lambda2_Hans/time_val_Hans
  
  stat_vec_Hans = c(
    100*median(vESS)/Ns,
    Ef_Hans,
    100*ESS_sigma2_Hans/Ns,
    Ef_sigma2_Hans,
    100*ESS_lambda2_Hans/Ns,
    Ef_lambda2_Hans,
    time_val_Hans)

  
  name_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  names(stat_vec_Hans) = name_vals 
  
  file_hans <- file.path(results_dir, paste0(dataset_name, "_output_Hans.RData"))
  # --- save the objects ---
  save(stat_vec_Hans, output_Hans, file = file_hans)
  
  # ======================== PC sampler =======================================
  
  # Run the modified PC sampler
  
  vtime_val_PC = c()
  results_PC <- NULL 
  
  for(i in 1:N_iter){
    time_val <- system.time({
      res_PC <- Modified_PC_Gibbs(
        X = X, y = y, a1=2, b1=1, u1=2, v1=1,
        nsamples=Ns, lambda_init=1, sigma2_init=1, thin = 1, verbose=100)
    })[3]
  
  vtime_val_PC[i] <- time_val
  
  if (i == 1) {
    output_PC <- list(
      mBeta = res_PC$mBeta,
      vsigma2 = res_PC$vsigma2,
      vlambda2 = res_PC$vlambda2
    )  # raw draws from a single run
  }
  
  # Initialize accumulators after first run
  if (is.null(results_PC)) {
    results_PC <- list(
      mBeta = res_PC$mBeta,
      vsigma2 = res_PC$vsigma2,
      vlambda2 = res_PC$vlambda2
    )
  } else {
    results_PC$mBeta    <- results_PC$mBeta + res_PC$mBeta
    results_PC$vsigma2  <- results_PC$vsigma2 + res_PC$vsigma2
    results_PC$vlambda2 <- results_PC$vlambda2 + res_PC$vlambda2
  }
 }
  

    # Take averages
  mBeta = (results_PC$mBeta)/N_iter
  vsigma2 = (results_PC$vsigma2)/N_iter
  vlambda2 = (results_PC$vlambda2)/N_iter
  time_val_PC = mean(vtime_val_PC)
  

  
  vESS <- c()
  for(j in 1:ps) {
    vESS[j] <- effective_sample_size(results_PC$mBeta[200:Ns,j])
  }
  Ef_PC = median(vESS)/time_val_PC
  
  ESS_sigma2_PC  = effective_sample_size(as.vector(results_PC$vsigma2[200:Ns]))
  Ef_sigma2_PC = ESS_sigma2_PC/time_val_PC
  
  ESS_lambda2_PC  = effective_sample_size(as.vector(results_PC$vlambda2[200:Ns]))
  Ef_lambda2_PC = ESS_lambda2_PC/time_val_PC
  
  stat_vec_PC = c(
    100*median(vESS)/Ns,
    Ef_PC,
    100*ESS_sigma2_PC/Ns,
    Ef_sigma2_PC,
    100*ESS_lambda2_PC/Ns,
    Ef_lambda2_PC,
    time_val_PC)
  

  
  name_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  names(stat_vec_PC) = name_vals 
  
  
  file_pc   <- file.path(results_dir, paste0(dataset_name, "_output_PC.RData"))
  
  # --- save the objects ---
  save(stat_vec_PC, output_PC,   file = file_pc)
  
}




# ==========================
# ==========================
## Run samplers
# ==========================
# ==========================

if(dataset_name!="simulated"){
  set.seed(123)
  # ======================= Run PC ============================
  
  # Set prior hyperparameter constants
  a1 = 1.0E-2  # Prior shape for sigma2
  b1 = 1.0E-2  # Prior scale for sigma2
  u1 = 1.0E-2  # Prior shape for lambda2
  v1 = 1.0E-2  # Prior scale for lambda2
  
  # Initial values for lambda2 and sigma2
  lambda2_init  = 10
  lambda_init = sqrt(lambda2_init)
  sigma2_init = 1
  
  # Number of samples to run the MCMC
  nburn = 1000
  nsamples = 5000
  inds_use = (nburn + 1):nsamples
  N = length(inds_use)
  
  # To store elapsed time and results of the PC sampler
  vtime_val_PC = c()
  results_PC <- NULL  # to be initialized after the first run
  
  N_iter <- 5
  # Running the modified PC sampler 5 times and taking the average of the results across runs.
  for (i in 1:N_iter) {
    time_val <- system.time({
      res_PC <- Modified_PC_Gibbs(
        X = x, y = y, a1, b1, u1, v1,
        nsamples,
        lambda_init = lambda_init,
        sigma2_init = sigma2_init,
        thin = 1,
        verbose = 1000
      )
    })[3]
    
    vtime_val_PC[i] <- time_val
    
    # Initialize accumulators after first run
    if (is.null(results_PC)) {
      results_PC <- list(
        mBeta = res_PC$mBeta,
        vsigma2 = res_PC$vsigma2,
        vlambda2 = res_PC$vlambda2
      )
    } else {
      results_PC$mBeta    <- results_PC$mBeta + res_PC$mBeta
      results_PC$vsigma2  <- results_PC$vsigma2 + res_PC$vsigma2
      results_PC$vlambda2 <- results_PC$vlambda2 + res_PC$vlambda2
    }
  }
  

    # Take averages
  mBeta = (results_PC$mBeta)/N_iter
  vsigma2 = (results_PC$vsigma2)/N_iter
  vlambda2 = (results_PC$vlambda2)/N_iter
  time_val_PC = mean(vtime_val_PC)
  
  # Compute effective sample sizes  
  vESS <- c()
  # p = ncol(x)
  for(j in 1:p) {
    vESS[j] <- effective_sample_size(mBeta[inds_use,j])
  }
  Ef_PC = median(vESS)/time_val_PC
  
  ESS_sigma2_PC  = effective_sample_size(as.vector(vsigma2[inds_use]))
  Ef_sigma2_PC = ESS_sigma2_PC/time_val_PC
  
  ESS_lambda2_PC  = effective_sample_size(as.vector(vlambda2[inds_use]))
  Ef_lambda2_PC = ESS_lambda2_PC/time_val_PC
  
  stat_vec_PC = c(
    100*median(vESS)/N,
    Ef_PC,
    100*ESS_sigma2_PC/N,
    Ef_sigma2_PC,
    100*ESS_lambda2_PC/N,
    Ef_lambda2_PC,
    time_val_PC)
  
  name_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  names(stat_vec_PC) = name_vals 
  
  file_pc   <- file.path(results_dir, paste0(dataset_name, "_output_PC.RData"))
  
  # --- save the objects ---
  save(stat_vec_PC,   file = file_pc)
  
  
  # ======================= Run Hans ============================
  
  # To store elapsed time and results of the Hans sampler
  vtime_val_Hans = c()
  results_Hans <- NULL  # to be initialized after the first run
  
  # Running the modified Hans sampler 5 times and taking the average of the results across runs.
  for (i in 1:N_iter) {
    time_val <- system.time({
      res_Hans <- Modified_Hans_Gibbs(
        X = x, y = y, beta_init = as.vector(colMeans(mBeta)),
        a1, b1, u1, v1,
        nsamples,
        lambda_init = lambda_init,
        sigma2_init = sigma2_init,
        verbose = 1000, tune_lambda2 = TRUE,
        rao_blackwellization = FALSE
      )
    })[3]
    
    vtime_val_Hans[i] <- time_val
    
    # Initialize accumulators after first run
    if (is.null(results_Hans)) {
      results_Hans <- list(
        mBeta = res_Hans$mBeta,
        vsigma2 = res_Hans$vsigma2,
        vlambda2 = res_Hans$vlambda2
      )
    } else {
      results_Hans$mBeta    <- results_Hans$mBeta + res_Hans$mBeta
      results_Hans$vsigma2  <- results_Hans$vsigma2 + res_Hans$vsigma2
      results_Hans$vlambda2 <- results_Hans$vlambda2 + res_Hans$vlambda2
    }
  }
  

    # Take averages
  mBeta = (results_Hans$mBeta)/N_iter
  vsigma2 = (results_Hans$vsigma2)/N_iter
  vlambda2 = (results_Hans$vlambda2)/N_iter
  time_val_Hans = mean(vtime_val_Hans)
  
  # Compute effective sample sizes
  vESS <- c()
  for(j in 1:p) {
    vESS[j] <- effective_sample_size(mBeta[inds_use,j])
  }
  Ef_Hans = median(vESS)/time_val_Hans
  
  ESS_sigma2_Hans  = effective_sample_size(as.vector(vsigma2[inds_use]))
  Ef_sigma2_Hans = ESS_sigma2_Hans/time_val_Hans
  
  ESS_lambda2_Hans  = effective_sample_size(as.vector(vlambda2[inds_use]))
  Ef_lambda2_Hans = ESS_lambda2_Hans/time_val_Hans
  
  stat_vec_Hans = c(
    100*median(vESS)/N,
    Ef_Hans,
    100*ESS_sigma2_Hans/N,
    Ef_sigma2_Hans,
    100*ESS_lambda2_Hans/N,
    Ef_lambda2_Hans,
    time_val_Hans)
  
  name_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  names(stat_vec_Hans) = name_vals 
  
  file_hans <- file.path(results_dir, paste0(dataset_name, "_output_Hans.RData"))
  # --- save the objects ---
  save(stat_vec_Hans, file = file_hans)
  
  
  
  
  # Set prior hyperparameter constants
  a = 1.0E-2  # Prior shape for sigma2
  b = 1.0E-2  # Prior scale for sigma2
  u = 1.0E-2  # Prior shape for lambda2
  v = 1.0E-2  # Prior scale for lambda2
  
  trials <- 5
  
  # ======================= Run monomvn ============================
  
  if(dataset_name !="Kakadu2"){
    res_monomvn <- NULL
    
    if (requireNamespace("monomvn", quietly = TRUE)) {
      
      
      res_monomvn = benchmark_blasso_monomvn(y, x,
                                             lambda_init, sigma2_init, a, b, u, v, 
                                             nburn, nsamples, trials= trials, 
                                             beta_inds = NA) 
      
      stat_vec_monomvn <- colMeans(res_monomvn$mStat)
      
      file_monomvn <- file.path(results_dir, paste0(dataset_name, "_output_monomvn.RData"))
      # --- save the objects ---
      save(stat_vec_monomvn, file = file_monomvn)
      
    }else {
      stop("Skipping monomvn benchmark: package 'monomvn' is not installed.")
    }
  }
  
  
  # ======================= Run bayeslm ============================
  
  res_bayeslm <- NULL
  
  if (requireNamespace("bayeslm", quietly = TRUE)) {
    
    res_bayeslm = benchmark_blasso_bayeslm(y, x,
                                           lambda_init, sigma2_init, a, b, u, v, 
                                           nburn, nsamples, trials = trials, 
                                           beta_inds = NA) 
    
    stat_vec_bayeslm <- colMeans(res_bayeslm$mStat)
    
    file_bayeslm <- file.path(results_dir, paste0(dataset_name, "_output_bayeslm.RData"))
    # --- save the objects ---
    save(stat_vec_bayeslm, file = file_bayeslm)
    
  }else {
    stop("Skipping bayeslm benchmark: package 'bayeslm' is not installed.")
  }
  
  
  # ======================= Run rstan ============================
  
  res_rstan <- NULL
  
  if (requireNamespace("rstan", quietly = TRUE)) {
    
    res_rstan = benchmark_blasso_rstan(y, x,
                                       lambda_init, sigma2_init, a, b, u, v, 
                                       nburn, nsamples, trials = trials, 
                                       beta_inds = NA)
    
    stat_vec_rstan <- colMeans(res_rstan$mStat)
    
    file_rstan <- file.path(results_dir, paste0(dataset_name, "_output_rstan.RData"))
    # --- save the objects ---
    save(stat_vec_rstan, file = file_rstan)
    
  }else {
    stop("Skipping rstan benchmark: package 'rstan' is not installed.")
  }
  
  
  # ======================= Run bayesreg ============================
  
  
  res_bayesreg <- NULL
  
  if (requireNamespace("bayesreg", quietly = TRUE)) {
    
    res_bayesreg = benchmark_blasso_bayesreg(y, x,
                                             lambda_init, sigma2_init, a, b, u, v, 
                                             nburn, nsamples, trials = trials, 
                                             beta_inds = NA) 
    
    stat_vec_bayesreg <- colMeans(res_bayesreg$mStat)
    
    file_bayesreg <- file.path(results_dir, paste0(dataset_name, "_output_bayesreg.RData"))
    # --- save the objects ---
    save(stat_vec_bayesreg, file = file_bayesreg)
    
  }else {
    stop("Skipping bayesreg benchmark: package 'bayesreg' is not installed.")
  }
  
}



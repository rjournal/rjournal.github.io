
#' @export
benchmark_blasso_bayesreg <- function(
    vy,
    mX,
    lambda_init,
    sigma2_init,
    a, b, u, v,
    nburn, nsamples,
    trials,
    beta_inds=NA)
{
  if (!requireNamespace("bayesreg", quietly = TRUE)) {
    stop(
      "The 'bayesreg' package is required for this benchmark.\n",
      "Install it with install.packages('bayesreg').",
      call. = FALSE
    )
  }

  if (!is.numeric(nsamples) || !is.numeric(nburn) || length(nsamples) != 1L || length(nburn) != 1L) {
    stop("'nsamples' and 'nburn' must be numeric scalars.", call. = FALSE)
  }
  if (nsamples <= nburn) {
    stop("'nsamples' must be greater than 'nburn'.", call. = FALSE)
  }
  if (!is.numeric(trials) || length(trials) != 1L || trials < 1) {
    stop("'trials' must be a positive scalar.", call. = FALSE)
  }

  
  

  mStat = c()
#   for (i in seq_len(trials)) {
#   
#     # Recreate dat with fresh jitter each trial for p>n stability
#     if (ncol(mX) > 1000) {
#       mX_stable <- mX + matrix(rnorm(nrow(mX)*ncol(mX), 0, 1e-6),
#                                nrow(mX), ncol(mX))
#       dat <- data.frame(vy = vy, mX_stable)
#     } else {
#       dat <- data.frame(vy = vy, mX)
#     }
#     # Initial fit
# 
#     time_val = system.time({
#       res_mcmc <- bayesreg::bayesreg(
#         vy~ .,
#         data=dat,
#         model = "normal",
#         prior = "lasso",
#         n.samples = (nsamples-nburn),
#         burnin = nburn,
#         thin = 1,
#         t.dof = 25,
#         n.cores = 1
#       )
#     })[3]
#     # print(time_val)
# 
#     inds_use = 1:(nsamples - nburn)
# 
#     # Calculate summary statistics of efficiencies and mixing rates
#     stats = mcmc_stats(t(res_mcmc$beta), res_mcmc$sigma2, res_mcmc$tau2, time_val, inds_use)
#     # print(stats)
# 
#     mStat = rbind(mStat,stats)
#   }
# 
#   colname_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
#   colnames(mStat) <- colname_vals
# 
#   return(mcmc_diagnostics(t(res_mcmc$beta), res_mcmc$sigma2, res_mcmc$tau2, beta_inds, mStat, doplots = FALSE))
# }

  for (i in seq_len(trials)) {
    
    res_mcmc <- NULL
    jitter_sd <- if (ncol(mX) > nrow(mX)) 1e-4 else 0
    attempt   <- 0
    
    while (is.null(res_mcmc) && attempt < 6) {
      attempt <- attempt + 1
      
      if (jitter_sd > 0) {
        mX_use <- mX + matrix(rnorm(nrow(mX) * ncol(mX), 0, jitter_sd),
                              nrow(mX), ncol(mX))
      } else {
        mX_use <- mX
      }
      dat <- data.frame(vy = vy, mX_use)
      
      time_val <- system.time({
        res_mcmc <- tryCatch(
          bayesreg::bayesreg(
            vy ~ ., data = dat, model = "normal", prior = "lasso",
            n.samples = (nsamples - nburn), burnin = nburn,
            thin = 1, t.dof = 25, n.cores = 1
          ),
          error = function(e) {
            message("Trial ", i, " attempt ", attempt,
                    " failed (jitter_sd = ", jitter_sd, "): ", e$message)
            jitter_sd <<- max(jitter_sd * 10, 1e-4)  # escalate
            NULL
          }
        )
      })[3]
    }
    
    if (is.null(res_mcmc))
      stop("bayesreg failed after ", attempt, " attempts on trial ", i)
    
    inds_use <- 1:(nsamples - nburn)
    stats    <- mcmc_stats(t(res_mcmc$beta), res_mcmc$sigma2,
                           res_mcmc$tau2, time_val, inds_use)
    mStat    <- rbind(mStat, stats)
  }
  
  colnames(mStat) <- c("mix_beta", "eff_beta", "mix_sigma2",
                       "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  
  return(mcmc_diagnostics(t(res_mcmc$beta), res_mcmc$sigma2,
                          res_mcmc$tau2, beta_inds, mStat, doplots = FALSE))
}
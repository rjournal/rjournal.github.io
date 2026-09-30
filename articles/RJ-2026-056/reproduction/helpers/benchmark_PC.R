
benchmark_PC <- function(
    vy,
    mX,
    lambda_init,
    sigma2_init,
    a, b, u, v,
    nburn, nsamples,
    trials,
    beta_inds=NA)
{

  if (!is.numeric(nsamples) || !is.numeric(nburn) || length(nsamples) != 1L || length(nburn) != 1L) {
    stop("'nsamples' and 'nburn' must be numeric scalars.", call. = FALSE)
  }
  if (nsamples <= nburn) {
    stop("'nsamples' must be greater than 'nburn'.", call. = FALSE)
  }
  if (!is.numeric(trials) || length(trials) != 1L || trials < 1) {
    stop("'trials' must be a positive scalar.", call. = FALSE)
  }


  # Run the Gibbs sampler trials times
  mStat = NULL
  for (i in seq_len(trials)) {
    time_val = system.time({
      res_mcmc = Modified_PC_Gibbs(mX,
                                  vy,
                                  a,
                                  b,
                                  u,
                                  v,
                                  nsamples,
                                  lambda_init,
                                  sigma2_init,
                                  thin = 1,
                                  verbose=nsamples/5)
    })[3]
    # print(time_val)

    inds_use = (nburn + 1):nsamples

    # Calculate summary statistics of efficiencies and mixing rates
    stats = mcmc_stats(res_mcmc$mBeta, res_mcmc$vsigma2, res_mcmc$vlambda2, time_val, inds_use)
    # print(stats)

    mStat = rbind(mStat,stats)
  }
  
  colname_vals = c( "mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  
  #rownames(mStat) <- NA
  colnames(mStat) <- colname_vals

  return(mcmc_diagnostics(res_mcmc$mBeta[inds_use,], res_mcmc$vsigma2[inds_use], res_mcmc$vlambda2[inds_use], beta_inds, mStat))

}

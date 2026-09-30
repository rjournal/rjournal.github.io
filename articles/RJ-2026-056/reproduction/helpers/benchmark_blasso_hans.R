
#' @export
benchmark_blasso_hans <- function(
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
  p = ncol(mX)

  for (i in seq_len(trials)) {
    time_val = system.time({
      res_mcmc = Modified_Hans_Gibbs(mX, vy, beta_init = rep(0,p), a, b, u, v, nsamples,
                                                     lambda_init = lambda_init,
                                                     sigma2_init = sigma2_init,
                                                     verbose = nsamples/5,
                                                     tune_lambda2 = TRUE,
                                                     rao_blackwellization = FALSE)
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

  # Optional plotting
  if (isTRUE(plot)) {
    # Plot the acf for sigma2 and lambda2
    acf(res_mcmc$vsigma2[inds_use])
    acf(res_mcmc$vlambda2[inds_use])

    # Trace plots for sigma2 and lambda2
    plot(res_mcmc$vsigma2[inds_use], type="l")
    plot(res_mcmc$vlambda2[inds_use], type="l")
  }

  # Calculate the rhat values
  # Optional rhat (posterior)
  rhat_sigma2 <- NA_real_
  rhat_lambda2 <- NA_real_
  # if (isTRUE(compute_rhat)) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message("Skipping R-hat: package 'posterior' is not installed.")
  } else {
    rhat_sigma2  <- posterior::rhat(res_mcmc$vsigma2[inds_use])
    rhat_lambda2 <- posterior::rhat(res_mcmc$vlambda2[inds_use])
    }
  # }

  if (any(is.na(beta_inds))) {
    vbeta_hat = apply(res_mcmc$mBeta[inds_use,],2,mean)
    beta_inds = order(abs(vbeta_hat),decreasing = TRUE)[1:10]
  }

  ldens_beta <- list()
  for (J in 1:length(beta_inds)) {
    j = beta_inds[J]
    dens  = density(res_mcmc$mBeta[inds_use,j])
    ldens_beta[[J]] = dens
  }

  dens_sigma2  = density(res_mcmc$vsigma2[inds_use])
  dens_lambda2 = density(res_mcmc$vlambda2[inds_use])


  return(list(
    ldens_beta = ldens_beta,
    dens_sigma2 = dens_sigma2,
    dens_lambda2 = dens_lambda2,
    mStat = mStat,
    beta_inds = beta_inds,
    rhat_sigma2 = rhat_sigma2,
    rhat_lambda2 = rhat_lambda2
  ))
}

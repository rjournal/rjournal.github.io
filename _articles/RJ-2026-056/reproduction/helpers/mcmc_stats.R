
effective_sample_size <- function(samples) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message(
      "Package 'posterior' is required to compute effective sample size (ESS).\n",
      "Install it with install.packages('posterior') to enable ESS diagnostics."
    )
    return(NA_real_)
  }
  
  as.numeric(posterior::ess_basic(as.numeric(samples)))
}


#' Internal MCMC summary statistics
#'
#' @noRd
mcmc_stats = function(mBeta, vsigma2, vlambda2, time_val, inds_use)
{
  mBeta <- as.matrix(mBeta)
  N = length(inds_use)
  p <- ncol(mBeta)

  # vESS <- numeric(p)
  # for(j in 1:p) {
  #   vESS[j] <- effective_sample_size(mBeta[inds_use,j])
  # }
  # sample_cols <- sample(ncol(mBeta), min(250, ncol(mBeta)))
  # vESS <- apply(mBeta[inds_use, sample_cols], 2, effective_sample_size)
  vESS <- apply(mBeta[inds_use, ], 2, effective_sample_size)
  Ef = stats::median(vESS)/time_val
  ESS_sigma2  = effective_sample_size(as.vector(vsigma2[inds_use]))
  Ef_sigma2 = ESS_sigma2/time_val
  ESS_lambda2  = effective_sample_size(as.vector(vlambda2[inds_use]))
  Ef_lambda2 = ESS_lambda2/time_val

  stat_vec = c(
    100*stats::median(vESS)/N,
    Ef,
    100*ESS_sigma2/N,
    Ef_sigma2,
    100*ESS_lambda2/N,
    Ef_lambda2,
    time_val)

  name_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  names(stat_vec) = name_vals

  return(stat_vec)
}

################################################################################

#' Internal MCMC diagnostics
#'
#' @noRd
mcmc_diagnostics <- function(mBeta, vsigma2, vlambda2, beta_inds, mStat, doplots=TRUE)
{
  mBeta <- as.matrix(mBeta)

  if (doplots) {
    # Plot the acf for sigma2 and lambda2
    stats::acf(vsigma2)
    stats::acf(vlambda2)


    # Trace plots for sigma2 and lambda2
    graphics::plot(vsigma2, type = "l")
    graphics::plot(vlambda2, type = "l")
  }

  # Compute R-hat using posterior if available
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message(
      "Package 'posterior' is required to compute R-hat diagnostics.\n",
      "Install it with install.packages('posterior') to enable rhat()."
    )
    rhat_sigma2  <- NA_real_
    rhat_lambda2 <- NA_real_
  } else {
    # posterior::rhat expects draws with chain dimension.
    # If we only have a single chain vector, rhat is not defined.
    # So we try rhat() and fall back to NA with a message if it errors.
    rhat_sigma2 <- tryCatch(
      as.numeric(posterior::rhat(vsigma2)),
      error = function(e) {
        message("posterior::rhat() failed for sigma2 (likely only 1 chain). Returning NA.")
        NA_real_
      }
    )
    rhat_lambda2 <- tryCatch(
      as.numeric(posterior::rhat(vlambda2)),
      error = function(e) {
        message("posterior::rhat() failed for lambda2 (likely only 1 chain). Returning NA.")
        NA_real_
      }
    )
  }

  # Choose beta indices to report densities for
  if (any(is.na(beta_inds))) {
    vbeta_hat <- colMeans(mBeta)
    beta_inds <- order(abs(vbeta_hat), decreasing = TRUE)[1:min(10, ncol(mBeta))]
  } else {
    beta_inds <- beta_inds[beta_inds >= 1 & beta_inds <= ncol(mBeta)]
    if (length(beta_inds) == 0) {
      beta_inds <- order(abs(colMeans(mBeta)), decreasing = TRUE)[1:min(10, ncol(mBeta))]
    }
  }

  ldens_beta <- lapply(beta_inds, function(j) {
    stats::density(mBeta[, j])
  })





  dens_sigma2  = stats::density(vsigma2)
  dens_lambda2 = stats::density(vlambda2)

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



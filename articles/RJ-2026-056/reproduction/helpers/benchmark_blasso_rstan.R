


bayesian_lasso.stan =
  "data {
  int<lower=1> N;  // Number of observations
  int<lower=1> P;  // Number of predictors
  matrix[N, P] X;  // Design matrix
  vector[N] y;     // Response vector
}
parameters{
  vector[P] beta;      // Regression coefficients
  real<lower=0> sigma2;  // Variance (σ²)
  real<lower=0> lambda2; // Regularization parameter squared
}
model{
  // Likelihood (normal model)
  y ~ normal(X * beta, sqrt(sigma2));

  // Laplace prior via normal-exponential mixture
  beta ~ double_exponential(0, sqrt(sigma2)/sqrt(lambda2));

  sigma2 ~ inv_gamma(0.01, 0.01);
  lambda2 ~ gamma(0.01, 0.01);
}
generated quantities{
  vector[N] y_pred;
  for (n in 1:N) {
    y_pred[n] = normal_rng(dot_product(X[n], beta), sqrt(sigma2));
  }
}";
#' @export
benchmark_blasso_rstan <- function(
    vy,
    mX,
    lambda_init,
    sigma2_init,
    a, b, u, v,
    nburn, nsamples,
    trials,
    beta_inds=NA)
{

  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(
      "The 'rstan' package is required for this benchmark.\n",
      "Install it first (and make sure a C++ toolchain is available).",
      call. = FALSE
    )
  }

  mX <- as.matrix(mX)
  n <- nrow(mX)
  p <- ncol(mX)

  stan_data <- list(N = n, P = p, X = mX, y = as.vector(vy), lambda = 1)

  fit <- rstan::stan(model_code = bayesian_lasso.stan, data = stan_data, warmup = nburn, iter = (nburn+1), chains = 1)


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
  mStat = c()
  for (i in seq_len(trials)) {
    # Initial fit

    time_val = system.time({
      res_mcmc <-rstan::stan(model_code = bayesian_lasso.stan, data = stan_data, warmup = nburn, iter = nsamples, chains = 1)
    })[3]
    # print(time_val)

    res_mcmc = rstan::extract(res_mcmc)
    inds_use = 1:(nsamples - nburn)

    # Calculate summary statistics of efficiencies and mixing rates
    stats = mcmc_stats(res_mcmc$beta, res_mcmc$sigma2, res_mcmc$lambda2, time_val, inds_use)
    # print(stats)

    mStat = rbind(mStat,stats)
  }

  colname_vals = c("mix_beta", "eff_beta", "mix_sigma2", "eff_sigma2", "mix_lambda2", "eff_lambda2", "time")
  colnames(mStat) <- colname_vals

  if (isTRUE(plot)) {
    # Plot the acf for sigma2 and lambda2
    acf(res_mcmc$sigma2[inds_use])
    acf(res_mcmc$lambda2[inds_use])

    # Trace plots for sigma2 and lambda2
    plot(res_mcmc$sigma2[inds_use], type = "l")
    plot(res_mcmc$lambda2[inds_use], type = "l")
  }


  # Calculate the rhat values
  rhat_sigma2 <- NA_real_
  rhat_lambda2 <- NA_real_
  # if (isTRUE(compute_rhat)) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message("Skipping R-hat: package 'posterior' is not installed.")
  } else {
    rhat_sigma2  <- posterior::rhat(res_mcmc$sigma2[inds_use])
    rhat_lambda2 <- posterior::rhat(res_mcmc$lambda2[inds_use])
  }
  # }

  if (any(is.na(beta_inds))) {
    vbeta_hat = apply(res_mcmc$beta[inds_use,],2,mean)
    beta_inds = order(abs(vbeta_hat),decreasing = TRUE)[1:10]
  }

  ldens_beta <- list()
  for (J in 1:length(beta_inds)) {
    j = beta_inds[J]
    dens  = density(res_mcmc$beta[inds_use,j])
    ldens_beta[[J]] = dens
  }

  dens_sigma2  = density(res_mcmc$sigma2[inds_use])
  dens_lambda2 = density(res_mcmc$lambda2[inds_use])


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

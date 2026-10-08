#!/usr/bin/env Rscript

# Reproduce the four dapper workloads used in the paper and record indicative
# wall-clock timings and MCMC diagnostics.
required_packages <- c(
  "dapper",
  "fmcmc",
  "future",
  "MASS",
  "posterior",
  "tibble",
  "tidyr",
  "VGAM"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before benchmarking: ",
    paste(missing_packages, collapse = ", ")
  )
}

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

if (length(script_argument) == 1) {
  script_path <- normalizePath(
    sub("^--file=", "", script_argument),
    mustWork = TRUE
  )
} else {
  script_path <- normalizePath(
    file.path("benchmark", "run-benchmarks.R"),
    mustWork = TRUE
  )
}

benchmark_directory <- dirname(script_path)
results_path <- file.path(benchmark_directory, "benchmark-results.csv")
parallel_workers <- 4L

finite_max <- function(x) {
  if (any(is.finite(x))) max(x[is.finite(x)]) else NA_real_
}

cpu_description <- function() {
  description <- Sys.info()[["machine"]]

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    detected <- tryCatch(
      system2(
        "sysctl",
        c("-n", "machdep.cpu.brand_string"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character()
    )

    if (length(detected) > 0 && nzchar(detected[[1]])) {
      description <- detected[[1]]
    }
  }

  description
}

benchmark_metadata <- list(
  benchmark_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  operating_system = paste(
    Sys.info()[["sysname"]],
    Sys.info()[["release"]],
    Sys.info()[["machine"]]
  ),
  cpu = cpu_description(),
  physical_cores = parallel::detectCores(logical = FALSE),
  logical_cores = parallel::detectCores(logical = TRUE),
  r_version = R.version.string,
  dapper_version = as.character(utils::packageVersion("dapper"))
)

run_benchmark <- function(
  example,
  observations,
  statistic_dimension,
  iterations,
  warmup,
  chains,
  workers,
  fit_expression
) {
  message("Benchmarking ", example, " ...")
  invisible(gc())

  timing <- system.time({
    fit <- force(fit_expression)
  })

  draw_summary <- as.data.frame(summary(fit))
  elapsed <- unname(timing[["elapsed"]])

  result <- data.frame(
    example = example,
    observations = observations,
    statistic_dimension = statistic_dimension,
    iterations = iterations,
    warmup = warmup,
    chains = chains,
    workers = workers,
    retained_draws = (iterations - warmup) * chains,
    elapsed_seconds = round(elapsed, 3),
    min_bulk_ess = round(min(draw_summary$ess_bulk, na.rm = TRUE), 1),
    median_bulk_ess = round(stats::median(draw_summary$ess_bulk, na.rm = TRUE), 1),
    min_tail_ess = round(min(draw_summary$ess_tail, na.rm = TRUE), 1),
    median_tail_ess = round(stats::median(draw_summary$ess_tail, na.rm = TRUE), 1),
    min_bulk_ess_per_second = round(
      min(draw_summary$ess_bulk, na.rm = TRUE) / elapsed,
      3
    ),
    max_rhat = round(finite_max(draw_summary$rhat), 4),
    mean_acceptance = round(mean(fit$mean_accept), 4),
    min_component_acceptance = round(min(fit$comp_accept), 4),
    stringsAsFactors = FALSE
  )

  for (metadata_name in names(benchmark_metadata)) {
    result[[metadata_name]] <- benchmark_metadata[[metadata_name]]
  }

  message(
    "Completed ", example, " in ",
    format(round(elapsed, 1), nsmall = 1), " seconds."
  )

  result
}

# Example 1: 2x2 contingency table with randomized response.
confidential_admissions <- tibble::tibble(
  sex = c(1, 1, 0, 0),
  status = c(1, 0, 1, 0),
  n = c(1198, 1493, 557, 1278)
)
confidential_admissions <- tidyr::uncount(confidential_admissions, n)

set.seed(1)
admission_indices <- sample(
  seq_len(nrow(confidential_admissions)),
  400,
  replace = FALSE
)
confidential_admissions <- confidential_admissions[admission_indices, ]

randomized_indices <- as.logical(stats::rbinom(800, 1, 1 / 2))
randomized_answers <- stats::rbinom(sum(randomized_indices), 1, 1 / 2)
randomized_release <- c(as.matrix(confidential_admissions))
randomized_release[randomized_indices] <- randomized_answers

table_latent_f <- function(theta) {
  possible_records <- list(c(1, 1), c(1, 0), c(0, 1), c(0, 0))
  sampled_records <- sample(
    possible_records,
    400,
    replace = TRUE,
    prob = theta
  )
  do.call(rbind, sampled_records)
}

table_posterior_f <- function(dmat, theta) {
  sex <- dmat[, 1]
  status <- dmat[, 2]
  cell_counts <- c(
    sum(sex & status),
    sum(sex & !status),
    sum(!sex & status),
    sum(!sex & !status)
  )
  gamma_draws <- stats::rgamma(4, cell_counts + 1, 1)
  gamma_draws / sum(gamma_draws)
}

randomized_statistic_f <- function(xi, sdp, i) {
  n <- length(sdp) %/% 2
  sum(xi == sdp[c(i, n + i)])
}

randomized_mechanism_f <- function(sdp, sx) {
  sx * log(3 / 4) +
    (length(sdp) - sx) * log(1 / 4)
}

randomized_model <- dapper::new_privacy(
  posterior_f = table_posterior_f,
  latent_f = table_latent_f,
  mechanism_f = randomized_mechanism_f,
  statistic_f = randomized_statistic_f,
  npar = 4,
  varnames = c("pi_11", "pi_10", "pi_01", "pi_00")
)

future::plan(future::multisession, workers = parallel_workers)

benchmark_results <- list(
  run_benchmark(
    example = "Randomized response",
    observations = 400,
    statistic_dimension = 800,
    iterations = 6000,
    warmup = 1000,
    chains = 4,
    workers = parallel_workers,
    fit_expression = dapper::dapper_sample(
      randomized_model,
      sdp = randomized_release,
      seed = 123,
      niter = 6000,
      warmup = 1000,
      chains = 4,
      init_par = rep(0.25, 4)
    )
  )
)

future::plan(future::sequential)

# Example 2: 2x2 contingency table with discrete Gaussian noise.
discrete_gaussian_statistic_f <- function(xi, sdp, i) {
  if (xi[1] & xi[2]) {
    c(1, 0, 0, 0)
  } else if (xi[1] & !xi[2]) {
    c(0, 1, 0, 0)
  } else if (!xi[1] & xi[2]) {
    c(0, 0, 1, 0)
  } else {
    c(0, 0, 0, 1)
  }
}

discrete_gaussian_mechanism_f <- function(sdp, sx) {
  sum(dapper::ddnorm(sdp - sx, mu = 0, sigma = 6.32, log = TRUE))
}

discrete_gaussian_release <- c(
  pi_11 = 110,
  pi_10 = 131,
  pi_01 = 47,
  pi_00 = 110
)

discrete_gaussian_model <- dapper::new_privacy(
  posterior_f = table_posterior_f,
  latent_f = table_latent_f,
  mechanism_f = discrete_gaussian_mechanism_f,
  statistic_f = discrete_gaussian_statistic_f,
  npar = 4,
  varnames = names(discrete_gaussian_release)
)

benchmark_results[[2]] <- run_benchmark(
  example = "Discrete Gaussian",
  observations = 400,
  statistic_dimension = 4,
  iterations = 2000,
  warmup = 1000,
  chains = 1,
  workers = 1,
  fit_expression = dapper::dapper_sample(
    discrete_gaussian_model,
    sdp = discrete_gaussian_release,
    seed = 123,
    niter = 2000,
    warmup = 1000,
    chains = 1,
    init_par = rep(0.25, 4)
  )
)

# Example 3: conjugate linear regression.
regression_latent_f <- function(theta) {
  xmat <- MASS::mvrnorm(
    50,
    mu = c(0.9, -1.17),
    Sigma = diag(2)
  )
  y <- cbind(1, xmat) %*% theta + stats::rnorm(50, sd = sqrt(2))
  cbind(y, xmat)
}

conjugate_posterior_f <- function(dmat, theta) {
  x <- cbind(1, dmat[, -1])
  y <- dmat[, 1]
  posterior_covariance <- solve(
    (1 / 2) * crossprod(x) + (1 / 4) * diag(3)
  )
  posterior_mean <- posterior_covariance %*% crossprod(x, y) * (1 / 2)
  MASS::mvrnorm(
    1,
    mu = posterior_mean,
    Sigma = posterior_covariance
  )
}

clamp_data <- function(dmat) {
  pmin(pmax(dmat, -10), 10) / 10
}

regression_statistic_f <- function(xi, sdp, i) {
  clamped_record <- clamp_data(xi)
  clamped_response <- clamped_record[1]
  clamped_design <- cbind(1, t(clamped_record[-1]))
  s1 <- crossprod(clamped_design, clamped_response)
  s2 <- crossprod(clamped_response)
  s3 <- crossprod(clamped_design)
  c(
    c(s1),
    c(s2),
    s3[upper.tri(s3, diag = TRUE)][-1]
  )
}

regression_mechanism_f <- function(sdp, sx) {
  sum(VGAM::dlaplace(sdp - sx, 0, 15 / 10, log = TRUE))
}

set.seed(1)
regression_x <- MASS::mvrnorm(
  50,
  mu = c(0.9, -1.17),
  Sigma = diag(2)
)
regression_beta <- c(-1.79, -2.89, -0.66)
regression_y <- cbind(1, regression_x) %*% regression_beta +
  stats::rnorm(50, sd = sqrt(2))
regression_data <- cbind(regression_y, regression_x)

regression_release <- apply(
  sapply(seq_len(nrow(regression_data)), function(i) {
    regression_statistic_f(regression_data[i, ], NULL, i)
  }),
  1,
  sum
)
regression_release <- regression_release + VGAM::rlaplace(
  length(regression_release),
  location = 0,
  scale = 15 / 10
)

# Match the article's external seed, including draws used during input validation.
set.seed(123)
conjugate_regression_model <- dapper::new_privacy(
  posterior_f = conjugate_posterior_f,
  latent_f = regression_latent_f,
  mechanism_f = regression_mechanism_f,
  statistic_f = regression_statistic_f,
  npar = 3,
  varnames = c("beta0", "beta1", "beta2")
)

benchmark_results[[3]] <- run_benchmark(
  example = "Conjugate regression",
  observations = 50,
  statistic_dimension = 9,
  iterations = 25000,
  warmup = 1000,
  chains = 1,
  workers = 1,
  fit_expression = dapper::dapper_sample(
    conjugate_regression_model,
    sdp = regression_release,
    niter = 25000,
    warmup = 1000,
    chains = 1,
    init_par = rep(0, 3)
  )
)

# Example 4: regression wrapper around an fmcmc transition.
log_latent_posterior <- function(theta, x, y) {
  log_prior_theta <- sum(stats::dt(theta, df = 5, log = TRUE))
  mean_y <- cbind(1, x) %*% theta
  log_likelihood_y <- sum(
    stats::dnorm(y, mean = mean_y, sd = sqrt(2), log = TRUE)
  )
  log_prior_theta + log_likelihood_y
}

wrapper_posterior_f <- function(dmat, theta) {
  one_step <- fmcmc::MCMC(
    initial = theta,
    fun = log_latent_posterior,
    nsteps = 2,
    burnin = 0,
    kernel = fmcmc::kernel_normal(scale = 0.2),
    progress = FALSE,
    x = dmat[, -1],
    y = dmat[, 1]
  )
  one_step[2, ]
}

set.seed(1)
wrapper_regression_model <- dapper::new_privacy(
  posterior_f = wrapper_posterior_f,
  latent_f = regression_latent_f,
  mechanism_f = regression_mechanism_f,
  statistic_f = regression_statistic_f,
  npar = 3,
  varnames = c("beta0", "beta1", "beta2")
)

benchmark_results[[4]] <- run_benchmark(
  example = "Wrapper regression",
  observations = 50,
  statistic_dimension = 9,
  iterations = 25000,
  warmup = 1000,
  chains = 1,
  workers = 1,
  fit_expression = dapper::dapper_sample(
    wrapper_regression_model,
    sdp = regression_release,
    niter = 25000,
    warmup = 1000,
    chains = 1,
    init_par = rep(0, 3)
  )
)

benchmark_results <- do.call(rbind, benchmark_results)
utils::write.csv(
  benchmark_results,
  results_path,
  row.names = FALSE,
  na = ""
)

message("Benchmark results written to ", results_path)
print(
  benchmark_results[, c(
    "example",
    "elapsed_seconds",
    "min_bulk_ess",
    "min_tail_ess",
    "min_bulk_ess_per_second",
    "max_rhat"
  )],
  row.names = FALSE
)

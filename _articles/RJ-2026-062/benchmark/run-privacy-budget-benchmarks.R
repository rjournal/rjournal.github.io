#!/usr/bin/env Rscript

# Measure how the privacy budget affects MCMC efficiency in the conjugate
# regression example. The confidential data, unit-scale Laplace noise draw,
# MCMC seed, and sampling configuration are held fixed. For each budget, the
# common noise draw is rescaled by Delta / epsilon, producing a valid release
# under the corresponding Laplace mechanism.

required_packages <- c(
  "dapper",
  "future",
  "MASS",
  "posterior",
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
    file.path("benchmark", "run-privacy-budget-benchmarks.R"),
    mustWork = TRUE
  )
}

benchmark_directory <- dirname(script_path)
results_path <- file.path(
  benchmark_directory,
  "privacy-budget-results.csv"
)
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

sample_size <- 50L
sensitivity <- 15
iterations <- 6000L
warmup <- 1000L
chains <- 4L
privacy_budgets <- c(2.5, 5, 10, 20, 40)
privacy_budget_labels <- c("2.5", "5", "10 (Example 3)", "20", "40")

latent_f <- function(theta) {
  xmat <- MASS::mvrnorm(
    sample_size,
    mu = c(0.9, -1.17),
    Sigma = diag(2)
  )
  y <- cbind(1, xmat) %*% theta +
    stats::rnorm(sample_size, sd = sqrt(2))
  cbind(y, xmat)
}

posterior_f <- function(dmat, theta) {
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

statistic_f <- function(xi, sdp, i) {
  clamped_record <- clamp_data(xi)
  clamped_response <- clamped_record[1]
  clamped_design <- cbind(1, t(clamped_record[-1]))
  s1 <- crossprod(clamped_design, clamped_response)
  s2 <- crossprod(clamped_response)
  s3 <- crossprod(clamped_design)
  c(c(s1), c(s2), s3[upper.tri(s3, diag = TRUE)][-1])
}

make_laplace_mechanism <- function(epsilon) {
  laplace_scale <- sensitivity / epsilon

  function(sdp, sx) {
    sum(
      VGAM::dlaplace(
        sdp - sx,
        location = 0,
        scale = laplace_scale,
        log = TRUE
      )
    )
  }
}

# Reproduce the confidential data from Example 3. Drawing unit-scale noise
# once and rescaling it couples the releases across epsilon without changing
# the marginal Laplace mechanism at any individual budget.
set.seed(1)
confidential_x <- MASS::mvrnorm(
  sample_size,
  mu = c(0.9, -1.17),
  Sigma = diag(2)
)
beta <- c(-1.79, -2.89, -0.66)
confidential_y <- cbind(1, confidential_x) %*% beta +
  stats::rnorm(sample_size, sd = sqrt(2))
confidential_data <- cbind(confidential_y, confidential_x)

confidential_statistic <- Reduce(
  `+`,
  lapply(
    seq_len(sample_size),
    function(i) statistic_f(confidential_data[i, ], NULL, i)
  )
)
unit_laplace_noise <- VGAM::rlaplace(
  length(confidential_statistic),
  location = 0,
  scale = 1
)

future::plan(future::multisession, workers = parallel_workers)
benchmark_rows <- vector("list", length(privacy_budgets))

for (budget_index in seq_along(privacy_budgets)) {
  epsilon <- privacy_budgets[[budget_index]]
  laplace_scale <- sensitivity / epsilon
  observed_release <- confidential_statistic +
    laplace_scale * unit_laplace_noise

  message(
    "Benchmarking epsilon = ",
    privacy_budget_labels[[budget_index]],
    " ..."
  )

  privacy_model <- dapper::new_privacy(
    posterior_f = posterior_f,
    latent_f = latent_f,
    mechanism_f = make_laplace_mechanism(epsilon),
    statistic_f = statistic_f,
    npar = 3,
    varnames = c("beta0", "beta1", "beta2")
  )

  invisible(gc())
  timing <- system.time({
    fit <- dapper::dapper_sample(
      privacy_model,
      sdp = observed_release,
      seed = 123,
      niter = iterations,
      warmup = warmup,
      chains = chains,
      init_par = rep(0, 3)
    )
  })

  draw_summary <- as.data.frame(summary(fit))
  elapsed <- unname(timing[["elapsed"]])

  benchmark_rows[[budget_index]] <- data.frame(
    epsilon_label = privacy_budget_labels[[budget_index]],
    epsilon = epsilon,
    sensitivity = sensitivity,
    laplace_scale = laplace_scale,
    observations = sample_size,
    statistic_dimension = length(confidential_statistic),
    iterations = iterations,
    warmup = warmup,
    chains = chains,
    workers = parallel_workers,
    retained_draws = (iterations - warmup) * chains,
    elapsed_seconds = round(elapsed, 3),
    min_bulk_ess = round(min(draw_summary$ess_bulk, na.rm = TRUE), 1),
    median_bulk_ess = round(
      stats::median(draw_summary$ess_bulk, na.rm = TRUE),
      1
    ),
    min_tail_ess = round(min(draw_summary$ess_tail, na.rm = TRUE), 1),
    median_tail_ess = round(
      stats::median(draw_summary$ess_tail, na.rm = TRUE),
      1
    ),
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
    benchmark_rows[[budget_index]][[metadata_name]] <-
      benchmark_metadata[[metadata_name]]
  }

  message(
    "Completed epsilon = ",
    privacy_budget_labels[[budget_index]],
    " in ",
    format(round(elapsed, 1), nsmall = 1),
    " seconds."
  )
}

future::plan(future::sequential)

benchmark_results <- do.call(rbind, benchmark_rows)
utils::write.csv(
  benchmark_results,
  results_path,
  row.names = FALSE,
  na = ""
)

message("Privacy-budget results written to ", results_path)
print(
  benchmark_results[, c(
    "epsilon_label",
    "laplace_scale",
    "elapsed_seconds",
    "min_bulk_ess",
    "min_tail_ess",
    "min_bulk_ess_per_second",
    "max_rhat"
  )],
  row.names = FALSE
)

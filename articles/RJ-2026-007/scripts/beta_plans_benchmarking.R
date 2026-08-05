# beta_plans_benchmarking.R

library(kableExtra)
library(dplyr)
library(AccSamplingDesign)
library(bench)

# This part of code show result claimed in Section 7: 
# "Computational Performance and Benchmarking" for Beta-based plans optimization.
# Please allow 1+ minutes for grid search method to finish finding plan.

#-------------------------------------------------------------------------------
# This function describe grid search method for Beta optimal plan 
# Code from Govindaraju and Kissling (2015)
#-------------------------------------------------------------------------------
beta_plan_grid_search <- function(LSL, theta, p1, p2, alpha, beta, nsim = 3000) {
  # Compute mu such that P(X < LSL) = p for Beta(theta*mu, theta*(1-mu))
  mulimit <- function(p, LSL, theta) {
    f <- function(mu) pbeta(LSL, theta * mu, theta * (1 - mu)) - p
    uniroot(f, interval = c(0, 1), tol = 1e-9)$root
  }
  # Estimate probability of acceptance for given mu, k, m, LSL
  Pa_est <- function(nsim, mu, theta, k, m, LSL) {
    shape1 <- m * mu * theta
    shape2 <- m * (1 - mu) * theta
    ym <- rbeta(nsim, shape1, shape2)
    acc_crit <- ym - k * sqrt(ym * (1 - ym) / theta)
    mean(acc_crit > LSL)
  }
  
  # Check for AcceptanceSampling package
  if (!requireNamespace("AcceptanceSampling", quietly = TRUE)) {
    stop("Package 'AcceptanceSampling' is required but not installed.")
  }
  
  muU <- mulimit(p1, LSL, theta)  # preferred quality level
  muL <- mulimit(p2, LSL, theta)  # worse quality level
  
  vplan <- AcceptanceSampling::find.plan(
    c(p1, 1 - alpha), c(p2, beta),
    type = "normal", s.type = "known"
  )
  
  message("Starting grid search, this may take some time...")
  kvalues <- seq(0.5 * vplan$k, 2 * vplan$k, by = 0.01)
  mvalues <- seq(0.5 * vplan$n, 2 * vplan$n, by = 0.15)
  grid <- expand.grid(k = kvalues, m = mvalues)
  
  CR <- mapply(Pa_est, nsim = nsim, mu = muL, theta = theta,
               k = grid$k, m = grid$m, LSL = LSL)
  PR <- 1 - mapply(Pa_est, nsim = nsim, mu = muU, theta = theta,
                   k = grid$k, m = grid$m, LSL = LSL)
  
  feasible <- subset(cbind(grid, CR, PR), CR < beta & PR < alpha)
  feasible <- feasible[order(feasible$m, feasible$k), ]
  
  if (nrow(feasible) == 0) stop("No feasible plan found.")
  
  opt <- feasible[1, ]
  opt$m <- round(opt$m, 4) 
  opt$k <- round(opt$k, 4)
  
  message("Grid search completed. Optimal plan found.")
  return(list(n = opt$m, k = opt$k))
}

# Parameters (from Example #3)
LSL    <- 5.65e-6
theta  <- 6.6e+08
p1     <- 0.025
p2     <- 0.10
alpha  <- 0.05
beta   <- 0.10

#-------------------------------------------------------------------------------
# Benchmark performance (please allow about 1+ minute for grid_search to run)
#-------------------------------------------------------------------------------
set.seed(123)

# Run each method here and store results to compare
cat("Running Grid Search (may take 1+ minute)...\n")
res_grid <- beta_plan_grid_search(LSL, theta, p1, p2, alpha, beta)

cat("Running AccSamplingDesign NLP method...\n")
res_opt <- AccSamplingDesign::optPlan(
  PRQ = p1, CRQ = p2, alpha = alpha, beta = beta,
  distribution = "beta", theta_type = "known",
  theta = theta, LSL = LSL
)

# Benchmark run-time and memory
cat("Benchmarking AccSamplingDesign NLP vs Grid Search methods ...\n")
bm <- bench::mark(
  Govindaraju_and_Kissling_2015_GridSearch = beta_plan_grid_search(LSL, theta, 
                                                                   p1, p2, 
                                                                   alpha, beta),
  AccSamplingDesign_NLP = AccSamplingDesign::optPlan(
    PRQ = p1, CRQ = p2, alpha = alpha, beta = beta,
    distribution = "beta", theta_type = "known",
    theta = theta, LSL = LSL
  ),
  iterations = 1,
  check = FALSE
)

# Combine into summary table
summary_tbl <- tibble(
  Method = c("Govindaraju_and_Kissling_2015", "AccSamplingDesign"),
  n = c(res_grid$n, res_opt$n),
  k = c(res_grid$k, res_opt$k),
  `Median Time (s)` = round(as.numeric(bm$median), 3),
  `Memory Used` = format(bm$mem_alloc, units = "auto"),
)

#-------------------------------------------------------------------------------
# Display final table of benchmark results
#-------------------------------------------------------------------------------
kable(summary_tbl, caption = "Benchmarking AccSamplingDesign performance: Grid 
      Search vs. NLP for Beta-based sampling plan optimization.")

# Compute speed-up factor
time_grid <- as.numeric(bm$median[1])
time_nlp  <- as.numeric(bm$median[2])
speedup   <- round(time_grid / time_nlp, 1)

#-------------------------------------------------------------------------------
# Print speed-up factor
#-------------------------------------------------------------------------------
cat(sprintf("
\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
AccSamplingDesign::optPlan() - NLP method is approximately %.1f× faster than 
the Grid Search method for finding optimal Beta-based sampling plans. 
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", 
speedup))

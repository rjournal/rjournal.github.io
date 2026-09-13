## ---------------------------------------------
## 95% Credible intervals from MCMC output
## (equal-tailed: 2.5% and 97.5% quantiles)
## ---------------------------------------------

load("results/simulated_output_Hans.RData")

load("results/simulated_output_PC.RData")

set.seed(123)
# Inputs you already have
Ns <- 2000
burn <- 200

ns <- 100
ps <- 10
X <- matrix(rnorm(ns * ps), nrow = ns)
beta_true <- c(rep(2, 3), rep(0, ps - 3))
y <- X %*% beta_true + rnorm(ns)

# Assuming output_Hans and output_PC already exist
# output_Hans$mBeta, output_Hans$vsigma2, output_Hans$vlambda2
# output_PC$mBeta,   output_PC$vsigma2,   output_PC$vlambda2

# ---- Helper: CI for a vector of draws (scalar parameter) ----
ci_scalar <- function(draws, level = 0.95) {
  a <- (1 - level) / 2
  as.numeric(quantile(draws, probs = c(a, 1 - a), names = FALSE, type = 8))
}

# ---- Helper: CI for matrix of draws (iterations x p) ----
ci_matrix_cols <- function(draws_mat, level = 0.95) {
  stopifnot(is.matrix(draws_mat))
  a <- (1 - level) / 2
  lo <- apply(draws_mat, 2, quantile, probs = a,     names = FALSE, type = 8)
  hi <- apply(draws_mat, 2, quantile, probs = 1 - a, names = FALSE, type = 8)
  cbind(lo = lo, hi = hi)
}

# ---- Extract post-burn-in draws ----
beta_draws_H <- output_Hans$mBeta[(burn + 1):Ns, , drop = FALSE]
sig2_draws_H <- output_Hans$vsigma2[(burn + 1):Ns]
lam2_draws_H <- output_Hans$vlambda2[(burn + 1):Ns]

beta_draws_P <- output_PC$mBeta[(burn + 1):Ns, , drop = FALSE]
sig2_draws_P <- output_PC$vsigma2[(burn + 1):Ns]
lam2_draws_P <- output_PC$vlambda2[(burn + 1):Ns]

# ---- 95% credible intervals ----
CI_beta_H <- ci_matrix_cols(beta_draws_H, level = 0.95)
CI_sig2_H <- ci_scalar(sig2_draws_H, level = 0.95)
CI_lam2_H <- ci_scalar(lam2_draws_H, level = 0.95)

CI_beta_P <- ci_matrix_cols(beta_draws_P, level = 0.95)
CI_sig2_P <- ci_scalar(sig2_draws_P, level = 0.95)
CI_lam2_P <- ci_scalar(lam2_draws_P, level = 0.95)

# Put scalar CIs into named vectors for readability
CI_sig2_H <- setNames(CI_sig2_H, c("lo", "hi"))
CI_lam2_H <- setNames(CI_lam2_H, c("lo", "hi"))
CI_sig2_P <- setNames(CI_sig2_P, c("lo", "hi"))
CI_lam2_P <- setNames(CI_lam2_P, c("lo", "hi"))

# ---- Coverage for beta (known truth) ----
inside_H <- (beta_true >= CI_beta_H[, "lo"]) & (beta_true <= CI_beta_H[, "hi"])
inside_P <- (beta_true >= CI_beta_P[, "lo"]) & (beta_true <= CI_beta_P[, "hi"])

coverage_overall_H <- mean(inside_H)
coverage_overall_P <- mean(inside_P)

nz <- beta_true != 0
coverage_nonzero_H <- mean(inside_H[nz])
coverage_zero_H    <- mean(inside_H[!nz])

coverage_nonzero_P <- mean(inside_P[nz])
coverage_zero_P    <- mean(inside_P[!nz])

# ---- Print results ----
cat("\n=== 95% Credible intervals (Hans) ===\n")
print(CI_beta_H)
cat("sigma^2 CI:", CI_sig2_H["lo"], CI_sig2_H["hi"], "\n")
cat("lambda^2 CI:", CI_lam2_H["lo"], CI_lam2_H["hi"], "\n")
cat("Beta coverage (overall / nonzero / zero):",
    coverage_overall_H, coverage_nonzero_H, coverage_zero_H, "\n")

cat("\n=== 95% Credible intervals (PC) ===\n")
print(CI_beta_P)
cat("sigma^2 CI:", CI_sig2_P["lo"], CI_sig2_P["hi"], "\n")
cat("lambda^2 CI:", CI_lam2_P["lo"], CI_lam2_P["hi"], "\n")
cat("Beta coverage (overall / nonzero / zero):",
    coverage_overall_P, coverage_nonzero_P, coverage_zero_P, "\n")

# ---- Optional: tidy table for beta CIs ----
beta_ci_table_H <- data.frame(
  coef = paste0("beta", seq_len(ncol(beta_draws_H))),
  true = beta_true,
  lo = CI_beta_H[, "lo"],
  hi = CI_beta_H[, "hi"],
  covered = inside_H
)

beta_ci_table_P <- data.frame(
  coef = paste0("beta", seq_len(ncol(beta_draws_P))),
  true = beta_true,
  lo = CI_beta_P[, "lo"],
  hi = CI_beta_P[, "hi"],
  covered = inside_P
)

# View:
beta_ci_table_H
beta_ci_table_P
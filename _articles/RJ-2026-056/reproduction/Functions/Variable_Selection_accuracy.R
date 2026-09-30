## ------------------------------------------------------------
## Monte Carlo variable selection accuracy (R = 100)
## Hans vs PC, using 95% CI exclusion of 0 as selection rule
## ------------------------------------------------------------

# ---- User settings ----
set.seed(123)
R  <- 100          # number of simulation replicates
Ns <- 2000         # MCMC iterations
burn <- 200        # burn-in
ns <- 100          # sample size
ps <- 10           # number of predictors
beta_true <- c(rep(2, 3), rep(0, ps - 3))

# Hyperparameters / init (same as your one-run code)
a1 <- 2; b1 <- 1; u1 <- 2; v1 <- 1
beta_init  <- rep(1, ps)
lambda_init <- 1
sigma2_init <- 1
verbose_val <- 0   # set 0 to keep output clean; change if you want

# ---- Helpers ----
ci_matrix_cols <- function(draws_mat, level = 0.95) {
  stopifnot(is.matrix(draws_mat))
  alpha <- (1 - level) / 2
  lo <- apply(draws_mat, 2, quantile, probs = alpha,     names = FALSE, type = 8)
  hi <- apply(draws_mat, 2, quantile, probs = 1 - alpha, names = FALSE, type = 8)
  cbind(lo = lo, hi = hi)
}

select_from_ci <- function(CI_beta) {
  # selected if 0 NOT inside [lo,hi]
  !(CI_beta[, "lo"] <= 0 & CI_beta[, "hi"] >= 0)
}

selection_metrics <- function(selected, beta_true) {
  true_signal <- beta_true != 0
  
  TP <- sum(selected &  true_signal)
  FP <- sum(selected & !true_signal)
  FN <- sum(!selected &  true_signal)
  TN <- sum(!selected & !true_signal)
  
  acc  <- (TP + TN) / length(beta_true)
  sens <- TP / sum(true_signal)       # TPR
  spec <- TN / sum(!true_signal)      # TNR
  fdr  <- if ((TP + FP) > 0) FP / (TP + FP) else 0
  fnr  <- FN / sum(true_signal)
  
  c(TP=TP, FP=FP, FN=FN, TN=TN,
    Accuracy=acc, Sensitivity=sens, Specificity=spec,
    FDR=fdr, FNR=fnr)
}

# ---- Storage ----
res_H <- matrix(NA_real_, nrow = R, ncol = 10)
res_P <- matrix(NA_real_, nrow = R, ncol = 10)
colnames(res_H) <- colnames(res_P) <- c("TP","FP","FN","TN",
                                        "Accuracy","Sensitivity","Specificity",
                                        "FDR","FNR","Time")

# ---- Monte Carlo loop ----
for (r in 1:R) {
  # simulate data
  X <- matrix(rnorm(ns * ps), nrow = ns)
  y <- drop(X %*% beta_true + rnorm(ns))  # sigma^2=1 noise
  
  ## ---- Hans ----
  tH <- system.time({
    outH <- Modified_Hans_Gibbs(
      X = X, y = y, beta_init=beta_init, a1=a1, b1=b1, u1=u1, v1=v1,
      nsamples=Ns, lambda_init=lambda_init,
      sigma2_init=sigma2_init, verbose=verbose_val,tune_lambda2 = TRUE,
      rao_blackwellization = FALSE
    )
  })[3]
  
  beta_draws_H <- outH$mBeta[(burn+1):Ns, , drop = FALSE]
  CI_beta_H <- ci_matrix_cols(beta_draws_H, level = 0.95)
  selected_H <- select_from_ci(CI_beta_H)
  metH <- selection_metrics(selected_H, beta_true)
  res_H[r, ] <- c(metH, Time = tH)
  
  ## ---- PC ----
  tP <- system.time({
    outP <- Modified_PC_Gibbs(
      X = X, y = y, a1=a1, b1=b1, u1=u1, v1=v1,
      nsamples=Ns, lambda_init=lambda_init,
      sigma2_init=sigma2_init, verbose=verbose_val
    )
  })[3]
  
  beta_draws_P <- outP$mBeta[(burn+1):Ns, , drop = FALSE]
  CI_beta_P <- ci_matrix_cols(beta_draws_P, level = 0.95)
  selected_P <- select_from_ci(CI_beta_P)
  metP <- selection_metrics(selected_P, beta_true)
  res_P[r, ] <- c(metP, Time = tP)
  
  if (r %% 10 == 0) cat("Completed replicate", r, "of", R, "\n")
}

# ---- Summaries ----
summ <- function(mat) {
  data.frame(
    Mean = colMeans(mat, na.rm = TRUE),
    SD   = apply(mat, 2, sd, na.rm = TRUE)
  )
}

sum_H <- summ(res_H)
sum_P <- summ(res_P)

cat("\n--- Hans summary (mean ± sd) ---\n")
print(round(sum_H, 4))

cat("\n--- PC summary (mean ± sd) ---\n")
print(round(sum_P, 4))

# ---- Optional: quick comparison table (accuracy/sens/spec/FDR/FNR/time) ----
keep <- c("Accuracy","Sensitivity","Specificity","FDR","FNR","Time")
comp <- rbind(
  Hans_Mean = sum_H[keep, "Mean"],
  Hans_SD   = sum_H[keep, "SD"],
  PC_Mean   = sum_P[keep, "Mean"],
  PC_SD     = sum_P[keep, "SD"]
)
cat("\n--- Comparison (selected metrics) ---\n")
print(round(comp, 4))

# Save results if you want
# save(res_H, res_P, sum_H, sum_P, file = "sim_selection_results_R100.RData")


## -------------------------------------------
## Out of sample prediction error
## -------------------------------------------

# Generate test data
X_test <- matrix(rnorm(ns * ps), nrow = ns)
y_test <- drop(X_test %*% beta_true + rnorm(ns))

# Posterior mean beta
beta_hat_H <- colMeans(beta_draws_H)
beta_hat_P <- colMeans(beta_draws_P)

# Predictions
y_pred_H <- drop(X_test %*% beta_hat_H)
y_pred_P <- drop(X_test %*% beta_hat_P)

# MSE
mse_H <- mean((y_test - y_pred_H)^2)
mse_P <- mean((y_test - y_pred_P)^2)

mse_H
mse_P
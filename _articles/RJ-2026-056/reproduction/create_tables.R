# ============================================================
# Script: create_tables.R
# Purpose: Load pre-computed .RData files and display
#          Tables 5 and 6 with group separator lines.
# Requires: knitr, kableExtra
#   install.packages(c("knitr", "kableExtra"))
#
# Expected .RData files (set paths below):
#   Table_5_Diabeties2.Rdata
#   Table_5_Kakadu2.Rdata
#   Table_5_Crime.Rdata
#   Table_6_Eye.Rdata
#   Table_6_Riboflavin.Rdata
# ============================================================

# Set working directory to the folder containing this script
script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
setwd(script_dir)

library(BayesianLasso)
library(knitr)
library(kableExtra)

# ── File paths (adjust if needed) ────────────────────────────
path_sim   <- "tables/Table_2_Simulated.Rdata"
path_diab  <- "tables/Table_5_Diabeties2.Rdata"
path_kak   <- "tables/Table_5_Kakadu2.Rdata"
path_crime <- "tables/Table_5_Crime.Rdata"
path_eye   <- "tables/Table_6_Eye.Rdata"
path_rib   <- "tables/Table_6_Riboflavin.Rdata"

# ── Load and extract matrices ─────────────────────────────────
load(path_sim);   m_sim   <- Table_2_Simulated
load(path_diab);  m_diab  <- Table_5_Diabeties2   # 6 x 7 matrix
load(path_kak);   m_kak   <- Table_5_Kakadu2       # 5 x 7 matrix (no monomvn)
load(path_crime); m_crime <- Table_5_Crime         # 6 x 7 matrix
load(path_eye);   m_eye   <- Table_6_eye           # matrix (note: lowercase 'e')
load(path_rib);   m_rib   <- Table_6_Riboflavin    # matrix


# ── Scale Eff columns (divide by 10^2) ───────────────────────
eff_cols <- c("eff_beta", "eff_sigma2", "eff_lambda2")

m_sim[  , eff_cols] <- m_sim[  , eff_cols] / 100
m_diab[ , eff_cols] <- m_diab[ , eff_cols] / 100
m_kak[  , eff_cols] <- m_kak[  , eff_cols] / 100
m_crime[, eff_cols] <- m_crime[, eff_cols] / 100
m_eye[  , eff_cols] <- m_eye[  , eff_cols] / 100
m_rib[  , eff_cols] <- m_rib[  , eff_cols] / 100

# ── Helper: matrix -> display data frame ─────────────────────
# Rounds values, inserts NA row for missing methods,
# and prepends Dataset + Method columns.

ALL_METHODS_5 <- c("Hans", "PC", "monomvn", "bayeslm", "rstan", "bayesreg")
ALL_METHODS_6 <- c("Hans", "PC", "bayesreg")

mat_to_df <- function(mat, dataset_label, all_methods, digits = 2) {
  present <- rownames(mat)
  rows <- lapply(all_methods, function(m) {
    if (m %in% present) {
      round(mat[m, ], digits)
    } else {
      setNames(rep(NA_real_, ncol(mat)), colnames(mat))
    }
  })
  df <- as.data.frame(do.call(rbind, rows))
  df <- cbind(
    Dataset = c(dataset_label, rep("", length(all_methods) - 1)),
    Method  = all_methods,
    df,
    stringsAsFactors = FALSE
  )
  rownames(df) <- NULL
  df
}

# ── Build Table 2 data frame ──────────────────────────────────


ALL_METHODS_2   <- c("Hans", "PC")
df_sim          <- mat_to_df(m_sim, "Simulated", ALL_METHODS_2)
Table_2_display <- df_sim
Table_2_display[is.na(Table_2_display)] <- "NA"



# ── Build Table 3 data frame ──────────────────────────────────

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
beta_draws_H_3 <- output_Hans$mBeta[(burn + 1):Ns, , drop = FALSE]
sig2_draws_H <- output_Hans$vsigma2[(burn + 1):Ns]
lam2_draws_H <- output_Hans$vlambda2[(burn + 1):Ns]

beta_draws_P_3 <- output_PC$mBeta[(burn + 1):Ns, , drop = FALSE]
sig2_draws_P <- output_PC$vsigma2[(burn + 1):Ns]
lam2_draws_P <- output_PC$vlambda2[(burn + 1):Ns]

# ---- 95% credible intervals ----
CI_beta_H <- ci_matrix_cols(beta_draws_H_3, level = 0.95)
CI_sig2_H <- ci_scalar(sig2_draws_H, level = 0.95)
CI_lam2_H <- ci_scalar(lam2_draws_H, level = 0.95)

CI_beta_P <- ci_matrix_cols(beta_draws_P_3, level = 0.95)
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




# ── Build Table 4 data frame ──────────────────────────────────

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
  
  # TP=TP, FP=FP, FN=FN, TN=TN,
  
  c(Accuracy=acc, Sensitivity=sens, Specificity=spec,
    FDR=fdr, FNR=fnr)
}

# ---- Storage ----
res_H <- matrix(NA_real_, nrow = R, ncol = 6)
res_P <- matrix(NA_real_, nrow = R, ncol = 6)

# "TP","FP","FN","TN",

colnames(res_H) <- colnames(res_P) <- c("Accuracy","Sensitivity","Specificity",
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
  
  # if (r %% 10 == 0) cat("Completed replicate", r, "of", R, "\n")
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



# ---- Optional: quick comparison table (accuracy/sens/spec/FDR/FNR/time) ----
keep <- c("Accuracy","Sensitivity","Specificity","FDR","FNR","Time")
comp <- rbind(
  Hans_Mean = sum_H[keep, "Mean"],
  Hans_SD   = sum_H[keep, "SD"],
  PC_Mean   = sum_P[keep, "Mean"],
  PC_SD     = sum_P[keep, "SD"]
)
# cat("\n--- Comparison (selected metrics) ---\n")
# print(round(comp, 4))

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






# ── Build Table 5 data frame ──────────────────────────────────
df_diab  <- mat_to_df(m_diab,  "Diabetes\u00b2", ALL_METHODS_5)
df_kak   <- mat_to_df(m_kak,   "Kakadu\u00b2",   ALL_METHODS_5)
df_crime <- mat_to_df(m_crime, "Crime",           ALL_METHODS_5)

Table_5_display <- rbind(df_diab, df_kak, df_crime)

# Replace NA with the string "NA" for display
Table_5_display[is.na(Table_5_display)] <- "NA"

# ── Build Table 6 data frame ──────────────────────────────────
df_eye <- mat_to_df(m_eye, "Eye",        ALL_METHODS_6)
df_rib <- mat_to_df(m_rib, "Riboflavin", ALL_METHODS_6)

Table_6_display <- rbind(df_eye, df_rib)
Table_6_display[is.na(Table_6_display)] <- "NA"

# ── Column names for display ──────────────────────────────────
col_names <- c(
  "Dataset", "Method",
  "Mix %", "Eff (x10^2)",
  "Mix %", "Eff (x10^2)",
  "Mix %", "Eff (x10^2)",
  "Time (s)"
)


# ── Print Table 2 ─────────────────────────────────────────────
cat("\n===== Table 2 =====\n")

kable(
  Table_2_display,
  format    = "simple",
  col.names = col_names,
  align     = c("l", "l", rep("r", 7)),
  caption   = paste(
    "Mixing percentages, efficiencies, and computation times (in seconds)",
    "for the simulated dataset using the Hans and PC Gibbs samplers from",
    "the BayesianLasso package. Efficiencies are reported in units of",
    "x10^2. Higher mixing percentages and efficiencies indicate improved",
    "sampling performance."
  )
) |>
  pack_rows("Simulated", 1, 2, hline_after = TRUE) |>
  print()


# ── Print Table 3 ─────────────────────────────────────────────
cat("\n===== Table 3 =====\n")

# Build display data frame
param_labels <- c(
  paste0("$\\beta_{", 1:ps, "}$ (true = ", beta_true, ")"),
  "$\\sigma^2$ (true = 1)",
  "$\\lambda^2$"
)

ci_fmt <- function(lo, hi) sprintf("(%.2f, %.2f)", lo, hi)

Table_3_display <- data.frame(
  Parameter    = param_labels,
  Hans_CI      = c(ci_fmt(CI_beta_H[,"lo"], CI_beta_H[,"hi"]),
                   ci_fmt(CI_sig2_H["lo"],  CI_sig2_H["hi"]),
                   ci_fmt(CI_lam2_H["lo"],  CI_lam2_H["hi"])),
  Hans_Covered = c(ifelse(inside_H, "Yes", "No"), "Yes", "--"),
  PC_CI        = c(ci_fmt(CI_beta_P[,"lo"], CI_beta_P[,"hi"]),
                   ci_fmt(CI_sig2_P["lo"],  CI_sig2_P["hi"]),
                   ci_fmt(CI_lam2_P["lo"],  CI_lam2_P["hi"])),
  PC_Covered   = c(ifelse(inside_P, "Yes", "No"), "Yes", "--"),
  stringsAsFactors = FALSE
)

# Append coverage summary rows
coverage_rows <- data.frame(
  Parameter    = c("Coverage (nonzero $\\beta$)",
                   "Coverage (zero $\\beta$)",
                   "Coverage (overall $\\beta$)"),
  Hans_CI      = c(sprintf("%.2f", coverage_nonzero_H),
                   sprintf("%.2f", coverage_zero_H),
                   sprintf("%.2f", coverage_overall_H)),
  Hans_Covered = c("", "", ""),
  PC_CI        = c(sprintf("%.2f", coverage_nonzero_P),
                   sprintf("%.2f", coverage_zero_P),
                   sprintf("%.2f", coverage_overall_P)),
  PC_Covered   = c("", "", ""),
  stringsAsFactors = FALSE
)

Table_3_display <- rbind(Table_3_display, coverage_rows)

kable(
  Table_3_display,
  format    = "simple",
  col.names = c("Parameter",
                "95% CI (lo, hi) Hans", "Covered?",
                "95% CI (lo, hi) PC", "Covered?"),
  align     = c("l", "c", "c", "c", "c"),
  caption   = paste(
    "Simulation study: 95% equal-tailed credible intervals (CIs) for",
    "regression coefficients and hyperparameters based on post-burn-in",
    "MCMC samples (iterations 201-2000). Coverage refers to whether the",
    "true parameter value lies within the reported 95% CI (reported as",
    "proportions for nonzero and zero coefficients)."
  )
) |>
  add_header_above(c(" " = 1, "Hans" = 2, "PC" = 2)) |>
  pack_rows("Beta coefficients", 1, ps, hline_after = TRUE) |>
  pack_rows("Hyperparameters",   ps + 1, ps + 2, hline_after = TRUE) |>
  pack_rows("Coverage",          ps + 3, ps + 5, hline_after = FALSE) |>
  print()



# ── Print Table 4 ─────────────────────────────────────────────
cat("\n===== Table 4 =====\n")

fmt_meansd <- function(mean_val, sd_val) {
  sprintf("%.2f $\\pm$ %.2f", mean_val, sd_val)
}

keep <- c("Accuracy", "Sensitivity", "Specificity", "FDR", "FNR")

Table_4_display <- data.frame(
  Method   = c("Hans", "PC"),
  Accuracy    = c(fmt_meansd(sum_H["Accuracy",    "Mean"], sum_H["Accuracy",    "SD"]),
                  fmt_meansd(sum_P["Accuracy",    "Mean"], sum_P["Accuracy",    "SD"])),
  Sensitivity = c(fmt_meansd(sum_H["Sensitivity", "Mean"], sum_H["Sensitivity", "SD"]),
                  fmt_meansd(sum_P["Sensitivity", "Mean"], sum_P["Sensitivity", "SD"])),
  Specificity = c(fmt_meansd(sum_H["Specificity", "Mean"], sum_H["Specificity", "SD"]),
                  fmt_meansd(sum_P["Specificity", "Mean"], sum_P["Specificity", "SD"])),
  FDR         = c(fmt_meansd(sum_H["FDR",         "Mean"], sum_H["FDR",         "SD"]),
                  fmt_meansd(sum_P["FDR",         "Mean"], sum_P["FDR",         "SD"])),
  FNR         = c(fmt_meansd(sum_H["FNR",         "Mean"], sum_H["FNR",         "SD"]),
                  fmt_meansd(sum_P["FNR",         "Mean"], sum_P["FNR",         "SD"])),
  stringsAsFactors = FALSE
)

kable(
  Table_4_display,
  format    = "simple",
  col.names = c("Method", "Accuracy", "Sensitivity", "Specificity", "FDR", "FNR"),
  align     = c("l", rep("c", 5)),
  caption   = paste(
    "Simulation study (100 replicates): variable selection performance of",
    "the modified Hans and modified PC Gibbs samplers. A coefficient is",
    "declared selected if its 95% credible interval excludes zero.",
    "Reported values are mean +/- standard deviation across replicates."
  )
) |>
  print()

# ── Print Table 5 ─────────────────────────────────────────────
cat("\n===== Table 5 =====\n")

kable(
  Table_5_display,
  format    = "simple",
  col.names = col_names,
  align     = c("l", "l", rep("r", 7)),
  caption   = paste(
    "Mixing percentages, efficiencies, and computation times (in seconds)",
    "for each dataset using the Hans and PC Gibbs samplers from the",
    "BayesianLasso package, as well as the R packages monomvn,",
    "bayeslm, rstan, and bayesreg."
  )
) |>
  pack_rows("Diabetes^2", 1,  6, hline_after = TRUE) |>
  pack_rows("Kakadu^2",   7, 12, hline_after = TRUE) |>
  pack_rows("Crime",     13, 18, hline_after = TRUE) |>
  print()

# ── Print Table 6 ─────────────────────────────────────────────
cat("\n===== Table 6 =====\n")

kable(
  Table_6_display,
  format    = "simple",
  col.names = col_names,
  align     = c("l", "l", rep("r", 7)),
  caption   = paste(
    "Mixing percentages, efficiencies, and computation times (in seconds)",
    "for the Eye and Riboflavin datasets using the Hans and PC Gibbs",
    "samplers from the BayesianLasso package, and the bayesreg package."
  )
) |>
  pack_rows("Eye",        1, 3, hline_after = TRUE) |>
  pack_rows("Riboflavin", 4, 6, hline_after = TRUE) |>
  print()




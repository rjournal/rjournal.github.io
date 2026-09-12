# ============================================================
# 01-replicate.R — Full replication of metafrontier R Journal paper
# ============================================================
# Replicates all 10 targets from:
#   Enstad (2026). "metafrontier: Unified Metafrontier Analysis
#   for Efficiency and Productivity in R." The R Journal.
#
# Run after 00-setup-renv.R:
#   source("01-replicate.R")
#
# Expected runtime: ~30 minutes (MC studies dominate)
# ============================================================

library(metafrontier)

# ---- Logging infrastructure ----
results <- list()
pass_count <- 0L
fail_count <- 0L
total_checks <- 0L

check <- function(name, condition, detail = "") {
  total_checks <<- total_checks + 1L
  if (isTRUE(condition)) {
    pass_count <<- pass_count + 1L
    cat(sprintf("  [PASS] %s\n", name))
  } else {
    fail_count <<- fail_count + 1L
    cat(sprintf("  [FAIL] %s %s\n", name, detail))
  }
}

check_numeric <- function(name, actual, expected, tol, units = "") {
  diff <- abs(actual - expected)
  ok <- diff <= tol
  detail <- sprintf("(got %.6f, expected %.6f, diff=%.6f, tol=%.6f%s)",
                    actual, expected, diff, tol,
                    if (nzchar(units)) paste0(" ", units) else "")
  check(name, ok, if (!ok) detail else "")
  invisible(ok)
}

sink_file <- file("replication-log.txt", open = "wt")
sink(sink_file, split = TRUE)  # log to file AND console

cat("============================================================\n")
cat("METAFRONTIER REPLICATION — ", format(Sys.time()), "\n")
cat("Package version:", as.character(packageVersion("metafrontier")), "\n")
cat("R version:", R.version.string, "\n")
cat("============================================================\n\n")

t_start <- Sys.time()

# ============================================================
# TARGET 1: Code examples §4.1–4.5
# ============================================================
cat("=== TARGET 1: Code examples (§4.1–4.5) ===\n\n")

# §4.1 — Simulate data
cat("--- §4.1: simulate_metafrontier ---\n")
sim <- simulate_metafrontier(n_groups = 3, n_per_group = 200, seed = 42)
dat <- sim$data

check("sim nrow = 600", nrow(dat) == 600)
check("sim ncol includes log_y,log_x1,log_x2,group,true_te,true_tgr",
      all(c("log_y","log_x1","log_x2","group","true_te","true_tgr") %in% names(dat)))

# Check first row (seed-dependent)
check_numeric("row1 log_y", dat$log_y[1], 3.371473, 0.001)
check_numeric("row1 true_te", dat$true_te[1], 0.6700025, 0.001)
check("row1 true_tgr = 1 (G1)", dat$true_tgr[1] == 1)

# §4.2 — Deterministic metafrontier
cat("\n--- §4.2: Deterministic metafrontier ---\n")
fit_det <- metafrontier(log_y ~ log_x1 + log_x2, data = dat,
                        group = "group", method = "sfa",
                        meta_type = "deterministic", dist = "hnormal")
s <- summary(fit_det)

# Group G1 coefficients
g1_coef <- s$group_tables[["G1"]][, "Estimate"]
check_numeric("G1 intercept", g1_coef["(Intercept)"], 1.04540, 0.005)
check_numeric("G1 log_x1", g1_coef["log_x1"], 0.49023, 0.005)
check_numeric("G1 log_x2", g1_coef["log_x2"], 0.19249, 0.005)

# Metafrontier coefficients
mc <- coef(fit_det)
check_numeric("meta intercept", mc["(Intercept)"], 1.0454, 0.005)
check_numeric("meta log_x1", mc["log_x1"], 0.4902, 0.005)
check_numeric("meta log_x2", mc["log_x2"], 0.1925, 0.005)

# Efficiency decomposition
eff <- s$efficiency_summary
check_numeric("G1 Mean_TE", eff$Mean_TE[eff$Group == "G1"], 0.7962, 0.005)  # v0.3.0: BC88 default (was JLMS 0.7892)
check_numeric("G1 Mean_TGR", eff$Mean_TGR[eff$Group == "G1"], 1.0000, 0.001)
check_numeric("G2 Mean_TE", eff$Mean_TE[eff$Group == "G2"], 0.8488, 0.005)  # v0.3.0: BC88 default (was JLMS 0.8435)
check_numeric("G2 Mean_TGR", eff$Mean_TGR[eff$Group == "G2"], 0.7385, 0.005)
check_numeric("G3 Mean_TE_star", eff$Mean_TE_star[eff$Group == "G3"], 0.4780, 0.005)

# TGR summary
tgr_s <- tgr_summary(fit_det)
check_numeric("G2 TGR mean", tgr_s$Mean[tgr_s$Group == "G2"], 0.7385, 0.005)
check_numeric("G3 TGR mean", tgr_s$Mean[tgr_s$Group == "G3"], 0.6223, 0.005)

# §4.3 — Stochastic metafrontier
cat("\n--- §4.3: Stochastic metafrontier ---\n")
fit_sto <- metafrontier(log_y ~ log_x1 + log_x2, data = dat,
                        group = "group", method = "sfa",
                        meta_type = "stochastic", dist = "hnormal")
sc <- coef(fit_sto)
check_numeric("stoch intercept", sc["(Intercept)"], 0.7975, 0.01)
check_numeric("stoch log_x1", sc["log_x1"], 0.4928, 0.005)
check_numeric("stoch log_x2", sc["log_x2"], 0.1867, 0.005)

ci <- confint(fit_sto)
check_numeric("stoch CI intercept lower", ci["(Intercept)", 1], 0.5580, 0.02)
check_numeric("stoch CI intercept upper", ci["(Intercept)", 2], 1.0371, 0.02)

# §4.4 — DEA metafrontier
cat("\n--- §4.4: DEA metafrontier ---\n")
dat_lev <- within(dat, {
  y <- exp(log_y)
  x1 <- exp(log_x1)
  x2 <- exp(log_x2)
})
fit_dea <- metafrontier(y ~ x1 + x2, data = dat_lev,
                        group = "group", method = "dea", rts = "vrs")
tgr_dea <- tgr_summary(fit_dea)
check_numeric("DEA G1 TGR mean", tgr_dea$Mean[tgr_dea$Group == "G1"], 0.9761, 0.01)
check_numeric("DEA G2 TGR mean", tgr_dea$Mean[tgr_dea$Group == "G2"], 0.8426, 0.01)
check_numeric("DEA G3 TGR mean", tgr_dea$Mean[tgr_dea$Group == "G3"], 0.5905, 0.01)

# §4.5 — Identity and diagnostics
cat("\n--- §4.5: Identity check and poolability ---\n")
te <- efficiencies(fit_det, type = "group")
tgr <- efficiencies(fit_det, type = "tgr")
te_star <- efficiencies(fit_det, type = "meta")
identity_ok <- isTRUE(all.equal(te_star, te * tgr, tolerance = 1e-10))
check("TE* = TE x TGR identity", identity_ok)

pt <- poolability_test(fit_det)
check_numeric("poolability LR stat", pt$statistic, 275.4, 1.0)
check("poolability p-value < 0.001", pt$p.value < 0.001)

results$target1 <- list(pass = pass_count, fail = fail_count)
cat(sprintf("\nTarget 1 summary: %d/%d checks passed\n\n",
            pass_count, total_checks))


# ============================================================
# TARGET 2: Real-data example (§4.6)
# ============================================================
cat("=== TARGET 2: Real-data example (§4.6) ===\n\n")
t2_start <- pass_count

library(sfaR)  # NOTE: masks efficiencies(); later calls are namespace-qualified
data("utility", package = "sfaR")
# regu = 1 marks plants in states that enacted retail-access legislation;
# regu = 0 marks plants that remained under traditional regulation.
utility$group <- ifelse(utility$regu == 1, "Retail access", "Regulated")
utility$log_y     <- log(utility$y)
utility$log_k     <- log(utility$k)
utility$log_labor <- log(utility$labor)
utility$log_fuel  <- log(utility$fuel)

fit_util <- metafrontier(log_y ~ log_k + log_labor + log_fuel,
                         data = utility, group = "group",
                         method = "sfa", meta_type = "deterministic")
tgr_util <- tgr_summary(fit_util)

# The regu == 0 (traditionally regulated) group has N = 297 and the higher
# mean TGR; the regu == 1 (retail access) group has N = 494. Only the group
# labels changed relative to earlier revisions; the numeric oracle values
# are unchanged.
check_numeric("utility Regulated N", tgr_util$N[tgr_util$Group == "Regulated"], 297, 0)
check_numeric("utility Retail access N", tgr_util$N[tgr_util$Group == "Retail access"], 494, 0)
check_numeric("utility Regulated TGR mean", tgr_util$Mean[tgr_util$Group == "Regulated"], 0.9458, 0.005)
check_numeric("utility Retail access TGR mean", tgr_util$Mean[tgr_util$Group == "Retail access"], 0.8920, 0.005)

t2_passed <- pass_count - t2_start
cat(sprintf("\nTarget 2 summary: %d new checks passed\n\n", t2_passed))


# ============================================================
# TARGET 3: Monte Carlo study — Table 5 (§6.1–6.3)
# ============================================================
cat("=== TARGET 3: Monte Carlo — Table 5 (§6.1–6.3) ===\n")
cat("Running 500 replications (this takes ~5 minutes)...\n\n")

n_rep <- 500
true_beta <- c(1.0, 0.5, 0.3)
true_gap <- c(0, 0.4)
true_sigma_u <- c(0.2, 0.3)
true_sigma_v <- 0.15

beta_det <- matrix(NA, n_rep, 3)
beta_sto <- matrix(NA, n_rep, 3)
tgr_rank_det <- numeric(n_rep)
tgr_rank_sto <- numeric(n_rep)
n_converged <- 0L

t3_time <- system.time({
  for (r in seq_len(n_rep)) {
    if (r %% 50 == 0) cat(sprintf("  rep %d/%d\n", r, n_rep))

    sim_r <- simulate_metafrontier(
      n_groups = 2, n_per_group = 100,
      beta_meta = true_beta,
      tech_gap = true_gap,
      sigma_u = true_sigma_u,
      sigma_v = true_sigma_v,
      seed = 1000 + r
    )

    fit_d <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_r$data,
                   group = "group", method = "sfa",
                   meta_type = "deterministic"),
      error = function(e) NULL
    )
    fit_s <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_r$data,
                   group = "group", method = "sfa",
                   meta_type = "stochastic"),
      error = function(e) NULL
    )

    if (!is.null(fit_d) && !is.null(fit_s)) {
      n_converged <- n_converged + 1L
      beta_det[r, ] <- coef(fit_d)
      beta_sto[r, ] <- coef(fit_s)

      # TGR rank correlation
      true_tgr_r <- sim_r$data$true_tgr
      tgr_rank_det[r] <- cor(fit_d$tgr, true_tgr_r, method = "spearman")
      tgr_rank_sto[r] <- cor(fit_s$tgr, true_tgr_r, method = "spearman")
    }
  }
})

cat(sprintf("\nMC completed in %.1f seconds\n", t3_time[3]))
cat(sprintf("Converged: %d / %d\n\n", n_converged, n_rep))

valid <- complete.cases(beta_det)
bd <- beta_det[valid, ]
bs <- beta_sto[valid, ]

# Compute bias
bias_det <- colMeans(bd) - true_beta
bias_sto <- colMeans(bs) - true_beta
sd_det <- apply(bd, 2, sd)
sd_sto <- apply(bs, 2, sd)

cat("--- Table 5 replication ---\n")
cat(sprintf("                  Det bias    Sto bias    Det SD      Sto SD\n"))
cat(sprintf("beta_1 (0.5):     %+.4f      %+.4f      %.4f        %.4f\n",
            bias_det[2], bias_sto[2], sd_det[2], sd_sto[2]))
cat(sprintf("beta_2 (0.3):     %+.4f      %+.4f      %.4f        %.4f\n",
            bias_det[3], bias_sto[3], sd_det[3], sd_sto[3]))
cat(sprintf("Intercept (1.0):  %+.4f      %+.4f      %.4f        %.4f\n",
            bias_det[1], bias_sto[1], sd_det[1], sd_sto[1]))

# Oracle checks — Table 5
check_numeric("MC beta1 det bias", bias_det[2], 0.0005, 0.003)
check_numeric("MC beta1 sto bias", bias_sto[2], 0.0007, 0.003)
check_numeric("MC beta2 det bias", bias_det[3], 0.0000, 0.003)
check_numeric("MC beta2 sto bias", bias_sto[3], -0.0002, 0.003)
check_numeric("MC intercept det bias", bias_det[1], -0.0269, 0.02)
check_numeric("MC intercept sto bias", bias_sto[1], -0.1683, 0.03)

# SD checks
check_numeric("MC beta1 det SD", sd_det[2], 0.0135, 0.005)
check_numeric("MC beta1 sto SD", sd_sto[2], 0.0145, 0.005)

# TGR rank correlation
mean_tgr_rank_det <- mean(tgr_rank_det[valid], na.rm = TRUE)
mean_tgr_rank_sto <- mean(tgr_rank_sto[valid], na.rm = TRUE)
cat(sprintf("\nTGR rank correlation: det=%.3f, sto=%.3f\n",
            mean_tgr_rank_det, mean_tgr_rank_sto))
check_numeric("MC TGR rank corr det", mean_tgr_rank_det, 0.883, 0.02)
check_numeric("MC TGR rank corr sto", mean_tgr_rank_sto, 0.866, 0.02)

check("MC convergence rate = 100%", n_converged == n_rep,
      sprintf("(got %d/%d)", n_converged, n_rep))

# Save MC results
results$mc_table5 <- data.frame(
  Parameter = c("beta_1", "beta_1", "beta_2", "beta_2", "Intercept", "Intercept"),
  Method = rep(c("Deterministic", "Stochastic"), 3),
  True = rep(true_beta[c(2,2,3,3,1,1)]),
  Bias = c(bias_det[2], bias_sto[2], bias_det[3], bias_sto[3], bias_det[1], bias_sto[1]),
  SD = c(sd_det[2], sd_sto[2], sd_det[3], sd_sto[3], sd_det[1], sd_sto[1])
)


# ============================================================
# TARGET 4: Small-sample MC (§6.4)
# ============================================================
cat("\n=== TARGET 4: Small-sample MC (§6.4) ===\n")
cat("Running 500 replications with n=30/group...\n\n")

tgr_rank_small <- numeric(n_rep)
n_conv_small <- 0L
beta_det_small <- matrix(NA, n_rep, 3)

t4_time <- system.time({
  for (r in seq_len(n_rep)) {
    if (r %% 100 == 0) cat(sprintf("  rep %d/%d\n", r, n_rep))

    sim_r <- simulate_metafrontier(
      n_groups = 2, n_per_group = 30,
      beta_meta = true_beta,
      tech_gap = true_gap,
      sigma_u = true_sigma_u,
      sigma_v = true_sigma_v,
      seed = 5000 + r
    )

    fit_d <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_r$data,
                   group = "group", method = "sfa",
                   meta_type = "deterministic"),
      error = function(e) NULL
    )

    if (!is.null(fit_d)) {
      n_conv_small <- n_conv_small + 1L
      beta_det_small[r, ] <- coef(fit_d)
      true_tgr_r <- sim_r$data$true_tgr
      tgr_rank_small[r] <- cor(fit_d$tgr, true_tgr_r, method = "spearman")
    }
  }
})

cat(sprintf("\nSmall-sample MC completed in %.1f seconds\n", t4_time[3]))
cat(sprintf("Converged: %d / %d\n", n_conv_small, n_rep))

valid_s <- complete.cases(beta_det_small)
bias_small <- colMeans(beta_det_small[valid_s, ]) - true_beta
mean_rank_small <- mean(tgr_rank_small[valid_s], na.rm = TRUE)

cat(sprintf("Slope biases: beta1=%.4f, beta2=%.4f\n", bias_small[2], bias_small[3]))
cat(sprintf("TGR rank correlation: %.3f\n", mean_rank_small))

check_numeric("small-sample TGR rank corr", mean_rank_small, 0.869, 0.03)
check("small-sample slope bias beta1 < 0.01", abs(bias_small[2]) < 0.01)
check("small-sample slope bias beta2 < 0.01", abs(bias_small[3]) < 0.01)


# ============================================================
# TARGET 5: Murphy–Topel SEs (Table 4)
# ============================================================
cat("\n=== TARGET 5: Murphy–Topel SEs (Table 4) ===\n\n")

se_uncorrected <- sqrt(diag(vcov(fit_sto)))
se_corrected   <- sqrt(diag(vcov(fit_sto, correction = "murphy-topel")))
ratio <- se_corrected / se_uncorrected

cat("Uncorrected SEs:", round(se_uncorrected, 4), "\n")
cat("Murphy-Topel SEs:", round(se_corrected, 4), "\n")
cat("Ratios:", round(ratio, 4), "\n")

check_numeric("MT ratio intercept", ratio[1], 1.0862, 0.05)
check_numeric("MT ratio log_x1", ratio[2], 1.6759, 0.10)
check_numeric("MT ratio log_x2", ratio[3], 1.7703, 0.10)
check("MT SEs uniformly larger", all(se_corrected >= se_uncorrected))


# ============================================================
# TARGET 6: Bootstrap CIs (Table 3)
# ============================================================
cat("\n=== TARGET 6: Bootstrap CIs (Table 3) ===\n\n")

boot <- boot_tgr(fit_det, R = 199, type = "parametric",
                 ci_type = "percentile", seed = 1, progress = FALSE)
boot_ci <- confint(boot)

# Check structure
check("boot class", inherits(boot, "boot_tgr"))
check("boot R_effective > 0", boot$R_effective > 0)
check("boot CI matrix dimensions", nrow(boot_ci) == nrow(dat) && ncol(boot_ci) == 2)

# Group-level mean TGR
ci_group <- boot$ci_group
check_numeric("boot G1 mean TGR", ci_group$Mean_TGR[ci_group$Group == "G1"], 1.0000, 0.01)
check_numeric("boot G2 mean TGR", ci_group$Mean_TGR[ci_group$Group == "G2"], 0.7385, 0.01)
check_numeric("boot G3 mean TGR", ci_group$Mean_TGR[ci_group$Group == "G3"], 0.6223, 0.01)

# CIs are non-degenerate (lower < upper for non-dominant groups)
g2_idx <- which(dat$group == "G2")
check("boot G2 CIs non-degenerate", all(boot_ci[g2_idx[1:5], 1] < boot_ci[g2_idx[1:5], 2]))


# ============================================================
# TARGET 7: Scenario A efficiency recovery (Monte Carlo evidence)
# ============================================================
# Mirrors the paper's `monte-carlo` chunk (mc_eff collection) and the
# aggregation in the `mc-eff` chunk: 500 Scenario A replications, three
# estimators (deterministic SFA, stochastic SFA, DEA VRS), mean TE, TE*
# and TGR plus Spearman rank correlations against the true values.
cat("\n=== TARGET 7: Scenario A efficiency recovery ===\n")
cat("Running 500 replications x 3 estimators (this takes several minutes)...\n\n")

set.seed(123)
n_reps <- 500
beta_true <- c(1.0, 0.5, 0.3)
mc_eff <- vector("list", n_reps)

# Efficiency-recovery statistics for one fitted metafrontier object
eff_stats <- function(fit, dat, method_label) {
  te  <- metafrontier::efficiencies(fit, type = "group")
  tes <- metafrontier::efficiencies(fit, type = "meta")
  tgr <- metafrontier::efficiencies(fit, type = "tgr")
  data.frame(
    method = method_label,
    te_true = mean(dat$true_te),   te_est = mean(te),
    rho_te = cor(te, dat$true_te, method = "spearman"),
    tes_true = mean(dat$true_te_star), tes_est = mean(tes),
    rho_tes = cor(tes, dat$true_te_star, method = "spearman"),
    tgr_true = mean(dat$true_tgr), tgr_est = mean(tgr),
    rho_tgr = suppressWarnings(
      cor(tgr, dat$true_tgr, method = "spearman"))
  )
}

t7_time <- system.time({
  for (r in seq_len(n_reps)) {
    if (r %% 50 == 0) cat(sprintf("  rep %d/%d\n", r, n_reps))

    sim_mc <- simulate_metafrontier(
      n_groups = 2, n_per_group = 100,
      beta_meta = beta_true,
      tech_gap = c(0, 0.4),
      sigma_u = c(0.2, 0.3),
      sigma_v = 0.15,
      seed = r * 1000
    )

    fit_d <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_mc$data,
                   group = "group", method = "sfa",
                   meta_type = "deterministic"),
      error = function(e) NULL
    )
    fit_s <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_mc$data,
                   group = "group", method = "sfa",
                   meta_type = "stochastic"),
      error = function(e) NULL
    )
    fit_dea <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_mc$data,
                   group = "group", method = "dea",
                   orientation = "output", rts = "vrs"),
      error = function(e) NULL
    )

    if (!is.null(fit_d) && !is.null(fit_s) && !is.null(fit_dea)) {
      mc_eff[[r]] <- rbind(
        eff_stats(fit_d,   sim_mc$data, "Deterministic SFA"),
        eff_stats(fit_s,   sim_mc$data, "Stochastic SFA"),
        eff_stats(fit_dea, sim_mc$data, "DEA (VRS)")
      )
    }
  }
})

mc_eff_df <- do.call(rbind, mc_eff)
n_eff_a <- sum(!vapply(mc_eff, is.null, logical(1)))

eff_agg <- aggregate(cbind(te_true, te_est, rho_te,
                           tes_true, tes_est, rho_tes,
                           tgr_true, tgr_est, rho_tgr) ~ method,
                     data = mc_eff_df, FUN = mean)
eff_agg <- eff_agg[match(c("Deterministic SFA", "Stochastic SFA",
                           "DEA (VRS)"), eff_agg$method), ]

cat(sprintf("\nScenario A efficiency recovery completed in %.1f seconds\n",
            t7_time[3]))
cat(sprintf("Successful replications (all 3 estimators): %d / %d\n\n",
            n_eff_a, n_reps))
cat("--- Scenario A efficiency recovery (means over replications) ---\n")
for (i in seq_len(nrow(eff_agg))) {
  cat(sprintf("%-18s TE true=%.3f est=%.3f rho=%.3f | TE* true=%.3f est=%.3f rho=%.3f | TGR true=%.3f est=%.3f rho=%.3f\n",
              eff_agg$method[i],
              eff_agg$te_true[i], eff_agg$te_est[i], eff_agg$rho_te[i],
              eff_agg$tes_true[i], eff_agg$tes_est[i], eff_agg$rho_tes[i],
              eff_agg$tgr_true[i], eff_agg$tgr_est[i], eff_agg$rho_tgr[i]))
}
# Oracle values frozen from the verified run of 2026-07-14 (v0.3.0)
det_a <- eff_agg[eff_agg$method == "Deterministic SFA", ]
dea_a <- eff_agg[eff_agg$method == "DEA (VRS)", ]
check_numeric("ScenA n successful", n_eff_a, 500, 0)
check_numeric("ScenA det TGR est mean", det_a$tgr_est, 0.840, 0.005)
check_numeric("ScenA det TGR rank corr", det_a$rho_tgr, 0.883, 0.010)
check_numeric("ScenA det TE* rank corr", det_a$rho_tes, 0.897, 0.010)
check_numeric("ScenA DEA TGR est mean", dea_a$tgr_est, 0.929, 0.005)
check_numeric("ScenA DEA TGR rank corr", dea_a$rho_tgr, 0.833, 0.010)

results$mc_eff_scenario_a <- eff_agg


# ============================================================
# TARGET 8: Scenario B efficiency recovery (Monte Carlo evidence)
# ============================================================
# Mirrors the paper's `mc-scenario-b` chunk: three groups with
# group-specific slope coefficients (beta_groups), group-specific input
# means, and within-firm log-input correlation of 0.6, so the true TGR
# varies within every group.
cat("\n=== TARGET 8: Scenario B efficiency recovery ===\n")
cat("Running 500 replications x 3 estimators (this takes several minutes)...\n\n")

set.seed(321)
beta_G <- rbind(G1 = c(1.0, 0.55, 0.25),
                G2 = c(0.9, 0.40, 0.40),
                G3 = c(0.8, 0.30, 0.55))
input_mu <- rbind(G1 = c(3.5, 2.0),
                  G2 = c(2.5, 2.5),
                  G3 = c(2.0, 3.5))
input_R <- matrix(c(1, 0.6, 0.6, 1), 2, 2)

mc_eff_b <- vector("list", n_reps)
n_b_fail <- 0L
t8_time <- system.time({
  for (r in seq_len(n_reps)) {
    if (r %% 50 == 0) cat(sprintf("  rep %d/%d\n", r, n_reps))

    sim_b <- simulate_metafrontier(
      n_groups = 3, n_per_group = 100,
      beta_groups = beta_G,
      input_means = input_mu,
      input_corr = input_R,
      sigma_u = c(0.2, 0.25, 0.3),
      sigma_v = 0.15,
      seed = r * 3000
    )
    fit_bd <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_b$data,
                   group = "group", method = "sfa",
                   meta_type = "deterministic"),
      error = function(e) NULL
    )
    fit_bs <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_b$data,
                   group = "group", method = "sfa",
                   meta_type = "stochastic"),
      error = function(e) NULL
    )
    fit_bdea <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_b$data,
                   group = "group", method = "dea",
                   orientation = "output", rts = "vrs"),
      error = function(e) NULL
    )
    if (!is.null(fit_bd) && !is.null(fit_bs) && !is.null(fit_bdea)) {
      mc_eff_b[[r]] <- rbind(
        eff_stats(fit_bd,   sim_b$data, "Deterministic SFA"),
        eff_stats(fit_bs,   sim_b$data, "Stochastic SFA"),
        eff_stats(fit_bdea, sim_b$data, "DEA (VRS)")
      )
    } else {
      n_b_fail <- n_b_fail + 1L
    }
  }
})
mc_eff_b_df <- do.call(rbind, mc_eff_b)
n_b_success <- sum(!vapply(mc_eff_b, is.null, logical(1)))

eff_agg_b <- aggregate(cbind(te_true, te_est, rho_te,
                             tes_true, tes_est, rho_tes,
                             tgr_true, tgr_est, rho_tgr) ~ method,
                       data = mc_eff_b_df, FUN = mean)
eff_agg_b <- eff_agg_b[match(c("Deterministic SFA", "Stochastic SFA",
                               "DEA (VRS)"), eff_agg_b$method), ]

cat(sprintf("\nScenario B efficiency recovery completed in %.1f seconds\n",
            t8_time[3]))
cat(sprintf("Successful replications (all 3 estimators): %d / %d (%d failed)\n\n",
            n_b_success, n_reps, n_b_fail))
cat("--- Scenario B efficiency recovery (means over successful replications) ---\n")
for (i in seq_len(nrow(eff_agg_b))) {
  cat(sprintf("%-18s TE true=%.3f est=%.3f rho=%.3f | TE* true=%.3f est=%.3f rho=%.3f | TGR true=%.3f est=%.3f rho=%.3f\n",
              eff_agg_b$method[i],
              eff_agg_b$te_true[i], eff_agg_b$te_est[i], eff_agg_b$rho_te[i],
              eff_agg_b$tes_true[i], eff_agg_b$tes_est[i], eff_agg_b$rho_tes[i],
              eff_agg_b$tgr_true[i], eff_agg_b$tgr_est[i], eff_agg_b$rho_tgr[i]))
}
# Oracle values frozen from the verified run of 2026-07-14 (v0.3.0)
det_b <- eff_agg_b[eff_agg_b$method == "Deterministic SFA", ]
dea_b <- eff_agg_b[eff_agg_b$method == "DEA (VRS)", ]
check_numeric("ScenB det TGR est mean", det_b$tgr_est, 0.608, 0.005)
check_numeric("ScenB true TGR mean", det_b$tgr_true, 0.925, 0.005)
check_numeric("ScenB det TGR rank corr", det_b$rho_tgr, 0.761, 0.010)
check_numeric("ScenB DEA TGR est mean", dea_b$tgr_est, 0.902, 0.005)
check_numeric("ScenB DEA TGR rank corr", dea_b$rho_tgr, 0.731, 0.010)

results$mc_eff_scenario_b <- eff_agg_b


# ============================================================
# TARGET 9: LP vs QP TGR difference (Monte Carlo evidence)
# ============================================================
# Mirrors the paper's `mc-lpqp` chunk: refit the deterministic
# metafrontier under both Battese-Rao-O'Donnell (2004) identification
# criteria (LP: minimum sum of absolute deviations; QP: minimum sum of
# squared deviations) on the first 50 Scenario A designs and report the
# maximum absolute TGR difference.
cat("\n=== TARGET 9: LP vs QP TGR difference ===\n\n")

lpqp_reps <- 50
lpqp_diff <- numeric(lpqp_reps)
for (r in seq_len(lpqp_reps)) {
  sim_lq <- simulate_metafrontier(
    n_groups = 2, n_per_group = 100, beta_meta = c(1.0, 0.5, 0.3),
    tech_gap = c(0, 0.4), sigma_u = c(0.2, 0.3), sigma_v = 0.15,
    seed = r * 1000
  )
  f_lp <- metafrontier(log_y ~ log_x1 + log_x2, data = sim_lq$data,
                       group = "group", objective = "lp")
  f_qp <- metafrontier(log_y ~ log_x1 + log_x2, data = sim_lq$data,
                       group = "group", objective = "qp")
  lpqp_diff[r] <- max(abs(f_lp$tgr - f_qp$tgr))
}
max_lpqp_diff <- max(lpqp_diff)

cat(sprintf("Max abs LP vs QP TGR difference over %d designs: %.6g\n",
            lpqp_reps, max_lpqp_diff))
# Oracle frozen from the verified run of 2026-07-14 (v0.3.0)
check_numeric("LP vs QP max TGR diff < 1e-9", as.numeric(max_lpqp_diff < 1e-9), 1, 0)

results$max_lpqp_diff <- max_lpqp_diff


# ============================================================
# TARGET 10: Unbalanced groups (Monte Carlo evidence)
# ============================================================
# Mirrors the paper's `mc-unbalanced` chunk: 200 firms in the
# frontier-defining group and 20 in the lagging group (10:1 ratio, with
# the technology gap on the small group), all other Scenario A
# parameters unchanged.
cat("\n=== TARGET 10: Unbalanced groups (200 vs 20 firms) ===\n")
cat("Running 500 replications...\n\n")

set.seed(789)
n_reps_unb <- 500
mc_unb <- vector("list", n_reps_unb)

t10_time <- system.time({
  for (r in seq_len(n_reps_unb)) {
    if (r %% 100 == 0) cat(sprintf("  rep %d/%d\n", r, n_reps_unb))

    sim_u <- simulate_metafrontier(
      n_groups = 2, n_per_group = c(200, 20),
      beta_meta = c(1.0, 0.5, 0.3),
      tech_gap = c(0, 0.4),
      sigma_u = c(0.2, 0.3),
      sigma_v = 0.15,
      seed = r * 4000
    )
    fit_u <- tryCatch(
      metafrontier(log_y ~ log_x1 + log_x2, data = sim_u$data,
                   group = "group", method = "sfa",
                   meta_type = "deterministic"),
      error = function(e) NULL
    )
    if (!is.null(fit_u)) {
      small_idx <- sim_u$data$group == "G2"
      tgr_u <- metafrontier::efficiencies(fit_u, type = "tgr")
      codes <- c(vapply(fit_u$group_models,
                        function(m) m$convergence, integer(1)),
                 fit_u$meta_convergence)
      mc_unb[[r]] <- list(
        coef = coef(fit_u),
        all_zero = all(codes == 0L),
        tgr_cor = cor(tgr_u, sim_u$data$true_tgr, method = "spearman"),
        tgr_bias_small = mean(tgr_u[small_idx]) -
          mean(sim_u$data$true_tgr[small_idx])
      )
    }
  }
})
mc_unb <- mc_unb[!sapply(mc_unb, is.null)]
n_unb_ok <- length(mc_unb)
n_unb_zero <- sum(sapply(mc_unb, function(x) x$all_zero))
unb_bias <- rowMeans(sapply(mc_unb, function(x) x$coef)) - c(1.0, 0.5, 0.3)
unb_tgr_cor <- mean(sapply(mc_unb, function(x) x$tgr_cor))
unb_tgr_bias_small <- mean(sapply(mc_unb, function(x) x$tgr_bias_small))

cat(sprintf("\nUnbalanced MC completed in %.1f seconds\n", t10_time[3]))
cat(sprintf("Replications completed without error: %d / %d\n",
            n_unb_ok, n_reps_unb))
cat(sprintf("Replications with convergence code zero at every stage: %d\n",
            n_unb_zero))
cat(sprintf("Slope biases: beta1=%.4f, beta2=%.4f\n",
            unb_bias[2], unb_bias[3]))
cat(sprintf("Overall TGR rank correlation: %.3f\n", unb_tgr_cor))
cat(sprintf("Mean TGR bias in the 20-firm group: %.4f\n",
            unb_tgr_bias_small))
# Oracle values frozen from the verified run of 2026-07-14 (v0.3.0)
check_numeric("Unbalanced n complete", n_unb_ok, 500, 0)
check_numeric("Unbalanced slope bias beta1", unb_bias[2], -0.0002, 0.005)
check_numeric("Unbalanced slope bias beta2", unb_bias[3], -0.0010, 0.005)
check_numeric("Unbalanced TGR rank corr", unb_tgr_cor, 0.598, 0.010)
check_numeric("Unbalanced small-group TGR bias", unb_tgr_bias_small, -0.0188, 0.005)

results$unbalanced <- list(
  n_complete = n_unb_ok,
  n_all_zero = n_unb_zero,
  slope_bias = unb_bias,
  tgr_rank_cor = unb_tgr_cor,
  tgr_bias_small = unb_tgr_bias_small
)


# ============================================================
# SUMMARY
# ============================================================
t_end <- Sys.time()
elapsed <- as.numeric(difftime(t_end, t_start, units = "mins"))

cat("\n============================================================\n")
cat("REPLICATION COMPLETE\n")
cat("============================================================\n")
cat(sprintf("Total time: %.1f minutes\n", elapsed))
cat(sprintf("Checks passed: %d / %d\n", pass_count, total_checks))
cat(sprintf("Checks failed: %d / %d\n", fail_count, total_checks))

if (fail_count == 0) {
  cat("\n*** ALL CHECKS PASSED — FULL REPLICATION SUCCESSFUL ***\n")
} else {
  cat(sprintf("\n*** %d CHECKS FAILED — SEE LOG FOR DETAILS ***\n", fail_count))
}

cat("\nResults saved to: replication-log.txt\n")
cat("============================================================\n")

cat("\n--- sessionInfo() ---\n")
print(sessionInfo())

sink()
close(sink_file)

# Save structured results
saveRDS(
  list(
    timestamp = Sys.time(),
    pkg_version = as.character(packageVersion("metafrontier")),
    r_version = R.version.string,
    pass = pass_count,
    fail = fail_count,
    total = total_checks,
    elapsed_min = elapsed,
    mc_table5 = results$mc_table5,
    mc_eff_scenario_a = results$mc_eff_scenario_a,
    mc_eff_scenario_b = results$mc_eff_scenario_b,
    max_lpqp_diff = results$max_lpqp_diff,
    unbalanced = results$unbalanced
  ),
  file = "replication-results.rds"
)

# ------------------------------------------------------------
# Comparison for Mills ratio: package vs naive vs high-precision
# m(d) = Phi(-d) / phi(-d), for d >= 0
#
# Uses:
#   - BayesianLasso::MillsRatio(d)   (package implementation)
#   - naive: pnorm(-d)/dnorm(-d)     (standard library, may underflow)
#   - mpfr reference via erfc        (high precision)
#
# ------------------------------------------------------------

# install.packages(c("BayesianLasso","Rmpfr","ggplot2"))
suppressPackageStartupMessages({
  library(BayesianLasso)  # provides MillsRatio(d) :contentReference[oaicite:1]{index=1}
  library(Rmpfr)
  library(ggplot2)
})

# ---- 1) Package Mills ratio (documented for scalar d) ----
mills_pkg <- function(d) {
  # docs show d is scalar; vectorize safely:
  sapply(d, BayesianLasso::MillsRatio)
}

# ---- 2) Naive library evaluation ----
mills_naive <- function(d) pnorm(-d) / dnorm(-d)

# ---- 3) High-precision reference using mpfr + erfc ----
mills_ref_mpfr <- function(d, precBits = 256) {
  stopifnot(is.numeric(d), all(is.finite(d)), all(d >= 0))
  d_mp <- mpfr(d, precBits)
  
  # Phi(-d) = 0.5 * erfc(d / sqrt(2))
  logPhi <- log(mpfr(0.5, precBits)) + log(erfc(d_mp / sqrt(mpfr(2, precBits))))
  
  # phi(-d) = (1/sqrt(2*pi)) * exp(-d^2/2)
  logphi <- -mpfr(0.5, precBits) * d_mp^2 - mpfr(0.5, precBits) * log(2 * pi)
  
  m_mp <- exp(logPhi - logphi)
  asNumeric(m_mp)  # convert to double for reporting
}

# ---- 4) Run comparison on grids (adjust as needed) ----
grid_0_600 <- c(seq(0, 10, length.out = 2001),
                seq(10, 50, length.out = 2001),
                seq(50, 600, length.out = 4001))

grid_600_2000 <- c(seq(600, 800, length.out = 1001),
                   seq(800, 1200, length.out = 1001),
                   seq(1200, 2000, length.out = 1601))

run_compare <- function(d, label, precBits = 256) {
  ref   <- mills_ref_mpfr(d, precBits = precBits)
  pkg   <- mills_pkg(d)
  naive <- mills_naive(d)
  
  out <- data.frame(
    grid = label,
    d = d,
    ref = ref,
    pkg = pkg,
    naive = naive
  )
  
  # Relative errors where defined
  out$rel_err_pkg <- abs(out$pkg / out$ref - 1)
  
  out$naive_ok <- is.finite(out$naive) & out$naive > 0
  out$rel_err_naive <- NA_real_
  out$rel_err_naive[out$naive_ok] <- abs(out$naive[out$naive_ok] / out$ref[out$naive_ok] - 1)
  
  # Digits ~ -log10(rel_err)
  out$digits_pkg <- pmin(20, -log10(pmax(out$rel_err_pkg, .Machine$double.xmin)))
  
  # Summary
  cat("\n=== Grid:", label, "===\n")
  cat("n =", nrow(out), "\n")
  cat("Package vs ref: max rel err =", format(max(out$rel_err_pkg), scientific = TRUE, digits = 4), "\n")
  cat("Package vs ref: median rel err =", format(median(out$rel_err_pkg), scientific = TRUE, digits = 4), "\n")
  cat("Package digits: min =", round(min(out$digits_pkg), 3),
      " median =", round(median(out$digits_pkg), 3), "\n")
  
  cat("Naive finite/positive fraction =", round(mean(out$naive_ok), 4), "\n")
  if (any(out$naive_ok)) {
    cat("Naive vs ref (where finite): max rel err =",
        format(max(out$rel_err_naive, na.rm = TRUE), scientific = TRUE, digits = 4), "\n")
  }
  
  out
}

df1 <- run_compare(grid_0_600,   "[0,600]",     precBits = 256)
df2 <- run_compare(grid_600_2000,"(600,2000]",  precBits = 256)
df  <- rbind(df1, df2)

# ---- 5) Plots ----
# (a) Digits of accuracy for package method
p_digits <- ggplot(df, aes(d, digits_pkg)) +
  geom_point(size = 0.25, alpha = 0.5) +
  labs(
    title = "Mills ratio: package accuracy vs high-precision reference",
    subtitle = "Digits ≈ -log10(relative error)",
    x = "d",
    y = "Approx. correct significant digits"
  ) +
  theme_minimal(base_size = 12)
print(p_digits)

# (b) Relative error (package) on log scale
p_rel <- ggplot(df, aes(d, rel_err_pkg)) +
  geom_point(size = 0.25, alpha = 0.5) +
  scale_y_log10() +
  labs(
    title = "Mills ratio: relative error of package implementation",
    subtitle = "Compared to mpfr reference (log scale)",
    x = "d",
    y = "Relative error"
  ) +
  theme_minimal(base_size = 12)
print(p_rel)

# (c) Optional: show naive failures (where it becomes Inf/NaN/0)
df_naive_bad <- subset(df, !naive_ok)
cat("\nNaive evaluation failures (count):", nrow(df_naive_bad), "\n")
if (nrow(df_naive_bad) > 0) {
  cat("First few d where naive fails:\n")
  print(head(df_naive_bad$d, 20))
}

# ---- 6) Optional: compact table at selected points ----
d_pts <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, 400, 600, 800, 1200, 2000)
tab <- run_compare(d_pts, "selected points", precBits = 256)
tab_show <- tab[, c("d","ref","pkg","naive","rel_err_pkg","rel_err_naive","digits_pkg","naive_ok")]
print(tab_show, row.names = FALSE)
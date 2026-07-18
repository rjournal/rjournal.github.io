# ============= kernels =============
library(CovEsts)
x <- c(0.2, 0.4, 0.6)
theta <- 0.9
kernel_ec(x, "gaussian", c(theta))
nu <- 1
dim <- 1
kernel_ec(x, "bessel_j", c(theta, nu, dim))

window_ec(x, "tukey")
window_ec(x, "blackman", c(0.16))

x <- c(-0.4, -0.2, 0, 0.2, 0.4)
kernel_symm_ec(x, "gaussian", c(theta))

window_symm_ec(x, "blackman", c(0.16))

# plots of kernels, symmetric kernels and windows
x <- seq(0, 20, by = 0.005)
theta <- 2
nu <- 1
dim <- 1
a <- 1
b <- 1

plot(x, kernel_ec(x, 'gaussian', params=c(theta)), type='l', lwd=2, lty=2, col=1, ylim=c(-0.2, 1), xlab = 'x', ylab='a(x)', cex.axis=2, cex.lab=2)
lines(x, kernel_ec(x, 'exponential', params=c(theta)), type='l', lwd=2, lty=3, col=2)
lines(x, kernel_ec(x, 'wave', params=c(theta)), type='l', lwd=2, lty=4, col=3)
lines(x, kernel_ec(x, 'rational_quadratic', params=c(theta)), type='l', lwd=2, lty=5, col=4)
lines(x, kernel_ec(x, 'spherical', params=c(theta)), type='l', lwd=2, lty=6, col=5)
lines(x, kernel_ec(x, 'circular', params=c(theta)), type='l', lwd=2, lty=11, col=6)
lines(x, kernel_ec(x, 'bessel_j', params=c(theta, nu, dim)), type='l', lwd=2, lty=8, col=7)
lines(x, kernel_ec(x, 'matern', params=c(theta, nu)), type='l', lwd=2, lty=9, col=8)
lines(x, kernel_ec(x, 'cauchy', params=c(theta, a, b)), type='l', lwd=2, lty=10, col=10)

ltys <- c(2, 3, 4, 5, 6, 11, 8, 9, 10)
colours <- c(1:8, 10)
legend('topright', c('gaussian', 'exponential', 'wave', 'rational_quadratic', 'spherical', 'circular', 'bessel_j', 'matern', 'cauchy'),
       col=colours, lty=c(ltys), lwd=rep(2, 9), y.intersp=1.05, cex=1.5)

# Symmetric kernels
x <- seq(-20, 20, by = 0.005)

plot(x, kernel_symm_ec(x, 'gaussian', params=c(theta)), type='l', lwd=2, lty=2, col=1, ylim=c(-0.03, 0.4), xlab = 'x', ylab='a(x)', cex.axis=2, cex.lab=2)
lines(x, kernel_symm_ec(x, 'wave', params=c(theta)), type='l', lwd=2, lty=3, col=2)
lines(x, kernel_symm_ec(x, 'rational_quadratic', params=c(theta)), type='l', lwd=2, lty=4, col=3)
lines(x, kernel_symm_ec(x, 'bessel_j', params=c(theta, nu, dim)), type='l', lwd=2, lty=5, col=4)

ltys <- c(2, 3, 4, 5)
colours <- 1:4
legend('topright', c('gaussian', 'wave', 'rational_quadratic', 'bessel_j'),
       col=colours, lty=c(ltys), lwd=rep(2, 3), y.intersp=1.05, cex=1.5)

# Windows
x <- seq(0, 1, by = 0.005)
plot(x, window_ec(x, 'tukey'), type='l', lwd=2, lty=2, col=1, ylim=c(0, 1), xlim=c(0, 1.3), xlab = 'x', ylab='w(x)', cex.axis=2, cex.lab=2)
lines(x, window_ec(x, 'triangular'), type='l', lwd=2, lty=4, col=3)
lines(x, window_ec(x, 'sine'), type='l', lwd=2, lty=5, col=4)
lines(x, window_ec(x, 'power_sine', params=c(0.3)), type='l', lwd=2, lty=6, col=6)
lines(x, window_ec(x, 'blackman', params=c(0.16)), type='l', lwd=2, lty=11, col=7)
lines(x, window_ec(x, 'hann_poisson', params=c(theta)), type='l', lwd=2, lty=8, col=8)
lines(x, window_ec(x, 'welch'), type='l', lwd=2, lty=9, col=9)

ltys <- c(2, 4, 5, 6, 11, 8, 9)
colours <- c(1, 3, 4, 6:9)
legend('bottomright', c('tukey', 'triangular', 'sine', 'power_sine', 'blackman', 'hann_poisson', 'welch'),
       col=colours, lty=c(ltys), lwd=rep(2, 7), y.intersp=1, cex=1.65, ncol=1)

# Symmetric Windows
x <- seq(-1.2, 1.2, by = 0.005)
plot(x, window_symm_ec(x, 'tukey'), type='l', lwd=2, lty=2, col=1, xlim=c(-1.1, 1.575), ylim=c(0, 1.05), xlab = 'x', ylab='w(x)', cex.axis=2, cex.lab=2, xaxt='n', yaxt='n')
axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, by=0.2), cex.axis=1.8, las=2)
axis(1, at = seq(-1, 1, 0.2), labels = seq(-1, 1, by=0.2), cex.axis=1.8, las=1)
lines(x, window_symm_ec(x, 'triangular'), type='l', lwd=2, lty=4, col=3)
lines(x, window_symm_ec(x, 'sine'), type='l', lwd=2, lty=5, col=4)
lines(x, window_symm_ec(x, 'power_sine', params=c(0.3)), type='l', lwd=2, lty=6, col=6)
lines(x, window_symm_ec(x, 'blackman', params=c(0.16)), type='l', lwd=2, lty=11, col=7)
lines(x, window_symm_ec(x, 'hann_poisson', params=c(theta)), type='l', lwd=2, lty=8, col=8)
lines(x, window_symm_ec(x, 'welch'), type='l', lwd=2, lty=9, col=9)

ltys <- c(2, 4, 5, 6, 11, 8, 9)
colours <- c(1, 3, 4, 6:9)
legend('topright', c('tukey', 'triangular', 'sine', 'power_sine', 'blackman', 'hann_poisson', 'welch'),
       col=colours, lty=ltys, lwd=rep(2, 7), y.intersp=1, x.intersp = 0.3, cex=1.45, ncol=1)

# ============= Gaussian Covariance =============
set.seed(135)
N <- 2001
x <- seq(0, 40, length.out = N)
Z <- rnorm(N)
dist_mat <- abs(outer(x, x, '-'))
cov_mat <- exp(- (dist_mat^2))

eig <- eigen(cov_mat)
X <- as.vector((eig$vectors %*% sqrt(diag(zapsmall(eig$values)))) %*% Z)

maxLag <- 251

t <- x[1:maxLag]

# standard estimators
Cs <- standard_est(X, maxLag = maxLag - 1, pd = FALSE, x = x)
Css <- standard_est(X, maxLag = maxLag - 1, pd = TRUE, x = x, type = "autocorrelation")

# Hall's estimators
hall_1 <- adjusted_est(X, x, t, 0.1, "gaussian", type = "autocorrelation")
hall_2 <- truncated_est(X, x, t, 3, 4, 0.1, "gaussian", type = "autocorrelation")

# tapered
tapered <- tapered_est(X, 1, "tukey", maxLag = maxLag - 1, x = x, type = "autocorrelation")

# splines
splines <- splines_est(X, x, Cs, 3, 2, maxLag = maxLag - 1, type = "autocorrelation")
Cs <- normalise_acf(Cs)

# Correction
corrected <- corrected_est(X, "gaussian", N_T=5*length(X), maxLag = maxLag - 1, x = x, type = "autocorrelation")

# Plot
par(mar=c(4,5.25,0.25,0.25)+.1)
plot(x[1:maxLag], exp(-x[1:maxLag]^2), type='l', lwd=2, ylim=c(-0.3, 1), xlab=expression(h), ylab=expression(hat(rho)*'(h)'), cex.axis=2, cex.lab=2)
lines(Cs, lwd=3, lty=2, col=2)
lines(Css, lwd=3, lty=3, col=3)
lines(hall_1, lwd=3, lty=4, col=4)
lines(hall_2, lwd=3, lty=5, col=6)
lines(tapered, lwd=3, lty=6, col=7)
lines(splines, lwd=3, lty=7, col=8)
lines(corrected, lwd=3, lty=8, col=13)

legend('topright', c('True', expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                     expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)'), expression(hat('C')^'B'*('h')), expression('C'[T]^'(a)'*'(h)')),
       col=c(1, 2, 3, 4, 6, 7, 8, 13), lty=c(1, 2, 3, 4, 5, 6, 7, 8), lwd=c(2, rep(3, 7)), y.intersp=1, x.intersp = 0.3, cex=1.6, ncol=2)

plot(x, X, type='l', lwd=2, cex.axis=2, cex.lab=2)

# Compare some estimates
area_between(hall_1, hall_2, plot=T)
max_distance(hall_1, hall_2, plot=T)
spectral_norm(Cs, Css)
mse(Cs, Css)
hilbert_schmidt(Cs, Css)
check_pd(Cs)
check_pd(Css)
check_pd(make_pd(Cs))
check_pd(nearest_pd(Cs))

# bootstrap
plot(block_bootstrap(X, maxLag, x, l = maxLag), ylim=c(-0.25, 1), cex.axis=2, cex.lab=2)
plot(block_bootstrap(X, maxLag, x, l = maxLag, boot_type = 'circular'), ylim=c(-0.25, 1), cex.axis=2, cex.lab=2)

# ============= sunspots.year =============
X <- as.vector(sunspot.year)
x <- 1:length(X)
maxLag <- 128

# standard estimators
Cs <- standard_est(X, maxLag = maxLag - 1, pd = FALSE, meanX = mean(X), x = x, type = "autocorrelation")
Css <- standard_est(X, maxLag = maxLag - 1, pd = TRUE, meanX = mean(X), x = x, type = "autocorrelation")

# Hall's estimators
hall_1 <- adjusted_est(X, x, x[1:maxLag], b = 0.1, kernel_name = "wave", type = "autocorrelation")
hall_2 <- adjusted_est(X, x, x[1:maxLag], b = 0.1, kernel_name = "wave", pd = FALSE, type = "autocorrelation")

# tapered
tapered <- tapered_est(X, 0.01, "tukey", maxLag = maxLag - 1, x = x, type = "autocorrelation")

par(mar=c(4,5.25,0.25,0.25)+.1)

plot(Cs, lwd=3, lty=2, col=2, type='l', ylim=c(-0.5, 1), xlab=expression(h), ylab=expression(hat(rho)*'(h)'), cex.axis=2, cex.lab=2)
lines(Css, lwd=3, lty=3, col=3)
lines(hall_1, lwd=3, lty=4, col=4)
lines(hall_2, lwd=3, lty=5, col=6)
lines(tapered, lwd=3, lty=6, col=7)

legend('topright', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                     expression(hat('C')[H]^'1'*'(h)'), expression(hat('C')[N]^'a'*'(h)')),
       col=c(2, 3, 4, 6, 7), lty=c(2, 3, 4, 5, 6), lwd=c(rep(3, 5)),  y.intersp=1, x.intersp = 0.3, cex=1.6, ncol=3)

plot(1700:1988, as.vector(sunspot.year), type='l', xlab='Year', ylab='sunspot.year', lwd=2, cex.axis=2, cex.lab=2)

# smoothing/correcton
Cs_smooth <- kernel_est(Cs, 'wave', N_T = 50, maxLag = length(Cs) - 1)
Cs_smooth_gauss <- kernel_est(Cs, 'gaussian', N_T = 4000, maxLag = length(Cs) - 1)

plot(Cs, lwd=3, lty=2, col=2, type='l', ylim=c(-0.4, 1), xlab=expression(h), ylab=expression(hat(rho)*'(h)'), cex.axis=2, cex.lab=2)
lines(Cs_smooth, lwd=3, lty=6, col=12)
lines(Cs_smooth_gauss, lwd=3, lty=6, col=15)

legend('topright', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                     expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)')),
       col=c(2, 3, 4, 6, 7), lty=c(2, 3, 4, 5, 6), lwd=c(rep(3, 5)), y.intersp=1, cex=1.7, ncol=3)

# ============= BLS data =============
library(zoo)
bls <- read.csv("data/BLS.csv", header=T, row.names = 1)

bls_vec <- as.vector(unlist(t(bls)))
X_bls <- c()
for(i in 2:740) {
  X_bls <- c(X_bls, bls_vec[i] - bls_vec[i-1])
}

# 12 years
maxLag <- 144
X <- X_bls
x <- 1:length(X)

# standard estimators
Cs <- standard_est(X, maxLag = maxLag - 1, pd = FALSE, x = x)
Css <- standard_est(X, maxLag = maxLag - 1, pd = TRUE, x = x, type = "autocorrelation")

# Hall's estimators
hall_1 <- adjusted_est(X, x, x[1:maxLag], b =0.1, kernel_name = "rational_quadratic", type = "autocorrelation")
hall_2 <- truncated_est(X, x, x[1:maxLag], 110, 120, b = 0.1, kernel_name = "rational_quadratic", type = "autocorrelation")

# tapered
tapered <- tapered_est(X, 1, "tukey", maxLag = maxLag - 1, x = x, type = "autocorrelation")

# splines
splines <- splines_est(X, x, Cs, 3, 2, maxLag = maxLag - 1, type = "autocorrelation")
Cs <- normalise_acf(Cs)

# Correction
corrected <- corrected_est(X, "rational_quadratic", N_T = 5*length(X), maxLag = maxLag - 1, x = x, type = "autocorrelation")

colours <- c(2, 3, 4, 6, 7, 8, 13)
ltys <- c(2, 3, 4, 5, 6, 7, 8)

par(mar=c(4,5.25,0.25,0.25)+.1)

plot(Cs, lwd=3, lty=2, col=2, type='l', ylim=c(-0.3, 1), xlab=expression(h), ylab=expression(hat(rho)*'(h)'), cex.axis=2, cex.lab=2)
lines(Css, lwd=3, lty=3, col=3)
lines(hall_1, lwd=3, lty=4, col=4)
lines(hall_2, lwd=3, lty=5, col=6)
lines(tapered, lwd=3, lty=6, col=7)
lines(splines, lwd=3, lty=7, col=8)
lines(corrected, lwd=3, lty=8, col=13)

legend('topright', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                     expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)'), expression(hat('C')^'B'*('h')), expression('C'^'(a)'*'(h)')),
       col=colours, lty=ltys, lwd=c(2, rep(3, 6)), y.intersp=1, x.intersp = 0.3, cex=1.6, ncol=3)


plot(x, X, type='l', lwd=2, cex.axis=2, cex.lab=2)

smooth_est <- function(est, n) {
  zoo_mean_3 <- rollmean(est$acf, 3)
  zoo_mean_n <- rollmean(est$acf, n)
  est$acf <- (c(est$acf[1], zoo_mean_3[1:(floor(n / 2) - 1)], zoo_mean_n, zoo_mean_3[(length(est$acf) - floor(n/2)):(length(est$acf) - 2)], est$acf[length(est$acf)]))
  return(est)
}

smooth_Cs <- smooth_est(Cs, 13)
smooth_Css <- smooth_est(Css, 13)
smooth_hall_1 <- smooth_est(hall_1, 13)
smooth_hall_2 <- smooth_est(hall_2, 13)
smooth_tapered <- smooth_est(tapered, 13)
smooth_splines <- smooth_est(splines, 13)
smooth_corrected <- smooth_est(corrected, 13)

par(mar=c(4,5.25,0.25,0.25)+.1)
plot(smooth_Cs, lwd=3, lty=2, col=2, type='l', ylim=c(-0.2, 1.05), xlab=expression(h), ylab=expression(hat(rho)*'(h)'), cex.axis=2, cex.lab=2)
lines(smooth_Css, lwd=3, lty=3, col=3)
lines(smooth_hall_1, lwd=3, lty=4, col=4)
lines(smooth_hall_2, lwd=3, lty=5, col=6)
lines(smooth_tapered, lwd=3, lty=6, col=7)
lines(smooth_splines, lwd=3, lty=7, col=8)
lines(smooth_corrected, lwd=3, lty=8, col=13)

legend('topright', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                     expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)'), expression(hat('C')^'B'*('h')), expression('C'^'(a)'*'(h)')),
       col=colours, lty=ltys, lwd=c(2, rep(3, 6)), y.intersp=1, x.intersp = 0.3, cex=1.6, ncol=2)

# ============= parallel benchmarks =============
# Note:
# This code took ~30 minutes to run on an Desktop running Windows 10, AMD 5600X CPU with 32GB DDR4 RAM
# The file `101_1001_by50_mem_100_iters.RData` is a saved workspace image after running this parallel loop
# and computing the data frames with results. The code to load this file is on line 422.

library(foreach)
library(doParallel)
library(bench)
library(CovEsts)

registerDoParallel(detectCores() - 1)

get_realisation <- function(N = 1001) {
  x <- seq(0, 40, length.out = N)
  Z <- rnorm(N)
  dist_mat <- abs(outer(x, x, '-'))
  cov_mat <- exp(- (dist_mat^2))

  eig <- eigen(cov_mat)
  X <- as.vector((eig$vectors %*% sqrt(diag(zapsmall(eig$values)))) %*% Z)
  meanX <- mean(X)

  return(list(x=x, X=X, meanX = meanX))
}

kernel_name <- 'gaussian'
b <- 0.1
kernel_params <- c()

Ns <- seq(101, 1001, by = 50)
iters <- 100
outerList <- list()
sT <- Sys.time()
outerList <- foreach(j=1:iters, .packages = c("bench", "CovEsts")) %dopar% {
  innerList <- list()
  for(i in 1:length(Ns)) {
    N <- Ns[i]
    maxLag <- ceiling(N * 0.125)

    realisation <- get_realisation(N)

    x <- realisation$x
    X <- realisation$X

    t <- x[1:maxLag]

    resList <- list(N=N)

    std_est_bench <- mark(standard_est(X, maxLag = maxLag - 1, pd = FALSE), min_time = 0.001, iterations = 1)
    resList[["std_est_df"]] <- cbind(as.numeric(std_est_bench$total_time), as.numeric(std_est_bench$mem_alloc))

    std_est_pd_bench <- mark(standard_est(X, maxLag = maxLag - 1), min_time = 0.001, iterations = 1)
    resList[["std_est_pd_df"]] <- cbind(as.numeric(std_est_pd_bench$total_time), as.numeric(std_est_pd_bench$mem_alloc))

    hall_1_bench <- mark(adjusted_est(X, x, t, b, kernel_name), min_time = 0.001, iterations = 1)
    resList[["hall_1_df"]] <- cbind(as.numeric(hall_1_bench$total_time), as.numeric(hall_1_bench$mem_alloc))

    hall_2_bench <- mark(truncated_est(X, x, t, 3, 4, b, kernel_name), min_time = 0.001, iterations = 1)
    resList[["hall_2_df"]] <- cbind(as.numeric(hall_2_bench$total_time), as.numeric(hall_2_bench$mem_alloc))

    tapered_bench <- mark(tapered_est(X, 0.5, 'tukey'), min_time = 0.001, iterations = 1)
    resList[["tapered_df"]] <- cbind(as.numeric(tapered_bench$total_time), as.numeric(tapered_bench$mem_alloc))

    splines_bench <- mark(splines_est(X, x, std_est_pd_bench$result[[1]], 3, 2, maxLag = maxLag - 1), min_time = 0.001, iterations = 1)
    resList[["splines_df"]] <- cbind(as.numeric(splines_bench$total_time), as.numeric(splines_bench$mem_alloc))

    corrected_bench <- mark(corrected_est(X, "gaussian", N_T=5*length(X), maxLag = maxLag - 1), min_time = 0.001, iterations = 1)
    resList[["corrected_df"]] <- cbind(as.numeric(corrected_bench$total_time), as.numeric(corrected_bench$mem_alloc))

    innerList[[i]] <- resList
  }
  return(innerList)
}
eT <- Sys.time()
print(eT - sT)

std_est_time <- numeric(iters)
std_est_pd_time <- numeric(iters)
hall_1_time <- numeric(iters)
hall_2_time <- numeric(iters)
tapered_time <- numeric(iters)
splines_time <- numeric(iters)
corrected_time <- numeric(iters)

std_est_mem <- numeric(iters)
std_est_pd_mem <- numeric(iters)
hall_1_mem <- numeric(iters)
hall_2_mem <- numeric(iters)
tapered_mem <- numeric(iters)
splines_mem <- numeric(iters)
corrected_mem <- numeric(iters)

std_est_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
std_est_pd_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
hall_1_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
hall_2_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
tapered_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
splines_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))
corrected_df <- data.frame(time=numeric(length(Ns)), mem=numeric(length(Ns)), mem_gb=numeric(length(Ns)), mem_mb=numeric(length(Ns)))

for(n in 1:length(Ns)) {
  for(i in 1:iters) {
    std_est_time[i] <- outerList[[i]][[n]]$std_est_df[1]
    std_est_pd_time[i] <- outerList[[i]][[n]]$std_est_pd_df[1]
    hall_1_time[i] <- outerList[[i]][[n]]$hall_1_df[1]
    hall_2_time[i] <- outerList[[i]][[n]]$hall_2_df[1]
    tapered_time[i] <- outerList[[i]][[n]]$tapered_df[1]
    splines_time[i] <- outerList[[i]][[n]]$splines_df[1]
    corrected_time[i] <- outerList[[i]][[n]]$corrected_df[1]

    std_est_mem[i] <- outerList[[i]][[n]]$std_est_df[2]
    std_est_pd_mem[i] <- outerList[[i]][[n]]$std_est_pd_df[2]
    hall_1_mem[i] <- outerList[[i]][[n]]$hall_1_df[2]
    hall_2_mem[i] <- outerList[[i]][[n]]$hall_2_df[2]
    tapered_mem[i] <- outerList[[i]][[n]]$tapered_df[2]
    splines_mem[i] <- outerList[[i]][[n]]$splines_df[2]
    corrected_mem[i] <- outerList[[i]][[n]]$corrected_df[2]
  }

  std_est_df[n, ] <- c(median(std_est_time), median(std_est_mem), 0, 0)
  std_est_pd_df[n, ] <- c(median(std_est_pd_time), median(std_est_pd_mem), 0, 0)
  hall_1_df[n, ] <- c(median(hall_1_time), median(hall_1_mem), 0, 0)
  hall_2_df[n, ] <- c(median(hall_2_time), median(hall_2_mem), 0, 0)
  tapered_df[n, ] <- c(median(tapered_time), median(tapered_mem), 0, 0)
  splines_df[n, ] <- c(median(splines_time), median(splines_mem), 0, 0)
  corrected_df[n, ] <- c(median(corrected_time), median(corrected_mem), 0, 0)

  #GB
  std_est_df[n, 'mem_gb'] <- std_est_df[n, 'mem'] / 10^9
  std_est_pd_df[n, 'mem_gb'] <- std_est_pd_df[n, 'mem'] / 10^9
  hall_1_df[n, 'mem_gb'] <- hall_1_df[n, 'mem'] / 10^9
  hall_2_df[n, 'mem_gb'] <- hall_2_df[n, 'mem'] / 10^9
  tapered_df[n, 'mem_gb'] <- tapered_df[n, 'mem'] / 10^9
  splines_df[n, 'mem_gb'] <- splines_df[n, 'mem'] / 10^9
  corrected_df[n, 'mem_gb'] <- corrected_df[n, 'mem'] / 10^9

  #MB
  std_est_df[n, 'mem_mb'] <- std_est_df[n, 'mem'] / 10^6
  std_est_pd_df[n, 'mem_mb'] <- std_est_pd_df[n, 'mem'] / 10^6
  hall_1_df[n, 'mem_mb'] <- hall_1_df[n, 'mem'] / 10^6
  hall_2_df[n, 'mem_mb'] <- hall_2_df[n, 'mem'] / 10^6
  tapered_df[n, 'mem_mb'] <- tapered_df[n, 'mem'] / 10^6
  splines_df[n, 'mem_mb'] <- splines_df[n, 'mem'] / 10^6
  corrected_df[n, 'mem_mb'] <- corrected_df[n, 'mem'] / 10^6
}

# save.image("data/101_1001_by50_mem_100_iters.RData")
# load("data/101_1001_by50_mem_100_iters.RData")

# time
par(mar=c(4,5.25,0.25,0.25)+.1)
plot(Ns, log10(std_est_df$time), type='o', pch=16, lwd=2, lty=2, col=2, ylim=c(-4, 4), cex.axis=2, cex.lab=2, xlab=expression(N), ylab=expression('log10(Time (s))'), yaxt='n')
axis(side = 2, at=seq(-8, 1, 1), cex.axis=1.9)
lines(Ns, log10(std_est_pd_df$time), type='o', pch=16, lwd=2, lty=3, col=3)
lines(Ns, log10(hall_1_df$time), type='o', pch=16, lwd=2, lty=4, col=4)
lines(Ns, log10(hall_2_df$time), type='o', pch=16, lwd=2, lty=5, col=6)
lines(Ns, log10(tapered_df$time), type='o', pch=16, lwd=2, lty=6, col=7)
lines(Ns, log10(splines_df$time), type='o', pch=16, lwd=2, lty=7, col=8)
lines(Ns, log10(corrected_df$time), type='o', pch=16, lwd=2, lty=8, col=13)

legend('topleft', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                    expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)'), expression(hat('C')^'B'*('h')), expression('C'[T]^'(a)'*'(h)')),
       col=c(2, 3, 4, 6, 7, 8, 13), lty=c(2, 3, 4, 5, 6, 7, 8), lwd=rep(3, 7), x.intersp = 0.5,  y.intersp=0.6, cex=2, ncol = 2)

# memory
plot(Ns,  log10(std_est_df$mem_mb), type='o', pch=16, col=2, ylim=c(-3, 7), yaxt="n", cex.axis=2, cex.lab=2, xlab=expression(N), ylab=expression('log10(Memory Usage (MB))'))
axis(side = 2, at=seq(-3, 4, 1), cex.axis=1.8)
lines(Ns, log10(std_est_pd_df$mem_mb), type='o', pch=16, lwd=2, lty=3, col=3)
lines(Ns, log10(hall_1_df$mem_mb), type='o', pch=16, lwd=2, lty=4, col=4)
lines(Ns, log10(hall_2_df$mem_mb), type='o', pch=16, lwd=2, lty=5, col=6)
lines(Ns, log10(tapered_df$mem_mb), type='o', pch=16, lwd=2, lty=6, col=7)
lines(Ns, log10(splines_df$mem_mb), type='o', pch=16, lwd=2, lty=7, col=8)
lines(Ns, log10(corrected_df$mem_mb), type='o', pch=16, lwd=2, lty=8, col=13)

legend('topleft', c(expression(hat('C')^'*'*'(h)'), expression(hat('C')^'**'*'(h)'), expression(hat('C')[H]*'(h)'),
                    expression(hat('C')[1]*'(h)'), expression(hat('C')[N]^'a'*'(h)'), expression(hat('C')^'B'*('h')), expression('C'[T]^'(a)'*'(h)')),
       col=c(2, 3, 4, 6, 7, 8, 13), lty=c(2, 3, 4, 5, 6, 7, 8), lwd=rep(3, 7),  x.intersp = 0.5,  y.intersp=0.5, cex=2, ncol = 3)

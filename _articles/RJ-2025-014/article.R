library(yuima)
########################################
# This is YUIMA Project package v.1.15.22
# Why don't you try yuimaGUI package?
# Visit: http://www.yuima-project.com
########################################
################################
# t-Student Density Comparison #
################################
nu <- 3
h <- 1
up <- 10
low <- -10


x <- seq(low, up, length.out = 100001)
law_COS <- setLaw_th(
  h = h, method = "COS", up = up, low = low,
  N = 180
)

cos_time <- system.time(
  ycdf_COS <- cdf(law_COS, x, list(nu = nu))
)

law_FFT <- setLaw_th(
  h = h, method = "FFT", up = up, low = low,
  N = 180
)

FFT_time <- system.time(
  ycdf_FFT <- cdf(law_FFT, x, list(nu = nu))
)

law_LAG <- setLaw_th(
  h = h, method = "LAG", up = up, low = low,
  N = 180
)

Lag_time <- system.time(
  ycdf_LAG <- cdf(law_LAG, x, list(nu = nu))
)

cdf_std <- pt(x, nu)#?pt

x11()
par(mfrow = c(2, 2))
plot(x, cdf_std,
  type = "l", main = "t - Student",
  ylab = expression(F[x]), cex.main = 1
)

plot(x, ycdf_COS,
  type = "l", main = "COS - Method",
  ylab = expression(F[x]), cex.main = 1, col = "blue"
)

plot(x, ycdf_FFT,
  type = "l", main = "FFT - Method",
  ylab = expression(F[x]), cex.main = 1, col = "red"
)

plot(x, ycdf_LAG,
  type = "l", main = "LAGUERRE - Method",
  ylab = expression(F[x]), cex.main = 1, col = "green"
)

err2 <- mean((ycdf_COS - cdf_std)^2)
err2 <- c(err2, mean((ycdf_FFT - cdf_std)^2))
err2 <- c(err2, mean((ycdf_LAG - cdf_std)^2))

err2max <- max((ycdf_COS - cdf_std)^2)
err2max <- c(err2max, max((ycdf_FFT - cdf_std)^2))
err2max <- c(err2max, max((ycdf_LAG - cdf_std)^2))

err2min <- min((ycdf_COS - cdf_std)^2)
err2min <- c(err2min, min((ycdf_FFT - cdf_std)^2))
err2min <- c(err2min, min((ycdf_LAG - cdf_std)^2))

err <- sqrt(err2)
errmax <- sqrt(err2max)
errmin <- sqrt(err2min)

res <- rbind(err, errmax, errmin)
res <- rbind(sqrt(res), c(cos_time[1], FFT_time[1], Lag_time[1]))
rownames(res) <- c("Mean", "Sup", "Inf", "time")
colnames(res) <- c("Cos", "FFT", "LAG")
Summary1 <- res

Summary1

dev.off()
###############################
# Tail behaviour for h = 0.01 #
###############################

h <- 0.01
up <- 7
low <- -up

x <- seq(low, up, length.out = 100001)
law_COS <- setLaw_th(
  h = h, method = "COS", up = up, low = low,
  N = 180
)

cos_time <- system.time(
  ycdf_COS <- cdf(law_COS, x, list(nu = nu))
)

law_FFT <- setLaw_th(
  h = h, method = "FFT", up = up, low = low,
  N = 180
)

FFT_time <- system.time(
  ycdf_FFT <- cdf(law_FFT, x, list(nu = nu))
)

law_LAG <- setLaw_th(
  h = h, method = "LAG", up = up, low = low,
  N = 180
)

Lag_time <- system.time(
  ycdf_LAG <- cdf(law_LAG, x, list(nu = nu))
)

x11()
par(mfrow = c(3, 1))
plot(x, ycdf_COS,
  type = "l", main = "COS - Method",
  ylab = expression(F[x]), cex.main = 1, col = "blue"
)

plot(x, ycdf_FFT,
  type = "l", main = "FFT - Method",
  ylab = expression(F[x]), cex.main = 1, col = "red"
)

plot(x, ycdf_LAG,
  type = "l", main = "LAGUERRE - Method",
  ylab = expression(F[x]), cex.main = 1, col = "green"
)
dev.off()


##########
# Inputs #
##########
library(yuima)
method_Fourier <- "FFT"
up <- 6
low <- -6
N <- 2^17
N_grid <- 60000 # Input for Fourier Inversion
Factor <- 1
Factor1 <- 1
initial <- 0
Final_Time <- 50 * Factor
h <- 0.02 / Factor1 # Sample Grid

#######################################
# Example 1: Deterministic Regressors #
#######################################

mu1 <- 5
mu2 <- -1
scale <- 3
nu <- 3 # Model Parameters

# Model Definition
law1 <- setLaw_th(
  method = method_Fourier, up = up, low = low,
  N = N, N_grid = N_grid
) # yuima.th definition

class(law1)

# [1] "yuima.th"
# attr(,"package")
# [1] "yuima"

regr1 <- setModel(
  drift = c("-5*sin(5*t)", "cos(t)"), diffusion = matrix("0", 2, 1),
  solve.variable = c("X1", "X2"), xinit = c(1, 0)
) # Regressors definition

Mod1 <- setLRM(unit_Levy = law1, yuima_regressors = regr1) # t-Regression Model

class(Mod1)
# [1] "yuima.LevyRM"
# attr(,"package")
# [1] "yuima"

# Simulation
samp <- setSampling(initial, Final_Time, n = Final_Time / h)
true.par <- unlist(list(mu1 = mu1, mu2 = mu2, sigma0 = scale, nu = nu))
set.seed(1)
time1 <- system.time(
  sim1 <- simulate(Mod1, true.parameter = true.par, sampling = samp)
)

print(time1)
# User      System     Tot.
#  1.18      0.06      1.29

x11()
plot(sim1@data, main = "t-Lévy Regression Model", cex.main=1.5)
dev.off()

# Estimation
lower <- list(mu1 = -10, mu2 = -10, sigma0 = 0.1)
upper <- list(mu1 = 10, mu2 = 10, sigma0 = 10.01)

start <- list(
  mu1 = runif(1, -10, 10),
  mu2 = runif(1, -10, 10),
  sigma0 = runif(1, 0.01, 4)
)

Bn <- 15 * Factor
time1_e <- system.time(
  est1 <- estimation_LRM(
    start = start, model = sim1, data = sim1@data,
    upper = upper, lower = lower, PT = Bn
  )
)
print(time1_e)
# User    System      Tot.
# 0.04      0.00      0.05
class(est1)
# [1] "yuima.qmle"
# attr(,"package")
# [1] "yuima"
summary(est1)
time1 + time1_e
# Quasi-Maximum likelihood estimation
#
# Call:
#   estimation_RLM(start = start, model = sim1, data = sim1@data,
#                  upper = upper, lower = lower, PT = Bn)
#
# Coefficients:
#   Estimate Std. Error
# mu1     5.030804 0.25015389
# mu2    -1.134982 1.27440483
# sigma0  2.424705 0.12521124
# nu      2.735990 0.08549836
#
# -2 log L: 3234.85 73.36191


###########################################################################
# Example 1 vers. 2: Deterministic Regressors with an alternative dataset #
###########################################################################

# Dataset construction without YUIMA
time_t <- index(get.zoo.data(sim1@data)[[1]])
X1 <- zoo(cos(5 * time_t), order.by = time_t)
X2 <- zoo(sin(time_t), order.by = time_t)
law1_h <- setLaw_th(
  h = h, method = method_Fourier, up = up, low = low,
  N = N, N_grid = N_grid
)

print(c(law1_h@h, law1@h))
# [1] 0.02 1.00

set.seed(1)
names(nu) <- "nu"
Z_t <- zoo(cumsum(c(0, rand(law1_h, Final_Time / h, nu))), order.by = time_t) 
# Simulation of the t-Levy process. 
J_t <- Z_t/sqrt(nu)
# Scaled t-Levy process. 
Y <- mu1 * X1 + mu2 * X2 + scale *J_t

data1_a <- merge(X1, X2, Y)

# Estimation
est1_a <- estimation_LRM(
  start = start, model = Mod1, data = setData(data1_a),
  upper = upper, lower = lower, PT = Bn
)

summary(est1_a)

# Quasi-Maximum likelihood estimation
#
# Call:
#   estimation_RLM(start = start, model = sim1, data = sim1@data,
#                  upper = upper, lower = lower, PT = Bn)
#
# Coefficients:
#   Estimate Std. Error
# mu1     4.988453  0.2569998
# mu2    -1.145978  1.3092899
# sigma0  2.490245  0.1285957
# nu      2.408679  0.4124045
#
# -2 log L: 3231.577 85.28444


# We use the result in the summary to construct a confidence interval for
# each parameter

info_sum <- summary(est1_a)@coef

alpha <- 0.025
Confidence_Int_95 <- rbind(
  info_sum[, 1] + info_sum[, 2] * qnorm(alpha),
  info_sum[, 1] + info_sum[, 2] * qnorm(1 - alpha),
  unlist(true.par),
  info_sum[, 1]
)
rownames(Confidence_Int_95) <- c("LB", "UB", "True_par", "Est_par")

print(Confidence_Int_95, digit = 4)


#################################
# Example 3: Real data - Boh!!! #
#################################
rm(list = ls())
library(yuima)
library(quantmod)
getSymbols("^SPX", from = "2014-12-04", to = "2023-05-12")
# Regressors
getSymbols("EURUSD=X", from = "2014-12-04", to = "2023-05-12")
getSymbols("VIX", from = "2014-12-04", to = "2023-05-12")
getSymbols("JPYUSD=X", from = "2014-12-04", to = "2023-05-12")

SP <- zoo(x = SPX$SPX.Close, order.by = index(SPX$SPX.Close))
Vix <- zoo(x = VIX$VIX.Close / 1000, order.by = index(VIX$VIX.Close))
EURUSD <- zoo(x = `EURUSD=X`[, 4], order.by = index(`EURUSD=X`))
JPYUSD <- zoo(x = `JPYUSD=X`[, 4], order.by = index(`JPYUSD=X`))
Data <- na.omit(na.approx(merge(Vix, EURUSD, JPYUSD, SP)))
colnames(Data) <- c("VIX", "EURUSD", "JPYUSD", "SP")
days <- as.numeric(index(Data)) - as.numeric(index(Data))[1]

# equally spaced grid time data
Data <- zoo(log(Data), order.by = days)
Data_eq <- na.approx(Data, xout = days[1]:tail(days, 1L))
yData <- setData(zoo(Data_eq, order.by = index(Data_eq) / 30)) # Data on montly basis

x11()
plot(yData)
dev.off()
# t-Regression Model Definition

method_Fourier <- "FFT"
up <- 12
low <- -12
N <- 2^16
N_grid <- 60000 # Input for Fourier Inversion

# Model Definition
law <- setLaw_th(
  method = method_Fourier, up = up, low = low,
  N = N, N_grid = N_grid
) # yuima.th definition

regr <- setModel(
  drift = c("0", "0", "0"), diffusion = matrix("0", 3, 1),
  solve.variable = c("VIX", "EURUSD", "JPYUSD"), xinit = c(0, 0, 0)
) # Regressors definition

Mod <- setLRM(unit_Levy = law, yuima_regressors = regr, LevyRM = "SP")

lower <- list(mu1 = -100, mu2 = -200, mu3 = -100, sigma0 = 0.01)
upper <- list(mu1 = 100, mu2 = 100, mu3 = 100, sigma0 = 200.01)

start <- list(
  mu1 = runif(1, -100, 100),
  mu2 = runif(1, -100, 100),
  mu3 = runif(1, -100, 100),
  sigma0 = runif(1, 0.01, 100)
)

est <- estimation_LRM(
  start = start, model = Mod, data = yData, upper = upper, lower = lower,
  PT = floor(tail(index(Data_eq) / 30, 1L) / 2)
)
summary(est)

################################################
# Example 4: Simulated Distribution for Errors #
################################################
# This code is useful for constructing the distribution of the studentized 
# estimates as done in 
# Masuda (2019) "Non-Gaussian quasi-likelihood estimation of SDE driven by locally 
# stable L\'evy process", Stochastic Processes and their Applications, Volume 129, 
# Issue 3, 2019, Pages 1013-1059, https://doi.org/10.1016/j.spa.2018.04.004.


rm(list = ls())


library(yuima)
method_Fourier <- "FFT"
up <- 20
low <- -20
N <- 2^14
N_grid <- 10^6 # Input for Fourier Inversion
Factor <- 1
Factor1 <- 7.2
initial <- 0
Final_Time <- 500
h <- 0.02 / Factor1 # Sample Grid
Bn <- 1

#######################################
# Example 1: Deterministic Regressors #
#######################################

mu1 <- 5
mu2 <- -1
scale <- 3
nu <- 3 # Model Parameters

set.seed(1)

lower <- list(mu1 = -10, mu2 = -10, sigma0 = 0.001)
upper <- list(mu1 = 10, mu2 = 10, sigma0 = 10)

# Model Definition
law1 <- setLaw_th(
  method = method_Fourier, up = up, low = low,
  N = N, N_grid = N_grid
) # yuima.th definition

regr1 <- setModel(
  drift = c("-5*sin(5*t)", "cos(t)"), diffusion = matrix("0", 2, 1),
  solve.variable = c("X1", "X2"), xinit = c(1, 0)
) # Regressors definition

Mod1 <- setLRM(unit_Levy = law1, yuima_regressors = regr1) # t-Regression Model

samp <- setSampling(initial, Final_Time, n = Final_Time / h)
true.par <- unlist(list(mu1 = mu1, mu2 = mu2, sigma0 = scale, nu = nu))

set.seed(1)
t2 <- system.time(
  sim1 <- simulate(Mod1, true.parameter = true.par, sampling = samp)
)
t2 <- t2 + system.time(
  est1 <- estimation_LRM(
    start = true.par, model = sim1, data = sim1@data,
    upper = upper, lower = lower, PT = Bn
  )
)
t2

summary(est1)
est1@min

# Construction of the simulated distributions for the studentized estimates and 
# Comparison with the theoretical ones.

NumbOfRep <- 1000 # Number of repetations.
set.seed(1)
estimates <- NULL
stdmy <- NULL
term <- NULL
u_f <- NULL
names(nu) <- "nu"

for (t in c(1:NumbOfRep)) {
  sim1 <- simulate(Mod1, true.parameter = true.par, sampling = samp)
  est1 <- estimation_LRM(
    start = true.par, model = sim1, data = sim1@data,
    upper = upper, lower = lower, PT = Bn
  )
  Inv <- solve(est1@vcov)
  u_f <- cbind(u_f, as.numeric(t(est1@coef[1:2] - c(mu1, mu2)) %*% Inv[1:2, 1:2] %*% (est1@coef[1:2]) - c(mu1, mu2)))
  estimates <- rbind(estimates, est1@coef)
  term <- rbind(term, diag(chol(Inv)))
  stdmy <- rbind(estimates, sqrt(diag(est1@vcov)))
  cat("\n", c(t, est1@coef))
}

u_m1 <- (estimates[, 1] - mu1) * term[, 1]

x11()
p <- hist(u_m1,
  freq = FALSE, nclass = 30, main = expression(mu[1]),
  xlab = "", ylab = "", ylim = c(0, 0.6)
)
lines(p$mids, dnorm(p$mids), col = "red")

u_m2 <- (estimates[, 2] - mu2) * term[, 2]
p <- hist(u_m2,
  freq = FALSE, nclass = 30, main = expression(mu[2]),
  xlab = "", ylab = "", ylim = c(0, 0.6)
)
lines(p$mids, dnorm(p$mids), col = "red")

u_sig <- (estimates[, 3] - scale) / sqrt((2 * estimates[, 3]^2)) * sqrt(Bn / h)

p <- hist(u_sig,
  freq = FALSE, nclass = 30, main = expression(sigma[0]),
  xlab = "", ylab = "", ylim = c(0, 0.6)
)
lines(p$mids, dnorm(p$mids), col = "red")
c(mean(u_sig), sd(u_sig))
qqnorm(u_sig)
qqline(u_sig)
u_nu <- (estimates[, 4] - nu) * term[, 4]
p <- hist(u_nu,
  freq = FALSE, nclass = 30, main = expression(nu),
  xlab = "", ylab = "", ylim = c(0, 0.7)
)
lines(p$mids, dnorm(p$mids), col = "red")
c(mean(u_nu), sd(u_nu))
qqnorm(u_nu)
qqline(u_nu)

summary(estimates)
rbind(
  apply(estimates, 2, mean),
  apply(estimates, 2, sd)
)

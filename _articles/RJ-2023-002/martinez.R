###############################################################################
## This file contains the code used in the paper "DGLMExtPois: Advances in
## Dealing with Over and Underdispersion in a Double GLM Framework" by
## Antonio J. Saez-Castillo, Antonio Conde-Sanchez and Francisco Martinez.
##
## Contents:
##
## Section: Numerical examples -> example of application
## Section: Numerical examples -> performance comparison
## Section: Numerical examples -> simulation
## Section: Numerical examples -> the optimization process
##
## Note: R version 4.1.2 used throughout.
##
###############################################################################


###############################################################################
#
# Section Numerical examples -> example of application
#
###############################################################################
library(DGLMExtPois)
formula.loc   <- ncust ~ nhu + aid + aha + dnc + ds # Mean formula
formula.disp0 <- ncust ~ 1                  # Dispersion parameter formula

pois <- glm(formula.loc, data = CustomerProfile, family = 'poisson')

hp0  <- glm.hP(formula.loc, formula.disp0, data = CustomerProfile)
cmp0 <- glm.CMP(formula.loc, formula.disp0, data = CustomerProfile)
# summary(pois) # to see estimated coefficients in Table 3
# summary(hp0)  # to see estimated coefficients in Table 3
# summary(cmp0) # to see estimated coefficients in Table 3
c(AIC(pois), AIC(hp0), AIC(cmp0))

lrt_hp <- -2 * (hp0$logver + logLik(pois)[1])
pchisq(lrt_hp,1, lower.tail = FALSE)
lrt_cmp <- -2 * (cmp0$logver + logLik(pois)[1])
pchisq(lrt_cmp,1, lower.tail = FALSE)

formula.disp1 <- ncust ~ dnc         # New dispersion parameter formula
hp1           <- glm.hP(formula.loc, formula.disp1, data = CustomerProfile)
summary(hp1)
lrtest(hp0, hp1)

# Dispersion plot
plot(CustomerProfile$ncust, hp1$gamma,
     xlab = expression(ncust[i]),
     ylab = expression(gamma[i]),
     pch = 19
)
abline(h = 1, lty = "dashed", col = "blue")

# Diagnostics
par(mfrow = c(2, 2))
set.seed(21)
plot(hp1) # Residuals against fitted values and normal QQ plot
# Observed and expected marginal frequencies
expec_hp <- hP_expected(hp1)
barplot(expec_hp$observed_freq[0:33], xlab = "ncust", ylab = "Frequencies")
lines(expec_hp$frequencies[1:33], x = c(0:32), col = "blue")
text(x = c(25,25),
     y = c(10, 9),
     c(paste("Dif = ", round(expec_hp$dif, 2)),
       paste(expression(chi ^ 2), "=",
             round(expec_hp$chi2, 2))), pos = 4)
# Envelope
r1 <- residuals(hp1, envelope = TRUE)
envelope_down <- apply(r1$sim_residuals, 1, min)
envelope_up <- apply(r1$sim_residuals, 1, max)
weirds <- which(r1$residuals < envelope_down | r1$residuals > envelope_up)
score_weirds <- qnorm(weirds / 111)
points(score_weirds, r1$residuals[weirds], col = "red")

###############################################################################
#
# Section Numerical examples -> performance comparison
#
###############################################################################
library(COMPoissonReg)
library(mpcmp)

# Functions to compute goodness of fit criteria
MPB <- function(fit) {  # Mean Prediction Bias
  sum(fit$weights * residuals(fit, "response") / sum(fit$weights))
}

MAD <- function(fit) { # Mean absolute deviance
  sum(fit$weights * abs(residuals(fit, "response"))) / sum(fit$weights)
}

MSPE <- function(fit) { # Mean Squared Predictive Error
  sum(fit$weights * residuals(fit, "response") ^ 2 / sum(fit$weights))
}

pearson <- function(fit) { sum(residuals(fit)^2) }

# Takeover bids -----------------------------------------------------------
library(Ecdat) # includes dataset Bids
Bids$size.sq <- Bids$size^2
formula <- numbids ~ leglrest + rearest + finrest + whtknght + bidprem + insthold + size + size.sq + regulatn

# hyper-Poisson
time <- unname(system.time(
  x <- microbenchmark(fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = Bids), times = 1)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = Bids)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg (it fails)
fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = Bids)

# COM-Poisson in package mpcmp
time <- unname(system.time(
  fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = Bids)
)[3])
cat("AIC:", summary(fit)$aic, "Time:", time, "\n")

# Days absent -------------------------------------------------------------
library(mpcmp) # includes datasets attendance and cottonbolls
formula <- daysabs ~ gender + math + prog

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = attendance)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = attendance)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg (it fails)
fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = attendance)

# COM-Poisson in package mpcmp (it fails)
fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = attendance)

# Cotton bolls ------------------------------------------------------------
formula  <- nc ~ stages * (def + def2)

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = cottonbolls)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = cottonbolls)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg
time <- unname(system.time(
  fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = cottonbolls)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "\n")

# COM-Poisson in package mpcmp
time <- unname(system.time(
  fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = cottonbolls)
)[3])
cat("AIC:", summary(fit)$aic, "Time:", time, "\n")

# Korea -------------------------------------------------------------------
address <- "https://raw.githubusercontent.com/franciscomartinezdelrio/datasets/master/korea_rhx_raw_data.csv"
korea   <- read.csv(url(address))
korea$Presence.of.comm.area    <- as.factor(korea$Presence.of.comm.area)
korea$Presence.of.speed.hump   <- as.factor(korea$Presence.of.speed.hump)
korea$Track.circuit.controller <- as.factor(korea$Track.circuit.controller)
korea$Presence.of.guide        <- as.factor(korea$Presence.of.guide)

formula <- Crashes ~ ln.AADT + Railway_traffic + Presence.of.comm.area + Distance.of.train.detector + Warning.time.difference + Presence.of.speed.hump + Track.circuit.controller + Presence.of.guide

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = korea)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = korea)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg (it fails)
fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = korea)

# COM-Poisson in package mpcmp (it fails)
fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = korea)

# Toronto crashes ---------------------------------------------------------
address <- "https://raw.githubusercontent.com/franciscomartinezdelrio/datasets/master/TNdata.csv"
toronto <- read.csv(url(address))
formula <- y ~ log(F1) + log(F2)

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = toronto)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = toronto)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg (it fails)
fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = toronto)

# COM-Poisson in package mpcmp
time <- unname(system.time(
  fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = toronto)
)[3])
cat("AIC:", summary(fit)$aic, "Time:", time, "\n")

# Insurance claims --------------------------------------------------------
library(MASS)
formula <- Claims ~ offset(log(Holders)) + District + Group + Age

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = Insurance)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = Insurance)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg
time <- unname(system.time(
  fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = Insurance)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "\n")

# COM-Poisson in package mpcmp
time <- unname(system.time(
  fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = Insurance)
)[3])
cat("AIC:", summary(fit)$aic, "Time:", time, "\n")

# Credit card reports -----------------------------------------------------
library(AER) # includes dataset CreditCard
data("CreditCard")
formula <- reports ~ expenditure + income + age

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = CreditCard)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = CreditCard)
)[3])
fit$nloptr$message # the optimization process ends prematurely due to an error
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg (it fails)
fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = CreditCard)

# COM-Poisson in package mpcmp (it fails)
fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = CreditCard)

# Children ----------------------------------------------------------------
library(catdata) # includes dataset children
data("children")
formula <- child ~ age + dur + nation + god + univ

# hyper-Poisson
time <- unname(system.time(
  fit <- glm.hP(formula.mu = formula, formula.gamma = formula, data = children)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package DGLMExtPois
time <- unname(system.time(
  fit <- glm.CMP(formula.mu = formula, formula.nu = formula, data = children)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "MPB:", MPB(fit), "MAD:", MAD(fit),
    "MSPE:", MSPE(fit), "Pearson:", pearson(fit), "\n")

# COM-Poisson in package COMPossionReg
time <- unname(system.time(
  fit <- COMPoissonReg::glm.cmp(formula.lambda = formula,
                              formula.nu = formula, data = children)
)[3])
cat("AIC:", AIC(fit), "Time:", time, "\n")

# COM-Poisson in package mpcmp
time <- unname(system.time(
  fit <- mpcmp::glm.cmp(formula = formula, formula_nu = formula, data = children)
)[3])
cat("AIC:", summary(fit)$aic, "Time:", time, "\n")

###############################################################################
#
# Section Numerical examples -> Simulation
#
###############################################################################

#
# Simulation for the hyper-Poisson model
#

# Simulation of a dataset with m observations for hyper-Poisson 
# with beta coefficients and delta as covariate
data_generation_hp <- function(m = 500, beta, delta) {
  # Data generation with explanatory uniform variables
  x1 <- runif(m)
  z1 <- runif(m)
  
  mu    <- exp(beta[1] + beta[2] * x1)
  gamma <- exp(delta[1] + delta[2] * z1)
  
  # lambda is obtained
  ec <- function(mu, gamma) {
    obj <- function(lambda) (mu - DGLMExtPois:::means_hp(lambda, gamma))^2
    lower_bound <- min(mu, max(mu+gamma-1, mu*gamma))
    upper_bound <- max(mu, min(mu*gamma, mu+gamma-1))
    if (upper_bound < lower_bound) {
      upper_bound <- lower_bound + 1
      warning("Bad bounds")
    }
    if (upper_bound > 2.02e+19)
      warning("Too high bounds [", lower_bound, ", ", upper_bound, "]")
    if (upper_bound == lower_bound) {
      lam <- upper_bound
    } else {
      output <- optimize(f = obj, interval = c(lower_bound, upper_bound),
                         tol = .Machine$double.eps)
      if (output$objective > 0.1) {
        warning("Bad solutions of the link equation ", mu, " ", gamma, " ",
                output$minimum, " ", output$objective)
      }
      lambda <- output$minimum
    }
    lambda
  }
  lambda <- mapply(ec, mu, gamma)
  
  # Data generation for hyper-Poisson
  y <- mapply(rhP, gamma = gamma, lambda = lambda, n = 1)
  data.frame(y, x1, z1)
}  

# Montecarlo simulation for hyper-Poisson model in the paper
set.seed(2601)
T <- 1000 # number of replications
beta  <- c(1, 0.5)
delta <- c(0.5, -1) # change to do other simulations

r <- matrix(nrow = T, ncol = 8)
colnames(r) <- c("B0", "B1", "se_B0", "se_B1", "D0", "D1", "se_D0", "se_D1")
pb <- utils::txtProgressBar(min = 1, max = T, initial = 1, style = 3)
for (n in 1:T) {
  utils::setTxtProgressBar(pb, n)
  data <- data_generation_hp(500, beta = beta, delta = delta)
  hp <- glm.hP(formula.mu = y~x1, formula.gamma = y~z1, data = data)
  r[n, ] <- c(hp$betas, DGLMExtPois:::se_betas_hp(hp), hp$deltas, 
              DGLMExtPois:::se_deltas_hp(hp))
}
cat("Beta0: Mean = ", mean(r[, "B0"]), ", sd = ", sd(r[, "B0"]),
    ", Mean s.e. = ", mean(r[, "se_B0"]), "\n", sep = "")
cat("Beta1: Mean = ", mean(r[, "B1"]), ", sd = ", sd(r[, "B1"]),
    " Mean s.e. = ", mean(r[, "se_B1"]), "\n", sep = "")
cat("Delta0: Mean = ", mean(r[, "D0"]), ", sd = ", sd(r[, "D0"]),
    ", Mean s.e. = ", mean(r[, "se_D0"]), "\n", sep = "")
cat("Delta1: Mean = ", mean(r[, "D1"]), ", sd = ", sd(r[, "D1"]),
    ", Mean s.e. = ", mean(r[, "se_D1"]), "\n", sep = "")

#
# Simulation for the COM-Poisson model
#
library(DGLMExtPois)
library(mpcmp)

data_generation_cmp <- function(m = 500, beta, delta) {
  # Data generation with explanatory uniform variables
  x1 <- runif(m)
  z1 <- runif(m)
  
  mu <- exp(beta[1] + beta[2] * x1)
  nu <- exp(delta[1] + delta[2] * z1)
  
  # Data generation for COM-Poisson
  y <- mapply(rcomp, mu = mu, nu = nu, n = 1)
  data.frame(y, x1, z1)
}

# Montecarlo simulation in the paper
set.seed(2601)
T <- 1000 # number of replications
beta  <- c(1, 0.5)
delta <- c(0.5, 0.25) # change to do other simulations

r <- matrix(nrow = T, ncol = 8)
colnames(r) <- c("B0", "B1", "se_B0", "se_B1", "D0", "D1", "se_D0", "se_D1")
pb <- utils::txtProgressBar(min = 1, max = T, initial = 1, style = 3)
for (n in 1:T) {
  utils::setTxtProgressBar(pb, n)
  data <- data_generation_cmp(500, beta = beta, delta = delta)
  cmp <- glm.CMP(formula.mu = y~x1, formula.nu = y~z1, data = data)
  r[n, ] <- c(cmp$betas, DGLMExtPois:::se_betas_cmp(cmp), cmp$deltas,
              DGLMExtPois:::se_deltas_cmp(cmp))
}
cat("Beta0: Mean = ", mean(r[, "B0"]), ", sd = ", sd(r[, "B0"]),
    ", Mean s.e. = ", mean(r[, "se_B0"]), "\n", sep = "")
cat("Beta1: Mean = ", mean(r[, "B1"]), ", sd = ", sd(r[, "B1"]),
    " Mean s.e. = ", mean(r[, "se_B1"]), "\n", sep = "")
cat("Delta0: Mean = ", mean(r[, "D0"]), ", sd = ", sd(r[, "D0"]),
    ", Mean s.e. = ", mean(r[, "se_D0"]), "\n", sep = "")
cat("Delta1: Mean = ", mean(r[, "D1"]), ", sd = ", sd(r[, "D1"]),
    ", Mean s.e. = ", mean(r[, "se_D1"]), "\n", sep = "")


# Comparison of computing time as the dataset increases
# This comparison has not been included in the paper
library(microbenchmark)
set.seed(0902)

for (size in c(100, 200, 500, 1e3)) {
  data <- data_generation_hp(size, beta = c(1, 0.5), delta = c(0.5, -1))
  # increase times in the following call to get a better estimate of the execution time
  t <- microbenchmark(hp <- glm.hP(y~x1, y~z1, data = data), times = 1)
  cat("Time for size ", size, ": ", mean(t$time) / 1e9, " seconds\n", sep ="")  
  cat("Number of iterations in the optimization:", hp$nloptr$iterations, "\n\n")
}

###############################################################################
#
# Section Numerical examples -> The optimization process
#
###############################################################################
library(mpcmp)
formula <- daysabs ~ gender + math + prog
fit <- glm.hP(formula, formula, data = attendance)
fit$nloptr$message # termination condition
fit$nloptr$iterations # number of iterations

# Remove xtol_rel condition and modify maximum number of iterations
fit1 <- glm.hP(formula, formula, data = attendance) # default termination conditions
AIC(fit1)
fit1$nloptr$iterations
fit2 <- glm.hP(formula, formula, data = attendance,
               opts = list(xtol_rel = -1, maxeval = 250))
AIC(fit2)
fit2$nloptr$iterations

# Remove xtol_rel and maxeval conditions and set maximum time in seconds
fit3 <- glm.hP(formula, formula, data = attendance,
               opts = list(xtol_rel = -1, maxeval = -1, maxtime = 5))
AIC(fit3)
fit3$nloptr$iterations

# To see how the optimization process evolves
fit4 <- glm.hP(formula, formula, data = attendance,
               opts = list(xtol_rel = -1, maxeval = 5, print_level = 1))



  


####################################################################
### Section 1. Examples: application to criminal recidivism data ###
####################################################################

library(penPHcure)
data("cpRossi",package = "penPHcure")
head(cpRossi)
str(cpRossi)

set.seed(123) # for reproducibility
# If you use R v3.6 or greater, uncomment the following line
# RNGkind(sample.kind="Rounding")

### Fit PH cure model + inference via bootstrap
fit <- penPHcure(
  Surv(tstart,tstop,arrest) ~ 
    fin + age + race + wexp + mar + paro + prio + educ + emp,
  cureform = ~ fin + age + race + wexp + mar + paro + prio + educ + emp,
  data = cpRossi, which.X = "mean", inference = TRUE
)
# Print results
summary(fit)

### PH cure model: variable selection 
# Define grid for selection of the tuning parameters
pen.tuneGrid <- list(
  CURE = list(lambda = seq(0.01, 0.12, by = 0.01),
              a = 3.7),
  SURV = list(lambda = seq(0.01, 0.12, by = 0.01),
              a = 3.7)
)
# Set the starting values equal to the estimates from the unpenalized model
SV <- list(b = fit$b, beta = fit$beta)
# Perform variable selection
tuneSCAD <- penPHcure(
  Surv(tstart,tstop,arrest) ~ 
    fin + age + race + wexp + mar + paro + prio + educ + emp,
  cureform = ~ fin + age + race + wexp + mar + paro + prio + educ + emp,
  data = cpRossi, which.X = "mean", pen.type = "SCAD",
  pen.tuneGrid = pen.tuneGrid, SV = SV
)
# Print results (BIC)
summary(tuneSCAD)
# Print results (AIC)
summary(tuneSCAD,crit.type = "AIC")



################################################################
### Section 2. SIMULATION STUDY variable selection (Table 2) ###
################################################################

library(penPHcure)

# Number of replications
repl <- 500

# The three variables hereafter define the simulation setting
#  (see Table 1 for all the parameterizations).
N <- 250 ## 500 or 1000
lambdaC <- 0.02 ## 0.3, 0.35, 0.75, 0.95 or 1.55
b0_0 <- 1.45 ## 2.35, 0.35, 1.45, -0.7, 0.7

# True coefficients latency
beta0 <- c(-0.7, 0, 1, 0, -0.5, 0.75, 0, 0)
# True coefficients incidence
b0 <- c(b0_0, 1.5, 0, -0.75, 0, -1.5, 0, 0.75, 0)
# Time scale used to generate the time-varying covariates
S <- seq(0.2, 6, length.out = 30)
# Covariance matrix normal distribution (incidence)
cor_CURE <- matrix(NA, 8, 8)
for (p in 1:8)
  for (q in 1:8)
    cor_CURE[p,q] <- 0.5^abs(p - q)
# Covariance matrix normal distribution (latency)    
cor_SURV <- matrix(NA,8,8)
for (p in 1:8)
  for (q in 1:8)
    cor_SURV[p, q] <- 0.5^abs(p - q)
# Define grid for selection of the tuning parameters
pen.tuneGridSCAD <- list(CURE = list(lambda = exp(seq(-6, -1, length.out = 10)),
                                     a = 3.7),
                         SURV = list(lambda = exp(seq(-6, -1, length.out = 10)),
                                     a = 3.7))

# This function is used to compute the survival probability (Cox's model)
compute.surv <- function(formula, data, beta, event_times, cumhaz){
  nobs <- nrow(data)
  K <- length(event_times)
  SurvObj <- model.frame(formula, data = data)[, 1]
  tstart <- SurvObj[, 1]
  tstop <- SurvObj[, 2]
  Z <- model.matrix(formula, data = data)[, -1, drop = FALSE]
  haz <- diff(c(0, cumhaz))
  eta <- Z %*% beta
  cumhaz_temp <- rep(0, nobs)
  id <- 1
  j <- 1
  i <- 1
  while (i <= nobs) {
    if (tstart[i] < tstop[i - 1] && i > 1) {
      id <- id + 1
      j <- 1
    }
    while (event_times[j] <= tstop[i] && event_times[j] > tstart[i] && j <= K) {
      cumhaz_temp[id] <- cumhaz_temp[id] + haz[j] * exp(eta[i])
      j <- j + 1
    }
    i <- i + 1
  }
  surv <- exp(-cumhaz_temp[1:id])
  return(surv)
}

# Initialize vectors used to store censoring and cure proportions
cens <- rep(NA, repl)
cure <- rep(NA, repl)
# Initialize vectors used to store the model estimation errors
MSE_CURE_ORACLE <- rep(NA, repl)
MSE_SURV_ORACLE <- rep(NA, repl)
MSE_CURE_FULL <- rep(NA, repl)
MSE_SURV_FULL <- rep(NA, repl)
MSE_CURE_SCAD_BIC <- rep(NA, repl)
MSE_SURV_SCAD_BIC <- rep(NA, repl)
MSE_SURV_COX_ORACLE <- rep(NA, repl)
# Initialize vectors used to store the number of correct/incorrect zeros
zeros_CURE <- (b0 == 0)[-1]
zeros_SURV <- (beta0 == 0)
corr0s_CURE_SCAD_BIC <- rep(NA, repl)
corr0s_SURV_SCAD_BIC <- rep(NA, repl)
incorr0s_CURE_SCAD_BIC <- rep(NA, repl)
incorr0s_SURV_SCAD_BIC <- rep(NA, repl)

# Perform the simulations
for (i in 1:repl) {
  data <- penPHcure.simulate(N = N,
                             S = S,
                             beta0 = beta0,
                             b0 = b0,
                             lambdaC = lambdaC,
                             cor_CURE = cor_CURE,
                             cor_SURV = cor_SURV,
                             gamma = 3)

  # Store censoring and cure proportion
  cens[i] <- attr(data, "perc_cens")
  cure[i] <- attr(data, "perc_cure")
  
  
  ### Fit CURE_FULL model (all covariates)
  survform <- Surv(time = tstart, time2 = tstop, event = status) ~ 
    z.1 + z.2 + z.3 + z.4 + z.5 + z.6 + z.7 + z.8
  cureform <- ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8
  fit <- penPHcure(survform, cureform, data = data, print.details = F)
  # Compute probabilities
  pred <- predict(fit, data)
  # Compute probabilities (TRUE model) and store the unique event times
  cumhaz0 <- attr(fit$cumhaz, "time")^3
  pred.TRUE <- predict(fit, data, b0 = b0, beta0 = beta0, cumhaz0 = cumhaz0)
  event_times <- attr(fit$cumhaz, "time")
  # Compute model estimation errors
  MSE_CURE_FULL[i] <- mean((pred$CURE - pred.TRUE$CURE)^2)
  MSE_SURV_FULL[i] <- mean((pred$SURV - pred.TRUE$SURV)^2)
  

  ### Fit CURE_ORACLE model (only covariates with non-zero coefficients)
  survform <- Surv(time = tstart, time2 = tstop, event = status) ~ 
    z.1 + z.3 + z.5 + z.6
  cureform <- ~ x.1 + x.3 + x.5 + x.7
  fit <- penPHcure(survform, cureform, data = data, print.details = F)
  # Compute probabilities
  pred <- predict(fit, data)
  # Compute model estimation errors
  MSE_CURE_ORACLE[i] <- mean((pred$CURE - pred.TRUE$CURE)^2)
  MSE_SURV_ORACLE[i] <- mean((pred$SURV - pred.TRUE$SURV)^2)
  

  ### Fit COX_ORACLE model (only covariates with non-zero coefficients)
  survform <- Surv(time = tstart, time2 = tstop, event = status) ~ 
    z.1 + z.3 + z.5 + z.6
  cureform <- ~ x.1 + x.3 + x.5 + x.7
  fitCOX <- coxph(survform, data = data, control = coxph.control(timefix = F))
  # Compute survival probabilities
  cumhazCOX <- basehaz(fitCOX, centered = F)
  cumhazCOX <- cumhazCOX$hazard[cumhazCOX$time %in% event_times]
  survCOX <- compute.surv(survform, data, 
                          fitCOX$coefficients, event_times, cumhazCOX)
  # Compute model estimation errors
  MSE_SURV_COX_ORACLE[i] <- mean((survCOX - pred.TRUE$SURV)^2)


  ### Tune CURE_SCAD model
  survform <- Surv(time = tstart,time2 = tstop,event = status) ~ 
    z.1 + z.2 + z.3 + z.4 + z.5 + z.6 + z.7 + z.8
  cureform <- ~ x.1 + x.2 + x.3 + x.4 + x.5 + x.6 + x.7 + x.8
  tune <- penPHcure(survform, cureform, data = data, pen.type = "SCAD",
                    pen.tuneGrid = pen.tuneGridSCAD, print.details = F)
  # Compute number of correct and incorrect 0's
  corr0s_CURE_SCAD_BIC[i] <- sum(tune$BIC$b[-1][zeros_CURE] == 0)
  corr0s_SURV_SCAD_BIC[i] <- sum(tune$BIC$beta[zeros_SURV] == 0)
  incorr0s_CURE_SCAD_BIC[i] <- sum(tune$BIC$b[-1][!zeros_CURE] == 0)
  incorr0s_SURV_SCAD_BIC[i] <- sum(tune$AIC$beta[!zeros_SURV] == 0)
  # Compute probabilities
  pred.AIC <- predict(tune, data, crit.type = "AIC")
  pred.BIC <- predict(tune, data, crit.type = "BIC")
  # Compute model estimation errors
  MSE_CURE_SCAD_BIC[i] <- mean((pred.BIC$CURE - pred.TRUE$CURE)^2)
  MSE_SURV_SCAD_BIC[i] <- mean((pred.BIC$SURV - pred.TRUE$SURV)^2)

  message("Simulation ", i, " of ", repl)
}

# Compute averages of the censoring and cure proportions
avg_cens <- mean(cens)
avg_cure <- mean(cure)
# Compute averages of the correct and incorrect number of 0s
avg_corr0s_CURE_SCAD_BIC <- mean(corr0s_CURE_SCAD_BIC)
avg_corr0s_SURV_SCAD_BIC <- mean(corr0s_SURV_SCAD_BIC)
avg_incorr0s_CURE_SCAD_BIC <- mean(incorr0s_CURE_SCAD_BIC)
avg_incorr0s_SURV_SCAD_BIC <- mean(incorr0s_SURV_SCAD_BIC)
# Compute the mean of the model estimation errors
mean_MSE_CURE_ORACLE <- mean(MSE_CURE_ORACLE)
mean_MSE_SURV_ORACLE <- mean(MSE_SURV_ORACLE)
mean_MSE_CURE_SCAD_BIC <- mean(MSE_CURE_SCAD_BIC)
mean_MSE_SURV_SCAD_BIC <- mean(MSE_SURV_SCAD_BIC)
mean_MSE_SURV_COX_ORACLE <- mean(MSE_SURV_COX_ORACLE)
# Compute the mean of the relative model estimation errors
mean_relMSE_CURE_ORACLE <- mean(MSE_CURE_ORACLE/MSE_CURE_FULL)
mean_relMSE_SURV_ORACLE <- mean(MSE_SURV_ORACLE/MSE_SURV_FULL)
mean_relMSE_CURE_SCAD_BIC <- mean(MSE_CURE_SCAD_BIC/MSE_CURE_FULL)
mean_relMSE_SURV_SCAD_BIC <- mean(MSE_SURV_SCAD_BIC/MSE_SURV_FULL)
mean_relMSE_SURV_COX_ORACLE <- mean(MSE_SURV_COX_ORACLE/MSE_SURV_FULL)





####################################################################
### Section 3. SIMULATION STUDY coverage probabilities (Table 3) ###
####################################################################

library(penPHcure)

# Number of replications
repl <- 500

# The 3 variables hereafter define the simulation setting
#  (see Table 1 for all the parameterizations).
N <- 250 ## 500 or 1000
lambdaC <- 0.02 ## 0.3, 0.35, 0.75, 0.95 or 1.55
b0_0 <- 1.45 ## 2.35, 0.35, 1.45, -0.7, 0.7

# True coefficients latency
beta0 <- c(-0.7, 0, 1, 0, -0.5, 0.75, 0, 0)
# True coefficients incidence
b0 <- c(b0_0,1.5,0,-0.75,0,-1.5,0,0.75,0)
# Time scale used to generate the time-varying covariates
S <- seq(0.2, 6, length.out = 30)
# Covariance matrix normal distribution (incidence)
cor_CURE <- matrix(NA,8,8)
for (p in 1:8)
  for (q in 1:8)
    cor_CURE[p,q] <- 0.5^abs(p - q)
# Covariance matrix normal distribution (latency)    
cor_SURV <- matrix(NA,8,8)
for (p in 1:8)
  for (q in 1:8)
    cor_SURV[p,q] <- 0.5^abs(p - q)

# Initialize vectors used to store censoring and cure proportions
cure <- rep(NA,repl)
cens <- rep(NA,repl)
# Initialize the matrices used to store the confidence intervals
percCI_CURE_down <- matrix(NA, repl, length(b0[b0 != 0]))
percCI_CURE_up <- matrix(NA, repl, length(b0[b0 != 0]))
percCI_SURV_down <- matrix(NA, repl, length(beta0[beta0 != 0]))
percCI_SURV_up <- matrix(NA, repl, length(beta0[beta0 != 0]))
basicCI_CURE_down <- matrix(NA, repl, length(b0[b0 != 0]))
basicCI_CURE_up <- matrix(NA, repl, length(b0[b0 != 0]))
basicCI_SURV_down <- matrix(NA, repl, length(beta0[beta0 != 0]))
basicCI_SURV_up <- matrix(NA, repl, length(beta0[beta0 != 0]))
# Initialize the matrices used to store the coverage indicators 
#  (TRUE if the conf. int. contains the true coefficient, FALSE otherwise)
basicCP_CURE <- matrix(NA, repl, length(b0[b0 != 0]))
percCP_CURE <- matrix(NA, repl, length(b0[b0 != 0]))
basicCP_SURV <- matrix(NA, repl, length(beta0[beta0 != 0]))
percCP_SURV <- matrix(NA, repl, length(beta0[beta0 != 0]))

nonzero_b0 <- b0[b0 != 0]
nonzero_beta0 <- beta0[beta0 != 0]

for (i in 1:repl) {
  data <- penPHcure.simulate(N = N, S = S, b0 = b0, beta0 = beta0, 
                             lambdaC = lambdaC, cor_CURE = cor_CURE, 
                             cor_SURV = cor_SURV, gamma = 3)
  cure[i] <- attr(data,"perc_cure")
  cens[i] <- attr(data,"perc_cens")
  
  ### Fit CURE_ORACLE model (all covariates)
  survform <- Surv(time = tstart,time2 = tstop,event = status) ~ 
    z.1 + z.3 + z.5 + z.6
  cureform <- ~ x.1 + x.3 + x.5 + x.7
  fit <- penPHcure(survform, cureform, data = data, 
                   print.details = F, inference = T, nboot = 500)
  res <- summary(fit)
  # Store the conf. int. obtained with the basic bootstrap method
  basicCI_CURE_down[i,] <- res$CURE[, 2]
  basicCI_CURE_up[i,] <- res$CURE[, 3]
  basicCI_SURV_down[i,] <- res$SURV[, 2]
  basicCI_SURV_up[i,] <- res$SURV[, 3]
  basicCP_CURE[i,] <- (
    nonzero_b0 < basicCI_CURE_up[i,] & nonzero_b0 > basicCI_CURE_down[i,]
  )
  basicCP_SURV[i,] <- (
    nonzero_beta0 < basicCI_SURV_up[i,] & nonzero_beta0 > basicCI_SURV_down[i,]
  )
  # Store the conf. int. obtained with the percentile bootstrap method
  res <- summary(fit, conf.int = "percentile")
  percCI_CURE_down[i,] <- res$CURE[,2]
  percCI_CURE_up[i,] <- res$CURE[,3]
  percCI_SURV_down[i,] <- res$SURV[,2]
  percCI_SURV_up[i,] <- res$SURV[,3]
  percCP_CURE[i,] <- (
    nonzero_b0 < percCI_CURE_up[i,] & nonzero_b0 > percCI_CURE_down[i,]
  )
  percCP_SURV[i,] <- (
    nonzero_beta0 < percCI_SURV_up[i,] & nonzero_beta0 > percCI_SURV_down[i,]
  )

  message("Simulation ",i," of ",repl)
}

# Compute the coverage probabilities
avgCPbasic_CURE <- colMeans(basicCP_CURE)
avgCPbasic_SURV <- colMeans(basicCP_SURV)
avgCPperc_CURE <- colMeans(percCP_CURE)
avgCPperc_SURV <- colMeans(percCP_SURV)
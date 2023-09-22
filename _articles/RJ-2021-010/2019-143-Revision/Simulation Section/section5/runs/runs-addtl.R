rm(list = ls())
library(cmprsk)
library(fastcmprsk)
library(readxl)
library(benchmarkme)
#Be sure to use correct data directory
# .../RJournal_Rfiles/section5/

get_ram()
get_cpu()

source("sourceFiles/utils.R")

simGrid <- read_xlsx("simulationList.xlsx", sheet = 3)

for(i in c(3, 1, 2)) {
  simVals <- simGrid[i, ]
  nobs <- simVals$nobs
  rho  <- simVals$rho
  seed <- simVals$seed
  u.max <- simVals$u.max
  ncovs <- simVals$ncovs
  pval  <- simVals$pval
  beta1 <- c(0.4, -0.4,  0, -0.50, 0, 0.60, 0.75, 0, 0, -0.8)
  beta1 <- rep(beta1, ncovs / 10)
  beta2 <- -beta1
  
  #Store results
  
  #V <- makeDesignMatrix(nobs, length(beta1), method = "ar", rho = rho)$Sigma
  #R <- chol(V)
  
  # Fit one time
  set.seed(seed)
  #X      <- matrix(rnorm(nobs * ncovs), nrow = nobs) %*% R
  X      <- matrix(rnorm(nobs * ncovs), nrow = nobs)
  dat    <- simulateTwoCauseFineGrayModel(nobs, beta1, beta2, X = X, p = .5, u.max = u.max, returnX = FALSE)
  delta  <- sum(dat$fstatus == 0) / nobs; delta
  delta1 <- sum(dat$fstatus == 1) / nobs; delta1
  
  # fastCrr w/o variance
  writeLines(paste0("Started at: ", Sys.time()))
  fast_time_no_var   <- system.time(fit.fast   <- fastCrr(Crisk(dat$ftime, dat$fstatus) ~ X, variance = FALSE))[1:3]
  writeLines(paste0("fastCrr w/o variance completed at: ", Sys.time()))
  save(fast_time_no_var, fit.fast, 
       file = "results/addtl_simulation_fast_no_var_", i, ".RData")
  
  # fastCrr w/ variance
  writeLines(paste0("Started at: ", Sys.time()))
  fast_time_var   <- system.time(fit.fast1   <- fastCrr(Crisk(dat$ftime, dat$fstatus) ~ X, variance = TRUE,
                                                        var.control = varianceControl(B = 100, 
                                                                                      seed = 2020)))[1:3]
  writeLines(paste0("fastCrr w/ variance completed at: ", Sys.time()))
  save(fast_time_var, fit.fast1, 
       file = "results/addtl_simulation_fast_var_", i, ".RData")
  
  # crr w/o variance
  writeLines(paste0("Started at: ", Sys.time()))
  cmprsk_time_no_var <- system.time(fit.cmprsk <- crr(dat$ftime, dat$fstatus, X, variance = FALSE))[1:3]
  writeLines(paste0("crr w/o variance completed at: ", Sys.time()))
  save(cmprsk_time_no_var, fit.cmprsk, 
       file = "results/addtl_simulation_crr_no_var_", i, ".RData")
  
  # crr w/ variance
  writeLines(paste0("Started at: ", Sys.time()))
  cmprsk_time_var <- system.time(fit.cmprsk1 <- crr(dat$ftime, dat$fstatus, X, variance = TRUE))[1:3]
  writeLines(paste0("cc w/ variance completed at: ", Sys.time()))
  save(cmprsk_time_var, fit.cmprsk1, 
       file = "results/addtl_simulation_crr_var_", i, ".RData")
}


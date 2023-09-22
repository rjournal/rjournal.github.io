rm(list = ls())
library(cmprsk)
library(fastcmprsk)
library(readxl)
#Be sure to use correct data directory
# .../RJournal_Rfiles/section5/

source("sourceFiles/utils.R")

simGrid <- read_xlsx("simulationList.xlsx", sheet = 1)

for(i in 2:7) {
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
  source("internal/sim-s51-internal.R")
  save.image(file = paste0("results/np_simulation_", i, ".RData"))
}

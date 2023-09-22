rm(list = ls())
gc()

# Assume current working directory set to be folder "biglasso_reproduce/"
setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-3_Logistic_regression/real_data/P53/")

source("../real_utilities_logistic.R")
load("p53_old_processed_data.RData")
n <- nrow(X)
p <- ncol(X)
X.bm <- attach.big.matrix(descriptorfile)

# --------------------------------------------------------------
## benchmark params
# --------------------------------------------------------------
lambda.log <- F
eps <- 1e-6
lam.min <- 0.1
date <- Sys.Date()
seed <- 1234
rep <- 20
dataname <- "p53"
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso')

# --------------------------------------------------------------
## testing params
rep <- 2
# --------------------------------------------------------------

# --------------------------------------------------------------
## benchmark timings
# --------------------------------------------------------------
res <- sim_real(n, p, rep, methods, eps, lam.min, lambda.log, seed, sample.y = FALSE)

summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

## replicate Section 4.4 Validation
summary(res$obj.diff.all[, 4, ])

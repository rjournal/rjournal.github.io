rm(list = ls())
gc()

# Assume current working directory set to be folder "biglasso_reproduce/"
setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-3_Logistic_regression/real_data/NEWS20/")

source("../real_utilities_logistic.R")
load("new20_full_sparse_data.RData")
## remove features with frequency less than 10
col.idx <- which(nnz.per.col > 10)
X.raw <- X[, col.idx]
dim(X.raw)
y.raw <- y
rm(X, y)
gc()

# --------------------------------------------------------------
## benchmark params
# --------------------------------------------------------------
n <- 2500 # randomly sample 2500 data points
p <- ncol(X.raw)
lambda.log <- F
eps <- 1e-6
lam.min <- 0.1
date <- Sys.Date()
seed <- 1234
rep <- 20

backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

dataname <- "news20"
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso')

# --------------------------------------------------------------
## testing params
# n <- 20
# rep <- 2
# --------------------------------------------------------------

# --------------------------------------------------------------
## benchmark timings
# --------------------------------------------------------------
res <- sim_real_samp(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                     X.raw, y.raw, backingfile, descrpfile, backingpath) 

summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

## replicate Section 4.4 Validation
summary(res$obj.diff.all[, 4, ])

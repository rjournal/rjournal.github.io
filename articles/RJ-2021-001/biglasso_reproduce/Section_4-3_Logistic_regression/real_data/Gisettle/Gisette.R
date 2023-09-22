rm(list = ls())
gc()

# Assume current working directory set to be folder "biglasso_reproduce/"
setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-3_Logistic_regression/real_data/Gisettle/")

source("../real_utilities_logistic.R")
load("gisette_scale_full_sparse_data.RData")

X.raw <- X
y.raw <- y
rm(X, y, res)
gc()

# --------------------------------------------------------------
## benchmark params
# --------------------------------------------------------------
n <- 5000 # randomly sample 5000 data points
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

dataname <- "Gisette"
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso')

# --------------------------------------------------------------
## testing params
# n <- 200
# rep <- 2
# --------------------------------------------------------------

# --------------------------------------------------------------
## benchmark timings
# --------------------------------------------------------------
res <- sim_real_samp(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                     X.raw, y.raw, backingfile, descrpfile, backingpath) 
summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

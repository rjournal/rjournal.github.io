
# ============================================================
# Case 2: vary p
# ============================================================
rm(list = ls())
gc()

# Assume current working directory set to be folder "JSS_biglasso_reproduce/"
# Change to this directory if not.
setwd("~/GitHub/biglasso_experiment/JSS_biglasso_reproduce/")
setwd("./Section_4-3_Logistic_regression/")
source("./sim_utilities_logistic_parallel.R")

set.seed(1234)
date <- Sys.Date()
n <- 1000
p <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
q <- 20
eff.nonzero <- 1
corr <- 0
rep <- 20

methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso (1 core)',
             'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)')

eps <- 1e-6
lam.min <- 0.1
lam.log <- FALSE
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

# ===================================
# testing parameters
n <- 50
p <- c(50, 100, 150)
rep <- 2
# ===================================

res <- sim(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr, 
           rep = rep, methods = methods, eps = eps, lam.min = lam.min, 
           lam.log = lam.log, backingfile = backingfile, 
           descrpfile = descrpfile, backingpath = backingpath)
res.post <- post_analysis(res, date)

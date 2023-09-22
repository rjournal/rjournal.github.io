
# ============================================================
# Case 1: vary n
# ============================================================
rm(list = ls())
gc()

# Assume current working directory set to be folder "biglasso_reproduce/"
# Change to this directory if not.
# setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-2_Linear_regression/")
source("./sim_utilities_linear_parallel.R")

set.seed(1234)
date <- Sys.Date()

n <- c(100, 200, 500, 1000, 2000, 5000, 10000, 20000)
p <- 10000
q <- 20
eff.nonzero <- 1
corr <- 0
rep <- 20
methods <- c('glmnet', 'ncvreg', 'picasso', 'biglasso (1 core)', 'biglasso (2 cores)',
             'biglasso (4 cores)', 'biglasso (8 cores)')

# methods <- c('picasso', 'ncvreg', 'glmnet', 'SEDPP', 'SSR', 'SSR-Dome', 
#              'SSR-BEDPP', 'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)')

eps <- 1e-6
lam.min <- 0.1
lam.log <- FALSE
sigma <- 0.1
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

# ===================================
# testing parameters
p <- 100
n <- c(10, 20, 50)
rep <- 2
# ===================================

res <- sim(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr, 
           rep = rep, methods = methods, eps = eps, lam.min = lam.min, 
           lam.log = lam.log, sigma = sigma, backingfile = backingfile, 
           descrpfile = descrpfile, backingpath = backingpath)

res.post <- post_analysis(res, date)

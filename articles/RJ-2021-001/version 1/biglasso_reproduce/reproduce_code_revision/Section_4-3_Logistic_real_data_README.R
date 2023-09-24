## -----------------------------------------------------------------------------
## Replication code accompanying the paper "The biglasso Package: A Memory-
## and Computation-Efficient Solver for Lasso Model Fitting with Big Data in R"
## Authors: Yaohui Zeng and Patrick Breheny
##
## benchmarking platform:
##    MacBook Pro with Intel Core i7 @ 2.3 GHz and 16 GB RAM.
##
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Section 4.3: Computational efficiency: Logistic regression - Real data

## Replicating Table 4

# ------------------------------------------------------------------------------
# Utility functions
# ------------------------------------------------------------------------------
require(ncvreg)
require(glmnet)
require(picasso)
require(plyr)
require(ggplot2)
require(mvtnorm)
require(biglasso)

## data preprocessing utility function
read_libsvm_to_R <- function(file, nlines = -1, verbose = 10) {
  
  # the total number of lines
  com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep = "")
  n <- as.numeric(system(command = com, intern = TRUE))
  
  if (nlines == -1) {
    nlines <- n
  }
  
  lab.vec <- NULL
  row.vec <- NULL
  col.vec <- NULL
  val.vec <- NULL  
  
  con <- file(description = file, open = 'r')
  for (i in 1:nlines) {
    
    if (i %% verbose == 1) {
      cat("scanning line: ", i, "; ", format(Sys.time()), "\n")
    }
    tmp <- unlist(strsplit(readLines(con = con, n = 1, skipNul = TRUE), split = ' '))
    nnz.tmp <- length(tmp[-1]) # first element is label
    if (nnz.tmp == 0) {
      print(tmp)
      cat("\tline", i, " has zero elements: ", tmp, "\n")
    } else {
      lab.vec <- c(lab.vec, tmp[1])
      tmp <- tmp[-1] # remove first element (label)
      
      row.tmp <- rep(i, nnz.tmp) # row index of non-zero elements at line i
      tmp <- strsplit(tmp, split = ":") # feature-value pair
      col.tmp <- as.integer(unlist(lapply(tmp, function(x) x[1])))
      val.tmp <- as.double(unlist(lapply(tmp, function(x) x[2])))
      
      row.vec <- c(row.vec, row.tmp)
      col.vec <- c(col.vec, col.tmp)
      val.vec <- c(val.vec, val.tmp)
    }
    
  }
  close(con)
  
  # cat("Start creating X sparse matrix: ", format(Sys.time()), "\n\n")
  X <- sparseMatrix(i = row.vec, j = col.vec, x = val.vec)
  # cat("End creating X sparse matrix: ", format(Sys.time()), "\n\n")
  y <- ifelse(as.integer(lab.vec) == -1, 0, 1)
  
  list(X = X,
       y = y,
       n = n,
       p = ncol(X),
       file = file,
       lab.vec = lab.vec,
       row.vec = row.vec,
       col.vec = col.vec,
       val.vec = val.vec
  )
  
}

# picasso loss and objective
picasso_obj <- function(fit, x, y) {
  n <- length(y)
  beta <- as.matrix(fit$beta)
  int <- fit$intercept
  lambda <- fit$lambda
  obj.val <- NULL
  loss <- NULL
  if (fit$family == "binomial") {
    for (i in 1:length(lambda)) {
      eta <- int[i] + x %*% beta[, i]
      tmp <- -sum(y * eta - log(1 + exp(eta))) # negative log-likelihood
      loss <- c(loss, tmp)
      obj.val <- c(obj.val, tmp / n + lambda[i] * sum(abs(beta[, i])))
    }
  } else if (fit$family == "gaussian") {
    for (i in 1:length(lambda)) {
      tmp <- crossprod(y - x %*% beta[, i] - int[i]) # residual sum of squares
      loss <- c(loss, tmp)
      obj.val <- c(obj.val, tmp / (2*n) + lambda[i] * sum(abs(beta[, i])))
    }
  }
  list(loss = loss, obj.val = obj.val)
}

# picasso vs glmnet, relative objective difference at each lambda
rel_obj_diff_pic <- function(fit.glm, fit, x, y) {
  n <- length(y)
  dev.glm <- (1 - fit.glm$dev.ratio) * fit.glm$nulldev / 2
  obj.glm <- as.numeric(dev.glm / n  + fit.glm$lambda * colSums(abs(fit.glm$beta)))
  obj.fit <- picasso_obj(fit, x, y)
  rel.diff.obj <- (obj.fit$obj.val - obj.glm) / obj.glm
  rel.diff.obj
}

# relative objective difference at each lambda
rel_obj_diff <- function(fit.glm, fit) {
  n <- fit.glm$nobs
  dev.glm <- (1 - fit.glm$dev.ratio) * fit.glm$nulldev / 2
  obj.glm <- as.numeric(dev.glm / n  + fit.glm$lambda * colSums(abs(fit.glm$beta)))
  
  if ("glmnet" %in% class(fit)) {
    dev.glm.fit <- (1 - fit$dev.ratio) * fit$nulldev / 2
    obj.fit <- as.numeric(dev.glm.fit / n  + fit$lambda * colSums(abs(fit$beta)))
  } else {
    obj.fit <- as.numeric(fit$loss / n + fit$lambda * colSums(abs(fit$beta[-1, ])))
  }
  rel.diff.obj <- (obj.fit - obj.glm) / obj.glm
  rel.diff.obj
}

## benchmark timings: sample submatrix and response
sim_real_samp <- function(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                          X.raw, y.raw, backingfile, descrpfile, backingpath) {
  parms <- list(n = n, p = p, rep = rep, methods = methods, eps = eps,
                lam.min = lam.min, lam.log = lambda.log, seed = seed)
  
  cat("\nStart simulation: ", format(Sys.time()))
  cat("\n============================================================\n")
  cat("\nR sessionInfo: \n\n")
  print(sessionInfo())
  
  ## print out simulation setting
  cat("\n============================================================\n")
  cat("\t Simulation settings: \n")
  cat("\t ---------------------------------------------\n")
  cat("\t n = ", n, '\n')
  cat("\t p = ", p, '\n')
  cat("\t rep = ", rep, '\n')
  cat("\t methods = ", methods, '\n')
  cat("\t eps = ", eps, '\n')
  cat("\t lam.min = ", lam.min, '\n')
  cat("\t lam.log = ", lambda.log, '\n')
  cat("\n============================================================\n\n")
  
  time.all <- array(NA, dim = c(rep, length(methods), length(p)))
  obj.diff.all <- array(NA, dim = c(100 * rep, length(methods), length(p)))
  
  for (i in 1:length(p)) {
    cat("\tp =", p[i], "; start time: ", format(Sys.time()), '\n')
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      new.seed <- seed + 100*i+j
      set.seed(new.seed)
      row.idx <- seq(100*(j-1) + 1, 100*j)
      time <- NULL
      
      # sample submatrix
      sub.row.idx <- sample(nrow(X.raw), size = n)
      X <- X.raw[sub.row.idx, ]
      y <- y.raw[sub.row.idx]
      X <- as.matrix(X)
      
      X.bm <- as.big.matrix(X, backingfile = backingfile, descriptorfile = descrpfile,
                            backingpath = backingpath, type = 'double')
      
      fit.hsr0 <- biglasso(X.bm, y, family = 'binomial', screen = 'SSR', 
                           lambda.log.scale = lambda.log, lambda.min = lam.min,
                           eps = eps)
      lambda <- fit.hsr0$lambda
      
      # glmnet true for comparing coef and obj difference
      glmnet.control(fdev = 0, devmax = 1)
      fit.glm.true <- glmnet(X, y, family = 'binomial', lambda = lambda, thresh = eps)
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(X, y, family = 'binomial', 
                                           prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 1, i] <- rel_obj_diff_pic(fit.glm.true, fit.pic, X, y)
      rm(fit.pic)
      gc()
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(X, y, penalty = 'lasso', family = 'binomial',
                                          lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 2, i] <- rel_obj_diff(fit.glm.true, fit.ncv)
      rm(fit.ncv)
      gc()
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm <- glmnet(X, y, family = 'binomial', 
                                          lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 3, i] <- rel_obj_diff(fit.glm.true, fit.glm)
      rm(fit.glm)
      rm(X)
      gc()
      
      # SSR-Slores, 1 core
      st <- system.time(fit.slores1 <- biglasso(X.bm, y, family = 'binomial', 
                                                screen = "SSR-Slores", 
                                                safe.thresh = 0, ncores = 1,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 4, i] <- rel_obj_diff(fit.glm.true, fit.slores1)
      rm(fit.slores1)
      gc()
      
      file.remove(paste0(backingpath, '/', backingfile))
      file.remove(paste0(backingpath, '/', descrpfile))
      
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time.all = ', format(time, nsmall=3), "\n")
      
    }
    cat("\tp =", p[i], "; end time: ", format(Sys.time()), "\n")  
    cat("\n============================================================\n")
    
  }
  
  list(time.all = time.all,
       parms = parms,
       obj.diff.all = obj.diff.all,
       which.vary = p,
       which.vary.c = "p")
  
}

sim_real <- function(n, p, rep, methods, eps, lam.min, lambda.log, seed, sample.y = FALSE) {
  
  parms <- list(n = n, p = p, rep = rep, methods = methods, eps = eps,
                lam.min = lam.min, lam.log = lambda.log, seed = seed)
  
  cat("\nStart simulation: ", format(Sys.time()))
  cat("\n============================================================\n")
  cat("\nR sessionInfo: \n\n")
  print(sessionInfo())
  
  ## print out simulation setting
  cat("\n============================================================\n")
  cat("\t Simulation settings: \n")
  cat("\t ---------------------------------------------\n")
  cat("\t n = ", n, '\n')
  cat("\t p = ", p, '\n')
  cat("\t rep = ", rep, '\n')
  cat("\t methods = ", methods, '\n')
  cat("\t eps = ", eps, '\n')
  cat("\t lam.min = ", lam.min, '\n')
  cat("\t lam.log = ", lambda.log, '\n')
  cat("\n============================================================\n\n")
  
  time.all <- array(NA, dim = c(rep, length(methods), length(p)))
  
  obj.diff.all <- array(NA, dim = c(100 * rep, length(methods), length(p)))
  
  for (i in 1:length(p)) {
    cat("\tp =", p[i], "; start time: ", format(Sys.time()), '\n')
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      new.seed <- seed + 100*i+j
      set.seed(new.seed)
      row.idx <- seq(100*(j-1) + 1, 100*j)
      time <- NULL
      
      if (sample.y) {
        ## sample response verctor for different replications (NYT and MNIST data only)
        y.idx <- sample(ncol(Y), 1)
        y <- as.numeric(Y[, y.idx])
      }
      
      fit.hsr0 <- biglasso(X.bm, y, family = 'binomial', screen = 'SSR',
                           lambda.log.scale = lambda.log, lambda.min = lam.min,
                           eps = eps)
      lambda <- fit.hsr0$lambda
      
      # glmnet true for comparing coef and obj difference
      glmnet.control(fdev = 0, devmax = 1)
      fit.glm.true <- glmnet(X, y, family = 'binomial', lambda = lambda, thresh = eps)
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(X, y, family = 'binomial',
                                           prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 1, i] <- rel_obj_diff_pic(fit.glm.true, fit.pic, X, y)
      
      rm(fit.pic)
      gc()
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(X, y, penalty = 'lasso', family = 'binomial',
                                          lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 2, i] <- rel_obj_diff(fit.glm.true, fit.ncv)
      
      rm(fit.ncv)
      gc()
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm <- glmnet(X, y, family = 'binomial',
                                          lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 3, i] <- rel_obj_diff(fit.glm.true, fit.glm)
      
      rm(fit.glm)
      gc()
      
      # SSR-Slores, 1 core
      st <- system.time(fit.slores1 <- biglasso(X.bm, y, family = 'binomial',
                                                screen = "SSR-Slores",
                                                safe.thresh = 0, ncores = 1,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      obj.diff.all[row.idx, 4, i] <- rel_obj_diff(fit.glm.true, fit.slores1)
      
      rm(fit.slores1)
      gc()
      
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time.all = ', format(time, nsmall=3), "\n")
      
    }
    cat("\tp =", p[i], "; end time: ", format(Sys.time()), "\n")
    cat("\n============================================================\n")
    
  }
  
  list(time.all = time.all,
       parms = parms,
       obj.diff.all = obj.diff.all,
       which.vary = p,
       which.vary.c = "p")
  
}

summary_res <- function(res, dataname) {
  cat("\n============================================================\n")
  # cat("\n Time.all: \n\n")
  # print(res$time.all)
  
  cat("\n Time.all Mean: \n\n")
  time.mean.all <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
  rownames(time.mean.all) <- methods
  colnames(time.mean.all) <- dataname
  print(time.mean.all)
  
  cat("\n Time.all SE: \n\n")
  time.se.all <- apply(res$time.all, c(2, 3), function(x) {
    x <- x[!is.na(x)]
    if (length(x) <= 1) {
      return(NA)
    } else {
      return(sd(x) / sqrt(length(x)))
    }
  })
  rownames(time.se.all) <- methods
  colnames(time.se.all) <- dataname
  print(time.se.all)
}

# --------------------------------------------------------------
# Gisette (data file: gisette_scale_full_sparse_data.RData)
# --------------------------------------------------------------
#
# To replicate this part, need first download data and preprocess
#
# 1. download file "gisette_scale.bz2" at https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html, then unzip
#
# 2. run following R code to get .Rdata
library(bigmemory)
library(Matrix)
## parse libsvm-format data
file <- "gisette_scale"
# ## the number of lines
com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep = "")
n <- as.numeric(system(command = com, intern=TRUE)) # 19996
## call the data preprocessing utility function
res <- read_libsvm_to_R(file, nlines = -1, verbose = 1000)
X <- res$X
y <- res$y
n <- nrow(X)
p <- ncol(X)
summ.X <- summary(X)
nnz.elems <- length(summ.X$x)
nnz.per.row <- as.integer(table(summ.X$i))
nnz.per.col <- as.integer(table(summ.X$j))
save(X, y, nnz.elems, nnz.per.row, nnz.per.col, res,
     file = paste0(file, "_full_sparse_data.RData"))

# 3. code for simulation and timing
# --------------------------------------------------------------
load("gisette_scale_full_sparse_data.RData")

X.raw <- X
y.raw <- y
rm(X, y, res)
gc()

## benchmark params
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

## benchmark timings
res <- sim_real_samp(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                     X.raw, y.raw, backingfile, descrpfile, backingpath) 
summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))


# --------------------------------------------------------------
# P53 (data file: p53_old_processed_data.RData)
# --------------------------------------------------------------
#
# To replicate this part, need first download data and preprocess
#
# 1. download file "p53_old_2010.zip" at https://archive.ics.uci.edu/ml/machine-learning-databases/p53/, then unzip to get folder "p53_old_2010". The data file is K8.data.
#
# 2. run following R code to process the data
rm(list = ls())
library(bigmemory)
# in raw data, 16772 rows, 5409 attributes: 5408 features + 1 label column 
dat.old <- fread(file = "./p53_old_2010/K8.data", sep = ",", header = F)
p <- ncol(dat.old)

# drop last column, which is NA
dat <- as.data.frame(dat.old)[, -ncol(dat.old)]
y <- dat[, ncol(dat)]
y <- ifelse(y == "active", 1, 0)

## design matrix:
#convert to numeric, remove missing rows
X <- apply(dat[, -ncol(dat)], 2, as.numeric) 
# na.idx2 <- which(is.na(X[, 1]))
na.idx <- apply(X, 1, function(x) any(is.na(x)))
# final design matrix: 16592-by-5408
X <- X[!na.idx, ] 
y <- y[!na.idx]

backingfile <- 'p53_old_bin.bin'
descriptorfile <- 'p53_old_des.desc'
backingpath <- getwd()

X.bm <- as.big.matrix(X, type = "double", 
                      descriptorfile = descriptorfile, 
                      backingfile = backingfile,
                      backingpath = backingpath)
save(X, y, backingfile, descriptorfile, backingpath, file = "p53_old_processed_data.RData")

# 3. code for simulation and timing
# --------------------------------------------------------------
rm(list = ls())
gc()

load("p53_old_processed_data.RData")
n <- nrow(X)
p <- ncol(X)
X.bm <- attach.big.matrix(descriptorfile)

## benchmark params
lambda.log <- F
eps <- 1e-6
lam.min <- 0.1
date <- Sys.Date()
seed <- 1234
rep <- 20
dataname <- "p53"
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso')

## benchmark timings
res <- sim_real(n, p, rep, methods, eps, lam.min, lambda.log, seed, sample.y = FALSE)

summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

## replicate Section 4.4 Validation
summary(res$obj.diff.all[, 4, ])


# --------------------------------------------------------------
# NEWS20 (data file: new20_full_sparse_data.RData)
# --------------------------------------------------------------
#
# To replicate this part, need first download data and preprocess
#
# 1. download file "news20.binary.bz2" at https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html, then unzip to get file "news20.binary". 
#
# 2. run following R code to get process the data.
rm(list = ls())
library(data.table)
library(bigmemory)
library(Matrix)

## parse libsvm-format data
file <- "news20.binary"
res <- read_libsvm_to_R(file, nlines = -1, verbose = 100)
X <- res$X
na.row.idx <- which(rowSums(X) == 0)
X <- X[-na.row.idx, ]
na.col.idx <- which(colSums(X) == 0)
if (length(na.col.idx)) {
  X <- X[, -na.col.idx]
}
y <- res$y
n <- nrow(X)
p <- ncol(X)
summ.X <- summary(X)
nnz.elems <- length(summ.X$x)
nnz.per.row <- as.integer(table(summ.X$i))
nnz.per.col <- as.integer(table(summ.X$j))

save(X, y, nnz.elems, nnz.per.row, nnz.per.col, res, file = "new20_full_sparse_data.RData")

# 3. code for simulation and timing
# --------------------------------------------------------------
rm(list = ls())
gc()
load("new20_full_sparse_data.RData")
## remove features with frequency less than 10
col.idx <- which(nnz.per.col > 10)
X.raw <- X[, col.idx]
dim(X.raw)
y.raw <- y
rm(X, y)
gc()

## benchmark params
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

## benchmark timings
res <- sim_real_samp(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                     X.raw, y.raw, backingfile, descrpfile, backingpath) 

summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

## replicate Section 4.4 Validation
summary(res$obj.diff.all[, 4, ])


# --------------------------------------------------------------
# RCV1 (data file: rcv1_full_sparse_data.RData)
# --------------------------------------------------------------
#
# To replicate this part, need first download data and preprocess
#
# 1. download file "rcv1_train.binary.bz2" at https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html, then unzip to get file "rcv1_train.binary". 
#
# 2. run following R code to get process the data.
#
rm(list = ls())
# library(data.table)
library(bigmemory)
library(Matrix)

## parse libsvm-format data
file <- "rcv1_train.binary"
com <- paste("wc -l ", file, " | awk '{ print $1 }'", sep = "")
n <- as.numeric(system(command = com, intern=TRUE)) # 19996
res <- read_libsvm_to_R(file, nlines = -1, verbose = 1000)
X <- res$X
y <- res$y
n <- nrow(X)
p <- ncol(X)
summ.X <- summary(X)
nnz.elems <- length(summ.X$x)
nnz.per.row <- as.integer(table(summ.X$i))
nnz.per.col <- as.integer(table(summ.X$j))

save(X, y, nnz.elems, nnz.per.row, nnz.per.col, res, file = "rcv1_full_sparse_data.RData")

#
# 3. code for simulation and timing
# --------------------------------------------------------------
rm(list = ls())
gc()
load("rcv1_full_sparse_data.RData")
X.raw <- X
y.raw <- y
rm(X, y)
gc()

## benchmark params
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

dataname <- "rcv1"
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso')

## benchmark timings
res <- sim_real_samp(n, p, rep, methods, eps, lam.min, lambda.log, seed, date,
                     X.raw, y.raw, backingfile, descrpfile, backingpath) 

summary_res(res, dataname)
save(res, file = paste0(date, "_", dataname, "_", eps, "_Results.RData"))

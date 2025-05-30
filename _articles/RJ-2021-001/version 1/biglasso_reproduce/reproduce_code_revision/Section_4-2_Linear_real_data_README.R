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
## Section 4.2: Computational efficiency: Linear regression - Real data

## Replicating Table 3

# --------------------------------------------------------------
# GENE (data file: bcTCGA.RData)
# --------------------------------------------------------------

rm(list = ls())
require(biglasso)
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)
require(microbenchmark)

load("bcTCGA.RData")
x <- X
p <- ncol(x)
n <- nrow(x)
x.bm <- as.big.matrix(x, type = 'double')
date <- Sys.Date()

lambda.log <- F
eps <- 1e-6
safe.thresh <- 0
lam.min <- 0.1
fit.hsr0 <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR', 
                     lambda.log.scale = lambda.log, lambda.min = lam.min,
                     eps = eps)
cat("eps = ", eps, "; safe.thresh = ", safe.thresh, "; lambda.log = ", lambda.log)
t <- microbenchmark(
  fit.glm <- glmnet(x, y, family = 'gaussian', 
                    lambda = fit.hsr0$lambda,
                    lambda.min.ratio = lam.min, 
                    thresh = eps),
  fit.ncv <- ncvreg(x, y, penalty = 'lasso', 
                    lambda = fit.hsr0$lambda,
                    family = 'gaussian', 
                    eps = sqrt(eps)),
  fit.pic <- picasso(x, y, prec = eps, 
                     lambda = fit.hsr0$lambda
  ),
  fit.hsr.bedpp <- biglasso(x.bm, y, family = 'gaussian', screen = "SSR-BEDPP", 
                            # lambda.log.scale = lambda.log, 
                            lambda.min = lam.min,
                            lambda = fit.hsr0$lambda,
                            safe.thresh = safe.thresh,
                            eps = eps),
  times = 20,
  control = list(order = 'inorder')
)

method <- c('glmnet', 'ncvreg', 'picasso', 'biglasso')
t.mat <- matrix(t$time, nrow = length(method), byrow = FALSE) / 1e9 # nanosecond to second
time.mean <- apply(t.mat, 1, mean, na.rm = TRUE)
time.se <- apply(t.mat, 1, function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
})

t.df <- data.frame(
  method = method,
  time.mean = time.mean,
  time.se = time.se
)
print(t.df)
# --------------------------------------------------------------


# --------------------------------------------------------------
# GWAS (data file: Data.RData)
# --------------------------------------------------------------

rm(list = ls())
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)
require(biglasso)

cat("\n\n---------------------------------------------\n")
cat("\nSession Info: \n\n")
sessionInfo()
cat("\n\n")

picasso_obj <- function(fit, x, y) {
  n <- length(y)
  beta <- as.matrix(fit$beta)
  int <- fit$intercept
  lambda <- fit$lambda
  obj.val <- NULL
  for (i in 1:length(lambda)) {
    loss <- crossprod(y - x %*% beta[, i] - int[i]) / (2 * n) + lambda[i] * sum(abs(beta[, i]))
    obj.val <- c(obj.val, loss)
  }
  obj.val
}

rel_obj_diff <- function(fit.glm.true, fit, n, x, y) {
  dev.glm <- (1 - fit.glm.true$dev.ratio) * fit.glm.true$nulldev
  obj.glm <- as.numeric(dev.glm / (2 * n)  + fit.glm.true$lambda * colSums(abs(fit.glm.true$beta)))
  
  if ("glmnet" %in% class(fit)) {
    dev.glm.fit <- (1 - fit$dev.ratio) * fit$nulldev
    obj.fit <- as.numeric(dev.glm.fit / (2 * n)  + fit$lambda * colSums(abs(fit$beta)))
  } else if ('gaussian' %in% class(fit)) { # for PICASSO
    obj.fit <- picasso_obj(fit, x, y)
  } else {
    obj.fit <- as.numeric(fit$loss / (2 * n) + fit$lambda * colSums(abs(fit$beta[-1, ])))
  }
  rel.diff.obj <- (obj.fit - obj.glm) / obj.glm
  rel.diff.obj
}

## benchmark timings
sim <- function(n, p, p.samp, rep, case.method, backingfile, descrpfile, backingpath, seed, 
                eps = 1e-6, safe.thresh = 0, lam.min = 0.1, lambda.log = FALSE,
                sample.y = FALSE) {
  
  time.all <- array(NA, dim = c(rep, case.method, length(p)))
  
  obj.diff.ncv <- NULL
  obj.diff.pic <- NULL
  obj.diff.hsr.bedpp1 <- NULL
  
  for (i in 1:length(p)) {
    cat("\tp =", p[i], "; start time: ", format(Sys.time()), '\n')  
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      time <- NULL
      
      x <- X
      if (p.samp < p) {
        col.idx <- sample(p, p.samp)
        x <- X[, col.idx]
      } 
      storage.mode(x) <- 'double'
      if (sample.y) {
        ## sample response verctor for different replications (NYT and MNIST data only)
        y.idx <- sample(ncol(Y), 1)
        y <- as.numeric(Y[, y.idx])
      }
      
      fit.hsr0 <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR', 
                           lambda.log.scale = lambda.log, lambda.min = lam.min,
                           eps = eps, ncores = 4)
      lambda <- fit.hsr0$lambda
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm.true <- glmnet(x, y, family = 'gaussian', 
                                               lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(x, y, penalty = 'lasso', family = 'gaussian',
                                          lambda = lambda, eps = sqrt(eps)))
      time <- c(time, st['elapsed'])
      obj.diff.ncv <- c(obj.diff.ncv, rel_obj_diff(fit.glm.true, fit.ncv, n, x, y))
      
      rm(fit.ncv)
      gc()
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(x, y, prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      obj.diff.pic <- c(obj.diff.pic, rel_obj_diff(fit.glm.true, fit.pic, n, x, y))
      
      rm(fit.pic)
      gc()
      
      # SSR-BEDPP, 1 core
      st <- system.time(fit.bedpp1 <- biglasso(x.bm, y, family = 'gaussian', 
                                               screen = "SSR-BEDPP", 
                                               safe.thresh = 0, 
                                               lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      obj.diff.hsr.bedpp1 <- c(obj.diff.hsr.bedpp1, rel_obj_diff(fit.glm.true, fit.bedpp1, n, x, y))
      rm(fit.bedpp1)
      gc()
      
      time <- as.numeric(time)
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time = ', time, "\n")
      
    }
    cat("\tp =", p[i], "; end time: ", format(Sys.time()), "\n")  
    cat("\n============================================================\n")
    
  }
  
  obj.diff <- data.frame(
    obj.diff.ncv = obj.diff.ncv,
    obj.diff.pic = obj.diff.pic,
    obj.diff.hsr.bedpp1 = obj.diff.hsr.bedpp1
  )
  
  list(time.all = time.all,
       obj.diff = obj.diff
  )
}

load("Data.RData")
seed <- 1234
set.seed(seed)
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

x.bm <- attach.big.matrix(descrpfile)
n <- nrow(X)
p <- ncol(X)

date <- Sys.Date()
method <- c('glmnet', 'ncvreg', 'picasso', 'biglasso')
case.method <- length(method)

case.p <- length(p)
p.keep <- p
rep <- 20
eps <- 1e-6
safe.thresh = 0
lam.min = 0.1
lambda.log = FALSE

cat("\nStart simulation: ", format(Sys.time()))
cat("\n============================================================\n")

res <- sim(n, p, p.samp = p.keep,
           rep, case.method, backingfile, descrpfile, backingpath, seed = seed,
           eps=eps, safe.thresh = safe.thresh, lam.min = lam.min,
           lambda.log = lambda.log, sample.y = FALSE)

cat("\nEnd simulation: ", format(Sys.time()), "\n")

time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
})

t.df <- data.frame(
  method = method,
  time.mean = time.mean,
  time.se = time.se
)
cat("\nAverage run time: \n\n")
print(t.df)

save(list = ls(), file = paste0(date, '_GWAS_results.RData'))

## replicate Section 4.4 Validation
summary(res$obj.diff$obj.diff.hsr.bedpp1)


# --------------------------------------------------------------
# MNIST (data file: mnist_data.Rdata)
# --------------------------------------------------------------

rm(list = ls())
require(microbenchmark)
require(Matrix)
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)
require(biglasso)

cat("\n\n---------------------------------------------\n")
cat("\nSession Info: \n\n")
sessionInfo()
cat("\n\n")

picasso_obj <- function(fit, x, y) {
  n <- length(y)
  beta <- as.matrix(fit$beta)
  int <- fit$intercept
  lambda <- fit$lambda
  obj.val <- NULL
  for (i in 1:length(lambda)) {
    loss <- crossprod(y - x %*% beta[, i] - int[i]) / (2 * n) + lambda[i] * sum(abs(beta[, i]))
    obj.val <- c(obj.val, loss)
  }
  obj.val
}

rel_obj_diff <- function(fit.glm.true, fit, n, x, y) {
  dev.glm <- (1 - fit.glm.true$dev.ratio) * fit.glm.true$nulldev
  obj.glm <- as.numeric(dev.glm / (2 * n)  + fit.glm.true$lambda * colSums(abs(fit.glm.true$beta)))
  
  if ("glmnet" %in% class(fit)) {
    dev.glm.fit <- (1 - fit$dev.ratio) * fit$nulldev
    obj.fit <- as.numeric(dev.glm.fit / (2 * n)  + fit$lambda * colSums(abs(fit$beta)))
  } else if ('gaussian' %in% class(fit)) { # for PICASSO
    obj.fit <- picasso_obj(fit, x, y)
  } else {
    obj.fit <- as.numeric(fit$loss / (2 * n) + fit$lambda * colSums(abs(fit$beta[-1, ])))
  }
  rel.diff.obj <- (obj.fit - obj.glm) / obj.glm
  rel.diff.obj
}

## benchmark timings
sim <- function(n, p, p.samp, rep, case.method, backingfile, descrpfile, backingpath, 
                eps = 1e-6, safe.thresh = 0, lam.min = 0.1, lambda.log = FALSE) {
  
  time.all <- array(NA, dim = c(rep, case.method, length(p)))
  
  obj.diff.ncv <- NULL
  obj.diff.pic <- NULL
  obj.diff.hsr <- NULL
  obj.diff.hsr.bedpp1 <- NULL
  
  for (i in 1:length(p)) {
    cat("\tp =", p[i], "; start time: ", format(Sys.time()), '\n')  
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      time <- NULL
      
      ## MNIST data
      x <- X
      if (p.samp < p) {
        col.idx <- sample(p, p.samp)
        x <- X[, col.idx]
      } 
      storage.mode(x) <- 'double'
      y.idx <- sample(ncol(Y), 1)
      y <- as.numeric(Y[, y.idx])
      
      x.bm <- as.big.matrix(x, backingfile = backingfile, descriptorfile = descrpfile,
                            backingpath = backingpath, type = 'double')
      
      fit.hsr0 <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR', 
                           lambda.log.scale = lambda.log, lambda.min = lam.min,
                           eps = eps, ncores = 4)
      lambda <- fit.hsr0$lambda
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm.true <- glmnet(x, y, family = 'gaussian', 
                                               lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(x, y, penalty = 'lasso', family = 'gaussian',
                                          lambda = lambda, eps = sqrt(eps)))
      time <- c(time, st['elapsed'])
      obj.diff.ncv <- c(obj.diff.ncv, rel_obj_diff(fit.glm.true, fit.ncv, n, x, y))
      
      rm(fit.ncv)
      gc()
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(x, y, prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      obj.diff.pic <- c(obj.diff.pic, rel_obj_diff(fit.glm.true, fit.pic, n, x, y))
      
      rm(fit.pic)
      gc()
      
      # SSR-BEDPP, 1 core
      st <- system.time(fit.bedpp1 <- biglasso(x.bm, y, family = 'gaussian', 
                                               screen = "SSR-BEDPP", 
                                               safe.thresh = 0, 
                                               lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      obj.diff.hsr.bedpp1 <- c(obj.diff.hsr.bedpp1, rel_obj_diff(fit.glm.true, fit.bedpp1, n, x, y))
      rm(fit.bedpp1)
      gc()
      
      file.remove(paste0(backingpath, '/', backingfile))
      file.remove(paste0(backingpath, '/', descrpfile))
      
      time <- as.numeric(time)
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time = ', time, "\n")
      
    }
    cat("\tp =", p[i], "; end time: ", format(Sys.time()), "\n")  
    cat("\n============================================================\n")
    
  }
  
  obj.diff <- data.frame(
    obj.diff.ncv = obj.diff.ncv,
    obj.diff.pic = obj.diff.pic,
    obj.diff.hsr.bedpp1 = obj.diff.hsr.bedpp1
  )
  
  list(time.all = time.all,
       obj.diff = obj.diff
  )
}

load("mnist_data.RData")
## benchmark timings
seed <- 1234
set.seed(seed)
n <- nrow(X)
p <- ncol(X)
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

date <- Sys.Date()
method <- c('glmnet', 'ncvreg', 'picasso', 'biglasso')
case.method <- length(method)

case.p <- length(p)
p.keep <- p
rep <- 20
eps <- 1e-6
safe.thresh <- 0
lam.min <- 0.1
lambda.log <- FALSE

cat("\nStart simulation: ", format(Sys.time()))
cat("\n============================================================\n")
res <- sim(n, p, p.samp = p.keep, 
           rep, case.method, backingfile, descrpfile, backingpath, 
           eps=eps, safe.thresh = safe.thresh, lam.min = lam.min, 
           lambda.log = lambda.log)

cat("\nEnd simulation: ", format(Sys.time()), "\n")

time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
})

t.df <- data.frame(
  method = method,
  time.mean = time.mean,
  time.se = time.se
)
cat("\nAverage run time: \n\n")
print(t.df)

save(list = ls(), file = paste0(date, '_mnist_results.RData'))

## replicate Section 4.4 Validation
summary(res$obj.diff$obj.diff.hsr.bedpp1)


# --------------------------------------------------------------
# NYT (data file: nytimes_data_n5500_p55000.RData)
# --------------------------------------------------------------
#
# To replicate this part, need to first create memory-mapped files
#
require(biglasso)
backingfile <- 'nyt_n5500_p55000_back.bin'
descrpfile <- 'nyt_n5500_p55000_desc.desc'
backingpath <- getwd()

x <- as.matrix(X)
x.bm <- as.big.matrix(x, backingfile = backingfile, descriptorfile = descrpfile,
                      backingpath = backingpath, type = 'double')


## Code for simulation and timing
# --------------------------------------------------------------

rm(list = ls())
require(microbenchmark)
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)

picasso_obj <- function(fit, x, y) {
  n <- length(y)
  beta <- as.matrix(fit$beta)
  int <- fit$intercept
  lambda <- fit$lambda
  obj.val <- NULL
  for (i in 1:length(lambda)) {
    loss <- crossprod(y - x %*% beta[, i] - int[i]) / (2 * n) + lambda[i] * sum(abs(beta[, i]))
    obj.val <- c(obj.val, loss)
  }
  obj.val
}

rel_obj_diff <- function(fit.glm.true, fit, n, x, y) {
  dev.glm <- (1 - fit.glm.true$dev.ratio) * fit.glm.true$nulldev
  obj.glm <- as.numeric(dev.glm / (2 * n)  + fit.glm.true$lambda * colSums(abs(fit.glm.true$beta)))
  
  if ("glmnet" %in% class(fit)) {
    dev.glm.fit <- (1 - fit$dev.ratio) * fit$nulldev
    obj.fit <- as.numeric(dev.glm.fit / (2 * n)  + fit$lambda * colSums(abs(fit$beta)))
  } else if ('gaussian' %in% class(fit)) { # for PICASSO
    obj.fit <- picasso_obj(fit, x, y)
  } else {
    obj.fit <- as.numeric(fit$loss / (2 * n) + fit$lambda * colSums(abs(fit$beta[-1, ])))
  }
  rel.diff.obj <- (obj.fit - obj.glm) / obj.glm
  rel.diff.obj
}

## benchmark timings
sim <- function(n, p, p.samp, rep, case.method, backingfile, descrpfile, backingpath,
                eps, safe.thresh, lam.min, lambda.log) {
  
  time.all <- array(NA, dim = c(rep, case.method, length(p)))
  
  obj.diff.ncv <- NULL
  obj.diff.pic <- NULL
  obj.diff.hsr.bedpp1 <- NULL
  
  for (i in 1:length(p)) {
    cat("\tp =", p[i], "; start time: ", format(Sys.time()), '\n')
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      time <- NULL
      
      ## NYT data
      x <- as.matrix(X)
      storage.mode(x) <- 'double'
      y.idx <- sample(ncol(Y), 1)
      y <- as.numeric(Y[, y.idx])
      
      fit.hsr0 <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR',
                           lambda.log.scale = lambda.log, lambda.min = lam.min,
                           eps = eps)
      lambda <- fit.hsr0$lambda
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm.true <- glmnet(x, y, family = 'gaussian', 
                                               lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(x, y, penalty = 'lasso', family = 'gaussian',
                                          lambda = lambda, eps = sqrt(eps)))
      time <- c(time, st['elapsed'])
      obj.diff.ncv <- c(obj.diff.ncv, rel_obj_diff(fit.glm.true, fit.ncv, n, x, y))
      
      rm(fit.ncv)
      gc()
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(x, y, prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      obj.diff.pic <- c(obj.diff.pic, rel_obj_diff(fit.glm.true, fit.pic, n, x, y))
      
      rm(fit.pic)
      gc()
      
      # SSR-BEDPP, 1 core
      st <- system.time(fit.bedpp1 <- biglasso(x.bm, y, family = 'gaussian', 
                                               screen = "SSR-BEDPP", 
                                               safe.thresh = 0, 
                                               lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      obj.diff.hsr.bedpp1 <- c(obj.diff.hsr.bedpp1, rel_obj_diff(fit.glm.true, fit.bedpp1, n, x, y))
      
      rm(fit.bedpp1)
      gc()
      
      time <- as.numeric(time)
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time = ', time, "\n")
      
    }
    cat("\tp =", p[i], "; end time: ", format(Sys.time()), "\n")  
    cat("\n============================================================\n")
    
  }
  
  obj.diff <- data.frame(
    obj.diff.ncv = obj.diff.ncv,
    obj.diff.pic = obj.diff.pic,
    obj.diff.hsr.bedpp1 = obj.diff.hsr.bedpp1
  )
  
  list(time.all = time.all,
       obj.diff = obj.diff
  )
}

load("nytimes_data_n5500_p55000.RData")
x.bm <- attach.big.matrix("nyt_n5500_p55000_desc.desc")
n <- nrow(X)
p <- ncol(X)

seed <- 1234
set.seed(seed)
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

date <- Sys.Date()
method <- c('glmnet', 'ncvreg', 'picasso', 'biglasso')
case.method <- length(method)

case.p <- length(p)
p.keep <- p
rep <- 20
eps <- 1e-6
safe.thresh = 0
lam.min = 0.1
lambda.log = FALSE

cat("\nStart simulation: ", format(Sys.time()))
cat("\n============================================================\n")

res <- sim(n, p, p.samp = p.keep,
           rep, case.method, backingfile, descrpfile, backingpath,
           eps=eps, safe.thresh = safe.thresh, lam.min = lam.min,
           lambda.log = lambda.log)

cat("\nEnd simulation: ", format(Sys.time()), "\n")

time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)

time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
})

t.df <- data.frame(
  method = method,
  time.mean = time.mean,
  time.se = time.se
)
cat("\nAverage run time: \n\n")
print(t.df)

save(list = ls(), file = paste0(date, '_NYT_results.RData'))

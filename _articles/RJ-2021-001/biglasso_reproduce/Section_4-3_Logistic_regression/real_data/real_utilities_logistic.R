
require(ncvreg)
require(glmnet)
require(picasso)
require(plyr)
require(ggplot2)
require(mvtnorm)
require(biglasso)

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

## benchmark timings: sample submatrix and response, for news20 data
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

## benchmark timings
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

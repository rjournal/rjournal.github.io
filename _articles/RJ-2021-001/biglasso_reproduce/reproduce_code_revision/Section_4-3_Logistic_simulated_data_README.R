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
## Section 4.3: Computational efficiency: Logistic regression - Simulated data

## Replicating Figure 3

# Utility functions for simulation
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
rel_obj_diff_pic <- function(fit.glm, fit, n, x, y) {
  dev.glm <- (1 - fit.glm$dev.ratio) * fit.glm$nulldev / 2
  obj.glm <- as.numeric(dev.glm / n  + fit.glm$lambda * colSums(abs(fit.glm$beta)))
  
  obj.fit <- picasso_obj(fit, x, y)
  
  rel.diff.obj <- (obj.fit$obj.val - obj.glm) / obj.glm
  rel.diff.obj
}

# relative objective difference at each lambda
rel_obj_diff <- function(fit.glm, fit, n) {
  dev.glm <- (1 - fit.glm$dev.ratio) * fit.glm$nulldev / 2
  obj.glm <- as.numeric(dev.glm / n  + fit.glm$lambda * colSums(abs(fit.glm$beta)))
  
  if ("glmnet" %in% class(fit)) {
    dev.glm.fit <- (1 - fit$dev.ratio) * fit$nulldev
    obj.fit <- as.numeric(dev.glm.fit / n  + fit$lambda * colSums(abs(fit$beta)))
  } else {
    obj.fit <- as.numeric(fit$loss / n + fit$lambda * colSums(abs(fit$beta[-1, ])))
  }
  rel.diff.obj <- (obj.fit - obj.glm) / obj.glm
  rel.diff.obj
}

which_case <- function(n, p, q, eff.nonzero, corr) {
  if (length(n) > 1) {
    sim.case <- "vary_n"
    which.vary <- n
    which.vary.c <- 'n'
    
  } else if (length(p) > 1) {
    sim.case <- "vary_p"
    which.vary <- p
    which.vary.c <- 'p'
    
  } else if (length(q) > 1) {
    sim.case <- 'vary_q'
    which.vary <- q
    which.vary.c <- 'q'
    
  } else if (length(eff.nonzero) > 1) {
    sim.case <- 'vary_beta'
    which.vary <- eff.nonzero
    which.vary.c <- 'eff.nonzero'
    
  } else if (length(corr) > 1) {
    sim.case <- 'vary_corr'
    which.vary <- corr
    which.vary.c <- 'corr'
    
  } else {
    sim.case <- 'vary_NA'
    which.vary <- n
    which.vary.c <- 'NA'
  }
  list(sim.case = sim.case, which.vary = which.vary, which.vary.c = which.vary.c)
}

sim <- function(n, p, q, eff.nonzero, corr, rep, methods, eps, lam.min, lam.log,
                backingfile, descrpfile, backingpath) {
  
  case <- which_case(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr)
  sim.case <- case$sim.case
  which.vary <- case$which.vary
  which.vary.c <- case$which.vary.c
  
  parms <- list(case = case, n = n, p = p, q = q, eff.nonzero = eff.nonzero,
                corr = corr, rep = rep, methods = methods, eps = eps,
                lam.min = lam.min, lam.log = lam.log)
  
  cat("\nStart simulation: ", format(Sys.time()))
  cat("\n============================================================\n")
  cat("\nR sessionInfo: \n\n")
  print(sessionInfo())
  
  ## print out simulation setting
  cat("\n Simulation case: ", sim.case)
  cat("\n============================================================\n")
  cat("\t Simulation settings: \n")
  cat("\t ---------------------------------------------\n")
  cat("\t n = ", n, '\n')
  cat("\t p = ", p, '\n')
  cat("\t q = ", q, '\n')
  cat("\t eff.nonzero = ", eff.nonzero, '\n')
  cat("\t corr = ", corr, '\n')
  cat("\t rep = ", rep, '\n')
  cat("\t methods = ", methods, '\n')
  cat("\t eps = ", eps, '\n')
  cat("\t lam.min = ", lam.min, '\n')
  cat("\t lam.log = ", lam.log, '\n')
  cat("\n============================================================\n\n")
  
  
  time.all <- array(NA, dim = c(rep, length(methods), length(which.vary)))
  
  for (i in 1:length(which.vary)) {
    cat("\t", which.vary.c, " = ", which.vary[i], "; start time: ", format(Sys.time()), '\n')  
    cat("\t---------------------------------------------\n")
    
    for (j in 1:rep) {
      time <- NULL
      
      if (sim.case == 'vary_n') {
        beta.nonzero <- runif(q, -eff.nonzero, eff.nonzero)
        beta <- rep(0, p)
        nonzero.id <- sample(p, q)
        beta[nonzero.id] <- beta.nonzero
        
        if (corr < 1 && corr > 0) {
          Sigma <- matrix(corr, ncol = p, nrow = p)
          diag(Sigma) <- 1
          x <- rmvnorm(n[i], sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p * n[i]), ncol = p)
        }
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n[i], 1, prob)
        
      } else if (sim.case == 'vary_p') {
        
        beta.nonzero <- runif(q, -eff.nonzero, eff.nonzero)
        beta <- rep(0, p[i])
        nonzero.id <- sample(p[i], q)
        beta[nonzero.id] <- beta.nonzero
        
        if (corr < 1 && corr > 0) {
          Sigma <- matrix(corr, ncol = p[i], nrow = p[i])
          diag(Sigma) <- 1
          x <- rmvnorm(n, sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p[i] * n), ncol = p[i])
        }
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n, 1, prob)
        
      } else if (sim.case == 'vary_q') {
        
        beta.nonzero <- runif(q[i], -eff.nonzero, eff.nonzero)
        beta <- rep(0, p)
        nonzero.id <- sample(p, q[i])
        beta[nonzero.id] <- beta.nonzero
        
        if (corr < 1 && corr > 0) {
          Sigma <- matrix(corr, ncol = p, nrow = p)
          diag(Sigma) <- 1
          x <- rmvnorm(n, sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p * n), ncol = p)
        }
        
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n, 1, prob)
        
      } else if (sim.case == 'vary_beta') {
        
        beta.nonzero <- runif(q, -eff.nonzero[i], eff.nonzero[i])
        beta <- rep(0, p)
        nonzero.id <- sample(p, q)
        beta[nonzero.id] <- beta.nonzero
        
        if (corr < 1 && corr > 0) {
          Sigma <- matrix(corr, ncol = p, nrow = p)
          diag(Sigma) <- 1
          x <- rmvnorm(n, sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p * n), ncol = p)
        }
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n, 1, prob)
        
      } else if (sim.case == "vary_corr") {
        
        beta.nonzero <- runif(q, -eff.nonzero, eff.nonzero)
        beta <- rep(0, p)
        nonzero.id <- sample(p, q)
        beta[nonzero.id] <- beta.nonzero
        ## correlation matrix
        if (corr[i] < 1 && corr[i] > 0) {
          Sigma <- matrix(corr[i], ncol = p, nrow = p)
          diag(Sigma) <- 1
          x <- rmvnorm(n, sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p * n), ncol = p)
        }
        
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n, 1, prob)
        
        
      } else if (sim.case == 'vary_NA') {
        beta.nonzero <- runif(q, -eff.nonzero, eff.nonzero)
        beta <- rep(0, p)
        nonzero.id <- sample(p, q)
        beta[nonzero.id] <- beta.nonzero
        
        if (corr < 1 && corr > 0) {
          Sigma <- matrix(corr, ncol = p, nrow = p)
          diag(Sigma) <- 1
          x <- rmvnorm(n, sigma = Sigma, method = 'chol')
        } else {
          x <- matrix(rnorm(p * n), ncol = p)
        }
        
        eta <- x %*% beta
        prob <- exp(eta) / (1 + exp(eta))
        y <- rbinom(n, 1, prob)
        
      }
      
      x.bm <- as.big.matrix(x, backingfile = backingfile, descriptorfile = descrpfile,
                            backingpath = backingpath, type = 'double')
      
      fit.hsr0 <- biglasso(x.bm, y, family = 'binomial', screen = 'SSR', 
                           lambda.log.scale = lam.log, lambda.min = lam.min,
                           ncores = 4, eps = eps)
      lambda <- fit.hsr0$lambda
      
      # PICASSO
      st <- system.time(fit.pic <- picasso(x, y, family = 'binomial', 
                                           prec = eps, lambda = lambda))
      time <- c(time, st['elapsed'])
      
      rm(fit.pic)
      gc()
      
      # ncvreg
      st <- system.time(fit.ncv <- ncvreg(x, y, penalty = 'lasso', family = 'binomial',
                                          lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.ncv)
      gc()
      
      # glmnet
      glmnet.control(fdev = 0, devmax = 1)
      st <- system.time(fit.glm.true <- glmnet(x, y, family = 'binomial', 
                                               lambda = lambda, thresh = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.glm.true)
      gc()
      
      rm(x)
      gc()
      
      # SSR-Slores, 1 core
      st <- system.time(fit.slores1 <- biglasso(x.bm, y, family = 'binomial', 
                                                screen = "SSR-Slores", 
                                                safe.thresh = 0, ncores = 1,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.slores1)
      gc()
      
      # SSR-Slores, 2 core
      st <- system.time(fit.slores2 <- biglasso(x.bm, y, family = 'binomial', 
                                                screen = "SSR-Slores", 
                                                safe.thresh = 0, ncores = 2,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.slores2)
      gc()
      
      # SSR-Slores, 4 core
      st <- system.time(fit.slores4 <- biglasso(x.bm, y, family = 'binomial', 
                                                screen = "SSR-Slores", 
                                                safe.thresh = 0, ncores = 4,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.slores4)
      gc()
      
      # SSR-Slores, 8 core
      st <- system.time(fit.slores8 <- biglasso(x.bm, y, family = 'binomial', 
                                                screen = "SSR-Slores", 
                                                safe.thresh = 0, ncores = 8,
                                                lambda = lambda, eps = eps))
      time <- c(time, st['elapsed'])
      
      rm(fit.slores8)
      gc()
      
      file.remove(paste0(backingpath, '/', backingfile))
      file.remove(paste0(backingpath, '/', descrpfile))
      
      time <- as.numeric(time)
      time.all[j, , i] <- time
      
      cat("\t\trep", j, '; time = ', time, "\n")
      
    }
    cat("\t", which.vary.c, " = ", which.vary[i], "; end time: ", format(Sys.time()), '\n') 
    cat("\n============================================================\n")
    
  }
  
  cat("\nEnd simulation: ", format(Sys.time()), "\n")
  cat("\n============================================================\n")
  
  list(time.all = time.all,
       parms = parms
  )
  
}


# ============================================================
# Case 1: vary n
# ============================================================
rm(list = ls())
gc()

set.seed(1234)
date <- Sys.Date()

n <- c(100, 200, 500, 1000, 2000, 5000, 10000, 20000)
# n <- c(2000, 20000)
p <- 10000
q <- 20
eff.nonzero <- 1
corr <- 0
rep <- 20
methods <- c('picasso', 'ncvreg', 'glmnet', 'SSR-Slores', 'biglasso (2 cores)',
             'biglasso (4 cores)', 'biglasso (8 cores)')

eps <- 1e-6
lam.min <- 0.1
lam.log <- FALSE
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

res <- sim(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr, 
           rep = rep, methods = methods, eps = eps, lam.min = lam.min, 
           lam.log = lam.log, backingfile = backingfile, 
           descrpfile = descrpfile, backingpath = backingpath)

## Post analysis
methods <- res$parms$methods
which.vary <- res$parms$case$which.vary
sim.case <- res$parms$case$sim.case

if (sim.case == "vary_n") {
  xlab <- 'Number of observations'
} else if (sim.case == "vary_p") {
  xlab <- 'Number of features'
} else if (sim.case == 'vary_q') {
  xlab <- 'Number of active features'
} else if (sim.case == 'vary_beta') {
  xlab <- 'Magnitude of beta'
} else if (sim.case == 'vary_corr') {
  xlab <- 'Magnitude of correlation'
} else {
  xlab <- 'NA'
}

cat("\n============================================================\n")
cat("\nMean: \n\n")
time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
rownames(time.mean) <- methods
colnames(time.mean) <- which.vary
print(time.mean)

cat("\nSE: \n\n")
time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    return(NA)
  } else {
    return(sd(x) / sqrt(length(x)))
  }
})
rownames(time.se) <- methods
colnames(time.se) <- which.vary
print(time.se)

## plot
# -----------------------------------------------------------------------------
rule.name <- methods
time.df <- data.frame(time = matrix(t(time.mean), ncol = 1, byrow = T),
                      Method = rep(rule.name, each = length(which.vary)),
                      Which.vary = rep(which.vary, length(methods)))
time.df$Method <- factor(time.df$Method, methods)

## package comparison
time.df.pkgs <- subset(time.df, Method %in% c('picasso', "ncvreg", "glmnet", "SSR-Slores", 
                                              'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)'))
time.df.pkgs$Method <- revalue(time.df.pkgs$Method, c("SSR-Slores"="biglasso (1 core)"))
time.df.pkgs$Package <- time.df.pkgs$Method

# -----------------------------------------------------------------------------
# Figure 3 (b)
# -----------------------------------------------------------------------------
library(RColorBrewer)
gp.pkgs <- ggplot(time.df.pkgs, aes(x = Which.vary, y = time, color = Package)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = pretty(range(time.df$Which.vary)),
                     limits = range(time.df$Which.vary)) +
  scale_y_continuous(breaks = pretty(range(time.df$time))) +
  xlab(xlab) +
  ylab("Computing time (s)") +
  # theme(legend.position = 'top') +
  theme_bw() +
  theme(legend.position = c(.2, .7))

gg.colors <- gg.colors.default <- unique(ggplot_build(gp.pkgs)$data[[1]]$colour)
gg.colors[3] <- "#6495ED" # comflowerblue

gp.pkgs <-
  gp.pkgs + scale_colour_manual(
    values = gg.colors
  )
date <- Sys.Date()
pdf(file = paste0(date, '_', sim.case, '_pkgs_logistic.pdf'), width = 5, height = 4)
print(gp.pkgs)
dev.off()

# ============================================================
# Case 2: vary p
# ============================================================
rm(list = ls())
gc()

set.seed(1234)
date <- Sys.Date()

n <- 1000
p <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
q <- 20
eff.nonzero <- 1
corr <- 0
rep <- 20

methods <- c('picasso', 'ncvreg', 'glmnet', 'SSR-Slores',
             'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)')

eps <- 1e-6
lam.min <- 0.1
lam.log <- FALSE
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

res <- sim(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr, 
           rep = rep, methods = methods, eps = eps, lam.min = lam.min, 
           lam.log = lam.log, backingfile = backingfile, 
           descrpfile = descrpfile, backingpath = backingpath)

# post analysis
methods <- res$parms$methods
which.vary <- res$parms$case$which.vary
sim.case <- res$parms$case$sim.case

if (sim.case == "vary_n") {
  xlab <- 'Number of observations'
} else if (sim.case == "vary_p") {
  xlab <- 'Number of features'
} else if (sim.case == 'vary_q') {
  xlab <- 'Number of active features'
} else if (sim.case == 'vary_beta') {
  xlab <- 'Magnitude of beta'
} else if (sim.case == 'vary_corr') {
  xlab <- 'Magnitude of correlation'
} else {
  xlab <- 'NA'
}

cat("\n============================================================\n")
cat("\nMean: \n\n")
time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
rownames(time.mean) <- methods
colnames(time.mean) <- which.vary
print(time.mean)

cat("\nSE: \n\n")
time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    return(NA)
  } else {
    return(sd(x) / sqrt(length(x)))
  }
})
rownames(time.se) <- methods
colnames(time.se) <- which.vary
print(time.se)

## plot
# -----------------------------------------------------------------------------
rule.name <- methods
time.df <- data.frame(time = matrix(t(time.mean), ncol = 1, byrow = T),
                      Method = rep(rule.name, each = length(which.vary)),
                      Which.vary = rep(which.vary, length(methods)))
time.df$Method <- factor(time.df$Method, methods)

## package comparison
time.df.pkgs <- subset(time.df, Method %in% c('picasso', "ncvreg", "glmnet", "SSR-Slores", 
                                              'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)'))
time.df.pkgs$Method <- revalue(time.df.pkgs$Method, c("SSR-Slores"="biglasso (1 core)"))
time.df.pkgs$Package <- time.df.pkgs$Method

# -----------------------------------------------------------------------------
# Figure 3 (a)
# -----------------------------------------------------------------------------
library(RColorBrewer)
gp.pkgs <- ggplot(time.df.pkgs, aes(x = Which.vary, y = time, color = Package)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = pretty(range(time.df$Which.vary)),
                     limits = range(time.df$Which.vary)) +
  scale_y_continuous(breaks = pretty(range(time.df$time))) +
  xlab(xlab) +
  ylab("Computing time (s)") +
  # theme(legend.position = 'top') +
  theme_bw() +
  theme(legend.position = c(.2, .7))

gg.colors <- gg.colors.default <- unique(ggplot_build(gp.pkgs)$data[[1]]$colour)
gg.colors[3] <- "#6495ED" # comflowerblue

gp.pkgs <-
  gp.pkgs + scale_colour_manual(
    values = gg.colors
  )
date <- Sys.Date()
pdf(file = paste0(date, '_', sim.case, '_pkgs_logistic.pdf'), width = 5, height = 4)
print(gp.pkgs)
dev.off()


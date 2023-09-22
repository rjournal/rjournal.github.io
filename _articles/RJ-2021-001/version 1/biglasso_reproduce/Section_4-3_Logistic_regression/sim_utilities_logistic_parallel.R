
require(ncvreg)
require(glmnet)
require(picasso)
require(plyr)
require(ggplot2)
require(mvtnorm)
require(biglasso)

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

post_analysis <- function(res, date, width = 5, height = 4) {
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

  ## result analysis
  # -----------------------------------------------------------------------------
  rule.name <- methods
  time.df <- data.frame(time = matrix(t(time.mean), ncol = 1, byrow = T),
                        Method = rep(rule.name, each = length(which.vary)),
                        Which.vary = rep(which.vary, length(methods)))
  time.df$Method <- factor(time.df$Method, methods)
  time.df$Package <- time.df$Method

  ## package comparison
  gp.pkgs <- ggplot(time.df, aes(x = Which.vary, y = time, color = Package)) +
    geom_line(size = 1) +
    scale_x_continuous(breaks = pretty(range(time.df$Which.vary)),
                       limits = range(time.df$Which.vary)) +
    scale_y_continuous(breaks = pretty(range(time.df$time))) +
    xlab(xlab) +
    ylab("Computing time (s)") +
    # theme(legend.position = 'top') +
    theme_bw() +
    theme(legend.position = c(.2, .7))

  pdf(file = paste0(date, '_', sim.case, '_pkgs_logistic.pdf'), width = width, height = height)
  # png(filename = paste0(date, '_', sim.case, '_pkgs.png'), width = 5, height = 4,
  #     units = 'in', res = 120)
  print(gp.pkgs)
  dev.off()

  save(list = ls(), file = paste0(date, '_', sim.case, '_results.RData'))
}


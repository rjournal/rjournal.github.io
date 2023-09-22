###########################################################
## This is an R file part of the project "User-Specified
## General-to-Specific and Indicator Saturation Methods"
## by Genaro Sucarrat.
##
## First created 20 April 2018
## Last updated/changed 1 September 2020
##
## Contents:
##
## INITIATE
## USER-SPECIFICATION: GENERAL PRINCIPLES
## - The getsFun function
## - User-specified estimation
## - User-specified diagnostics
## - User-specified goodness-of-fit
## USER-SPECIFIED GETS AND ISAT METHODS: ILLUSTRATIONS
## - GETS modelling of Generalised Linear Models (GLMs)
## - Creating a gets method (S3) for the 'lm' class of models
## - Regression with ARMA error
## - Faster ISAT when n is large
##
###########################################################


###########################################################
## INITIATE
###########################################################

##remove everything in environment:
#rm(list=ls())

##load gets library:
library(gets)


###########################################################
## USER-SPECIFICATION: GENERAL PRINCIPLES
###########################################################

  ##The getsFun function:
  ##=====================
  
  n <- 40 #number of observations
  k <- 20 #number of Xs

  set.seed(123) #for reproducibility
  y <- rnorm(n) #generate Y
  x <- matrix(rnorm(n*k), n, k) #create matrix of Xs

  #do gets w/default estimator (ols), store output in 'result':
  result <- getsFun(y, x)

  #items in result:
  summary(result)


  ##User-specified estimation:
  ##==========================

  lmFun <- function(y, x, ...){

    ##create list:
    result <- list()

    ##n, k and df:
    result$n <- length(y)
    if( is.null(x) || NCOL(x) == 0 ){
      result$k <- 0
    }else{
      result$k <- NCOL(x)
    }
    result$df <- result$n - result$k

    ##call lm if k > 0:
    if( result$k > 0){
      tmp <- lm(y ~ x - 1)
      result$coefficients <- coef(tmp)
      result$vcov <- vcov(tmp)
      result$logl <- as.numeric(logLik(tmp))
    }else{
      result$coefficients <- NULL 
      result$vcov <- NULL
      result$logl <- sum(dnorm(y, sd = sqrt(var(y)), log = TRUE))
    }  

    ##return result: 
    return(result)

  }

  ##do gets:
  getsFun(y, x, user.estimator = list(name = "lmFun"))


  ##User-specified diagnostics:
  ##===========================

  ##re-define (add the 'residuals' part) function:
  lmFun <- function(y, x, ...){

    ##info needed for estimation:
    result <- list()
    result$n <- length(y)
    if( is.null(x) || NCOL(x)==0 ){
      result$k <- 0
    }else{
      result$k <- NCOL(x)
    }
    result$df <- result$n - result$k
    if( result$k > 0){
      tmp <- lm(y ~ x - 1)
      result$coefficients <- coef(tmp)
      result$vcov <- vcov(tmp)
      result$logl <- as.numeric(logLik(tmp))
    }else{
      result$coefficients <- NULL
      result$vcov <- NULL
      result$logl <- sum(dnorm(y, sd=sqrt(var(y)), log=TRUE))
    }   

    ##residuals:
    if( result$k > 0){
      result$residuals <- residuals(tmp)
    }else{
      result$residuals <- y
    }

    return(result)
  }

  ##user-defined diagnostics function:
  myDiagnostics <- function(x, ...){
    tmp <- shapiro.test(x$residuals) #do the test
    result <- rbind( c(tmp$statistic, NA, tmp$p.value) )
    return(result)
  }

  ##do gets:
  getsFun(y, x, user.estimator = list(name = "lmFun"),
    user.diagnostics = list(name = "myDiagnostics", pval = 0.05))


  ##User-specified goodness-of-fit:
  ##===============================

  ##re-define (add 'y' part) function:
  lmFun <- function(y, x, ...){

    ##info needed for estimation:
    result <- list()
    result$n <- length(y)
    if( is.null(x) || NCOL(x)==0 ){
      result$k <- 0
    }else{
      result$k <- NCOL(x)
    }
    result$df <- result$n - result$k
    if( result$k > 0){
      tmp <- lm(y ~ x - 1)
      result$coefficients <- coef(tmp)
      result$vcov <- vcov(tmp)
      result$logl <- as.numeric(logLik(tmp))
    }else{
      result$coefficients <- NULL
      result$vcov <- NULL
      result$logl <- sum(dnorm(y, sd=sqrt(var(y)), log=TRUE))
    }  

    ##residuals:
    if( result$k > 0){
      result$residuals <- residuals(tmp)
    }else{
      result$residuals <- y
    }

    ##info needed for r-squared:
    result$y <- y

    return(result)
  }

  ##make goodness-of-fit function:
  myGof <- function(object, ...){
    TSS <- sum((object$y - mean(object$y))^2)
    RSS <- sum(object$residuals^2)
    Rsquared <- 1 - RSS/TSS
    result <- 1 - (1 - Rsquared) * (object$n - 1)/(object$n - object$k)
    return(result)
  }
  
  ##do gets:
  getsFun(y, x, user.estimator = list(name = "lmFun"),
    user.diagnostics = list(name = "myDiagnostics", pval = 0.05),
    gof.function = list(name = "myGof"), gof.method = "max")


###########################################################
## USER-SPECIFIED GETS AND ISAT METHODS: ILLUSTRATIONS
###########################################################

  ##GETS modelling of Generalised Linear Models (GLMs):
  ##===================================================

  n <- 40 #number of observations
  k <- 20 #number of Xs
  set.seed(123) #for reproducibility
  y <- round(runif(40)) #generate Y
  x <- matrix(rnorm(n*k), n, k) #create matrix of Xs

  ##make logit-function:
  logitFun <- function(y, x, ...){

    ##create list:
    result <- list()

    ##n, k and df:
    result$n <- length(y)
    if( is.null(x) || NCOL(x)==0 ){
      result$k <- 0
    }else{
      result$k <- NCOL(x)
    }
    result$df <- result$n - result$k

    ##call glm if k > 0:
    if( result$k > 0){
      tmp <- glm(y ~ x - 1, family = binomial(link="logit"))
      result$coefficients <- coef(tmp)
      result$vcov <- vcov(tmp)
      result$logl <- as.numeric(logLik(tmp))
    }else{
      result$coefficients <- NULL
      result$vcov <- NULL
      result$logl <- result$n*log(0.5)
    }  

    ##return result:
    return(result)
  }

  ##do gets:
  getsFun(y, x, user.estimator=list(name="logitFun"))


  ##Creating a gets method (S3) for the 'lm' class of models:
  ##=========================================================

  ##make function:
  gets.lm <- function(object, ...){

    ##make y:
    y <- as.vector(object$model[, 1])
    yName <- names(object$model)[1]

    ##make x:
    x <- as.matrix(object$model[, -1])
    xNames <- colnames(x)
    if(NCOL(x) == 0){
      x <- NULL
      xNames <- NULL
    }else{
      if(is.null(xNames)){
        xNames <- paste0("X", 1:NCOL(x))
        colnames(x) <- xNames
      }
    }

    ##is there an intercept?:
    if(length(coef(object)) > 0){
      cTRUE <- names(coef(object))[1] == "(Intercept)"
      if(cTRUE){
        x <- cbind(rep(1, NROW(y)), x)
        xNames <- c("(Intercept)", xNames)
        colnames(x) <- xNames
      }
    }

    ##do gets:
    myspecific <- getsFun(y, x, ...)

    ##which are the retained regressors?:
    retainedXs <- xNames[myspecific$specific.spec]
    cat("Retained regressors:\n ", retainedXs, "\n")

    ##return result
    return(myspecific)

  }

  ##recall the dgp of the first experiment:
  n <- 40 #number of observations
  k <- 20 #number of Xs
  set.seed(123) #for reproducibility
  y <- rnorm(n) #generate Y
  x <- matrix(rnorm(n*k), n, k) #create matrix of Xs

  ##estimate startmodel, do gets modelling:
  startmodel <- lm(y ~ x)
  finallm <- gets(startmodel)	


  ##Regression with ARMA error:
  ##===========================

  set.seed(123) #for reproducibility
  eps <- arima.sim(list(ar = 0.4, ma = 0.1), 60) #epsilon
  x <- coredata(sim(eps, which.ones = 30)) #step-dummy at t = 30
  y <- 4*x + eps #the dgp
  plot(y, ylab="y", xlab="t", lwd = 2)

  ##make estimator:
  myEstimator <- function(y, x){

    ##create list:
    result <- list()

    ##estimate model:
    if( is.null(x) || NCOL(x)==0 ){
      result$k <- 0
      tmp <- arima(y, order = c(1,0,1)) #empty model
    }else{
      result$k <- NCOL(x)
      tmp <- arima(y, order = c(1,0,1), xreg = x)
      result$coefficients <- tmp$coef[-c(1:3)]
      result$vcov <- tmp$var.coef
      result$vcov <- result$vcov[-c(1:3),-c(1:3)]
    }

    ##rename and re-organise things:
    result$n <- tmp$nobs
    result$df <- result$n - result$k
    result$logl <- tmp$loglik

    return(result)
  }
	
  ##make regressors, do gets:
  xregs <- coredata(sim(eps, which.ones = 25:35)) #11 step-dummies
  getsFun(y, xregs, user.estimator = list(name = "myEstimator"))


  ##Faster ISAT when n is large:
  ##============================

  library(Matrix)
  olsFaster <- function(y, x){
    out <- list()
    out$n <- length(y)
    if (is.null(x)){ out$k <- 0 }else{ out$k <- NCOL(x) }
      out$df <- out$n - out$k
      if (out$k > 0) {
        x <- as(x, "dgeMatrix")
        out$xpy <- crossprod(x, y)
        out$xtx <- crossprod(x)
        out$coefficients <- as.numeric(solve(out$xtx,out$xpy))
        out$xtxinv <- solve(out$xtx)
        out$fit <- out$fit <- as.vector(x %*% out$coefficients)  
    }else{ out$fit <- rep(0, out$n)	}
    out$residuals <- y - out$fit
    out$residuals2 <- out$residuals^2
    out$rss <- sum(out$residuals2)
    out$sigma2 <- out$rss/out$df
    if (out$k > 0) { out$vcov <- as.matrix(out$sigma2 * out$xtxinv) }
    out$logl <- -out$n * log(2 * out$sigma2 * pi)/2 - out$rss/(2 * out$sigma2)
    return(out)            
  }

  set.seed(123) #for reproducibility
  y <- rnorm(1000)
  x <- matrix(rnorm(length(y)*20), length(y), 20)
  #w/ols:
  system.time( finalmodel <- isat(y, mxreg = x, max.paths = 5))
  #w/olsFaster:
  system.time( finalmodel <- isat(y, mxreg = x, max.paths = 5,
    user.estimator = list(name = "olsFaster")) )

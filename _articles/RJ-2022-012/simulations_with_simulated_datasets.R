## required packages
library(conformalInference)
library(forestError)
library(grf)
library(ranger)
library(rfinterval)
library(RFpredInterval)
library(quantregForest)

library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(reshape)

## ENTER CORRECT PATHS OF THE DATA AND OUTPUT FOLDERS..
path_data <- file.path(getwd(), "simulated data sets")
path_out <- file.path(getwd(), "output for simulated data sets")
if (!file.exists(path_data)) {
  dir.create(path_data)
}
if (!file.exists(path_out)) {
  dir.create(path_out)
}

## parameters
nrep <- 500
ntest <- 1000
ntree <- 2000
alpha <- 0.05

settings <- expand.grid(dgp = c("friedman1","friedman2","friedman3",
                                "tree_normal","tree_exp",
                                "peak","H2c"),
                        ntrain = c(200,500,1000,5000))

######################### data generation functions ########################
dgp_tree_normal <- function(n) {
  ## Y ~ N(f(X),1^2)
  ## mean function: f(X) = tree-based with X1-X7
  sd <- 1
  px <- 7
  x <- matrix(rnorm(n*px), nrow = n, ncol = px)
  colnames(x) <- paste0("x",1:px)
  
  u <- c(5, 10, 15, 20, 25, 30, 35, 40)
  y <- u[1] * (x[,1]<0 & x[,2]<0 & x[,4]<0) +
    u[2] * (x[,1]<0 & x[,2]<0 & x[,4]>=0) +
    u[3] * (x[,1]<0 & x[,2]>=0 & x[,5]<0) +
    u[4] * (x[,1]<0 & x[,2]>=0 & x[,5]>=0) +
    u[5] * (x[,1]>=0 & x[,3]<0 & x[,6]<0) +
    u[6] * (x[,1]>=0 & x[,3]<0 & x[,6]>=0) +
    u[7] * (x[,1]>=0 & x[,3]>=0 & x[,7]<0) +
    u[8] * (x[,1]>=0 & x[,3]>=0 & x[,7]>=0)
  
  e <- rnorm(n, mean = 0, sd = sd)
  y <- y + e
  
  data <- list(x = x, y = y,  e = e, sd = sd)
  return(data)
}

dgp_tree_exp <- function(n) {
  ## Y ~ f(X) + exp(1)
  ## mean function: f(X) = tree-based with X1-X7
  sd <- 1
  px <- 7
  x <- matrix(rnorm(n*px), nrow = n, ncol = px)
  colnames(x) <- paste0("x",1:px)
  
  u <- c(5, 10, 15, 20, 25, 30, 35, 40)
  y <- u[1] * (x[,1]<0 & x[,2]<0 & x[,4]<0) +
    u[2] * (x[,1]<0 & x[,2]<0 & x[,4]>=0) +
    u[3] * (x[,1]<0 & x[,2]>=0 & x[,5]<0) +
    u[4] * (x[,1]<0 & x[,2]>=0 & x[,5]>=0) +
    u[5] * (x[,1]>=0 & x[,3]<0 & x[,6]<0) +
    u[6] * (x[,1]>=0 & x[,3]<0 & x[,6]>=0) +
    u[7] * (x[,1]>=0 & x[,3]>=0 & x[,7]<0) +
    u[8] * (x[,1]>=0 & x[,3]>=0 & x[,7]>=0)
  
  # rate=1/sigma
  e <- rexp(n, rate = 1/sd)
  y <- y + e
  
  data <- list(x = x, y = y,  e = e, sd = sd)
  return(data)
}

dgp_friedman1 <- function(n) {
  ## Y ~ N(f(X),1)
  ## mean function: f(X) = 10*sin(pi*X1*X2) + 20*(X3-0.5)^2 + 10*X4 + 5*X5
  sd <- 1
  px <- 10
  x <- matrix(runif(n*px, min=0, max=1), nrow = n, ncol = px)
  colnames(x) <- paste0("x",1:px)
  
  y <- 10*sin(pi*x[, 1]*x[, 2]) + 20*(x[, 3]-0.5)^2 + 10*x[, 4] + 5*x[, 5]
  e <- rnorm(n, mean = 0, sd = sd)
  y <- y + e
  
  data <- list(x = x, y = y,  e = e, sd = sd)
  return(data)
}

dgp_friedman2 <- function(n) {
  ## Y ~ N(f(X),125^2)
  ## mean function: f(X) = sqrt(X1^2 + (X2*X3 - (1/(X2*X4)))^2)
  sd <- 125
  px <- 4
  x <- cbind(runif(n, min = 0, max = 100),
             runif(n, min = 40*pi, max = 560*pi),
             runif(n, min = 0, max = 1),
             runif(n, min = 1, max = 11))
  colnames(x) <- paste0("x",1:px)
  
  y <- sqrt(x[, 1]^2 + (x[, 2]*x[, 3] - (1/(x[, 2]*x[, 4])))^2)
  e <- rnorm(n, mean = 0, sd = sd)
  y <- y + e
  
  data <- list(x = x, y = y,  e = e, sd = sd)
  return(data)
}

dgp_friedman3 <- function(n) {
  ## Y ~ N(f(X),0.1^2)
  ## mean function: f(X) = atan((X2*X3 - (1/(X2*X4)))/X1)
  sd <- 0.1
  px <- 4
  x <- cbind(runif(n, min = 0, max = 100),
             runif(n, min = 40*pi, max = 560*pi),
             runif(n, min = 0, max = 1),
             runif(n, min = 1, max = 11))
  colnames(x) <- paste0("x",1:px)
  
  y <- atan((x[, 2]*x[, 3] - (1/(x[, 2]*x[, 4])))/x[, 1])
  e <- rnorm(n, mean = 0, sd = sd)
  y <- y + e
  
  data <- list(x = x, y = y,  e = e, sd = sd)
  return(data)
}

dgp_peak <- function(n) {
  px <- 20
  metro <- numeric(n)
  y <- numeric(n)
  x <- matrix(0, nrow = n, ncol = px)
  
  for (i in 1:n) {
    radius <- runif(1, min = 0, max = 3)
    x[i,] <- rnorm(px)
    metro[i] <- sqrt(sum(x[i,]^2))
    x[i,] <- radius * (x[i,]/metro[i])
    y[i] <- 25 * exp(-0.5* radius^2)
  }
  colnames(x) <- paste0("x",1:px)
  
  data <- list(x = x, y = y)
  return(data)
}

dgp_friedman1_H2c <- function(n) {
  px <- 10
  x <- matrix(runif(px*n, min = 0, max = 1), nrow = n, ncol = px)
  colnames(x) <- paste0("x",1:px)
  
  y <- 10*sin(pi*x[, 1]*x[, 2]) + 20*(x[, 3]-0.5)^2 + 10*x[, 4] + 5*x[, 5]
  y <- 3*(y - min(y))/(max(y) - min(y)) - 1.5
  
  sd <- 10*sin(pi*x[, 6]*x[, 7]) + 20*(x[, 8]-0.5)^2 + 10*x[, 9] + 5*x[, 10]
  scaledsd <- exp(3*(sd - min(sd))/(max(sd) - min(sd)) - 1.5)
  
  y <- y + rnorm(n, mean = 0, sd = scaledsd)
  
  data <- list(x = x, y = y, sd = scaledsd)
  return(data)
}


######################## simulated data generation ##########################
set.seed(3254)
seed_set <- round(10000*abs(rnorm(nrep)))

for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  for (rep in 1:nrep) {
    ## set seed
    set.seed(seed_set[rep])
    
    ## generate data
    if (dgp == "tree_normal") {
      traindata1 <- dgp_tree_normal(ntrain)
      testdata1 <- dgp_tree_normal(ntest)
    } else if (dgp == "tree_exp") {
      traindata1 <- dgp_tree_exp(ntrain)
      testdata1 <- dgp_tree_exp(ntest)
    } else if (dgp == "friedman1") {
      traindata1 <- dgp_friedman1(ntrain)
      testdata1 <- dgp_friedman1(ntest)
    } else if (dgp == "friedman2") {
      traindata1 <- dgp_friedman2(ntrain)
      testdata1 <- dgp_friedman2(ntest)
    } else if (dgp == "friedman3") {
      traindata1 <- dgp_friedman3(ntrain)
      testdata1 <- dgp_friedman3(ntest)
    } else if (dgp == "peak") {
      traindata1 <- dgp_peak(ntrain)
      testdata1 <- dgp_peak(ntest)
    } else if (dgp == "H2c") {
      traindata1 <- dgp_friedman1_H2c(ntrain)
      testdata1 <- dgp_friedman1_H2c(ntest)
    }
    xvar.names <- colnames(traindata1$x)
    traindata <- as.data.frame(traindata1$x)
    traindata$y <- traindata1$y
    testdata <- as.data.frame(testdata1$x)
    testdata$y <- testdata1$y
    
    saveRDS(traindata, file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    saveRDS(testdata, file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
  }
}


############################ start simulations #############################
set.seed(12345)
seed_set <- round(10000*abs(rnorm(nrep)))

## Run GRF method
method <- "grf"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    mtry <- min(ceiling(sqrt(px) + 20),px)
    min.node.size <- 5 # default
    honesty <- TRUE # default
    honesty.fraction <- 0.5 # default
    
    set.seed(seed_set[rep])
    
    ## convert nonnumeric columns to numeric
    ix_nonnumeric <- which(sapply(traindata[,xvar.names],class) != "numeric")
    if (length(ix_nonnumeric) > 0) {
      for (i in ix_nonnumeric) {
        traindata[,i] <- as.numeric(traindata[,i])
        testdata[,i] <- as.numeric(testdata[,i])
      }
    }
    
    time1 <- system.time(
      grf <- quantile_forest(X = traindata[,xvar.names],
                             Y = traindata$y,
                             quantiles = c(alpha/2,1-alpha/2),
                             num.trees = ntree,
                             mtry = mtry,
                             min.node.size = min.node.size,
                             honesty = honesty,
                             honesty.fraction = honesty.fraction)
    )
    
    time2 <- system.time(
      grf.pred <- predict(grf,testdata[,xvar.names],quantiles = c(alpha/2,1-alpha/2))
    )
    
    OUT.time[rep,"time_in_sec"] <- time1["elapsed"] + time2["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- grf.pred$predictions[,1]
    OUT.pi[,paste0(nr,"_upper")] <- grf.pred$predictions[,2]
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run OOB method
method <- "oob"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3),1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    
    set.seed(seed_set[rep])
    
    time <- system.time(
      oob <- rfinterval(formula,
                        train_data = traindata,
                        test_data = testdata,
                        method = "oob",
                        alpha = alpha,
                        params_ranger = params_ranger)
    )
    
    OUT.time[rep,"time_in_sec"] <- time["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- oob$oob_interval$lower
    OUT.pi[,paste0(nr,"_upper")] <- oob$oob_interval$upper
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run pialpha method
method <- "pialpha"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3),1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE,
                          keep.inbag = TRUE,
                          data = traindata,
                          formula = formula)
    
    set.seed(seed_set[rep])
    
    time1 <- system.time(
      rf <- do.call(ranger,params_ranger)
    )
    
    time2 <- system.time(
      pialpha <- quantForestError(
        forest = rf,
        X.train = traindata[,xvar.names],
        X.test = testdata[,xvar.names],
        Y.train = traindata$y,
        what = "interval",
        alpha = alpha
      )
    )
    
    OUT.time[rep,"time_in_sec"] <- time1["elapsed"] + time2["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- pialpha$lower_0.05
    OUT.pi[,paste0(nr,"_upper")] <- pialpha$upper_0.05
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run QRF method
method <- "qrf"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    set.seed(seed_set[rep])
    
    time1 <- system.time(
      qrf <- quantregForest(x = traindata[,xvar.names],
                            y = traindata$y,
                            mtry = max(floor(px/3),1),
                            ntree = ntree,
                            nodesize = 5,
                            replace = TRUE)
    )
    
    time2 <- system.time(
      qrf.pred  <- predict(qrf,newdata = testdata[,xvar.names],what = c(alpha/2,1-alpha/2))
    )
    
    OUT.time[rep,"time_in_sec"] <- time1["elapsed"] + time2["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- qrf.pred[,1]
    OUT.pi[,paste0(nr,"_upper")] <- qrf.pred[,2]
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run split conformal method
method <- "splitconformal"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    params_rf <- rf.funs(ntree = ntree,
                         nodesize = 5,
                         replace = TRUE,
                         varfrac = 1/3)
    
    set.seed(seed_set[rep])
    
    time <- system.time(
      sc <- conformal.pred.split(x = as.matrix(traindata[,xvar.names]),
                                 y = traindata$y,
                                 x0 = as.matrix(testdata[,xvar.names]),
                                 alpha = alpha,
                                 train.fun = params_rf$train.fun,
                                 predict.fun = params_rf$predict.fun)
    )
    
    OUT.time[rep,"time_in_sec"] <- time["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- sc$lo
    OUT.pi[,paste0(nr,"_upper")] <- sc$up
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run PIBF with CV calibration
method <- "pibf_cv"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  ## initialize storage
  OUT.cov <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pil <- matrix(NA,nrow=ntest,ncol=nrep)
  OUT.pi <- matrix(NA,nrow=ntest,ncol=2*nrep)
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3),1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    
    set.seed(seed_set[rep])
    
    time <- system.time(
      pibf <- pibf(formula,
                   traindata = traindata,
                   testdata = testdata,
                   alpha = alpha,
                   calibration = "cv",
                   numfolds = 5,
                   params_ranger = params_ranger)
    )
    
    OUT.time[rep,"time_in_sec"] <- time["elapsed"]
    OUT.pi[,paste0(nr,"_lower")] <- pibf$pred_interval$lower
    OUT.pi[,paste0(nr,"_upper")] <- pibf$pred_interval$upper
    OUT.pil[,nr] <- OUT.pi[,paste0(nr,"_upper")] - OUT.pi[,paste0(nr,"_lower")]
    OUT.cov[,nr] <- as.numeric((testdata$y <= OUT.pi[,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[,paste0(nr,"_lower")]))
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",filename,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",filename,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",filename,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",filename,".csv")))
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run RFPI methods
method <- "rfpi"
for (j in 1:nrow(settings)) {
  dgp <- settings$dgp[j]
  ntrain <- settings$ntrain[j]
  filename <- paste0(dgp,"_ntrain",ntrain)
  
  pi_methods <- c("lm","spi","quant","hdr","chdr")
  
  ## initialize storage
  OUT.cov <- vector("list",length = length(pi_methods))
  OUT.pil <- vector("list",length = length(pi_methods))
  OUT.pi <- vector("list",length = length(pi_methods))
  names(OUT.cov) <- pi_methods
  names(OUT.pil) <- pi_methods
  names(OUT.pi) <- pi_methods
  for (mtd in pi_methods) {
    OUT.cov[[mtd]] <- matrix(NA,nrow=ntest,ncol=nrep)
    OUT.pil[[mtd]] <- matrix(NA,nrow=ntest,ncol=nrep)
    OUT.pi[[mtd]] <- matrix(NA,nrow=ntest,ncol=2*nrep)
    colnames(OUT.cov[[mtd]]) <- paste0("rep",1:nrep)
    colnames(OUT.pil[[mtd]]) <- paste0("rep",1:nrep)
    colnames(OUT.pi[[mtd]]) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  }
  OUT.time <- matrix(NA,nrow=nrep,ncol=1)
  colnames(OUT.time) <- "time_in_sec"
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## read data
    traindata <- readRDS(file = file.path(path_data,paste0("traindata_",filename,"_rep",rep,".rds")))
    testdata <- readRDS(file = file.path(path_data,paste0("testdata_",filename,"_rep",rep,".rds")))
    xvar.names <- colnames(traindata)[grepl("x",colnames(traindata))]
    px <- length(xvar.names)
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3),1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    params_calib = list(range = c(1-alpha-0.005,1-alpha+0.005),
                        start = (1-alpha),
                        step = 0.01,
                        refine = TRUE)
    
    set.seed(seed_set[rep])
    
    time <- system.time(
      rfpi <- rfpi(formula,
                   traindata = traindata,
                   testdata = testdata,
                   alpha = alpha,
                   split_rule = "ls",
                   pi_method = pi_methods,
                   calibration = TRUE,
                   rf_package = "ranger",
                   params_ranger = params_ranger,
                   params_calib = params_calib)
    )
    
    OUT.time[rep,"time_in_sec"] <- time["elapsed"]
    
    ## lm interval
    OUT.pi[["lm"]][,paste0(nr,"_lower")] <- rfpi$lm_interval$lower
    OUT.pi[["lm"]][,paste0(nr,"_upper")] <- rfpi$lm_interval$upper
    OUT.pil[["lm"]][,nr] <- OUT.pi[["lm"]][,paste0(nr,"_upper")] - OUT.pi[["lm"]][,paste0(nr,"_lower")]
    OUT.cov[["lm"]][,nr] <- as.numeric((testdata$y <= OUT.pi[["lm"]][,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[["lm"]][,paste0(nr,"_lower")]))
    
    ## spi interval
    OUT.pi[["spi"]][,paste0(nr,"_lower")] <- rfpi$spi_interval$lower
    OUT.pi[["spi"]][,paste0(nr,"_upper")] <- rfpi$spi_interval$upper
    OUT.pil[["spi"]][,nr] <- OUT.pi[["spi"]][,paste0(nr,"_upper")] - OUT.pi[["spi"]][,paste0(nr,"_lower")]
    OUT.cov[["spi"]][,nr] <- as.numeric((testdata$y <= OUT.pi[["spi"]][,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[["spi"]][,paste0(nr,"_lower")]))
    
    ## quant interval
    OUT.pi[["quant"]][,paste0(nr,"_lower")] <- rfpi$quant_interval$lower
    OUT.pi[["quant"]][,paste0(nr,"_upper")] <- rfpi$quant_interval$upper
    OUT.pil[["quant"]][,nr] <- OUT.pi[["quant"]][,paste0(nr,"_upper")] - OUT.pi[["quant"]][,paste0(nr,"_lower")]
    OUT.cov[["quant"]][,nr] <- as.numeric((testdata$y <= OUT.pi[["quant"]][,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[["quant"]][,paste0(nr,"_lower")]))
    
    ## chdr interval
    OUT.pi[["chdr"]][,paste0(nr,"_lower")] <- rfpi$chdr_interval$lower
    OUT.pi[["chdr"]][,paste0(nr,"_upper")] <- rfpi$chdr_interval$upper
    OUT.pil[["chdr"]][,nr] <- OUT.pi[["chdr"]][,paste0(nr,"_upper")] - OUT.pi[["chdr"]][,paste0(nr,"_lower")]
    OUT.cov[["chdr"]][,nr] <- as.numeric((testdata$y <= OUT.pi[["chdr"]][,paste0(nr,"_upper")]) * (testdata$y >= OUT.pi[["chdr"]][,paste0(nr,"_lower")]))
    
    ## hdr interval
    OUT.pi.hdr <- lapply(1:ntest,function(i,hdr_int) {
      pi <- hdr_int[[i]]
      cbind(rep(i,nrow(pi)),1:nrow(pi),pi[,"lower"],pi[,"upper"]) },
      hdr_int = rfpi$hdr_interval)
    OUT.pi.hdr <- Reduce(rbind, OUT.pi.hdr)
    colnames(OUT.pi.hdr) <- c("test_obs","pi_no","lower","upper")
    rownames(OUT.pi.hdr) <- NULL
    OUT.pil[["hdr"]][,nr] <- unlist(Map(function(pi){sum(pi[,"upper"] - pi[,"lower"])},pi = rfpi$hdr_interval))
    OUT.cov[["hdr"]][,nr] <- as.numeric(unlist(Map(function(pi,truey){sum((pi[,"lower"] <= truey) * (truey <= pi[,"upper"]))},
                                                   pi = rfpi$hdr_interval,truey = testdata$y)))
  
    write.csv(OUT.pi.hdr,file.path(path_out,paste0(method,"_ls_hdr_pi_",filename,"_rep",rep,".csv")))
  }
  for (mtd in pi_methods) {
    write.csv(OUT.time,file.path(path_out,paste0(method,"_ls_",mtd,"_time_",filename,".csv")))
    write.csv(OUT.cov[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_coverage_",filename,".csv")))
    write.csv(OUT.pil[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_pi_length_",filename,".csv")))
    if (mtd != "hdr") {
      write.csv(OUT.pi[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_pi_",filename,".csv")))
    }
  }
  cat("Simulations for",filename,"with",method,"finished..","\n",sep=" ")
}
############################ end simulations #############################


########################## tables and figures ############################
methods <- c("pibf_cv","pialpha","oob",
             paste0("rfpi_ls_", c("lm","quant","spi","hdr","chdr")),
             "splitconformal","qrf","grf")

methods_name <- c("PIBF", bquote(widehat(PI)[alpha]),"OOB",
                  "LM","Quant","SPI","HDR","CHDR",
                  "SC","QRF","GRF")

dgp_names <- list("friedman1"="Friedman 1",
                  "friedman2"="Friedman 2",
                  "friedman3"="Friedman 3",
                  "peak"="Peak",
                  "H2c"="H2c",
                  "tree_normal"="Tree-Normal",
                  "tree_exp"="Tree-Exponential")

colors <- c("pibf_cv"="#F83839","pialpha"="#81B622","oob"="#C55FFC",
            "rfpi_ls_lm"="#FD7F20", "rfpi_ls_quant"="#0070E0",
            "rfpi_ls_spi"="#EC9EC0","rfpi_ls_hdr"="#FBEE0F","rfpi_ls_chdr"="#07BB9C",
            "splitconformal"="#FDB750","qrf"="#FF0080","grf"="#69B1F5")

all_files <- c()
for (j in 1:nrow(settings)) {
  all_files <- c(all_files,paste0(settings$dgp[j],"_ntrain",settings$ntrain[j]))
}
all_files <- sort(all_files)

## read results
all_mpil <- c()
all_pil <- list()
all_mean_mpil <- c()
all_mean_cov <- c()
for (m in methods) {
  files <- list.files(path=file.path(path_out),pattern=paste0(m,"_pi_length_"))
  files <- file.path(path_out,files)
  pil <- lapply(files,fread,data.table=FALSE,drop=c("V1"))
  mpil <- sapply(pil,colMeans)
  colnames(mpil) <- all_files
  mean_mpil <- colMeans(mpil)
  all_mean_mpil <- cbind(all_mean_mpil, mean_mpil)
  
  mpil <- reshape::melt(mpil,measure.vars=all_files,variable_name="setting")
  allset <- matrix(unlist(strsplit(as.character(mpil$X2),"_ntrain")),ncol=2,byrow=TRUE)
  allset <- data.frame(dgp=allset[,1],ntrain=as.numeric(allset[,2]))
  mpil <- cbind(allset,mpil)
  mpil <- mpil[,-(3:4)]
  mpil$method <- rep(m,nrow(mpil))
  all_mpil <- rbind(all_mpil,mpil)
  all_pil[[m]] <- mpil
  
  files <- list.files(path=file.path(path_out),pattern=paste0(m,"_coverage_"))
  files <- file.path(path_out,files)
  binary_cov <- lapply(files,fread,data.table=FALSE,drop=c("V1"))
  cov <- sapply(binary_cov, colMeans)
  colnames(cov) <- all_files
  mean_cov <- colMeans(cov)
  all_mean_cov <- cbind(all_mean_cov, mean_cov)
}
colnames(all_mean_mpil) <- methods
colnames(all_mean_cov) <- methods
all_mean_mpil <- as.data.frame(all_mean_mpil)
all_mean_cov <- as.data.frame(all_mean_cov)

## write summary tables
mat <- matrix(unlist(strsplit(rownames(all_mean_cov),split="_ntrain")),ncol=2,byrow=TRUE)
scenarios <- data.frame(ntrain=as.numeric(mat[,2]),
                        Data=factor(mat[,1],
                                    levels=names(dgp_names),
                                    labels=unname(unlist(dgp_names))))

all_mean_mpil <- cbind(scenarios, all_mean_mpil)
all_mean_cov <- cbind(scenarios, all_mean_cov)
all_mean_mpil <- all_mean_mpil[order(all_mean_mpil$ntrain,all_mean_mpil$Data),]
all_mean_cov <- all_mean_cov[order(all_mean_cov$ntrain,all_mean_cov$Data),]
rownames(all_mean_mpil) <- NULL
rownames(all_mean_cov) <- NULL

write.csv(all_mean_mpil, file.path(path_out,"mean_pi_length.csv"))
write.csv(all_mean_cov, file.path(path_out,"mean_coverage.csv"))

## draw one boxplot for % increase in mean PI length vs best over scenarios
mpil <- sapply(all_pil,function(x) x$value)
min_mpil <- apply(mpil,1,min)
mpil_perc <- 100*(mpil - min_mpil)/min_mpil
mpil_perc <- reshape::melt(mpil_perc,measure.vars=methods,variable_name="method")
mpil_perc <- mpil_perc[-1]
colnames(mpil_perc) <- c("method","value")
mpil_perc$method <- factor(mpil_perc$method,levels=methods)

maxy <- c()
for (m in methods) {
  maxy <- c(maxy,boxplot.stats(mpil_perc[mpil_perc$method==m,]$value)$stats)
}
maxy <- max(maxy)

perc.boxplot <- ggplot(mpil_perc,aes(x=method,y=value,fill=method)) + 
  geom_boxplot(outlier.shape=NA,lwd=0.3,fatten=2) +
  stat_summary(fun=mean,geom="point",aes(group=method),position=position_dodge(.75),shape=21,size=1.5,fill="white",color="black") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  font("title",size=10,color="black",face="bold") +
  font("ylab",size=9) + 
  font("legend.text",size=8) + 
  font("xy.text",size=7) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title.x=element_blank()) +
  ylab("% increase in mean PI length") + 
  ggtitle("% increase vs best performer - all runs") +
  scale_fill_manual(values=colors,
                    breaks=methods,
                    labels=methods_name) +
  scale_x_discrete(labels=methods_name) +
  coord_cartesian(ylim=c(0,maxy))

## draw one boxplot for mean coverages (mean of 500 replications) for each method
## each point in a boxplot represents a setting 

cov <- data.frame(matrix(NA,nrow(settings),length(methods),dimnames=list(1:nrow(settings),methods)))
for (j in 1:nrow(settings)) {
  filename <- paste0(settings$dgp[j],"_ntrain",settings$ntrain[j])
  
  for (m in methods) {
    binary.cov <- read.csv(file.path(path_out,paste0(m,"_coverage_",filename,".csv")),row.names=1)
    cov[j,m] <- mean(colMeans(binary.cov))
  }
}
cov <- reshape::melt(cov,measure.vars=methods,variable_name="method")

cov.boxplot <- ggplot(cov,aes(x=method,y=value,fill=method)) + 
  geom_boxplot(outlier.size=0.5,lwd=0.3,fatten=2) +
  geom_hline(yintercept=(1-alpha),linetype="dashed") +
  stat_summary(fun=mean,geom="point",aes(group=method),position=position_dodge(.75),shape=21,size=1.5,fill="white",color="black") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  font("title",size=10,color="black",face="bold") +
  font("ylab",size=9) + 
  font("legend.text",size=8) + 
  font("xy.text",size=7) + 
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank()) +
  ylab("Mean coverage") + 
  ggtitle("Mean coverage over 500 replications - all scenarios") +
  scale_fill_manual(values=colors,
                    breaks=methods,
                    labels=methods_name) +
  scale_x_discrete(labels=methods_name)

p <- ggarrange(cov.boxplot,perc.boxplot)
ggsave("global_simulated.pdf",p,path=path_out,dpi=300,width=8,height=5)


## draw boxplots for mean PI length over 500 replications for each data set
all_mpil$ntrain <- as.numeric(all_mpil$ntrain)
all_mpil$method <- factor(all_mpil$method,levels=methods)

set.dgp <- as.character(unique(settings$dgp))
for (i in 1:length(set.dgp)) {
  dgp <- set.dgp[i]
  mpil.boxplot <- ggplot(all_mpil[all_mpil$dgp==dgp,],aes(x=method,y=value,fill=method)) + 
    geom_boxplot(outlier.size=0.3,lwd=0.3,fatten=1) +
    stat_summary(fun=mean,geom="point",aes(group=method),position=position_dodge(.75),shape=21,size=0.7,fill="white",color="black") +
    theme_bw() + 
    theme(panel.grid=element_blank()) +
    font("title",size=10,color="black",face="bold") +
    font("ylab",size=9) + 
    font("legend.text",size=7) + 
    font("xy.text",size=7) + 
    theme(legend.position=c(.90,.98),
          legend.justification=c("right","top"),
          legend.background=element_blank(),
          legend.box.background=element_rect(colour="black",size=0.2),
          legend.margin=margin(t=0.01,b=0.1,r=0.1,l=0.1,unit="cm"),
          legend.title=element_blank(),
          legend.key.size=unit(0.3,"cm"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_text(hjust=0.5),
          strip.text.x=element_text(size=9),
          strip.background=element_rect(colour="black",
                                          fill="gray92")) +
    ylab("Mean PI length") + 
    ggtitle(dgp_names[dgp]) + 
    facet_wrap(vars(ntrain),nrow=1,scales="fixed",labeller=label_bquote(cols=n[train]==.(ntrain))) +
    scale_fill_manual(values=colors,
                      breaks=methods,
                      labels=methods_name)
  
  ggsave(filename=paste0("ml_",dgp,".pdf"),plot=mpil.boxplot,path=path_out,dpi=300,width=5,height=5)
}

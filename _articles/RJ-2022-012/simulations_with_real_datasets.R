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

## real data preparation
## note: if "real_data_preparation.R" is not in the same folder as the current file,
## ENTER THE CORRECT PATH OF THE FILE..
source(file.path(getwd(), "real_data_preparation.R"))


## ENTER PATHS OF THE DATA AND OUTPUT FOLDERS..
path_data <- file.path(getwd(), "real data sets")
path_out <- file.path(getwd(), "output for real data sets")
if (!file.exists(path_data)) {
  dir.create(path_data)
}
if (!file.exists(path_out)) {
  dir.create(path_out)
}

datasets <- c("abalone",
              "air_quality_ah",
              "air_quality_rh",
              "airfoil_selfnoise",
              "ames_housing",
              "auto_mpg",
              "boston",
              "computer_hardware",
              "concrete_compression",
              "concrete_slump",
              "energy_efficiency_cl",
              "energy_efficiency_hl",
              "servo")

## parameters
nrep <- 100
nfold <- 10
ntree <- 2000
alpha <- 0.05

## create cross-validation folds
set.seed(52489)
seed_set <- round(10000*abs(rnorm(nrep)))

for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  
  ## initialize storage
  OUT.folds <- matrix(NA, nrow=n, ncol=nrep)
  colnames(OUT.folds) <- paste0("rep",1:nrep)
  
  for (rep in 1:nrep) {
    ## create folds
    set.seed(as.numeric(seed_set[rep]))
    folds <- rep_len(1:nfold, n)
    folds <- sample(folds)
    OUT.folds[, rep] <- folds
  }
  write.csv(OUT.folds, file.path(path_data,paste0("folds_",dat,".csv")))
}


############################ start simulations #############################
set.seed(12345)
seed_set <- matrix(round(10000*abs(rnorm(nrep*nfold))),nrep,nfold)

## Run GRF method
method <- "grf"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    ## set parameters for RF
    mtry <- min(ceiling(sqrt(px) + 20), px)
    min.node.size <- 5 # default
    honesty <- TRUE # default
    honesty.fraction <- 0.5 # default
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)

      set.seed(seed_set[rep,fold])
      
      ## convert nonnumeric columns to numeric
      ix_nonnumeric <- which(sapply(traindata[, xvar.names], class) != "numeric")
      if (length(ix_nonnumeric) > 0) {
        for (i in ix_nonnumeric) {
          traindata[, i] <- as.numeric(traindata[, i])
          testdata[, i] <- as.numeric(testdata[, i])
        }
      }
      
      time1 <- system.time(
        grf <- quantile_forest(X = traindata[, xvar.names], 
                               Y = traindata$y, 
                               quantiles = c(alpha/2, 1-alpha/2), 
                               num.trees = ntree, 
                               mtry = mtry,
                               min.node.size = min.node.size,
                               honesty = honesty,
                               honesty.fraction = honesty.fraction)
      )
      
      time2 <- system.time(
        grf.pred <- predict(grf, testdata[, xvar.names], quantiles = c(alpha/2, 1-alpha/2))
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time1["elapsed"] + time2["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- grf.pred$prediction[, 1]
      OUT.pi[testindex, paste0(nr,"_upper")] <- grf.pred$prediction[, 2]
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run OOB method
method <- "oob"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3), 1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)

      set.seed(seed_set[rep,fold])
 
      time <- system.time(
        oob <- rfinterval(formula,
                          train_data = traindata, 
                          test_data = testdata, 
                          method = "oob", 
                          alpha = alpha, 
                          params_ranger = params_ranger)
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- oob$oob_interval$lower
      OUT.pi[testindex, paste0(nr,"_upper")] <- oob$oob_interval$upper
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run pialpha method
method <- "pialpha"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)
      
      ## set parameters for RF
      formula <- as.formula(y~.)
      params_ranger <- list(mtry = max(floor(px/3), 1),
                            num.trees = ntree,
                            min.node.size = 5,
                            replace = TRUE,
                            keep.inbag = TRUE,
                            data = traindata,
                            formula = formula)
      
      set.seed(seed_set[rep,fold])
      
      time1 <- system.time(
        rf <- do.call(ranger, params_ranger)
      )
      
      time2 <- system.time(
        pialpha <- quantForestError(
          forest = rf,
          X.train = traindata[, xvar.names],
          X.test = testdata[, xvar.names],
          Y.train = traindata$y,
          what = "interval",
          alpha = alpha
        )
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time1["elapsed"] + time2["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- pialpha$lower_0.05
      OUT.pi[testindex, paste0(nr,"_upper")] <- pialpha$upper_0.05
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run QRF method
method <- "qrf"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)
      
      set.seed(seed_set[rep,fold])
      
      time1 <- system.time(
        qrf <- quantregForest(x = traindata[, xvar.names], 
                              y = traindata$y,
                              mtry = max(floor(px/3), 1),
                              ntree = ntree,
                              nodesize = 5,
                              replace = TRUE)
      )
      
      time2 <- system.time(
        qrf.pred  <- predict(qrf, newdata = testdata[, xvar.names], what = c(alpha/2, 1-alpha/2))
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time1["elapsed"] + time2["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- qrf.pred[, 1]
      OUT.pi[testindex, paste0(nr,"_upper")] <- qrf.pred[, 2]
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run split conformal method
method <- "splitconformal"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## convert factors to numerical factor labels
  ix.factor <- xvar.names[sapply(data[,xvar.names], is.factor)]
  for (ix in ix.factor) {
    data[,ix] <- as.numeric(data[,ix])
  }
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    ## set parameters for RF
    params_rf <- rf.funs(ntree = ntree,
                         nodesize = 5,
                         replace = TRUE,
                         varfrac = 1/3)
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)
      
      set.seed(seed_set[rep,fold])
      
      time <- system.time(
        sc <- conformal.pred.split(x = as.matrix(traindata[, xvar.names]), 
                                   y = traindata$y, 
                                   x0 = as.matrix(testdata[, xvar.names]), 
                                   alpha = alpha,
                                   train.fun = params_rf$train.fun, 
                                   predict.fun = params_rf$predict.fun)
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- sc$lo
      OUT.pi[testindex, paste0(nr,"_upper")] <- sc$up
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run PIBF with CV calibration
method <- "pibf_cv"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  ## initialize storage
  OUT.cov <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pil <- matrix(NA, nrow=n, ncol=nrep)
  OUT.pi <- matrix(NA, nrow=n, ncol=2*nrep)
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.cov) <- paste0("rep",1:nrep)
  colnames(OUT.pil) <- paste0("rep",1:nrep)
  colnames(OUT.pi) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3), 1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)
      
      set.seed(seed_set[rep,fold])
      
      time <- system.time(
        pibf <- pibf(formula,
                     traindata = traindata,
                     testdata = testdata,
                     alpha = alpha,
                     calibration = "cv",
                     numfolds = 5,
                     params_ranger = params_ranger)
      )
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time["elapsed"]
      OUT.pi[testindex, paste0(nr,"_lower")] <- pibf$pred_interval$lower
      OUT.pi[testindex, paste0(nr,"_upper")] <- pibf$pred_interval$upper
      OUT.pil[testindex, nr] <- OUT.pi[testindex, paste0(nr, "_upper")] - OUT.pi[testindex, paste0(nr, "_lower")]
      OUT.cov[testindex, nr] <- as.numeric((testdata$y <= OUT.pi[testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[testindex, paste0(nr, "_lower")]))
    }
  }
  write.csv(OUT.cov,file.path(path_out,paste0(method,"_coverage_",dat,".csv")))
  write.csv(OUT.pil,file.path(path_out,paste0(method,"_pi_length_",dat,".csv")))
  write.csv(OUT.pi,file.path(path_out,paste0(method,"_pi_",dat,".csv")))
  write.csv(OUT.time,file.path(path_out,paste0(method,"_time_",dat,".csv")))
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
############################################################################

## Run RFPI methods
method <- "rfpi"
for (dat in datasets) {
  ## read data
  data <- readRDS(file.path(path_data,paste0(dat,".rds")))
  n <- nrow(data)
  xvar.names <- colnames(data)[grepl("x",colnames(data))]
  px <- length(xvar.names)
  
  ## read folds
  OUT.folds <- read.csv(file.path(path_data,paste0("folds_",dat,".csv")),row.names = 1)
  
  pi_methods <- c("lm","spi","quant","hdr","chdr")
  
  ## initialize storage
  OUT.cov <- vector("list", length = length(pi_methods))
  OUT.pil <- vector("list", length = length(pi_methods))
  OUT.pi <- vector("list", length = length(pi_methods))
  names(OUT.cov) <- pi_methods
  names(OUT.pil) <- pi_methods
  names(OUT.pi) <- pi_methods
  for (mtd in pi_methods) {
    OUT.cov[[mtd]] <- matrix(NA, nrow=n, ncol=nrep)
    OUT.pil[[mtd]] <- matrix(NA, nrow=n, ncol=nrep)
    OUT.pi[[mtd]] <- matrix(NA, nrow=n, ncol=2*nrep)
    colnames(OUT.cov[[mtd]]) <- paste0("rep",1:nrep)
    colnames(OUT.pil[[mtd]]) <- paste0("rep",1:nrep)
    colnames(OUT.pi[[mtd]]) <- c(paste0(paste0("rep",1:nrep),"_lower"),paste0(paste0("rep",1:nrep),"_upper"))
  }
  OUT.time <- matrix(NA, nrow=nrep, ncol=nfold)
  colnames(OUT.time) <- paste0("time_in_sec_fold",1:nfold)
  
  for (rep in 1:nrep) {
    cat("replication",rep,"\n",sep=" ")
    nr <- paste0("rep",rep)
    
    ## get folds
    folds <- OUT.folds[, nr]
    
    ## set parameters for RF
    formula <- as.formula(y~.)
    params_ranger <- list(mtry = max(floor(px/3), 1),
                          num.trees = ntree,
                          min.node.size = 5,
                          replace = TRUE)
    params_calib = list(range = c(1-alpha-0.005, 1-alpha+0.005),
                        start = (1-alpha),
                        step = 0.01,
                        refine = TRUE)
    
    OUT.pi.hdr1 <- c()
    
    for (fold in 1:nfold) {
      ## split data
      testindex <- which(folds == fold)
      traindata <- data[-testindex, ]
      testdata <- data[testindex, ]
      ntrain <- nrow(traindata)
      ntest <- nrow(testdata)
      
      set.seed(seed_set[rep,fold])
      
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
      
      OUT.time[rep, paste0("time_in_sec_fold",fold)] <- time["elapsed"]
      
      ## lm interval
      OUT.pi[["lm"]][testindex, paste0(nr,"_lower")] <- rfpi$lm_interval$lower
      OUT.pi[["lm"]][testindex, paste0(nr,"_upper")] <- rfpi$lm_interval$upper
      OUT.pil[["lm"]][testindex, nr] <- OUT.pi[["lm"]][testindex, paste0(nr, "_upper")] - OUT.pi[["lm"]][testindex, paste0(nr, "_lower")]
      OUT.cov[["lm"]][testindex, nr] <- as.numeric((testdata$y <= OUT.pi[["lm"]][testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[["lm"]][testindex, paste0(nr, "_lower")]))
      
      ## spi interval
      OUT.pi[["spi"]][testindex, paste0(nr,"_lower")] <- rfpi$spi_interval$lower
      OUT.pi[["spi"]][testindex, paste0(nr,"_upper")] <- rfpi$spi_interval$upper
      OUT.pil[["spi"]][testindex, nr] <- OUT.pi[["spi"]][testindex, paste0(nr, "_upper")] - OUT.pi[["spi"]][testindex, paste0(nr, "_lower")]
      OUT.cov[["spi"]][testindex, nr] <- as.numeric((testdata$y <= OUT.pi[["spi"]][testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[["spi"]][testindex, paste0(nr, "_lower")]))
      
      ## quant interval
      OUT.pi[["quant"]][testindex, paste0(nr,"_lower")] <- rfpi$quant_interval$lower
      OUT.pi[["quant"]][testindex, paste0(nr,"_upper")] <- rfpi$quant_interval$upper
      OUT.pil[["quant"]][testindex, nr] <- OUT.pi[["quant"]][testindex, paste0(nr, "_upper")] - OUT.pi[["quant"]][testindex, paste0(nr, "_lower")]
      OUT.cov[["quant"]][testindex, nr] <- as.numeric((testdata$y <= OUT.pi[["quant"]][testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[["quant"]][testindex, paste0(nr, "_lower")]))
      
      ## chdr interval
      OUT.pi[["chdr"]][testindex, paste0(nr,"_lower")] <- rfpi$chdr_interval$lower
      OUT.pi[["chdr"]][testindex, paste0(nr,"_upper")] <- rfpi$chdr_interval$upper
      OUT.pil[["chdr"]][testindex, nr] <- OUT.pi[["chdr"]][testindex, paste0(nr, "_upper")] - OUT.pi[["chdr"]][testindex, paste0(nr, "_lower")]
      OUT.cov[["chdr"]][testindex, nr] <- as.numeric((testdata$y <= OUT.pi[["chdr"]][testindex, paste0(nr, "_upper")]) * (testdata$y >= OUT.pi[["chdr"]][testindex, paste0(nr, "_lower")]))
      
      ## hdr interval
      hdr_int_list <- lapply(1:ntest, function(i, hdr_int) {
        pi <- hdr_int[[i]]
        cbind(rep(testindex[i],nrow(pi)), 1:nrow(pi), pi[,"lower"], pi[,"upper"]) },
        hdr_int = rfpi$hdr_interval)
      OUT.pi.hdr <- Reduce(rbind, hdr_int_list)
      colnames(OUT.pi.hdr) <- c("test_obs", "pi_no", "lower", "upper")
      rownames(OUT.pi.hdr) <- NULL
      OUT.pi.hdr1 <- rbind(OUT.pi.hdr1, OUT.pi.hdr)
      OUT.pil[["hdr"]][testindex, nr] <- unlist(Map(function(pi){sum(pi[, "upper"] - pi[, "lower"])}, pi = rfpi$hdr_interval))
      OUT.cov[["hdr"]][testindex, nr] <- as.numeric(unlist(Map(function(pi, truey){sum((pi[, "lower"] <= truey) * (truey <= pi[, "upper"]))},
                                                               pi = rfpi$hdr_interval, truey = testdata$y)))
    }
    write.csv(OUT.pi.hdr1,file.path(path_out,paste0(method,"_ls_hdr_pi_",dat,"_rep",rep,".csv")))
  }
  for (mtd in pi_methods) {
    write.csv(OUT.time,file.path(path_out,paste0(method,"_ls_",mtd,"_time_",dat,".csv")))
    write.csv(OUT.cov[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_coverage_",dat,".csv")))
    write.csv(OUT.pil[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_pi_length_",dat,".csv")))
    if (mtd != "hdr") {
      write.csv(OUT.pi[[mtd]],file.path(path_out,paste0(method,"_ls_",mtd,"_pi_",dat,".csv")))
    }
  }
  cat("Simulations for",dat,"with",method,"finished..","\n",sep=" ")
}
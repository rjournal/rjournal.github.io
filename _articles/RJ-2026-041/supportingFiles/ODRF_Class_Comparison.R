library(foreach)
library(doParallel)
library(randomForest)
library(xgboost)
library(grf)
library(RLT)
library(ODRF)
library(obliqueRF)
library(PPforest)
library(rotationForest)
library(rerf)
library(partykit)
library(aorsf)

source("./Datasets/ClassDataSets.R")

i.data <- 12
cnames <- c()
ret <- NULL
print("start")
i <- 0

dataA <- 9:23
for (i.data in dataA)
{
  print(paste(i + 1, length(dataA), sep = "/"))
  data <- Data.Set(i.data, wd = "./Datasets/Classification")
  X0 <- as.matrix(data$X)
  y0 <- as.factor(data$y)

  v <- which(apply(X0, 2, sd) > 1.0e-10)
  X0 <- X0[, v]
  p <- ncol(X0)
  N <- length(y0)

  rm(data)
  sn <- min(floor(N * 2 / 3), 1000)

  set.seed(i.data)

  I <- sample(1:N, N)
  XI <- X0[I, ]
  yI <- y0[I]

  X <- XI[1:sn, ]
  y <- yI[1:sn]

  SN <- min(N, sn + 2000)
  X1 <- XI[(sn + 1):SN, ]
  y1 <- yI[(sn + 1):SN]

  rm(XI)
  rm(yI)

  v <- which(apply(X, 2, sd) > 1.0e-10)
  X <- X[, v]
  X1 <- X1[, v]

  p1 <- ncol(X)
  name1 <- paste("v", 1:p1, sep = "")
  colnames(X) <- name1
  colnames(X1) <- name1


  level <- levels(y)
  ################################################################################
  t0 <- proc.time()
  A <- randomForest(X, y, ntree = 100)
  pred <- predict(A, X1)
  # pred=as.character(pred)
  e.RF <- mean(pred != y1)
  t1 <- proc.time()
  t.RF <- c(t1 - t0)[3]


  t0 <- proc.time()
  # Regular ensemble trees (Extremely Randomized Trees, Geurts, et. al., 2006)
  A <- RLT(X, y, model = "classification", ntrees = 100)
  pred <- predict(A, X1)$Prediction
  # pred=level[pred+1]
  e.ERT <- mean(pred != y1)
  t1 <- proc.time()
  t.ERT <- c(t1 - t0)[3]


  t0 <- proc.time()
  A <- ODRF(X, y, split = "entropy", parallel = FALSE, ntrees = 100)
  pred <- predict(A, X1)
  e.ODRF <- mean(pred != y1)
  t1 <- proc.time()
  t.ODRF <- c(t1 - t0)[3]


  t0 <- proc.time()
  # extreme gradient boosting (xgboost)
  if (length(level) > 2) {
    A <- xgboost(data = X, label = as.numeric(y) - 1, verbose = 0, nrounds = 100, objective = "multi:softmax", num_class = length(level)) # ,print_every_n= 0,save_period=1)
  } else {
    A <- xgboost(data = X, label = as.numeric(y) - 1, verbose = 0, nrounds = 100, objective = "binary:hinge") # ,print_every_n= 0,save_period=1)
  }
  pred <- predict(A, X1)
  e.XGB <- mean(pred != as.numeric(y1) - 1)
  t1 <- proc.time()
  t.XGB <- c(t1 - t0)[3]


  t0 <- proc.time()
  A <- probability_forest(X, y, num.trees = 100) # ,num.trees=500
  # A = regression_forest(X,y) # grf1: regression forest
  pred <- predict(A, X1)$predictions
  pred <- colnames(pred)[apply(pred, 1, which.max)]
  e.gRF <- mean(pred != y1)
  t1 <- proc.time()
  t.gRF <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- rotationForest(data.frame(X), y, L = 100)
  pred <- predict(A, data.frame(X1))
  pred <- (pred > 0.5) + 0
  e.rotRF <- mean(pred != y1)
  t1 <- proc.time()
  t.rotRF <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- RerF(X, y, num.cores = 1L, FUN = RandMatBinary, trees = 100)
  pred <- Predict(X1, A, num.cores = 1L)
  e.spoRF <- mean(pred != y1)
  t1 <- proc.time()
  t.spoRF <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- obliqueRF(X, as.numeric(y), verbose = F, ntree = 100)
  pred <- predict(A, X1)
  e.ORF <- mean(pred != as.numeric(y1))
  t1 <- proc.time()
  t.ORF <- c(t1 - t0)[3]

  t0 <- proc.time()
  # A <- PPforest(data = data.frame(X, y = y), class = "y", std = F, size.tr = 1, m = 100, size.p = 1 / 3, PPmethod = "LDA")
  A <- PPforest(data = data.frame(X, y = y), y = "y", std = F, size.tr = 1, m = 100, size.p = 2 / 3, PPmethod = "LDA")
  pred <- trees_pred(A, xnew = data.frame(X1))$predforest
  pred <- level[pred]
  e.PPF <- mean(pred != y1)
  t1 <- proc.time()
  t.PPF <- c(t1 - t0)[3]

  # library(aorsf)
  A <- orsf(
    formula = y ~ ., data = data.frame(X, y = y),
    n_tree = 100
  )
  pred <- predict(A,
    new_data = data.frame(X1),
    pred_type = "class"
  )
  e.ORSF <- mean(level[pred] != y1)
  t1 <- proc.time()
  t.ORSF <- c(t1 - t0)[3]

  ################################################################################
  e <- c(e.RF, e.gRF, e.ERT, e.XGB, e.rotRF, e.spoRF, e.PPF, e.ORF, e.ORSF, e.ODRF)
  t <- c(t.RF, t.gRF, t.ERT, t.XGB, t.rotRF, t.spoRF, t.PPF, t.ORF, t.ORSF, t.ODRF)
  et <- c(e, t)

  OUT <- c(i.data, N, p, et)

  MR <- c("MR.RF", "MR.GRF", "MR.ERT", "MR.XGB", "MR.RotRF", "MR.SPORF", "MR.PPF", "MR.ORF", "MR.ORSF", "MR.ODRF")
  Time <- c("Time.RF", "Time.GRF", "Time.ERT", "Time.XGB", "Time.RotRF", "Time.SPORF", "Time.PPF", "Time.ORF", "Time.ORSF", "Time.ODRF")
  names(OUT) <- c("i.data", "N", "p", c(MR, Time))
  # print(OUT, 4.4)
  # print(levels(y0))

  # write.table(t(OUT), file="./result/ODRF_Class_Comparison2_1.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  ret <- rbind(ret, OUT)
  cnames <- names(OUT)
  i <- i + 1
}

################################################################################

# save(ret, file = "./Results/Forest_Classification_Comparison_Results.rda")

## ODRF Class
# load("./Results/Forest_Classification_Comparison_Results.rda")
# fileName="./result/Comparison_Summaries4.csv"
# write.table(t(c("Forest_Classification_Comparison_Summaries",nrow(ret))), file=fileName,
#            sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)
# write.table(" ", file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)

j <- 10
Comparison <- NULL
ret <- ret[, -seq(3)]
for (i in seq(2) - 1) {
  ret0 <- ret[, 1:j + j * i]
  e <- colMeans(ret0)
  emin <- c(seq(j), max.col(-ret0))
  et <- table(emin) - 1
  out <- rbind(e, et)
  colnames(out) <- c("RF", "GRF", "ERT", "XGB", "RotRF", "SPORF", "PPF", "ORF", "ORSF", "ODRF")
  if (i == 0) {
    rownames(out) <- c("MR", "Total")
    out[1, ] <- round(out[1, ] * 100, 2)
    MR <- out
  }
  if (i == 1) {
    rownames(out) <- c("Time", "Total")
    out[1, ] <- round(out[1, ], 2)
    Time <- out
  }
  # write.table(out, file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = TRUE)
}

print(list(MR = MR, Time = Time))
save(MR, Time, file = "./Results/Forest_Classification_Comparison_Summaries.rda")

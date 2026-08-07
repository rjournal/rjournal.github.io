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

source("./Datasets/RegrDataSets.R")

i.data <- 1
cnames <- c()
ret <- NULL
print("start")
i <- 0

dataA <- seq(20)
for (i.data in dataA)
{
  print(paste(i + 1, length(dataA), sep = "/"))
  data <- Data.Set(i.data, wd = "./Datasets/Regression")
  X0 <- as.matrix(data$X)
  y0 <- c(data$y)

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


  level <- c()
  ##############################################################
  t0 <- proc.time()
  A <- randomForest(X, y, ntree = 100)
  pred <- predict(A, X1)
  # pred=as.character(pred)
  e.RF <- mean((pred - y1)^2) # mean(pred != y1)#
  t1 <- proc.time()
  t.RF <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- RLT(X, y, model = "regression", ntrees = 100) # , ntrees =100)
  pred <- predict(A, X1)$Prediction
  # pred=level[pred+1]
  e.ERT <- mean((pred - y1)^2) # mean(pred!=y1)
  t1 <- proc.time()
  t.ERT <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- ODRF(X, y, split = "mse", parallel = FALSE, ntrees = 100)
  # A = online(A,data.frame(X1,y=y1))
  # A = prune(A,data.frame(X1,y=y1))
  pred <- predict(A, X1)
  e.ODRF <- mean((pred - y1)^2) # mean(pred!=y1)
  t1 <- proc.time()
  t.ODRF <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- xgboost(data = X, label = y, verbose = 0, nrounds = 100, objective = "reg:squarederror")
  pred <- predict(A, X1)
  e.XGB <- mean((pred - y1)^2) # mean(pred != as.numeric(y1)-1)
  t1 <- proc.time()
  t.XGB <- c(t1 - t0)[3]

  t0 <- proc.time()
  # A = probability_forest(X, y)#,num.trees=500
  A <- regression_forest(X, y, num.trees = 100) # grf1: regression forest
  pred <- predict(A, X1)$predictions
  # pred =colnames(pred)[apply(pred, 1, which.max)]
  e.gRF <- mean((pred - y1)^2) # mean(pred != y1)
  t1 <- proc.time()
  t.gRF <- c(t1 - t0)[3]

  # library(aorsf)
  A <- orsf(
    formula = y ~ ., data = data.frame(X, y = y),
    n_tree = 100
  )
  pred <- predict(A,
    new_data = data.frame(X1),
    pred_type = "mean"
  )
  e.ORSF <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.ORSF <- c(t1 - t0)[3]


  e.0 <- mean((y1 - mean(y))^2)
  tree <- c(e.RF, e.gRF, e.ERT, e.XGB, e.ORSF, e.ODRF)
  e <- tree / e.0
  t <- c(t.RF, t.gRF, t.ERT, t.XGB, t.ORSF, t.ODRF)
  et <- c(e, t)

  OUT <- c(i.data, N, p, et)
  RPE <- c("RPE.RF", "RPE.GRF", "RPE.ERT", "RPE.XGB", "RPE.ORSF", "RPE.ODRF")
  Time <- c("Time.RF", "Time.GRF", "Time.ERT", "Time.XGB", "Time.ORSF", "Time.ODRF")
  names(OUT) <- c("i.data", "N", "p", c(RPE, Time))
  # print(OUT, 4.4)

  # write.table(t(OUT), file="./result/ODRF_Regression_Comparison12_1.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  ret <- rbind(ret, OUT)
  cnames <- names(OUT)
  i <- i + 1
}

################################################################################

# save(ret, file = "./Results/Forest_Regression_Comparison_Results.rda")

## ODRF Regression
# load("./Results/Forest_Regression_Comparison_Results.rda")
# fileName="./result/Comparison_Summaries4.csv"
# write.table(t(c("Forest_Regression_Comparison_Summaries",nrow(ret))), file=fileName,
#            sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)
# write.table(" ", file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)


j <- 6
Comparison <- NULL
ret <- ret[, -seq(3)]
for (i in seq(2) - 1) {
  ret0 <- ret[, 1:j + j * i]
  e <- colMeans(ret0)
  emin <- c(seq(j), max.col(-ret0))
  et <- table(emin) - 1
  out <- rbind(e, et)
  colnames(out) <- c("RF", "GRF", "ERT", "XGB", "ORSF", "ODRF")
  if (i == 0) {
    rownames(out) <- c("RPE", "Total")
    out[1, ] <- round(out[1, ], 3)
    RPE <- out
  }
  if (i == 1) {
    rownames(out) <- c("Time", "Total")
    out[1, ] <- round(out[1, ], 2)
    Time <- out
  }
  # write.table(out, file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = TRUE)
}

print(list(RPE = RPE, Time = Time))
save(RPE, Time, file = "./Results/Forest_Regression_Comparison_Summaries.rda")

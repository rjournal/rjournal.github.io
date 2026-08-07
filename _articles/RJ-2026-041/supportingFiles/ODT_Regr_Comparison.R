library(foreach)
library(doParallel)
library(rpart)
library(ODRF)
library(RLT)
library(evtree)
library(partykit)
library(oblique.tree)
library(rotationForest)
library(rerf)
library(PPtreereg)

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
  A <- rpart(y ~ ., data = data.frame(X, y = y))
  pred <- predict(A, data.frame(X1)) # factor
  e.Tree <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.Tree <- c(t1 - t0)[3]
  c.Tree <- length(which("<leaf>" == A[["frame"]][["var"]]))

  t0 <- proc.time()
  A <- ODT(X, y, split = "mse")
  # A = online(A,data.frame(X1,y=y1))
  # A = prune(A,data.frame(X1,y=y1))
  pred <- predict(A, X1)
  e.ODT <- mean((pred - y1)^2) # mean(pred != y1)
  t1 <- proc.time()
  t.ODT <- c(t1 - t0)[3]
  c.ODT <- length(which(A[["structure"]][["nodeCutValue"]] == 0))

  t0 <- proc.time()
  A <- RLT(X, y, model = "regression", ntrees = 1)
  pred <- predict(A, X1)$Prediction
  e.extraTree <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.extraTree <- c(t1 - t0)[3]
  c.extraTree <- length(which(is.nan(A[["FittedTrees"]][[1]][9, ])))

  # library(evtree)
  t0 <- proc.time()
  A <- evtree(y ~ ., data = data.frame(X, y = y), control = evtree.control(ntrees = 10, niterations = 1000L))
  pred <- predict(A, data.frame(X1))
  e.evTree <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.evTree <- c(t1 - t0)[3]
  c.evTree <- width(A)

  # library(partykit)
  t0 <- proc.time()
  A <- ctree(y ~ ., data = data.frame(X, y = y))
  pred <- predict(A, data.frame(X1))
  e.cTree <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.cTree <- c(t1 - t0)[3]
  c.cTree <- width(A)
  # depth(A)

  # library(PPtreereg)
  t0 <- proc.time()
  A <- try(PPTreereg(y ~ ., data = data.frame(X, y = y)), silent = TRUE)
  if ("try-error" %in% class(A)) {
    A <- ODT(X, y, split = "mse", NodeRotateFun = "RotMatRand")
  }
  pred <- predict(A, data.frame(X1))
  e.PPT <- mean((pred - y1)^2)
  t1 <- proc.time()
  t.PPT <- c(t1 - t0)[3]
  c.PPT <- length(A[["Tree.result"]][["Tree.Struct"]][A[["Tree.result"]][["Tree.Struct"]][, 2] == 0, 3])

  # calculate RPE
  e.0 <- mean((y1 - mean(y))^2)
  tree <- c(e.Tree, e.extraTree, e.evTree, e.cTree, e.PPT, e.ODT)
  e <- tree / e.0
  t <- c(t.Tree, t.extraTree, t.evTree, t.cTree, t.PPT, t.ODT)
  c <- c(c.Tree, c.extraTree, c.evTree, c.cTree, c.PPT, c.ODT)
  etc <- c(e, t, c)

  OUT <- c(i.data, N, p, etc)

  RPE <- c("RPE.CART", "RPE.ERT", "RPE.EVT", "RPE.CT", "RPE.PPT", "RPE.ODT")
  Time <- c("Time.CART", "Time.ERT", "Time.EVT", "Time.CT", "Time.PPT", "Time.ODT")
  Comp <- c("Comp.CART", "Comp.ERT", "Comp.EVT", "Comp.CT", "Comp.PPT", "Comp.ODT")
  names(OUT) <- c("i.data", "N", "p", c(RPE, Time, Comp))
  # print(OUT, 4.4)

  # write.table(t(OUT), file="./result/ODT_Regression_Comparison2_1.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  ret <- rbind(ret, OUT)
  cnames <- names(OUT)
  i <- i + 1
}

##################################################################################

# save(ret, file = "./Results/Tree_Regression_Comparison_Results.rda")

## ODRF Regression
# load("./Results/Tree_Regression_Comparison_Results.rda")
# fileName="./result/Comparison_Summaries4.csv"
# write.table(t(c("Tree_Regression_Comparison_Summaries",nrow(ret))), file=fileName,
#            sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)
# write.table(" ", file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)


j <- 6
Comparison <- NULL
ret <- ret[, -seq(3)]
for (i in seq(3) - 1) {
  ret0 <- ret[, 1:j + j * i]
  e <- colMeans(ret0)
  emin <- c(seq(j), max.col(-ret0))
  et <- table(emin) - 1
  out <- rbind(e, et)

  colnames(out) <- c("CART", "ERT", "EVT", "CT", "PPT", "ODT")
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
  if (i == 2) {
    rownames(out) <- c("Complexity", "Total")
    out[1, ] <- round(out[1, ], 2)
    Complexity <- out
  }
  # write.table(out, file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = TRUE)
}

print(list(RPE = RPE, Time = Time, Complexity = Complexity))
save(RPE, Time, Complexity, file = "./Results/Tree_Regression_Comparison_Summaries.rda")

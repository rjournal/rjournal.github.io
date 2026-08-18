library(foreach)
library(doParallel)
library(rpart)
library(ODRF)
library(PPtreeViz)
library(RLT)
library(evtree)
library(partykit)
library(oblique.tree)
library(rotationForest)
library(rerf)
source("./Datasets/ClassDataSets.R")

i.data <- 3
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
  ##############################################################
  t0 <- proc.time()
  A <- rpart(y ~ ., method = "class", data = data.frame(X, y = y))
  pred <- predict(A, data.frame(X1), type = "class") # factor
  e.Tree <- mean(pred != y1)
  t1 <- proc.time()
  t.Tree <- c(t1 - t0)[3]
  c.Tree <- length(which("<leaf>" == A[["frame"]][["var"]]))

  t0 <- proc.time()
  A <- ODT(X, y, split = "entropy")
  pred <- predict(A, X1)
  e.ODT <- mean(pred != y1)
  t1 <- proc.time()
  t.ODT <- c(t1 - t0)[3]
  c.ODT <- length(which(A[["structure"]][["nodeCutValue"]] == 0))

  t0 <- proc.time()
  A <- PPTreeclass(y ~ ., data = data.frame(X, y = y), "LDA")
  pred <- predict(A, data.frame(X1))
  e.ppTree <- mean(pred != y1)
  t1 <- proc.time()
  t.ppTree <- c(t1 - t0)[3]
  c.ppTree <- length(A[["Tree.Struct"]][A[["Tree.Struct"]][, 2] == 0, 3])

  # library(obliqueRF)
  # library(oblique.tree)
  t0 <- proc.time()
  A <- oblique.tree(y ~ ., data = data.frame(X, y = y), oblique.splits = "on")
  pred <- predict(A, newdata = data.frame(X1, y = y1), type = "class")
  e.obliqueTree <- mean(pred != y1)
  c.obliqueTree <- length(which(A[["frame"]][["splits"]][, 1] == ""))
  t1 <- proc.time()
  t.obliqueTree <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- rotationForest(data.frame(X), y, L = 1)
  pred <- predict(A, data.frame(X1))
  pred <- (pred > 0.5) + 0
  e.rotT <- mean(pred != y1)
  c.rotT <- length(which("<leaf>" == A[["models"]][[1]][["frame"]][["var"]]))
  t1 <- proc.time()
  t.rotT <- c(t1 - t0)[3]

  t0 <- proc.time()
  # BuildTree(x, y, FUN = RandMatBinary)
  A <- RerF(X, y, num.cores = 1L, FUN = RandMatBinary, trees = 1)
  pred <- Predict(X1, A, num.cores = 1L) # , Xtrain = X[trainIdx, ])
  e.spoT <- mean(pred != y1)
  c.spoT <- length(which(is.na(PrintTree(A)[, 4])))
  t1 <- proc.time()
  t.spoT <- c(t1 - t0)[3]

  t0 <- proc.time()
  A <- RLT(X, y, model = "classification", ntrees = 1)
  pred <- predict(A, X1)$Prediction
  e.extraTree <- mean(pred != y1)
  t1 <- proc.time()
  t.extraTree <- c(t1 - t0)[3]
  c.extraTree <- length(which(is.nan(A[["FittedTrees"]][[1]][9, ])))

  t0 <- proc.time()
  # library(evtree)
  A <- evtree(y ~ ., data = data.frame(X, y = y), control = evtree.control(ntrees = 10, niterations = 1000L))
  pred <- predict(A, data.frame(X1))
  e.evTree <- mean(pred != y1)
  t1 <- proc.time()
  t.evTree <- c(t1 - t0)[3]
  c.evTree <- width(A)

  t0 <- proc.time()
  # library(partykit)
  A <- ctree(y ~ ., data = data.frame(X, y = y))
  pred <- predict(A, data.frame(X1))
  e.cTree <- mean(pred != y1)
  t1 <- proc.time()
  t.cTree <- c(t1 - t0)[3]
  c.cTree <- width(A)

  # e.0 = mean((y1-mean(y))^2)
  e <- c(e.Tree, e.extraTree, e.evTree, e.cTree, e.rotT, e.spoT, e.ppTree, e.obliqueTree, e.ODT)
  # e=tree/e.0
  t <- c(t.Tree, t.extraTree, t.evTree, t.cTree, t.rotT, t.spoT, t.ppTree, t.obliqueTree, t.ODT)
  c <- c(c.Tree, c.extraTree, c.evTree, c.cTree, c.rotT, c.spoT, c.ppTree, c.obliqueTree, c.ODT)
  etc <- c(e, t, c)

  OUT <- c(i.data, N, p, etc)

  MR <- c("MR.CART", "MR.ERT", "MR.EVT", "MR.CT", "MR.RotT", "MR.SPORT", "MR.PPT", "MR.OT", "MR.ODT")
  Time <- c("Time.CART", "Time.ERT", "Time.EVT", "Time.CT", "Time.RotT", "Time.SPORT", "Time.PPT", "Time.OT", "Time.ODT")
  Complexity <- c("Comp.CART", "Comp.ERT", "Comp.EVT", "Comp.CT", "Comp.RotT", "Comp.SPORT", "Comp.PPT", "Comp.OT", "Comp.ODT")
  names(OUT) <- c("i.data", "n", "p", c(MR, Time, Complexity))
  # print(OUT, 4.4)
  # print(levels(y0))

  # write.table(t(OUT), file= "./result/ODT_Class_Comparison2_1.csv",sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  ret <- rbind(ret, OUT)
  cnames <- names(OUT)
  i <- i + 1
}

##################################################################################

# save(ret, file = "./Results/Tree_Classification_Comparison_Results.rda")

## ODT Class
# load("./Results/Tree_Classification_Comparison_Results.rda")
# fileName="./result/Comparison_Summaries4.csv"
# write.table(t(c("Tree_Classification_Comparison_Summaries",nrow(ret))), file=fileName,
#            sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)
# write.table(" ", file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = FALSE)

j <- 9
ret <- ret[, -seq(3)]
Complexity <- NULL
for (i in seq(3) - 1) {
  ret0 <- ret[, 1:j + j * i]
  e <- colMeans(ret0)
  emin <- c(seq(j), max.col(-ret0))
  et <- table(emin) - 1
  out <- rbind(e, et)
  colnames(out) <- c("CART", "ERT", "EVT", "CT", "RotT", "SPORT", "PPT", "OT", "ODT")
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
  if (i == 2) {
    rownames(out) <- c("Complexity", "Total")
    out[1, ] <- round(out[1, ], 2)
    Complexity <- out
  }
  # write.table(out, file=fileName,sep = ",",append=TRUE,row.names = FALSE,col.names = TRUE)
}

print(list(MR = MR, Time = Time, Complexity = Complexity))

if (j == 5) {
  save(MR, Time, file = "./Results/Tree_Classification_Comparison_Summaries.rda")
}
if (j == 9) {
  save(MR, Time, Complexity, file = "./Results/Tree_Classification_Comparison_Summaries.rda")
}

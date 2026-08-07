library(foreach)
library(doParallel)
library(rpart)
library(ODRF)
library(PPtreeViz)
library(RLT)
library(evtree)
library(partykit)
library(oblique.tree)
library(PPtreereg)

source("./Datasets/RegrDataSets.R")

depend_packages <- c("rpart", "ODRF", "PPtreeViz", "RLT", "evtree", "partykit", "oblique.tree", "PPtreereg")

cores <- Inf
Rep <- 100
cores <- min(c(detectCores(logical = FALSE), cores))
cl <- makeCluster(cores)

if (Rep <= cores) {
  chunks <- as.list(seq(Rep))
} else {
  chunks <- clusterSplit(cl, seq(Rep))
}


################################################################################
print("start")
print(c(cores = cores))
registerDoParallel(cl, cores = cores)

i.data <- 12
cnames <- c()
ret <- NULL
dataA <- seq(20)
for (i.data in dataA)
{
  # print(i.data)
  print(paste(i.data, length(dataA), sep = "/"))
  data <- Data.Set(i.data, wd = "./Datasets/Regression")
  X0 <- as.matrix(data$X)
  y0 <- c(data$y)

  v <- which(apply(X0, 2, sd) > 1.0e-10)
  X0 <- X0[, v]
  p <- ncol(X0)
  N <- length(y0)

  rm(data)
  sn <- min(floor(N * 2 / 3), 1000)
  co <- 1
  Err <- foreach(co = seq(length(chunks)), .combine = "rbind", .packages = depend_packages) %dopar% {
    E <- matrix(1, nrow = length(chunks[[co]]), ncol = 8)
    ico <- 1
    for (ico in chunks[[co]]) {
      set.seed(2025 + ico)

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
      A <- rpart(y ~ ., data = data.frame(X, y = y))
      pred <- predict(A, data.frame(X1)) # factor
      e.Tree <- mean((pred - y1)^2)

      A <- ODT(X, y, split = "mse")
      # A = online(A,data.frame(X1,y=y1))
      # A = prune(A,data.frame(X1,y=y1))
      pred <- predict(A, X1)
      e.ODT <- mean((pred - y1)^2) # mean(pred != y1)

      A <- ODT(X, y, split = "mse", NodeRotateFun = "RotMatRand")
      pred <- predict(A, X1)
      e.ODT.SPROT <- mean((pred - y1)^2)

      A <- ODT(X, y, split = "mse", NodeRotateFun = "RotMatRand", TreeRandRotate = TRUE)
      pred <- predict(A, X1)
      e.ODT.Rand <- mean((pred - y1)^2)

      A <- RLT(X, y, model = "regression", ntrees = 1)
      pred <- predict(A, X1)$Prediction
      e.extraTree <- mean((pred - y1)^2)

      # library(evtree)
      A <- evtree(y ~ ., data = data.frame(X, y = y), control = evtree.control(ntrees = 10, niterations = 1000L))
      pred <- predict(A, data.frame(X1))
      e.evTree <- mean((pred - y1)^2)

      # library(partykit)
      A <- ctree(y ~ ., data = data.frame(X, y = y))
      pred <- predict(A, data.frame(X1))
      e.cTree <- mean((pred - y1)^2)

      # library(PPtreereg)
      A <- try(PPTreereg(y ~ ., data = data.frame(X, y = y)), silent = TRUE)
      if ("try-error" %in% class(A)) {
        A <- ODT(X, y, split = "mse", NodeRotateFun = "RotMatRand")
      }
      pred <- predict(A, data.frame(X1))
      e.PPT <- mean((pred - y1)^2)

      # calculate RPE
      e.0 <- mean((y1 - mean(y))^2)
      tree <- c(e.Tree, e.extraTree, e.evTree, e.cTree, e.ODT.Rand, e.ODT.SPROT, e.PPT, e.ODT)
      e <- tree / e.0

      E[ico - (chunks[[co]][1] - 1), ] <- e
    }

    # return local results
    E
  }

  ###############################################################################

  OUT <- c(i.data, N, p, colMeans(Err))

  tree <- c("RPE.CART", "RPE.ERT", "RPE.EVT", "RPE.CT", "RPE.RotT", "RPE.SPORT", "e.PPT", "RPE.ODT")
  names(OUT) <- c("i.data", "n", "p", tree)
  # print(OUT, 4.4)

  # write.table(t(OUT), file= "./result/ODT_Regr_Error2_1.csv",
  #            sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))
  ret <- rbind(ret, OUT)
  rm(Err)
  cnames <- names(OUT)
}

Error <- ret[, -seq(3)]

meanRPE <- c(rep(0, 3), colMeans(Error))
ret <- rbind(ret, meanRPE)
ret[, -seq(3)] <- round(ret[, -seq(3)], 3)

Total <- c(rep(0, 3), colSums(Error == matrix(apply(Error, 1, min), nrow = nrow(Error), ncol = ncol(Error))))
ret <- rbind(ret, Total)

print(ret)
# write.table(ret, file= "./result/ODT_Regr_Error2_1.csv",sep = ",",row.names = FALSE,col.names = TRUE)
save(ret, file = "./Results/Tree_Regression_Error_Results.rda")

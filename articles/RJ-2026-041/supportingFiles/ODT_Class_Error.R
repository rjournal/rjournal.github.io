library(foreach)
library(doParallel)
library(rpart)
library(ODRF)
library(PPtreeViz)
library(RLT)
library(evtree)
library(partykit)
library(oblique.tree)

source("./Datasets/ClassDataSets.R")

depend_packages <- c("rpart", "ODRF", "PPtreeViz", "RLT", "evtree", "partykit", "oblique.tree")

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

i.data <- 2
cnames <- c()
ret <- NULL
dataA <- seq(23)
for (i.data in dataA)
{
  # print(i.data)
  print(paste(i.data, length(dataA), sep = "/"))
  data <- Data.Set(i.data, wd = "./Datasets/Classification")
  X0 <- as.matrix(data$X)
  y0 <- as.factor(data$y)

  v <- which(apply(X0, 2, sd) > 1.0e-10)
  X0 <- X0[, v]
  p <- ncol(X0)
  N <- length(y0)

  rm(data)
  sn <- min(floor(N * 2 / 3), 1000)
  ico <- 1
  Err <- foreach(co = seq(length(chunks)), .combine = "rbind", .packages = depend_packages) %dopar%
    # for (ico in 1:Rep)
    {
      E <- matrix(1, nrow = length(chunks[[co]]), ncol = 9)

      for (ico in chunks[[co]]) {
        set.seed(ico)

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

        # y0=y
        # c=mean(unique(y0))
        # y=as.factor(y)
        level <- levels(y)
        # y1=as.character(y1)
        ##############################################################
        # Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = attr(object,  :
        # A new level 38, 76 appears in factor v1 when i.data=18 and ico=1
        A <- rpart(y ~ ., method = "class", data = data.frame(X, y = y))
        pred <- predict(A, data.frame(X1), type = "class") # factor
        e.Tree <- mean(pred != y1)

        A <- ODT(X, y, split = "gini") # entropy
        # A = online(A,data.frame(X1,y=y1))
        # A = prune(A,data.frame(X1,y=y1))
        pred <- predict(A, X1)
        e.ODT <- mean(pred != y1)

        A <- ODT(X, y, split = "gini", NodeRotateFun = "RotMatRand")
        pred <- predict(A, X1)
        e.ODT.SPROT <- mean(pred != y1)

        A <- ODT(X, y, split = "gini", NodeRotateFun = "RotMatRF", TreeRandRotate = TRUE)
        pred <- predict(A, X1)
        e.ODT.Rand <- mean(pred != y1)

        # Error in if (sign != sign(a$projbest[index])) a$projbest <- -a$projbest :
        # the condition has length > 1 ,when i.data=9 and ico=24.
        A <- PPTreeclass(y ~ ., data = data.frame(X, y = y), "LDA")
        pred <- predict(A, data.frame(X1))
        pred <- as.character(pred)
        e.ppTree <- mean(pred != y1)

        # library(obliqueRF)
        # library(oblique.tree)
        A <- oblique.tree(y ~ ., data = data.frame(X, y = y), oblique.splits = "on")
        pred <- predict(A, newdata = data.frame(X1, y = y1), type = "class")
        e.obliqueTree <- mean(pred != y1)

        A <- RLT(X, y, model = "classification", ntrees = 1)
        pred <- predict(A, X1)$Prediction
        if (length(level) > 2) {
          pred <- level[pred + 1]
        }
        e.extraTree <- mean(pred != y1)


        # library(evtree)
        # i.data=3, 10, 19 and ico=20.
        # Error in h(simpleError(msg, call)) :
        # "error in evaluating the argument 'x' in selecting a method for function 'mean': level sets of factors are different"
        A <- evtree(y ~ ., data = data.frame(X, y = y), control = evtree.control(ntrees = 10, niterations = 1000L))
        pred <- predict(A, data.frame(X1))
        evTree <- mean(pred != y1)

        # library(partykit)
        A <- ctree(y ~ ., data = data.frame(X, y = y))
        pred <- predict(A, data.frame(X1))
        cTree <- mean(pred != y1)

        e <- c(e.Tree, e.extraTree, evTree, cTree, e.ODT.Rand, e.ODT.SPROT, e.ppTree, e.obliqueTree, e.ODT)

        E[ico - (chunks[[co]][1] - 1), ] <- e
      }

      # return local results
      E
    }

  ###############################################################################

  OUT <- c(i.data, N, p, nlevels(y0), colMeans(Err))
  tree <- c("MR.CART", "MR.ERT", "MR.EVT", "MR.CT", "MR.RotT", "MR.SPORT", "MR.PPT", "MR.OT", "MR.ODT")
  names(OUT) <- c("i.data", "n", "p", "C", tree)
  # print(OUT, 4.4)

  # write.table(t(OUT), file= "./result/ODT_Class_Error2_1.csv",
  #            sep = ",",append=TRUE,row.names = FALSE,col.names = is.null(cnames))#col.names = TRUE)#
  ret <- rbind(ret, OUT)
  rm(Err)
  cnames <- names(OUT)
}

Error <- ret[, -seq(4)]

meanMR <- c(rep(0, 4), colMeans(Error))
ret <- rbind(ret, meanMR)
ret[, -seq(4)] <- round(ret[, -seq(4)] * 100, 2)

Total <- c(rep(0, 4), colSums(Error == matrix(apply(Error, 1, min), nrow = nrow(Error), ncol = ncol(Error))))
ret <- rbind(ret, Total)

print(ret)
# write.table(ret, file= "./result/ODT_Class_Error2_1.csv",sep = ",",row.names = FALSE,col.names = TRUE)
save(ret, file = "./Results/Tree_Classification_Error_Results.rda")

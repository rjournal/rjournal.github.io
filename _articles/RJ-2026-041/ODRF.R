## Replication script for "ODRF: An R Package for Oblique Decision Tree
## and Its Random Forest" submitted to the R Journal.
#
# This document ('ODRF.R') contains the replication script for
# "ODRF: An R Package for Oblique Decision Tree and Its Random Forest" for
# consideration of publication in the R Journal.
# Following the R Journal author guidelines, the replication script contains the exact code
# used to reproduce the calculation in the manuscript. You have three ways to
# install the R package 'ODRF', see preliminaries section of this document for details.


## Data and scripts used in the manuscript
#
# Data and scripts used to create the manuscript are available in a
# Github project 'RJ_ODRF_Codes' that can be download from link
# https://github.com/liuyu-star/RJ_ODRF_Codes. The scripts can
# alternatively be downloaded from the online RJ submission.


## Data download
#
# Data sets used in the manuscript are too large be submitted directly. However,
# you can access them by downloading the "Datasets" folder from
#     https://github.com/liuyu-star/RJ_ODRF_Codes.
# To do so, please ensure that you are in the current working directory of R and
# then proceed with the download. Of course you can also follow our way to get
# the data automatically without downloading it manually.


## Attention
#
# Before proceeding with the Section 4 (Real examples) of the manuscript,
# this file 'ODRF.R' and folder 'supportingFiles', must be placed under the R workspace (getwd()).
# If you manually download the 'Datasets' folder from the Github project 'RJ_ODRF_Codes',
# you should also put it under the R workspace.

###################################################
### code chunk number 1: preliminaries
###################################################
# options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
# Section 3, Section 4 and Section 5.1
if (!require("rpart", quietly = T)) {
  install.packages("rpart", quiet = TRUE)
}
library(rpart)

# install 'ODRF'
if (!require("ODRF", quietly = T)) {
  # First way: Install via offline package.
  # if(!require("ODRF",quietly = T)){
  #  if(!require("doParallel",quietly = T)){
  #    install.packages("doParallel", quiet = TRUE)
  #  }
  #  if(!require("foreach",quietly = T)){
  #    install.packages("foreach", quiet = TRUE)
  #  }
  #  if(!require("Pursuit",quietly = T)){
  #    install.packages("Pursuit", quiet = TRUE)
  #  }
  #  if(!require("RcppArmadillo",quietly = T)){
  #    install.packages("RcppArmadillo", quiet = TRUE)
  #  }
  #  if(!require("partykit",quietly = T)){
  #    install.packages("partykit", quiet = TRUE)
  #  }
  #  install.packages("./supportingFiles/ODRF_0.0.5.tar.gz", repos = NULL, type = "source")
  # }
  # Second way: Install via Github.
  # install.packages("devtools")
  # devtools::install_github("liuyu-star/ODRF")
  # Third way: Install via CRAN.
  install.packages("ODRF")
}
library(ODRF)

if (!require("randomForest", quietly = T)) {
  install.packages("randomForest", quiet = TRUE)
}
library(randomForest)
if (!require("glmnet", quietly = T)) {
  install.packages("glmnet", quiet = TRUE)
}
library(glmnet)
if (!require("plotmo", quietly = T)) {
  install.packages("plotmo", quiet = TRUE)
}
library(plotmo)
library(MASS)


# Section 4.2 (Table 2)
if (!require("doParallel", quietly = T)) {
  install.packages("doParallel", quiet = TRUE)
}
if (!require("RLT", quietly = T)) {
  install.packages("RLT", quiet = TRUE)
}
if (!require("evtree", quietly = T)) {
  install.packages("evtree", quiet = TRUE)
}
# install 'PPtreereg'
if (!require("PPtreereg", quietly = T)) {
  if (!require("devtools", quietly = T)) {
    install.packages("devtools")
  }
  devtools::install_github("EK-Lee/PPtreereg")
}
# install 'oblique.tree'
if (!require("oblique.tree", quietly = T)) {
  if (!require("tree", quietly = T)) {
    install.packages("tree", quiet = TRUE)
  }
  install.packages("./supportingFiles/oblique.tree_1.1.1.tar.gz", repos = NULL, type = "source")
}

# Section 4.2 (Table 3)
if (!require("PPtreeViz", quietly = T)) {
  install.packages("PPtreeViz")
}

# Section 4.2 (Tree method results in Table 4)
if (!require("rotationForest", quietly = T)) {
  install.packages("rotationForest")
}
# install 'rerf'
if (!require("rerf", quietly = T)) {
  if (!require("Rcpp", quietly = T)) {
    install.packages("Rcpp", quiet = TRUE)
  }
  if (!require("RcppZiggurat", quietly = T)) {
    install.packages("RcppZiggurat", quiet = TRUE)
  }
  if (!require("mclust", quietly = T)) {
    install.packages("mclust", quiet = TRUE)
  }
  if (!require("utils", quietly = T)) {
    install.packages("utils", quiet = TRUE)
  }
  install.packages("./supportingFiles/dummies_1.5.6.tar.gz", repos = NULL, type = "source")
  install.packages("./supportingFiles/rerf_2.0.4.tar.gz", repos = NULL, type = "source")
}

# Section 4.2 (Forest method results in Table 4)
if (!require("xgboost", quietly = T)) {
  install.packages("xgboost", quiet = TRUE)
}
if (!require("grf", quietly = T)) {
  install.packages("grf", quiet = TRUE)
}
if (!require("PPforest", quietly = T)) {
  install.packages("PPforest", quiet = TRUE)
}
if (!require("aorsf", quietly = T)) {
  install.packages("aorsf", quiet = TRUE)
}
# install 'obliqueRF'
if (!require("obliqueRF", quietly = T)) {
  if (!require("ROCR", quietly = T)) {
    install.packages("ROCR", quiet = TRUE)
  }
  if (!require("pls", quietly = T)) {
    install.packages("pls", quiet = TRUE)
  }
  if (!require("mda", quietly = T)) {
    install.packages("mda", quiet = TRUE)
  }
  if (!require("e1071", quietly = T)) {
    install.packages("e1071", quiet = TRUE)
  }
  install.packages("./supportingFiles/obliqueRF_0.3.tar.gz", repos = NULL, type = "source")
}

# library("spsurvey.manuscript") # not required if data downloaded from online JSS submission
#'
#'
#' We will also load a reproducible seed by running.
#'
## ------------------------------------------------------------------------------------------------
# set.seed(2)
#'
#'
#' A raw replication script (with a `.R` extension) was also uploaded in the submission alongside the R Markdown replication script (with a `.Rmd` extension) to provide an easier way to run code interactively. This R script was generated using `knitr::purl(, documentation = 2)`. Next we provide code used to build the manuscript section-by-section.
#'
#' # 1. Introduction
#'
#' # 2. Statistical  methods
#'
#' There was no code in this two section (outside of installing and loading ODRF).

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
#' # 3. Overview of ODRF functions


#' ## 3.1. print the tree structure of ODT and ODRF
#'
###################################################
### code chunk number 2: print tree
###################################################
set.seed(38)
data(iris, package = "datasets")
tree <- ODT(Species ~ ., data = iris)
print(tree)
party.tree <- as.party(tree, data = iris)
print(party.tree)


###################################################
### code chunk number 3: print forest
###################################################
set.seed(38)
forest <- ODRF(Species ~ ., data = iris, parallel = FALSE)
print(forest)


###################################################
### code chunk number 4: creat linear model trees (LMT) with LASSO regularization
###################################################
set.seed(10)
cutpoint <- 50
mu <- rep(0, 100)
X <- matrix(rnorm(100 * 10), 100, 10)
age <- sample(seq(20, 80), 100, replace = TRUE)
height <- sample(seq(50, 200), 100, replace = TRUE)
weight <- sample(seq(5, 150), 100, replace = TRUE)
Z <- cbind(age = age, height = height, weight = weight)
mu[age <= cutpoint] <- X[age <= cutpoint, 1] + X[age <= cutpoint, 2]
mu[age > cutpoint] <- X[age > cutpoint, 1] + X[age > cutpoint, 3]
y <- mu + rnorm(100)
my.tree <- ODT(
  X = X, y = y, Xsplit = Z, split = "linear", lambda = 0, NodeRotateFun =
    "RotMatRF", glmnetParList = list(lambda = 0, family = "gaussian")
)
pred <- predict(my.tree, X, Xsplit = Z)
mean((pred - y)^2)

#' ## 3.2. Classification and regression using ODT and ODRF
#'
###################################################
### code chunk number 5: classification and regression
###################################################
data(body_fat, package = "ODRF")
train <- sample(1:252, 200)
bodyfat_train <- data.frame(body_fat[train, ])
bodyfat_test <- data.frame(body_fat[-train, ])
tree <- ODT(Density ~ ., bodyfat_train, split = "mse")
pred <- predict(tree, bodyfat_test[, -1])
(e.tree <- mean((pred - bodyfat_test[, 1])^2))
set.seed(12)
data(seeds, package = "ODRF")
train <- sample(1:209, 150)
seeds_train <- data.frame(seeds[train, ])
seeds_test <- data.frame(seeds[-train, ])
forest <- ODRF(varieties_of_wheat ~ ., seeds_train,
  split = "gini", parallel = FALSE
)
pred <- predict(forest, seeds_test[, -8])
(e.forest <- mean(pred != seeds_test[, 8]))
forest <- ODBT(varieties_of_wheat ~ ., seeds_train, seeds_test[, -8],
  model = "rpart",
  type = "class", max.terms = 10, parallel = FALSE, NodeRotateFun = "RotMatRF"
)
pred <- forest$results$prediction
(mean(pred != seeds_test[, 8]))


#' ## 3.3. Online updating
#'
###################################################
### code chunk number 6: online
###################################################
set.seed(17)
index <- sample(nrow(seeds_train), floor(nrow(seeds_train) / 2))
forest1 <- ODRF(varieties_of_wheat ~ ., seeds_train[index, ],
  split = "gini", parallel = FALSE
)
pred <- predict(forest1, seeds_test[, -8])
(e.forest.1 <- mean(pred != seeds_test[, 8]))
forest2 <- online(forest1, seeds_train[-index, -8], seeds_train[-index, 8])
pred <- predict(forest2, seeds_test[, -8])
(e.forest.online <- mean(pred != seeds_test[, 8]))
index <- seq(floor(nrow(bodyfat_train) / 2))
tree1 <- ODT(Density ~ ., bodyfat_train[index, ], split = "mse")
pred <- predict(tree1, bodyfat_test[, -1])
(e.tree.1 <- mean((pred - bodyfat_test[, 1])^2))
tree2 <- online(tree1, bodyfat_train[-index, -1], bodyfat_train[-index, 1])
pred <- predict(tree2, bodyfat_test[, -1])
(e.tree.online <- mean((pred - bodyfat_test[, 1])^2))



## ------------------------------------------------------------------------------------------------
#' ## 3.4. Visualization of ODT and ODRF and importance of variables
#'
#' plot the tree structure
###################################################
### code chunk number 7: plot the tree structure (eval = FALSE)
###################################################
## set.seed(0308)
## tree <- ODT(Species ~ ., data = iris, split = "gini")
## plot(tree, main = "")
## party.tree <- as.party(tree, data = iris)
## plot(party.tree)


# Figure 2(a) in the manuscript
###################################################
### code chunk number 8: plot the tree structure of class ODT
###################################################
set.seed(0308)
tree <- ODT(Species ~ ., data = iris, split = "gini")
plot(tree, main = "")


# Figure 2(b) in the manuscript
###################################################
### code chunk number 9: plot the tree structure of class part
###################################################
party.tree <- as.party(tree, data = iris)
plot(party.tree)



# Plot the error graph and dotchart of variable importance with respect to ODRF.
###################################################
### code chunk number 10: plot the ODRF graph (eval = FALSE)
###################################################
## set.seed(3)
## data(breast_cancer, package = "ODRF")
## train <- sample(1:569, 300)
## train_data <- breast_cancer[train, -1]
## test_data <- breast_cancer[-train, -1]
## forest <- ODRF(diagnosis ~ ., train_data,split = "gini", parallel = FALSE)
## error <- Accuracy(forest, train_data, test_data)
## plot(error)
## varimp <- VarImp(forest, train_data[, -1], train_data[, 1])
## plot(varimp, nvar = 10)


# Figure 3(a) in the manuscript
###################################################
### code chunk number 10: plot the error graph of ODRF
###################################################
set.seed(3)
data(breast_cancer, package = "ODRF")
train <- sample(1:569, 300)
train_data <- breast_cancer[train, -1]
test_data <- breast_cancer[-train, -1]
forest <- ODRF(diagnosis ~ ., train_data, split = "gini", parallel = FALSE)
error <- Accuracy(forest, train_data, test_data)
plot(error)


# Figure 3(b) in the manuscript
###################################################
### code chunk number 11: plot the dotchart of variable importance about ODRF
###################################################
set.seed(3)
varimp <- VarImp(forest, train_data[, -1], train_data[, 1])
plot(varimp, nvar = 10)



## ------------------------------------------------------------------------------------------------
#' # 3.5. Create a rotation matrix with RotMat* and user-defined functions
#'
#' We default to RotMatPPO with the argument model = "PPR"
###################################################
### code chunk number 12: RotMatPPO
###################################################
set.seed(14)
X <- matrix(rnorm(1000), 200, 5)
y <- (X[, 1] + X[, 2])^2 + X[, 4] - X[, 5] + runif(200)
tree <- ODT(X, y,
  split = "mse", NodeRotateFun = "RotMatPPO",
  paramList = list(model = "PPR", dimProj = 5, numProj = 1)
)
round(tree[["projections"]], 2)


###################################################
### code chunk number 13: Define projection matrix function makeRotMat
###################################################
makeRotMat <- function(dimX, dimProj, numProj, ...) {
  RotMat <- matrix(1, dimProj * numProj, 3)
  for (np in seq(numProj)) {
    RotMat[(dimProj * (np - 1) + 1):(dimProj * np), 1] <-
      sample(1:dimX, dimProj, replace = FALSE)
    RotMat[(dimProj * (np - 1) + 1):(dimProj * np), 2] <- np
    RotMat[(dimProj * (np - 1) + 1):(dimProj * np), 3] <-
      sample(c(1L, -1L), dimProj, replace = TRUE, prob = c(0.5, 0.5))
  }
  return(RotMat)
}
set.seed(35)
RotMat1 <- makeRotMat(dimX = 5, dimProj = 3, numProj = 2)
RotMat1


###################################################
### code chunk number 14: Define projection pursuit function makePP
###################################################
makePP <- function(X, y, ...) {
  LM <- lm(y ~ ., data = data.frame(X, y = y))
  theta <- as.matrix(LM[["coefficients"]])[-1, , drop = FALSE]
  theta <- theta / sqrt(sum(theta^2))
  return(theta)
}
set.seed(35)
RotMat3 <- RotMatMake(
  X = X, y = y, RotMatFun = "makeRotMat",
  PPFun = "makePP", paramList = list(dimX = 5, dimProj = 3, numProj = 2)
)
RotMat3

###################################################
### code chunk number 15: Train ODT with RotMatMake
###################################################
set.seed(23)
tree <- ODT(X, y,
  split = "mse", NodeRotateFun = "RotMatMake",
  paramList = list(
    RotMatFun = "makeRotMat", PPFun = "makePP",
    dimX = 5, dimProj = 5, numProj = 1
  )
)
round(tree[["projections"]], 2)



## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
#' # 4. Real examples
#'
## ------------------------------------------------------------------------------------------------
#' ## 4.1. Two data sets for complexity analysis
#'
#' ### The first data is the kyphosis data
###################################################
### code chunk number 16: Fitting error of kyphosis
###################################################
data(kyphosis, package = "rpart")
odt <- ODT(Kyphosis ~ Age + Number + Start,
  data = kyphosis,
  split = "gini", paramList = list(model = "PPR", numProj = 1)
)
tree <- rpart(Kyphosis ~ Age + Number + Start,
  data = kyphosis,
  method = "class"
)
pred <- predict(odt, kyphosis[, -1])
e.odt <- mean(pred != kyphosis[, 1])
pred <- predict(tree, kyphosis, type = "class")
e.tree <- mean(pred != kyphosis[, 1])
print(c(e.odt = e.odt, e.tree = e.tree))
print(round(odt[["projections"]], 3))


# Figure 4(a) in the manuscript
###################################################
### code chunk number 17: The ODT tree structure of kyphosis
###################################################
plot(odt, main = "")


# Figure 4(b) in the manuscript
###################################################
### code chunk number 18: The rpart tree structure of kyphosis
###################################################
tree.party <- as.party(tree)
plot(tree.party)


#' ### The second data is the cpus data
#'
# Figure 5 in the manuscript
###################################################
### code chunk number 19: The ODT tree structure of cpus
###################################################
data(cpus, package = "MASS")
odt <- ODT(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax,
  data = cpus, lambda = log(nrow(cpus)), split = "mse",
  paramList = list(model = "PPR", numProj = 1)
)
plot(odt, main = "")


# Figure 6 in the manuscript
###################################################
### code chunk number 20: The rpart tree structure of cpus
###################################################
tree <- rpart(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax,
  data = cpus
)
tree.party <- as.party(tree)
plot(tree.party)


###################################################
### code chunk number 21: Fitting error of cpus
###################################################
pred <- predict(odt, cpus[, 2:7])
e.odt <- mean((pred - log10(cpus[, 8]))^2)
pred <- predict(tree, cpus)
e.tree <- mean((pred - log10(cpus[, 8]))^2)
print(c(e.odt = e.odt, e.tree = e.tree))
print(round(odt[["projections"]], 3))


## ------------------------------------------------------------------------------------------------
#' ## 4.2. The importance of variables analysis
#'
###################################################
### code chunk number 22: plot the importance of variables for mtcars with LASSO
###################################################
set.seed(0306)
data(mtcars, package = "datasets")
X <- as.matrix(mtcars[-1])
y <- mtcars[, 1]
fit <- glmnet(X, y, family = "gaussian")
cvfit <- cv.glmnet(X, y, family = "gaussian")
fit$beta <- sign(fit$beta) * sqrt(abs(fit$beta))
plot_glmnet(fit, label = TRUE, s = cvfit$lambda.1se, main = c())


###################################################
### code chunk number 23: plot the importance of variables for mtcars with RF
###################################################
set.seed(0306)
rf <- randomForest(X, y, keep.forest = FALSE, importance = TRUE)
varImpPlot(rf, type = 1, main = c())


###################################################
### code chunk number 24: plot the importance of variables for mtcars with ODRF
###################################################
set.seed(0306)
odrf <- ODRF(X, y, ntrees = 500)
varimp <- VarImp(odrf, X, y)
plot(varimp, nvar = 10, digits = 0, main = "")


## ------------------------------------------------------------------------------------------------
#' ## 4.3. Prediction Performance
#'
## ENTER DESTINATION PATH FOR DATA DOWNLOADS.
wd0 <- getwd()
wd <- wd0
if (!file.exists("Datasets")) {
  path_codes <- file.path(wd0, "RJ_ODRF_Codes")
  if (!file.exists(path_codes)) {
    dir.create(path_codes)
  }

  filename <- file.path(path_codes, "RJ_ODRF_Codes.zip")
  files_url <- "https://github.com/liuyu-star/RJ_ODRF_Codes/archive/refs/heads/main.zip"
  download.file(files_url, destfile = filename)

  unzip(filename, exdir = path_codes)
  wd <- paste0(path_codes, "/RJ_ODRF_Codes-main")

  ## Remove other files.
  unlink(
    c(
      paste0(path_codes, "/RJ_ODRF_Codes.zip"),
      file.path(wd, setdiff(list.files(wd), "Datasets"))
    ),
    recursive = T
  )
}

setwd(wd)
if (!file.exists("Results")) {
  dir.create("Results")
}
#'
#' To generate a LaTeX table from R script ODT_Regr_Error.R (Table 2 in the manuscript), run
#'
source(paste0(wd0, "/supportingFiles/ODT_Regr_Error.R"))
# load("./Results/Tree_Regression_Error_Results.rda")
# View(ret)

#' To generate a LaTeX table from R script ODT_Class_Error.R (Table 3 in the manuscript), run
#'
source(paste0(wd0, "/supportingFiles/ODT_Class_Error.R"))
# load("./Results/Tree_Classification_Error_Results.rda")
# View(ret)

#' To generate a LaTeX table from R script ODT_Class_Comparison.R (Classification results using the tree methods in Table 4), run
#'
source(paste0(wd0, "/supportingFiles/ODT_Class_Comparison.R"))
# load("./Results/Tree_Classification_Comparison_Summaries.rda")
# View(MR)
# View(Time)
# View(Complexity)

#' To generate a LaTeX table from R script ODT_Regr_Comparison.R (Regression results using the tree methods in Table 4), run
#'
source(paste0(wd0, "/supportingFiles/ODT_Regr_Comparison.R"))
# load("./Results/Tree_Regression_Comparison_Summaries.rda")
# View(RPE)
# View(Time)
# View(Complexity)

#' To generate a LaTeX table from R script ODRF_Class_Comparison.R (Classification results using the forest methods in Table 4), run
#'
source(paste0(wd0, "/supportingFiles/ODRF_Class_Comparison.R"))
# load("./Results/Forest_Classification_Comparison_Summaries.rda")
# View(MR)
# View(Time)

#' To generate a LaTeX table from R script ODRF_Regr_Comparison.R (Regression results using the forest methods in Table 4), run
#'
source(paste0(wd0, "/supportingFiles/ODRF_Regr_Comparison.R"))
# load("./Results/Forest_Regression_Comparison_Summaries.rda")
# View(RPE)
# View(Time)


## remove downloaded files
unlink(file.path(wd, "Datasets"), recursive = T)

## -----------------------------------------------------------------------------
## Replication code accompanying the paper "The biglasso Package: A Memory-
## and Computation-Efficient Solver for Lasso Model Fitting with Big Data in R"
## Authors: Yaohui Zeng and Patrick Breheny
##
## benchmarking platform:
##    MacBook Pro with Intel Core i7 @ 2.3 GHz and 16 GB RAM.
##
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Section 5: data analysis example

rm(list = ls())
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(glmnet)
library(biglasso)

data(colon)
X <- colon$X
y <- colon$y
dim(X)
X[1:5, 1:5]
y

## convert X to a big.matrix object
X.bm <- as.big.matrix(X)
str(X.bm)
dim(X.bm)
X.bm[1:5, 1:5] 
## same results as X[1:5, 1:5]

## fit a logistic regression model
fit <- biglasso(X.bm, y, family = "binomial") 
plot(fit)

## 10-fold cross-valiation in parallel
cvfit <- cv.biglasso(X.bm, y, family = "binomial",
                     seed = 1234, nfolds = 10, ncores = 4)
png(filename = "jss_data_example_cv.png", width = 7, height = 6, units = "in", res = 120)
par(mfrow = c(2, 2), mar = c(3.5, 3.5, 3, 1) ,mgp = c(2.5, 0.5, 0))
plot(cvfit, type = "all")
dev.off()

png(filename = "jss_data_example.png", width = 7, height = 6, units = "in", res = 120)
plot(cvfit$fit)
abline(v = log(cvfit$lambda.min), col = 2, lty = 2)
dev.off()

## coefs
coefs <- as.matrix(coef(cvfit))
coefs[coefs != 0, ]

## predict
as.vector(predict(cvfit, X = X.bm, type = "class"))
predict(cvfit, type = "nvars")
predict(cvfit, type = "vars")

## summary
summary(cvfit)


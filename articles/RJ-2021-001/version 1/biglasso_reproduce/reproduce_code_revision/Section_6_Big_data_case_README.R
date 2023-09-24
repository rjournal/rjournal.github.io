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
## Section 6: big data case

# To replicate this part, need first simulate large-scale data set.
#
# 1. Simulate data using C program in file: Section_6_dataGenerate_logistic.c at
#   command line:
#   - compile: gcc Section_6_dataGenerate_logistic.c -o datagen -lm -std=c99
#   - run: ./datagen 3000 1340000 200
#
# This yields to two txt files:
#   - Feature data X: X_3000_1340000_200_gwas.txt
#   - Response y    : y_3000_1340000_200_gwas.txt
# 
# 2. Generate memory-mapped files
library(biglasso)
X <- setupX(filename = "X_3000_1340000_200_gwas.txt")
#
# This yields to two files:
#   - X_3000_1340000_200_gwas.bin
#   - X_3000_1340000_200_gwas.desc

## Model fitting
rm(list = ls())
gc()
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(biglasso)
## ------------------------------------------------------------------------------
## biglasso, gaussian
## ------------------------------------------------------------------------------
cat("\nStart Linear regression case: ", format(Sys.time()), "\n")
gc()
cat("\n\n")

xfname <- 'X_3000_1340000_200_gwas.desc'
yfname <- 'y_3000_1340000_200_gwas.txt'

X <- attach.big.matrix(xfname)
str(X)
dim(X)
X[1:10, 1:10]
y <- as.matrix(read.table(yfname, header = F))
head(y)
table(y)

cat("Start time: ", format(Sys.time()), "\n")
time <- system.time(
  fit.bm <- biglasso(X = X, y = y, family = "gaussian", screen = "SSR-BEDPP",
                     ncores = 4)
)
cat("End time: ", format(Sys.time()), "\n")
pdf("linear_case.pdf")
plot(fit.bm)
dev.off()
print(time)
save(fit.bm, file = "linear_case.Rdata")
cat("\nEnd Linear regression case: ", format(Sys.time()), "\n")

## ------------------------------------------------------------------------------
## biglasso, binomial
## ------------------------------------------------------------------------------

rm(list = ls())
gc()
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library(biglasso)

cat("\nStart logistic regression case: ", format(Sys.time()), "\n")
gc()
cat("\n\n")
xfname <- 'X_3000_1340000_200_gwas.desc'
yfname <- 'y_3000_1340000_200_gwas.txt'

X <- attach.big.matrix(xfname)
dim(X)
X[1:10, 1:10]
y <- as.matrix(read.table(yfname, header = F))
head(y)
table(y)

cat("Start time: ", format(Sys.time()), "\n")
time <- system.time(
  fit.bm <- biglasso(X = X, y = y, family = "binomial", screen = "SSR-Slores",
                     ncores = 4)
)
cat("End time: ", format(Sys.time()), "\n")
pdf("logistic_case.pdf")
plot(fit.bm)
dev.off()
print(time)
save(fit.bm, file = "logistic_case.Rdata")

## result analysis
#------------------------------------------------------------------------------
load("linear_case.Rdata")
fit <- fit.bm
coefs <- as.matrix(coef(fit, lambda = 0.04))
coefs[coefs != 0, ]
predict(fit, lambda = 0.04, type = "nvars")

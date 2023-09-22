remove(list = ls())
gc()

require(biglasso)
# Assume current working directory set to be folder "biglasso_reproduce/"
# setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-1_memory_efficiency/")
x.bm <- attach.big.matrix("x_e3_e5.desc")
y <- as.matrix(read.table("y_e3_e5.txt", header = F))

set.seed(1234)
lambda.min <- 0.05
eps <- 1e-6

cat("cv.biglasso start: ", format(Sys.time()), "\n\n")
cvfit.bedpp1 <- cv.biglasso(x.bm, y, family = 'gaussian', 
                            nfolds = 10,
                            seed = 1234,
                            screen = "SSR-BEDPP", 
                            safe.thresh = 0, ncores = 1,
                            lambda.min = lambda.min,
                            eps = eps)
cat("cv.biglasso end: ", format(Sys.time()), "\n\n")

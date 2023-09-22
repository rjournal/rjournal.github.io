remove(list = ls())
gc()

require(picasso)
load("x_e3_e5.RData")
y <- as.matrix(read.table("y_e3_e5.txt", header = F))

set.seed(1234)
lambda.min <- 0.05
eps <- 1e-6

cat("picasso start: ", format(Sys.time()), "\n\n")
fit.pic <- picasso(x, y, family = 'gaussian', 
                  lambda.min.ratio = lambda.min,
                  prec = eps)
cat("picasso end: ", format(Sys.time()), "\n\n")


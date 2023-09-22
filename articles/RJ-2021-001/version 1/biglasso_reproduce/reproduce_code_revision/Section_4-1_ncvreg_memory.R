remove(list = ls())
gc()

require(ncvreg)
load("x_e3_e5.RData")
y <- as.matrix(read.table("y_e3_e5.txt", header = F))

set.seed(1234)
lambda.min <- 0.05
eps <- 1e-6

cat("ncvreg start: ", format(Sys.time()), "\n\n")
fit.ncv <- ncvreg(x, y, family = 'gaussian', 
                  penalty = 'lasso',
                  lambda.min = lambda.min,
                  eps = sqrt(eps))
cat("ncvreg end: ", format(Sys.time()), "\n\n")
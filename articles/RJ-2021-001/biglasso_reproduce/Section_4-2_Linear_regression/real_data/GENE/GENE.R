rm(list = ls())
require(biglasso)
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)
require(microbenchmark)

# Assume current working directory set to be folder "biglasso_reproduce/"
# Change to this directory if not.
setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
load("./Section_4-2_Linear_regression/real_data/GENE/bcTCGA.RData")

x <- X
p <- ncol(x)
n <- nrow(x)
x.bm <- as.big.matrix(x, type = 'double')
date <- Sys.Date()

# --------------------------------------------------------------
## benchmark timings
# --------------------------------------------------------------
lambda.log <- F
eps <- 1e-6
safe.thresh <- 0
lam.min <- 0.1
fit.hsr0 <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR', 
                     lambda.log.scale = lambda.log, lambda.min = lam.min,
                     eps = eps)
cat("eps = ", eps, "; safe.thresh = ", safe.thresh, "; lambda.log = ", lambda.log)
t <- microbenchmark(
  fit.glm <- glmnet(x, y, family = 'gaussian', 
                    lambda = fit.hsr0$lambda,
                    lambda.min.ratio = lam.min, 
                    thresh = eps),
  fit.ncv <- ncvreg(x, y, penalty = 'lasso', 
                    lambda = fit.hsr0$lambda,
                    family = 'gaussian', 
                    eps = sqrt(eps)),
  fit.pic <- picasso(x, y, prec = eps, 
                     lambda = fit.hsr0$lambda
                     ),
  fit.hsr.bedpp <- biglasso(x.bm, y, family = 'gaussian', screen = "SSR-BEDPP", 
                            # lambda.log.scale = lambda.log, 
                            lambda.min = lam.min,
                            lambda = fit.hsr0$lambda,
                            safe.thresh = safe.thresh,
                            eps = eps),
  times = 20,
  control = list(order = 'inorder')
)

method <- c('glmnet', 'ncvreg', 'picasso', 'biglasso')
t.mat <- matrix(t$time, nrow = length(method), byrow = FALSE) / 1e9 # nanosecond to second
time.mean <- apply(t.mat, 1, mean, na.rm = TRUE)
time.se <- apply(t.mat, 1, function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
})

t.df <- data.frame(
  method = method,
  time.mean = time.mean,
  time.se = time.se
)
print(t.df)


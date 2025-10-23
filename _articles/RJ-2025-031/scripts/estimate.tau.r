#
# Compare tau estmated with the method by Bickel et al. for different
# sitings of the parameter beta, testing mean, max, xicor with n = 100, 500
# (Table 2 in the article)
#

library("moonboot")
source("utils.r")

set.seed(100)
file.name <- "estimate-tau.csv"
N <- 100

# list of upper and lower bounds to create beta sequence
beta.list <- list(c(0.2, 0.5), c(0.4, 0.8))
estimators <- c("mean", "max", "xicor")

# write header to file
cat("model n method beta.lower beta.upper estimated.tau\n",file = file.name, append = FALSE)

for (estimator in estimators) {
  model <- get.model(estimator)
  for (n in c(100, 500)) { # testing n = 100 & 500
    for (beta.bounds in beta.list) { # selecting beta bounds
      beta <- seq(beta.bounds[1], beta.bounds[2], length.out = 5)

      # estimating tau using selected beta
      res <- lapply(1:N, function(x) {
        data <- model$generate(n)
        estimated.tau.variance <- estimate.tau(data, model$statistic, R = 1000, beta = beta)
        estimated.tau.quantile <- estimate.tau(data, model$statistic, R = 1000, beta = beta, method = "quantile")
        return(c(log(estimated.tau.variance(100),100),log(estimated.tau.quantile(100),100)))
      })

      # merge the results
      taus.variance <- sapply(res, function(x) x[1])
      taus.quantile <-sapply(res, function(x) x[2])
      variance.mean <- mean(unlist(taus.variance))
      quantile.mean <- mean(unlist(taus.quantile))
      tau.mean <- mean(unlist(res))

      # write to file
      cat(model$name, n, "variance", beta.bounds[1], beta.bounds[2], variance.mean,"\n", file = file.name, append = TRUE)
      cat(model$name, n, "quantile", beta.bounds[1], beta.bounds[2], quantile.mean,"\n", file = file.name, append = TRUE)
    }
  }
}

# estimation of tau with the method by Bickel et al. for inconsistent estimators
# discussed in section 5 of the article


library("moonboot")
source("utils.r")

set.seed(100)
file.name <- "estimate-tau-inconsistent.csv"
N <- 100
start <- Sys.time()
# list of upper and lower bounds to create gamma sequence
gamma.list <- list(c(0.2, 0.5), c(0.4, 0.8))
estimators <- c("mean.first", "poisson")

# write header to file
cat("model n method gamma.lower gamma.upper estimated.tau\n",file = file.name, append = FALSE)

for (estimator in estimators) {
  model <- get.model(estimator)
  for (n in c(100, 500)) { # testing n = 100 & 500
    for (gamma.bounds in gamma.list) { # selecting gamma bounds
      gamma <- seq(gamma.bounds[1], gamma.bounds[2], length.out = 5)

      # estimating tau using selected gamma
      res <- lapply(1:N, function(x) {
        data <- model$generate(n)
        estimated.tau.variance <- estimate.tau(data, model$statistic, R = 1000, FALSE, 3, gamma)
        estimated.tau.quantile <- estimate.tau(data, model$statistic, R = 1000, FALSE, 3, gamma, method = "quantile")
        return(c(log(estimated.tau.variance(100),100),log(estimated.tau.quantile(100),100)))
      })

      # merge the results
      taus.variance <- sapply(res, function(x) x[1])
      taus.quantile <-sapply(res, function(x) x[2])
      variance.mean <- mean(unlist(taus.variance))
      quantile.mean <- mean(unlist(taus.quantile))
      tau.mean <- mean(unlist(res))

      # write to file
      cat(model$name, n, "variance", gamma.bounds[1], gamma.bounds[2], variance.mean,"\n", file = file.name, append = TRUE)
      cat(model$name, n, "quantile", gamma.bounds[1], gamma.bounds[2], quantile.mean,"\n", file = file.name, append = TRUE)
    }
  }
}

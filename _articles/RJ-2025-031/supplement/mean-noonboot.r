#
# Comparison of ordinary n-out-of-n bootstrap and m-out-of-n bootstrap
# for the mean of an unsymmetric distribution
# (generates file mean-noonboot.csv  used in Figure 5 in the article)
#

#
# IMPORTANT: Will run some time
#            => do not run on a laptop or desktop computer
#
stop("This script would likely run for over an hour.")

library(boot)
library(parallel)
source("utils.r")

# number of cores
mc.cores <- 5 # was set to 40, setting to 5 to run on local hardware
# number of bootstrap simulations
N <- 10^4
n.max <- 5000
ns <- ceiling(10^seq(log10(100), log10(n.max), length.out = 20))


is.inside <- function(x, low, up) {
  return(x <= up && x >= low)
}

# simulate the n-out-of-n bootstrap
simulate.noon <- function(model, ns, file = NULL, mc.cores = NULL, conf = 0.95, N = NULL, R = 1000) {
  # write header to file
  if (!is.null(file)) {
    cat("n", "m.method", "m.mean","p.cov", "p.length", sep = " ", file = file)
    cat("\n", file = file, append = TRUE)
  }

  for (n in ns) {
    help.function <- function(core) {
      res <- simulate.noon.single(model = model, n = n, N = ceiling(N / mc.cores), conf = conf, R = R)
    }

    if (mc.cores == 1)
      results <- lapply(1:mc.cores, FUN = help.function)
    else
      results <- mclapply(1:mc.cores, FUN = help.function, mc.cores = mc.cores)

    # merge the results of the different cores
    merged.result <- sapply(1:length(results[[1]]), function(i) mean(sapply(results, function(x) x[i])))

    # write results to file
    if (hasArg(file)) {
      print(n) # current status
      n <- merged.result[1]
      basic <- merged.result[2]
      basic.length <- merged.result[3]

      cat(n, "n-out-of-n", n, basic, basic.length, sep = " ", file = file, append = TRUE)
      cat("\n", file = file, append = TRUE)
    }
  }
}

# helper method
simulate.noon.single <- function(model, n, N, conf = 0.95, R = 1000) {
  # basic coverage & interval length
  basic <- 0
  basic.length <- 0
  real.value <- model$real.value

  for (k in 1:N) {
    # simulating the bootstrap
    boot.out <- boot(data = model$generate(n), statistic = model$statistic, R = R)
    ci <- boot.ci(boot.out, conf = conf, type = "basic")

    # intervals calculated using basic method
    theta1 <- ci$basic[4]
    theta2 <- ci$basic[5]
    basic <- basic + is.inside(real.value, theta1, theta2)
    basic.length <- basic.length + (theta2 - theta1)
  }


  basic <- basic / N
  basic.length <- basic.length / N
  ret <- c(n, basic, basic.length)
  return(ret)
}


start.time <- Sys.time()
simulate.noon(get.model("mean"), ns, file = "mean-noonboot.csv", mc.cores = mc.cores, N = N, R = 1000)
print(Sys.time() - start.time)

#
# Evaluation of dirrent data based methods for choosing m
# in the m-out-of-n bootstrap
# (generates the file pcov-databased-m.csv used in Figure 4 in the article)
#

#
# IMPORTANT: Ran 8 weeks on a computer with 40 cores
#            => do not run on a laptop or desktop computer
#
stop("This script would run for several weeks.")

# creates the file .
library(moonboot)
library(parallel)
source("utils.r")

# number of cores
mc.cores <- 5  # was set to 40, setting to 5 to run on local hardware
# number of bootstrap simulations
N <- 10^4
n.max <- 5000
ns <- ceiling(10^seq(log10(100), log10(n.max), length.out = 20))
file.name <- "pcov-databased-m.csv"

models <- c("max", "mean", "shorth", "xicor")

goetze.method <- c(name = "goetze", value = \(data, statistic, tau, replace, goetze.interval) estimate.m(data, statistic = statistic, tau = tau, replace = replace, method = "goetze", params = list(goetze.interval = goetze.interval)))
bickel.method <- c(name = "bickel", value = \(data, statistic, tau, replace) estimate.m(data, statistic = statistic, tau = tau, replace = replace, method = "bickel", params = list(q = 0.75)))
politis.method <- c(name = "politis", value = \(data, statistic, tau, replace) estimate.m(data, statistic = statistic, tau = tau, replace = replace, method = "politis", params = list(h.ci = 3, h.sigma = 3)))


simulate.moon <- function(model, replace, ns, N, m.method, mc.cores = 1, conf = 0.95, file.name = NULL) {
  for (n in ns) {
    N.per.core <- ceiling(N / mc.cores)
    real.value <- model$real.value
    tau <- model$tau

    # determine goetze search interval which is then used for all threads
    if (substring(m.method$name, 1, 6) == "goetze") {
      # run 100 times goetze to get search interval
      goetze.choosen.ms <- numeric()
      search.amount <- 100
      goetze.res <- mclapply(1:search.amount, function(core) {
        data <- model$generate(n)
        m <- round(m.method$value(data = data, statistic = model$statistic, tau = tau, replace = replace, goetze.interval = c(2, n)))
        return(m)
      }, mc.cores = mc.cores)
      goetze.choosen.ms <- unlist(goetze.res)

      # limiting the search space
      goetze.interval <- round(c(max(4, mean(goetze.choosen.ms) - 2 * sd(goetze.choosen.ms)), min(n, mean(goetze.choosen.ms) + 2 * sd(goetze.choosen.ms))))
      goetze.interval <- round(c(min(goetze.interval[1], min(goetze.choosen.ms)), max(goetze.interval[2], max(goetze.choosen.ms))))
      print("goetze search interval: ")
      print(goetze.interval)
    }


    # simulating the bootstrap using multiple cores
    res <- mclapply(1:mc.cores, function(core) {
      m.means <- 0
      basic.coverage <- 0
      basic.length <- 0
      m.values <- c()

      for (N.i in 1:N.per.core) {
        data <- model$generate(n)
        if (substring(m.method$name, 1, 6) == "goetze") { # passing the search interval for goetze method
          m.value <- round(m.method$value(data = data, statistic = model$statistic, tau = tau, replace = replace,
                                          goetze.interval = goetze.interval))
        }else { # calling the wrapper function to estimate m
          m.value <- round(m.method$value(data = data, statistic = model$statistic, tau = tau, replace = replace))
        }

        # update m
        m.means <- m.means + m.value
        m.values <- c(m.values, m.value)

        # simulating the m-out-of-n bootstrap using m.value as subsample size
        bout <- mboot(data, statistic = model$statistic, m = m.value, R = 1000, replace = replace)
        ci <- mboot.ci(bout, conf = conf, tau = model$tau, types = c("basic"))

        ci.basic <- ci$basic
        basic.coverage <- basic.coverage + ifelse(real.value >= ci.basic[1] && real.value <= ci.basic[2], 1, 0)
        basic.length <- basic.length + ci.basic[2] - ci.basic[1]
      }
      return(list(
        m.means = m.means,
        basic.coverage = basic.coverage,
        basic.length = basic.length,
        m.values = m.values
      ))
    }, mc.cores = mc.cores)


    # Merge results
    m.means <- numeric()
    basic.coverage <- 0
    basic.length <- 0
    N.rounded <- N.per.core * mc.cores

    mean.merged <- Reduce("+", lapply(res, function(x) {
      x$m.means
    })) / N.rounded
    basic.coverage.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.coverage
    })) / N.rounded
    basic.length.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.length
    })) / N.rounded

    # build a list of all m.values to calculate sd
    merged.m.values <- Reduce(function(x, y) c(x, y), lapply(res, function(x) {
      x$m.values
    }))

    # write to file
    if (!is.null(file.name) & !is.na(file.name)) {
      mean.m <- mean.merged
      basic.coverage <- basic.coverage.merged
      basic.length <- basic.length.merged
      sd.m <- sd(merged.m.values)
      tau.method <- "real"
      tau.variance <- 0 # in this simulations the real value of tau was used
      tau.value <- log(model$tau(100), 100)

      cat(sprintf("\"%s\" %d \"%s\" %f \"%s\" %f %f %f %f %f \n",
                  model$name, n, m.method$name, mean.m,
                  tau.method, tau.value, basic.coverage, basic.length,
                  tau.variance, sd.m), append = TRUE, file = file.name)
    }
  }
}


# write header to file
cat("\"estimator\" \"n\" \"m.method\" \"m.mean\" \"tau.method\" \"tau.mean\" \"p.cov\" \"p.length\" \"sd.tau\" \"sd.m\"\n", file = file.name, append = FALSE)


for (model.name in models) {
  model <- get.model(model.name)
  simulate.moon(model, replace = FALSE, ns = ns, N = N, bickel.method, conf = 0.95, mc.cores = mc.cores, file.name)
  simulate.moon(model, replace = FALSE, ns = ns, N = N, goetze.method, conf = 0.95, mc.cores = mc.cores, file.name)
  simulate.moon(model, replace = FALSE, ns = ns, N = N, politis.method, conf = 0.95, mc.cores = mc.cores, file.name)
}
print(Sys.time() - start)


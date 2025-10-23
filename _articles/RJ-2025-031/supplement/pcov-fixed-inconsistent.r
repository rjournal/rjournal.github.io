# Estimates the coverage probability for inconsistent estimators
# generates pcov-fixed-inconsistent.csv discussed in section 5

#
# IMPORTANT: Will run some time
#            => do not run on a laptop or desktop computer
#
stop("This script would run for many hours.")

library(parallel)
library(moonboot)
source("utils.r")

file.name <- "pcov-fixed-m-inconsistent.csv"
mc.cores <- 5 # was set to 40, setting to 5 to run on local hardware
N <- 10^4
n.max <- 5000
ns <- ceiling(10^seq(log10(100), log10(n.max), length.out = 20))
m.methods <- list(c(name = "1/2", value = \(x) sqrt(x)), c(name = "1/3", value = \(x)x^(1 / 3)), c(name = "3/4", value = \(x) x^(3 / 4)))

simulate.moon.fixed <- function(model, replace, ns, N, m.methods, mc.cores = 1, conf = 0.95, file.name = NULL) {
  for (n in ns) {
    N.per.core <- ceiling(N / mc.cores)
    model.real.p <- model$real.value
    tau <- model$tau

    res <- mclapply(1:mc.cores, function(core) {
      basic.coverage <- numeric(length(m.methods))
      basic.length <- numeric(length(m.methods))

      for (N.i in 1:N.per.core) {
        data <- model$generate(n)
        for (m.method.index in 1:length(m.methods)) {
          # calculate m value to be used
          beta <- m.methods[[m.method.index]]
          m.value <- beta$value(n)
          m.value <- round(m.value)

          # calculating basic interval using moonboot
          bout <- mboot(data, statistic = model$statistic, m = m.value, R = 1000, replace = replace)
          ci <- mboot.ci(bout, conf = conf, tau = model$tau, types = c("basic"))

          ci.basic <- ci$basic
          basic.coverage[m.method.index] <- basic.coverage[m.method.index] + ifelse(model.real.p >= ci.basic[1] && model.real.p <= ci.basic[2], 1, 0)
          basic.length[m.method.index] <- basic.length[m.method.index] + ci.basic[2] - ci.basic[1]
        }
      }
      return(list(
        basic.coverage = basic.coverage,
        basic.length = basic.length
      ))
    }, mc.cores = mc.cores)

    # Merge results
    basic.coverage <- numeric(length(m.methods))
    basic.length <- numeric(length(m.methods))
    # The actual number of simulations may be slightly higher than N
    N.rounded <- N.per.core * mc.cores

    basic.coverage.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.coverage
    })) / N.rounded
    basic.length.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.length
    })) / N.rounded

    # write to file
    if (!is.null(file.name) & !is.na(file.name)) {
      for (m.method.index in 1:length(m.methods)) {
        beta <- m.methods[[m.method.index]]$name
        m.value <- round(m.methods[[m.method.index]]$value(n))
        basic.coverage <- basic.coverage.merged[m.method.index]
        basic.length <- basic.length.merged[m.method.index]

        cat(sprintf("%s %d %s %d %f %f \n",
                    model$name, n, beta, m.value, basic.coverage,
                    basic.length), append = TRUE, file = file.name)
      }
    }
  }
}


# write header to file
cat("estimator n beta m p.cov p.length\n", file = file.name, append = FALSE)


# mean.first
simulate.moon.fixed(get.model("mean.first"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
                    file.name = file.name, mc.cores = mc.cores)

# poisson
simulate.moon.fixed(get.model("poisson"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
                    file.name = file.name, mc.cores = mc.cores)


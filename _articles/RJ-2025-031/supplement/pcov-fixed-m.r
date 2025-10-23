#
# Estimation of coverage probability for different choices of m
# for the mean of an unsymmetric distribution
# (generates file pcov-fixed.csv  used in Figure 3 in the article)
#

#
# IMPORTANT: Will run some time
#            => do not run on a laptop or desktop computer
#
stop("This script would run for many hours.")

library(parallel)
library(moonboot)
source("utils.r")

file.name = "pcov-fixed-m.csv"
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
      norm.coverage <- numeric(length(m.methods))
      basic.length <- numeric(length(m.methods))
      norm.length <- numeric(length(m.methods))

      for (N.i in 1:N.per.core) {
        data <- model$generate(n)
        for (m.method.index in 1:length(m.methods)) {
          # calculate m value to be used
          beta <- m.methods[[m.method.index]]
          m.value <- beta$value(n)
          m.value <- round(m.value)

          # calculating basic and norm interval using moonboot
          bout <- mboot(data, statistic = model$statistic, m = m.value, R = 1000, replace = replace)
          ci <- mboot.ci(bout, conf = conf, tau = model$tau, types = c("basic", "norm"))

          ci.basic <- ci$basic
          ci.norm <- ci$norm
          basic.coverage[m.method.index] <- basic.coverage[m.method.index] + ifelse(model.real.p >= ci.basic[1] && model.real.p <= ci.basic[2], 1, 0)
          norm.coverage[m.method.index] <- norm.coverage[m.method.index] + ifelse(model.real.p >= ci.norm[1] && model.real.p <= ci.norm[2], 1, 0)
          basic.length[m.method.index] <- basic.length[m.method.index] + ci.basic[2] - ci.basic[1]
          norm.length[m.method.index] <- norm.length[m.method.index] + ci.norm[2] - ci.norm[1]
        }
      }
      return(list(
        basic.coverage = basic.coverage,
        norm.coverage = norm.coverage,
        basic.length = basic.length,
        norm.length = norm.length
      ))
    }, mc.cores = mc.cores)

    # Merge results
    basic.coverage <- numeric(length(m.methods))
    norm.coverage <- numeric(length(m.methods))
    basic.length <- numeric(length(m.methods))
    norm.length <- numeric(length(m.methods))
    # The actual number of simulations may be slightly higher than N
    N.rounded <- N.per.core * mc.cores

    basic.coverage.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.coverage
    })) / N.rounded
    norm.coverage.merged <- Reduce("+", lapply(res, function(x) {
      x$norm.coverage
    })) / N.rounded
    basic.length.merged <- Reduce("+", lapply(res, function(x) {
      x$basic.length
    })) / N.rounded
    norm.length.merged <- Reduce("+", lapply(res, function(x) {
      x$norm.length
    })) / N.rounded

    # write to file
    if (!is.null(file.name) & !is.na(file.name)) {
      for (m.method.index in 1:length(m.methods)) {
        beta <- m.methods[[m.method.index]]$name
        m.value <- round(m.methods[[m.method.index]]$value(n))
        basic.coverage <- basic.coverage.merged[m.method.index]
        norm.coverage <- norm.coverage.merged[m.method.index]
        basic.length <- basic.length.merged[m.method.index]
        norm.length <- norm.length.merged[m.method.index]

        cat(sprintf("%s %d %s %d %f %f %f %f \n",
                    model$name, n, beta, m.value, basic.coverage,
                    norm.coverage, basic.length, norm.length), append = TRUE, file = file.name)
      }
    }
  }
}


# write header to file
cat("estimator n beta m p.cov n.cov p.length n.length\n", file = file.name, append = FALSE)


# mean
simulate.moon.fixed(get.model("mean"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
              file.name = file.name, mc.cores = mc.cores)

# max
simulate.moon.fixed(get.model("max"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
              file.name = file.name, mc.cores = mc.cores)

# shorth
simulate.moon.fixed(get.model("shorth"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
              file.name = file.name, mc.cores = mc.cores)

# xicor
simulate.moon.fixed(get.model("xicor"), replace = FALSE, ns = ns, N = N, m.methods = m.methods,
              file.name = file.name, mc.cores = mc.cores)


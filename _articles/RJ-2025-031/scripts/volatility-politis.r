#
# Compute volatility index after Politis et al. (1999) for shorth and xicor
# as a function of m
# (writes the data file used for Figure 1 in the article)
#

library(moonboot)
source("utils.r")

set.seed(65)
file.name <- "volatility-politis.csv"

# splitted estimate.m.politis into two methods to reuse bootstrap distribution
estimate.politis.a <- function(data, statistic, tau, R, replace = FALSE, min.m = 2, ...) {
  conf <- 0.95 # default value when calling estimate.m.politis
  n <- NROW(data)

  m.values <- seq(min.m, n, length.out = 50)
  m.values <- unique(round(m.values))

  bouts <- lapply(m.values, function(m) mboot(data, statistic, R, m = m, replace = replace, ...))
  cis <- lapply(bouts, function(bout) mboot.ci(bout, conf = conf, tau = tau, c("basic"))$basic)

  return(list(cis = cis, m.values = m.values, n = n))
}

estimate.m.politis.b <- function(politis.a, h.ci, h.sigma,model.name, file, add = FALSE, ...) {
  cis <- politis.a$cis
  m.values <- politis.a$m.values
  n <- politis.a$n
  # 2. as approximation was used, smooth lower and upper endpoints seperately
  # using a running mean of span rmean.size
  rmean.size <- h.ci * 2 + 1 # both sides
  cis.upper <- lapply(cis, function(x) x[1])
  cis.lower <- lapply(cis, function(x) x[2])
  smoothed.cis.upper <- filter(cis.upper, sides = 2, rep(1 / rmean.size, rmean.size))
  smoothed.cis.lower <- filter(cis.lower, sides = 2, rep(1 / rmean.size, rmean.size))
  # NA: edge cases - calculate them manually using a (slow) loop
  first.na <- 1:h.ci
  last.na <- (length(smoothed.cis.lower) - h.ci + 1):length(smoothed.cis.lower)
  # For na values: Calculate mean with given values
  # no mirroring is used as this could negativly impact the variance calculation
  for (i in 1:h.ci) {
    # first.na[i] = i => using i instead
    smoothed.cis.lower[i] <- mean(unlist(cis.lower[1:(h.ci + i)]))
    smoothed.cis.upper[i] <- mean(unlist(cis.upper[1:(h.ci + i)]))

    smoothed.cis.upper[last.na[i]] <- mean(unlist(cis.upper[(last.na[i] - h.ci):length(smoothed.cis.lower)]))
    smoothed.cis.lower[last.na[i]] <- mean(unlist(cis.lower[(last.na[i] - h.ci):length(smoothed.cis.lower)]))
  }

  VI.upper <- c()
  VI.lower <- c()
  VI <- c()

  # 3. for each m compute a volatility Index as standard deviation in their neighborhood of m
  for (i in 1:length(smoothed.cis.upper)) {
    # check if edge cases
    if (i - h.sigma < 1 || i + h.sigma > length(smoothed.cis.upper)) {
      VI[i] <- Inf # ignoring edge cases
      next
    }

    # sd of neighboorhood m-k to m+k
    VI.upper[i] <- sd(smoothed.cis.upper[max(1, i - h.sigma):min(length(smoothed.cis.upper), i + h.sigma)])
    VI.lower[i] <- sd(smoothed.cis.lower[max(1, i - h.sigma):min(length(smoothed.cis.lower), i + h.sigma)])
    VI[i] <- VI.upper[i] + VI.lower[i]
  }
  # Pick the value m corresponding to the smallest VI as m.star
  m.star <- m.values[which.min(VI)]
  m.values <- m.values[!is.infinite(VI)] # inf is used to mark edge cases
  VI <- VI[!is.infinite(VI)]
  return(list(VI = VI, m.values = m.values, m.star = m.star))
}

#
# write header to file
#

cat("\"estimator\"", "\"h.ci\"", "\"h.sigma\"", "\"VI\"","\"m.value\"", "\"m.star\"\n", file = file.name, append = FALSE, sep = ",")

# shorth h = 2
model <- get.model("shorth")
h <- 2
data <- model$generate(1000)

cis <- estimate.politis.a(data, model$statistic, model$tau, R = 1000, replace = FALSE, min.m = 2)
res <- estimate.m.politis.b(cis, h.ci = h, h.sigma = h)

# write to file
for(m.index in 1:length(res$m.values)){
  cat(model$name, h, h, res$VI[m.index], res$m.values[m.index], res$m.star, file = file.name, append = TRUE, sep = ",")
  cat("\n", file = file.name, append = TRUE, sep = ",")
}

# shorth h = 3
h <- 3
res <- estimate.m.politis.b(cis, h.ci = h, h.sigma = h, model.name = model$name, add = TRUE)

# write to file
for(m.index in 1:length(res$m.values)){
  cat(model$name, h, h, res$VI[m.index], res$m.values[m.index], res$m.star, file = file.name, append = TRUE, sep = ",")
  cat("\n", file = file.name, append = TRUE, sep = ",")
}

# xicor h = 2
set.seed(100)
model <- get.model("xicor")
h <- 2

data <- model$generate(1000)
cis <- estimate.politis.a(data, model$statistic, model$tau, R = 1000, replace = FALSE, min.m = 2)
res <- estimate.m.politis.b(cis, h.ci = h, h.sigma = h)

# write to file
for(m.index in 1:length(res$m.values)){
  cat(model$name, h, h, res$VI[m.index], res$m.values[m.index], res$m.star, file = file.name, append = TRUE, sep = ",")
  cat("\n", file = file.name, append = TRUE, sep = ",")
}

# xicor h = 3
h <- 3
res <- estimate.m.politis.b(cis, h.ci = h, h.sigma = h, model.name = model$name, add = TRUE)

# write to file
for(m.index in 1:length(res$m.values)){
  cat(model$name, h, h, res$VI[m.index], res$m.values[m.index], res$m.star, file = file.name, append = TRUE, sep = ",")
  cat("\n", file = file.name, append = TRUE, sep = ",")
}

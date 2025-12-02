#
# Plot distance after Goetze & Rackauskas (2001)
# should have a minimum somewehre in between
# (Figure 2 in the article)
#

# statistic: minimum of uniform distribution
statistic <- function(indices, data) {
  max(data[indices])
}

tau <- function(n) {n}

# plot distance as function of m
plot.dist.m <- function(x, stat.n, R=1000, smooth=3, ...)
{
  if (is.null(dim(x)))
    n <- length(x)
  else n <- dim(x)[1]

  computedist <- function(m) {
    indices <- replicate(R, sample(1:n, size = m, replace = F))
    res <- apply(indices, MAR=2, statistic, data=x, ...)
    res <- res[!is.na(res)]
    res <- tau(m) * (res - stat.n)
    m2 <- ceiling(m/2)
    indices2 <- replicate(R, sample(1:n, size = m2, replace = F))
    if (is.null(dim(indices2))) indices2 <- matrix(indices2, nrow=1)
    res2 <- apply(indices2, MAR=2, statistic, data=x, ...)
    res2 <- res2[!is.na(res2)]
    res2 <- tau(m2) * (res2 - stat.n)
    F <- ecdf(res)
    F2 <- ecdf(res2)
    allres <- c(res, res2)
    distances <- max(abs(F(allres) - F2(allres)))
    return (distances)
  }

  ms <- 2*(1:floor(n/4))
  distances <- numeric(length(ms))
  for (i in 1:length(ms)) {
    distances[i] <- mean(replicate(smooth, computedist(ms[i])))
  }
  plot(ms, distances, ylab="KS distance", xlab="m")
  #cat("Minimum at m =", ms[which.min(distances)], "\n")
}


n <- 200

set.seed(25)
dat <- runif(n)
stat.n <- statistic(1:n, dat)

# plot without smoothing
pdf(file="goetze-max-n200-nosmooth.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))
plot.dist.m(dat, stat.n, smooth=1)
dev.off()

# plot with smoothing
pdf(file="goetze-max-n200-smooth.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))
plot.dist.m(dat, stat.n, smooth=5)
dev.off()

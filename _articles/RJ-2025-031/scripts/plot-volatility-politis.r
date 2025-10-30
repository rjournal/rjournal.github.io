#
# Plot of volatility index after Politis et al. (1999) for shorth and xicor
# using the data file written by volatility-politis.r
# (Figure 1 in article)
#

#
# shorth
#
file <- "volatility-politis.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
result.df <- read.csv(file)
m.values <- 1:1000#range(result.df$m.value) # (1:n)
h.values = c(2, 3)

est <- "shorth"
pdf(file=paste("volatility-", est, ".pdf", sep=""), width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

x <- subset(result.df, estimator == est)

colors <- c(2, 4)
current.color <- 1
plot(c(2, 3), xlim = range(m.values), ylim = range(x$VI[is.finite(x$VI)]), xlab = "m", ylab = "volatility index")
for (h in h.values) {
  current.indices <- x$h.ci == h & x$h.sigma == h
  current.m.values <- x$m.value[current.indices]
  current.VI <- x$VI[current.indices]
  lines(current.m.values, current.VI, col = colors[current.color])
  m.star <- x$m.star[current.indices]
  abline(v = m.star, col = colors[current.color], lty = 2)
  current.color <- current.color + 1
}
legend("topleft", legend = paste0("h = ", h.values), col = colors, lty = 1)

dev.off()

#
# xicor
#

est <- "xicor"
pdf(file=paste("volatility-", est, ".pdf", sep=""), width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

x <- subset(result.df, estimator == est)

colors <- c(2, 4)
current.color <- 1
plot(c(2, 3), xlim = range(m.values), ylim = range(x$VI[is.finite(x$VI)]), xlab = "m", ylab = "volatility index")
for (h in h.values) {
  current.indices <- x$h.ci == h & x$h.sigma == h
  current.m.values <- x$m.value[current.indices]
  current.VI <- x$VI[current.indices]
  lines(current.m.values, current.VI, col = colors[current.color])
  m.star <- x$m.star[current.indices]
  abline(v = m.star, col = colors[current.color], lty = 2)
  current.color <- current.color + 1
}
legend("topright", legend = paste0("h = ", h.values), col = colors, lty = 1)

dev.off()

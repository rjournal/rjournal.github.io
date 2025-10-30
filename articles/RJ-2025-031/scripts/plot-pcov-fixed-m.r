#
# P_cov as a function of m for fixed m = n^beta
# (Figure 3 in article)
#

file <- "pcov-fixed-m.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
x <- read.table(file, header=T)

#
# plot for shorth
#
pdf("pcov-fixed-m-shorth.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

est <- "shorth"
y <- subset(x, estimator==est & beta=="1/2")
plot(y$n, y$p.cov, type="l", ylim=c(0.75,0.95),
     xlab="n", ylab=expression(P[cov]), log="x")
y <- subset(x, estimator==est & beta=="1/3")
lines(y$n, y$p.cov, col=2)
y <- subset(x, estimator==est & beta=="3/4")
lines(y$n, y$p.cov, col=4)
abline(h=0.95, lty=2)
legend("right", bty="n",
       c(expression(m == n^{1/2}),
         expression(m == n^{1/3}),
         expression(m == n^{3/4})), col=c(1,2,4), lty=1)
dev.off()

#
# plot for xicor
#
pdf("pcov-fixed-m-xicor.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

est <- "xicor"
y <- subset(x, estimator==est & beta=="1/2")
plot(y$n, y$p.cov, type="l", ylim=c(0.75,0.95),
     xlab="n", ylab=expression(P[cov]), log="x")
y <- subset(x, estimator==est & beta=="1/3")
lines(y$n, y$p.cov, col=2)
y <- subset(x, estimator==est & beta=="3/4")
lines(y$n, y$p.cov, col=4)
abline(h=0.95, lty=2)
legend("bottomright", bty="n",
       c(expression(m == n^{1/2}),
         expression(m == n^{1/3}),
         expression(m == n^{3/4})), col=c(1,2,4), lty=1)
dev.off()

#
# P_cov comparison m-out-of-n and ordinary basic bootstrap
# (Figure 5 in article)
#

# load data from different data files
file <- "mean-noonboot.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
x.noon <- read.table(file, header=T)
file <- "pcov-fixed-m.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
x.fixed <- subset(read.table(file, header=T), estimator=="mean")
file <- "pcov-databased-m.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
x.databased <- subset(read.table(file, header=T), estimator=="mean")


pdf("mean-comparison-pcov.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

y <- x.noon
plot(y$n, y$p.cov, type="l", ylim=c(0.9,0.951),
     xlab="n", ylab=expression(P[cov]), log="x")
y <- subset(x.fixed, beta=="1/3")
lines(y$n, y$p.cov, col=2)
#y <- subset(x.fixed, beta=="1/2")
#lines(y$n, y$p.cov, col=3)
y <- subset(x.databased, m.method=="bickel")
lines(y$n, y$p.cov, col=4)
abline(h=0.95, lty=2)
legend("bottomright", bty="n",
       c("n-out-of-n", expression(m == n^{1/3}), "m after Bickel"),
       col=c(1,2,4), lty=1)

dev.off()

pdf("mean-comparison-length.pdf", width=4, height=8/3)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

y <- x.noon
plot(y$n, y$p.length, type="l", 
     xlab="n", ylab="interval length", log="x")
y <- subset(x.fixed, beta=="1/3")
lines(y$n, y$p.length, col=2)
#y <- subset(x.fixed, beta=="1/2")
#lines(y$n, y$p.cov, col=3)
y <- subset(x.databased, m.method=="bickel")
lines(y$n, y$p.length, col=4)
abline(h=0.95, lty=2)
legend("topright", bty="n",
       c("n-out-of-n", expression(m == n^{1/3}), "m after Bickel"),
       col=c(1,2,4), lty=1)

dev.off()

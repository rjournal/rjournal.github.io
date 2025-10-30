#
# Plot P_cov and m(n) for data based choice of m
# (Figure 4 in article)
#

cols <- c(1,2,4)
width <- 4
height <- 4.2

file <- "pcov-databased-m.csv"
if (exists("dataprefix") && is.character(dataprefix)) {
  file <- paste(dataprefix, file, sep="")
} else {
  file <- paste("../data/", file, sep="")
}
y <- read.table(file, header=T)

#
# max
#
pdf("pcov-databased-m-max.pdf", width=width, height=height)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

par(mfrow=c(2,1))

#
# m(n)
#
x <- subset(y, estimator=="max" & m.method=="politis")
plot(x$n, x$m.mean, type="p", pch=16, ylim=c(2,800), xlim=c(100,5000),
     xlab="n", ylab="m", log="x", col=cols[1])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[1])
fit <- lm(log(m.mean) ~ log(n), x)
cat("beta for max:    Politis", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[1])

x <- subset(y, estimator=="max" & m.method=="bickel")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[2])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[2])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Bickel", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[2])

x <- subset(y, estimator=="max" & m.method=="goetze")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[3])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[3])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Goetze", coef(fit)[2], "\n")
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[3])

legend("topleft", bty="n",
       c(expression(paste("Politis: ", beta %~~% 1.0)),
         expression(paste("Bickel: ", beta %~~% 0.55)),
         expression(paste("Goetze: ", beta %~~% 0.16))), col=cols, lty=1)

#
# Pcov(n)
#
x <- subset(y, estimator=="max" & m.method=="politis")
plot(x$n, x$p.cov, type="l", ylim=c(0.7,0.96),
     xlab="n", ylab=expression(P[cov]), log="x", col=cols[1])
x <- subset(y, estimator=="max" & m.method=="bickel")
lines(x$n, x$p.cov, col=cols[2])
x <- subset(y, estimator=="max" & m.method=="goetze")
lines(x$n, x$p.cov, col=cols[3])
abline(h=0.95, lty=2)
legend("bottomright", c("Politis", "Bickel", "Goetze"), lty=1, col=cols, bty="n")

dev.off()


#
# shorth
#
pdf("pcov-databased-m-shorth.pdf", width=width, height=height)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

par(mfrow=c(2,1))

#
# m(n)
#
x <- subset(y, estimator=="shorth" & m.method=="politis")
plot(x$n, x$m.mean, type="p", pch=16, ylim=c(2,800), xlim=c(100,5000),
     xlab="n", ylab="m", log="x", col=cols[1])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[1])
fit <- lm(log(m.mean) ~ log(n), x)
cat("beta for shorth: Politis", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[1])

x <- subset(y, estimator=="shorth" & m.method=="bickel")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[2])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[2])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Bickel", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[2])

x <- subset(y, estimator=="shorth" & m.method=="goetze")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[3])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[3])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Goetze", coef(fit)[2], "\n")
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[3])

legend("topleft", bty="n",
       c(expression(paste("Politis: ", beta %~~% 1.0)),
         expression(paste("Bickel: ", beta %~~% 0.54)),
         expression(paste("Goetze: ", beta %~~% 0.58))), col=cols, lty=1)

#
# Pcov(n)
#
x <- subset(y, estimator=="shorth" & m.method=="politis")
plot(x$n, x$p.cov, type="l", ylim=c(0.7,0.96), xlim=c(100,5000),
     xlab="n", ylab=expression(P[cov]), log="x", col=cols[1])
x <- subset(y, estimator=="shorth" & m.method=="bickel")
lines(x$n, x$p.cov, col=cols[2])
x <- subset(y, estimator=="shorth" & m.method=="goetze")
lines(x$n, x$p.cov, col=cols[3])
abline(h=0.95, lty=2)
legend("right", c("Politis", "Bickel", "Goetze"), lty=1, col=cols, bty="n")

dev.off()


#
# xicor
#
pdf("pcov-databased-m-xicor.pdf", width=width, height=height)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

par(mfrow=c(2,1))

#
# m(n)
#
x <- subset(y, estimator=="xicor" & m.method=="politis")
plot(x$n, x$m.mean, type="p", pch=16, ylim=c(2,800), xlim=c(100,5000),
     xlab="n", ylab="m", log="x", col=cols[1])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[1])
fit <- lm(log(m.mean) ~ log(n), x)
cat("beta for xicor:  Politis", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[1])

x <- subset(y, estimator=="xicor" & m.method=="bickel")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[2])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[2])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Bickel", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[2])

x <- subset(y, estimator=="xicor" & m.method=="goetze")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[3])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[3])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Goetze", coef(fit)[2], "\n")
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[3])

legend("topleft", bty="n",
       c(expression(paste("Politis: ", beta %~~% 0.92)),
         expression(paste("Bickel: ", beta %~~% 0.60)),
         expression(paste("Goetze: ", beta %~~% 0.69))), col=cols, lty=1)

#
# Pcov(n)
#
x <- subset(y, estimator=="xicor" & m.method=="politis")
plot(x$n, x$p.cov, type="l", ylim=c(0.7,0.96), xlim=c(100,5000),
     xlab="n", ylab=expression(P[cov]), log="x", col=cols[1])
x <- subset(y, estimator=="xicor" & m.method=="bickel")
lines(x$n, x$p.cov, col=cols[2])
x <- subset(y, estimator=="xicor" & m.method=="goetze")
lines(x$n, x$p.cov, col=cols[3])
abline(h=0.95, lty=2)
legend("bottomright", c("Politis", "Bickel", "Goetze"), lty=1, col=cols, bty="n")

dev.off()


#
# mean
#
pdf("pcov-databased-m-mean.pdf", width=width, height=height)
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(1.5,0.5,0))

par(mfrow=c(2,1))

#
# m(n)
#
x <- subset(y, estimator=="mean" & m.method=="politis")
plot(x$n, x$m.mean, type="p", pch=16, ylim=c(2,800), xlim=c(100,5000),
     xlab="n", ylab="m", log="x", col=cols[1])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[1])
fit <- lm(log(m.mean) ~ log(n), x)
cat("beta for mean:   Politis", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[1])

x <- subset(y, estimator=="mean" & m.method=="bickel")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[2])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[2])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Bickel", coef(fit)[2])
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[2])

x <- subset(y, estimator=="mean" & m.method=="goetze")
lines(x$n, x$m.mean, type="p", pch=16, col=cols[3])
arrows(x$n, pmax(1,x$m.mean - x$sd.m), x$n, x$m.mean + x$sd.m, length=0.05, angle=90, code=3, col=cols[3])
fit <- lm(log(m.mean) ~ log(n), x)
cat(" Goetze", coef(fit)[2], "\n")
n <- seq(100,5000,10)
lines(n, exp(predict(fit, data.frame(n=n))), col=cols[3])

legend("topleft", bty="n",
       c(expression(paste("Politis: ", beta %~~% 0.98)),
         expression(paste("Bickel: ", beta %~~% 0.61)),
         expression(paste("Goetze: ", beta %~~% 0.86))), col=cols, lty=1)

#
# Pcov(n)
#
x <- subset(y, estimator=="mean" & m.method=="politis")
plot(x$n, x$p.cov, type="l", ylim=c(0.7,0.96),
     xlab="n", ylab=expression(P[cov]), log="x", col=cols[1])
x <- subset(y, estimator=="mean" & m.method=="bickel")
lines(x$n, x$p.cov, col=cols[2])
x <- subset(y, estimator=="mean" & m.method=="goetze")
lines(x$n, x$p.cov, col=cols[3])
abline(h=0.95, lty=2)
legend("bottomright", c("Politis", "Bickel", "Goetze"), lty=1, col=cols, bty="n")

dev.off()


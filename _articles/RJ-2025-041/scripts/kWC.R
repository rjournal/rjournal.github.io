rm(list=ls())
graphics.off()

library(elhmc)
library(plyr)
library(MASS)
library(magick)
library(ggplot2)
library(grid)
library(pdftools)

#source('../elhmc/R/ELHMC.R')
#source('../elhmc/R/ELU.R')
#source('../elhmc/R/HMC.R')
#source('../elhmc/R/Utils.R')



#Example 4.1

v <- rbind(c(1, 1), c(1, 0), c(1, -1), c(0, -1),
           c(-1, -1), c(-1, 0), c(-1, 1), c(0, 1))

g <- function(params, x) {
  params-x
}

dlg <- function(params, x) {
  rbind(c(1, 0), c(0, 1))
}

pr <- function(x) {
  -.5*(x[1]^2+x[2]^2)-log(2*pi)
}
dpr <- function(x) {
  -x
}

set.seed(476)
thetas <- ELHMC(initial = c(0.9, 0.95), data = v, fun = g, dfun = dlg,
                prior = pr, dprior = dpr, n.samples = 1000,
                lf.steps = 12, epsilon = 0.06, detailed = TRUE)

boxplot(thetas$samples,names = c(expression(theta[1]), expression(theta[2])))

q <- thetas$trajectory$trajectory.q[[1]]
par(pty="s")
plot(q, xlab = expression(theta[1]), ylab = expression(theta[2]),
     xlim = c(-1, 1), ylim = c(-1, 1), cex = 1, pch = 16)
points(v[,1],v[,2],type="p",cex=1.5,pch=16)
abline(h=-1);abline(h=1);abline(v=-1);abline(v=1)
arrows(q[-nrow(q), 1], q[-nrow(q), 2], q[-1, 1], q[-1, 2],
       length = 0.1, lwd = 1.5)

#Example 4.2

n <- rbind(c(5903,230),c(5157,350))
mat <- matrix(0,nrow=sum(n),ncol=2)
mat <- rbind(matrix(1,nrow=n[1,1],ncol=1)%*%c(0,0),
             matrix(1,nrow=n[1,2],ncol=1)%*%c(0,1),
             matrix(1,nrow=n[2,1],ncol=1)%*%c(1,0),
             matrix(1,nrow=n[2,2],ncol=1)%*%c(1,1))

#Specifying the population constraints.

gfr <- .06179*matrix(1,nrow=nrow(mat),ncol=1)
g <- matrix(1,nrow=nrow(mat),ncol=1)
amat <- matrix(mat[,2]*g-gfr,ncol=1)

data <- mat
g <- function(params, X) {
  result <- matrix(0, nrow = nrow(X), ncol = 3)
  a <- exp(params[1] + params[2] * X[, 1])
  a <- a / (1 + a)
  result[, 1] <- X[, 2] - a
  result[, 2] <- (X[, 2] - a) * X[, 1]
  result[, 3] <- X[, 2] - 0.06179
  result
}

dg <- function(params, X) {
  result <- array(0, c(3, 2, nrow(X)))
  a <- exp(params[1] + params[2] * X[, 1])
  a <- -a / (a + 1) ^ 2
  result[1, 1, ] <- a
  result[1, 2, ] <- result[1, 1, ] * X[, 1]
  result[2, 1, ] <- result[1, 2, ]
  result[2, 2, ] <- result[1, 2, ] * X[, 1]
  result[3, , ] <- 0
  result
}

pr <- function(x) {
  - 0.5*t(x)%*%x/10^4 - log(2*pi*10^4)
}

gpr <- function(x) {
  -x*10^(-4)
}

set.seed(1234)
bstart.init=c(-3.2,.55)
betas.init <- ELHMC(initial = bstart.init, data = data, FUN = g, DFUN = dg,
               n.samples = 50, prior = pr, dprior = gpr, epsilon = 0.001,
               lf.steps = 15, detailed = T, p.variance = 0.2,print.interval=10)

betas.init$acceptance.rate

bstart=betas.init$samples[50,]

betas <- ELHMC(initial = bstart, data = data, FUN = g, DFUN = dg,
               n.samples = 500, prior = pr, dprior = gpr, epsilon = 0.004,
               lf.steps = 30, detailed = F, p.variance = 0.2,print.interval=10,
		   plot.interval=1,which.plot=c(1,2))

betas$acceptance.rate
n.samp=nrow(betas$samples)

beta.density <- kde2d(betas$sample[round(n.samp/2):n.samp, 1], betas$samples[round(n.samp/2):n.samp, 2])

persp(beta.density, phi = 50, theta = 20, xlab = 'Intercept', ylab = '', zlab = 'Density', ticktype = 'detailed', cex.axis = 0.35, cex.lab = 0.35, d = 0.7)

dev.new()
par(mfrow=c(2,1))

acf(betas$sample[round(n.samp/2):n.samp, 1],main=expression(paste("Series ",beta[0])))
acf(betas$sample[round(n.samp/2):n.samp, 2],main=expression(paste("Series ",beta[1])))


dev.new()
par(mfrow=c(2,1))

plot(betas$sample[round(n.samp/2):n.samp, 1],main=expression(paste("Series ",beta[0])),type='l',ylab=" ")
plot(betas$sample[round(n.samp/2):n.samp, 2],main=expression(paste("Series ",beta[1])),type='l',ylab=" ")


dev.new()
image=image_read_pdf("contour.pdf")%>%image_trim()
beta.init=betas.init$samples
beta=betas$samples
plot(c(-3.5,-2.5),c(-.5,1.5),xlab=expression(beta[0]),ylab=expression(beta[1]),type='n')

rasterImage(image,-3.5,-.5,-2.5,1.5)

for(i in 1:(nrow(beta.init)-1)){
  points(beta.init[i,1],beta.init[i,2],cex=.5,pch=19,col=2)
  arrows(beta.init[i,1],beta.init[i,2],beta.init[(i+1),1],beta.init[(i+1),2],length=.075,lwd=1.75,col=2)
}

points(beta[,1],beta[,2],type='l',lwd=1.75)
points(beta[,1],beta[,2],type='p',cex=.5,pch=19)


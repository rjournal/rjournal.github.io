#rm(list=ls())
install.packages("NTS")
install.packages("tsDyn")
library(NTS)
library(tsDyn)
##################################################################
####Compare three functions, results are shown in table 2 in the paper

iter <- 200
n <- 200
thr <- 0.2
est <- matrix(0, iter, 3)
start.seed <- 1863

t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1,-1), sigma = c(1, 1))$series
	est1 <- uTAR(y = y, p1 = 2, p2 = 2, d = 2, thrQ = c(0, 1), include.mean = T, method = "RLS")
	est[i,1] <- est1$thr
}
t1 <- proc.time() - t0

t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1, -1), sigma = c(1, 1))$series
	est2 <- uTAR(y = y, p1 = 2, p2 = 2, d = 2, thrQ = c(0, 1), include.mean = T, method = "NeSS", k0 = 50)
	est[i,2] <- est2$thr
}
t2 <- proc.time() - t0


t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1,-1), sigma = c(1, 1))$series
	est3 <- setar(x = y, m = 2, thDelay = 1, trim = 0, trace = F)
	est[i, 3] <- est3$model.specific$coefficients[7]
}
t3 <- proc.time() - t0


print("Elapsed time for uTAR using recursive least squares method with sample size 200:")
print(t1)
print("Elapsed time for uTAR using NeSS algorithm with sample size 200:")
print(t2)
print("Elapsed time for tar with sample size 200:")
print(t3)
print("MSE for three methods:")
print((apply(abs(est - thr)^2, 2, mean)))



iter <- 200
n <- 2000
thr <- 0.2
est <- matrix(0, iter, 3)
t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1, -1), sigma = c(1, 1))$series
	est1 <- uTAR(y = y, p1 = 2, p2 = 2, d = 2, thrQ = c(0,1), include.mean = T, method = "RLS")
	est[i,1]<- est1$thr
}
t1 <- proc.time() - t0

t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1, -1), sigma = c(1, 1))$series
	est2 <- uTAR(y = y, p1 = 2, p2 = 2, d = 2, thrQ = c(0, 1), include.mean = T, method = "NeSS", k0 = 50)
	est[i,2] <- est2$thr
}
t2 <- proc.time() - t0


t0 <- proc.time()
for(i in 1:iter){
	set.seed(i+start.seed)
	y <- uTAR.sim(nob = n, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = thr, cnst = c(1,-1),sigma = c(1, 1))$series
	est3 <- setar(x = y, m = 2, thDelay = 1, trim = 0)
	est[i, 3] <- est3$model.specific$coefficients[7]
}
t3 <- proc.time() - t0


print("Elapsed time for uTAR using recurisive least squares method with sample size 2000:")
print(t1)
print("Elapsed time for uTAR using NeSS algorithm with sample size 2000:")
print(t2)
print("Elapsed time for tar with sample size 2000:")
print(t3)
print("MSE for three methods:")
print(apply(abs(est - thr)^2, 2, mean))



###############################################################################
### Example to show how to use uTAR.est, uTAR.pred, backTAR
set.seed(1687)
y <- uTAR.sim(nob = 2000, arorder = c(2, 2), phi = t(matrix(c(-0.3, 0.5, 0.6, -0.3), 2, 2)), d = 2, thr = 0.2, cnst = c(1, -1), sigma = c(1, 1))

###Figure 1 in the paper
#pdf("SETAR.pdf",height=4,width=8)
plot(y$series[1:200], type='l', xlab='Time', ylab='', main='Time series plot of a SETAR process')
#dev.off()
thr.est <- uTAR(y = y$series, p1 = 2, p2 = 2, d = 2, thrQ = c(0,1), Trim = c(0.1, 0.9), include.mean = T, method = "NeSS", k0 = 50)

thr.est <- uTAR(y = y$series, p1 = 2, p2 = 2, d = 2, thrQ = c(0,1), Trim = c(0.1, 0.9), include.mean = T, method = "RLS")
est <- uTAR.est(y = y$series, arorder = c(2, 2), thr = thr.est$thr, d = 2)
set.seed(12)
uTAR.pred(mode = est, orig = 2000, h = 1, iteration = 100, ci = 0.95, output = TRUE)
thr.test(y$series, p = 2, d = 2, ini = 40, include.mean = T)

set.seed(11)
backTAR(est, 50, 1, 3000)


thr.est2 <- uTAR(y = y$series, p1 = 2, p2 = 2, d = 1, thrQ = c(0, 1), Trim = c(0.1, 0.9), include.mean = T, method = "RLS")
est2 <- uTAR.est(y = y$series, arorder = c(2, 2), thr = thr.est2$thr, d = 1)
set.seed(11)
backTAR(est2, 50, 1, 3000)



##############################################################################
###ACMx
set.seed(12)
x <- rnorm(1000)*0.1
y <- matrix(0,1000, 1)
y[1] <- 2
lambda<- matrix(0,1000,1)
for (i in 2:1000){
	lambda[i] <- 2 + 0.2*y[i-1]/exp(x[i - 1]) + 0.5*lambda[i - 1]
	set.seed(i)
	y[i] <- rpois(1, exp(x[i])*lambda[i])
}
ACMx(y, order = c(1, 1), x, "po")

###############################################################################
#####CFAR
phi_func<- function(x)
{
	return(dnorm(x, mean = 0, sd = 0.1))
}
set.seed(101)
t <- 1000; N<- 50
x <- g_cfar(t, rho = 5, phi_func, sigma = 1, ini = 100)
f <- x$cfar[, seq(1, 1001, 1000/N)]
F_test_cfar(f, p.max = 2, df_b = 10, grid = 1000)
model <- est_cfar(f, p = 1, df_b = 10, grid = 1000)
print(c(model$rho, model$sigma))
pred <- p_cfar(model, f, m = 3)

num_obs <- rep(N+1, t); x_pos = matrix(rep(seq(0, 1, 1/N), each = t), t, N+1);
weight0 <- function(x){return(rep(1, length(x)))}
F_test_cfarh(f, weight0, p.max = 2, df_b = 10, grid = 1000, num_obs, x_pos)
modelh <- est_cfarh(f, weight0, p = 1, df_b = 10, grid = 1000, num_obs, x_pos)
print(c(modelh$rho, modelh$sigma))

#Figure 2 in the paper
#pdf("cfar.pdf",height=5,width=8)
plot(seq(-1, 1, 0.01), phi_func(seq(-1, 1, 0.01)), type = 'l', ylab = '', xlab = '')
points(seq(-1, 1, 1/1000), model$phi_func, type = 'l', lty = 2, col = 1)
legend.txt <- c("True", "Estimated")
legend("topright", legend.txt, lty=1:2, col=c(1, 1))
#dev.off()



#########################################################################
#####SMC

#####Generate the simulated data for SMC example
simPassiveSonar <- function(nn = 300, q, r, W, V, s2, start, seed){
  set.seed(seed)
  x <- matrix(nrow = 4, ncol = nn)
  y <- matrix(nrow = 2, ncol = nn)
  for(ii in 1:nn){
    if(ii == 1) x[, ii] <- start
    if(ii > 1) x[, ii] <- H%*%x[, ii - 1] + W%*%rnorm(2)
    y[1, ii] <- atan(x[2, ii]/x[1, ii])
    y[2, ii] <- atan(x[2, ii]/(x[1, ii] - s2))
    y[, ii] <- (y[, ii] + V%*%rnorm(2) + 0.5*pi)%%pi -0.5*pi
  }
  return(list(xx = x, yy = y, H = H, W = W, V = V))
}

s2 <- 20; nobs <- 300
q <- c(0.03, 0.03); r <- c(0.02, 0.02)
start <- c(10, 0, 0.01, 0.01)
H <- c(1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,  0,1)
H <- matrix(H, ncol = 4, nrow = 4, byrow = T)
W <- c(0.5*q[1], 0, 0, 0.5*q[2], q[1], 0, 0, q[2])
W <- matrix(W, ncol = 2, nrow = 4, byrow = T)
V <- diag(r)
simu_out <- simPassiveSonar(nobs, q, r, W, V, s2, start, seed = 2000)
yy <- simu_out$yy

#Figure 3 in the paper
pdf("SMC1.pdf",width=8,height=5)
plot(simu_out$xx[1, ], simu_out$xx[2, ], xlab = 'x', ylab = 'y', type = "l", xlim = c(-12, 52),
     ylim = c(-10, 43), main = "Target trajectory")
points(c(0, 20), c(0, 0), pch = 15, cex = 2)
dev.off()

### function for bearing only example to implement SMC filtering
SISstep.Sonar <- function(mm, xx, logww, yy, par, xdim = 1, ydim = 1){
  H <- par$H; W <- par$W; V <- par$V; s2 <- par$s2;
  xx <- H%*%xx + W%*%matrix(rnorm(2*mm), nrow = 2, ncol = mm)
  y1 <- atan(xx[2, ]/xx[1, ])
  y2 <- atan(xx[2,]/(xx[1,] - s2))
  res1 <- (yy[1] - y1 + 0.5*pi)%%pi - 0.5*pi
  res2 <- (yy[2] - y2 + 0.5*pi)%%pi - 0.5*pi
  uu <- -res1**2/2/V[1, 1]**2 - res2**2/2/V[2, 2]**2
  logww <- logww + uu
  return(list(xx = xx, logww = logww))
}


### function for bearing only example to implement SMC smoothing
SISstep.Smooth.Sonar <- function(mm, xxt, xxt1, ww, vv, par){
  H <- par$H; W <- par$W;
  uu <- 1:mm
  aa <- 1:mm
  xxt1p <- H%*%xxt
  for(i in 1:mm){
    res1 <- (xxt1[1, i] - xxt1p[1, ])/W[1,1]
    res2 <- (xxt1[2, i] - xxt1p[2, ])/W[2, 2]
    aa[i] <- sum(exp( -0.5*res1**2 - 0.5*res2**2)*ww)
  }
  for(j in 1:mm){
    res1 <- (xxt1[1, ] - xxt1p[1, j])/W[1, 1]
    res2 <- (xxt1[2, ] - xxt1p[2, j])/W[2, 2]
    uu[j] <- sum(exp( -0.5*res1**2 - 0.5*res2**2)*vv/aa)
  }
  vv <- ww*uu
  return(list(vv = vv))
}

###SMC filtering
mm <- 100000
set.seed(1)
resample.sch <- rep(1,nobs)
xdim <- 4; ydim <- 2
mu0 <- start; SS0 <- diag(c(1, 1, 1, 1))*0.01
xx.init <- mu0 + SS0%*%matrix(rnorm(mm*4), nrow = 4, ncol = mm)
par <- list(H = H, W = W, V = V, s2 = s2)
delay <- 10
out <- SMC(SISstep.Sonar, nobs, yy, mm, par, xx.init, xdim, ydim, resample.sch, delay)



tt <- 100:nobs
#### Figure 4 in the paper
#pdf("SMC2.pdf",width=5,height=5)
plot(simu_out$xx[1, tt], simu_out$xx[2, tt], xlab = 'x', ylab = 'y', xlim=c(-12, 46),
     ylim=c(17,38), main="Delayed filtering results", cex = 0.5)
for(dd in c(1, 6, 11)){
  tt <- 100:(nobs - dd)
  lines(out$xhat[1, tt, dd], out$xhat[2, tt, dd], lty = 23 - 2*dd, lwd = 1)
}
legend(25, 22.5, legend = c("delay 0", "delay 5", "delay 10"), lty = c(21, 11, 1))
#dev.off()

#pdf("SMC3.pdf",width=5,height=5)
plot(simu_out$xx[1, ] - out$xhat[1, , 1], ylab = 'x error', type = 'n', main = "Tracking error in x direction")
lines(simu_out$xx[1, ] - out$xhat[1, , 1], lty = 21, lwd = 1)
lines(simu_out$xx[1, 1:(nobs-6)] - out$xhat[1, 1:(nobs-6), 6], lty = 11, lwd = 1.5)
lines(simu_out$xx[1, 1:(nobs-11)] - out$xhat[1, 1:(nobs-11), 11], lty = 1, lwd = 2)
abline(h = 0)
legend(0, -0.85, legend = c("delay 0", "delay 5", "delay 10"), lty = c(21, 11, 1), lwd=c(1, 2, 2))
#dev.off()


#pdf("SMC4.pdf",width=5,height=5)
plot(simu_out$xx[2, ] - out$xhat[2, ,1], ylab = 'y error', type = 'n', main = "Tracking error in y direction")
lines(simu_out$xx[2, ] - out$xhat[2, ,1], lty = 21, lwd = 1)
lines(simu_out$xx[2, 1:(nobs-6)] - out$xhat[2, 1:(nobs-6), 6], lty = 11, lwd = 1.5)
lines(simu_out$xx[2, 1:(nobs-11)] - out$xhat[2, 1:(nobs-11), 11], lty = 1, lwd = 2)
abline(h = 0)
legend(-2, -1.1, legend = c("delay 0", "delay 5", "delay 10"), lty = c(21, 11, 1), lwd = c(1, 2, 2))
#dev.off()

###SMC smoothing
set.seed(1)
mm <- 5000
par <- list(H = H, W = W, V = V, s2 = s2)
xx.init <- mu0 + SS0%*%matrix(rnorm(mm*4), nrow = 4, ncol = mm)
out.s5K <- SMC.Smooth(SISstep.Sonar, SISstep.Smooth.Sonar, nobs, yy, mm, par, xx.init, xdim, ydim, resample.sch)


tt <- 100:nobs
#pdf("SMC5.pdf",height=5,width=5)
###Top right panel of the Figure 4 in the paper
plot(simu_out$xx[1, tt], simu_out$xx[2, tt], xlab = 'x', ylab = 'y', xlim = c(-12, 46), ylim=c(17,38), main='SMC smoothing results')
lines(out.s5K$xhat[1, tt], out.s5K$xhat[2, tt], lty = 1, lwd = 2)
legend(17, 19.5, legend = c("SMC Smoother"), lty = 1, lwd = 2)
#dev.off()


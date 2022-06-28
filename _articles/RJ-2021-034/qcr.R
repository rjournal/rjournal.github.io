###################################################
### code chunk number 1: load
###################################################
library("qcr")

###################################################
### code chunk number 1: bivariate
###################################################
mu <- c(0, 0)
Sigma <- matrix(c(1, 0, 0, 1), nrow = 2)
Y <- rmvnorm(540, mean = mu, sigma = Sigma)
u <- c(2, 2)
S <- matrix(c(4, 0, 0, 4), nrow = 2)
x <- rmvnorm(40, mean = u, sigma = S)


###################################################
### code chunk number 3: npqcsd
###################################################
x <- rbind(Y[501:540, ], x)
G <- Y[1:500, ]
data.npqcd <- npqcd(x, G)


###################################################
### code chunk number 4: rchart (eval = FALSE)
###################################################
res.npqcs <- npqcs.r(data.npqcd, method = "Liu", alpha = 0.025)
plot(res.npqcs, title = " r Control Chart")


###################################################
### code chunk number 5: rchart-plot
###################################################
res.npqcs <- npqcs.r(data.npqcd, method = "Liu", alpha = 0.025)
plot(res.npqcs, title = " r Control Chart")


###################################################
### code chunk number 6: Qchart (eval = FALSE)
###################################################
n <- 4    # samples
m <- 20   # measurements
k <- 2    # number of variables
x.a <- array( , dim = c(n, k, m))
for (i in 1:m) {
  x.a[, , i] <- x[(1 + (i - 1) * n):(i * n), ]
}
data.npqcd <- npqcd(x.a, G)
res.npqcs <- npqcs.Q(data.npqcd, method = "Liu", alpha = 0.025)
plot(res.npqcs, title = "Q Control Chart")


###################################################
### code chunk number 7: Schart-plot
###################################################
data.npqcd <- npqcd(x, G)
res.npqcs <- npqcs.S(data.npqcd, method = "Liu", alpha = 0.05)
plot(res.npqcs, title = "S Control Chart")


###################################################
### code chunk number 8: Simulated dataset
###################################################
library(fda.usc)
m <- 30
tt<-seq(0,1,len=m)
# H0
mu_0<-30 * tt * (1 - tt)^(3/2)
n0 <- 100
mdata<-matrix(NA,ncol=m,nrow=n0)
sigma <- exp(-3*as.matrix(dist(tt))/0.9)
for (i in 1:n0) mdata[i,]<- mu_0+0.5*mvrnorm(mu = mu_0,Sigma = sigma )

###################################################
### code chunk number 9: Plot of simulated dataset
###################################################
fdchart <- fdqcd(mdata)
plot(fdchart,type="l",col="gray",main="Functional data")

###################################################
### code chunk number 10: FDA Phase I control chart
###################################################
fddep <- fdqcs.depth(fdchart)
summary(fddep)

plot(fddep,title.fdata = "FDA chart",title.depth = "Depth chart")
out <- fddep$out; out

 alpha <- 0.1
 trim <- 0.1
 while (length(out)>0) {
   mdata <- fddep$fdata$data[-out,]
   fddep <- fdqcs.depth(mdata,ns = alpha, trim=trim, plot=FALSE)
   out <- fddep$out
   }
 plot(fddep,title.fdata = "Envelope with the 90% deepest curves",
         title.depth = "Depth control chart")

###################################################
### code chunk number 11: FDA Phase II control chart
###################################################
 mu_a<- 30 * tt^(3/2) * (1 - tt)
 n_a <- 50
 mdata_a<-matrix(NA,ncol=m,nrow=n_a)
 for (i in 1:n_a) mdata_a[i,]<- mu_a+0.5*mvrnorm(mu = mu_a,Sigma = sigma )

 fdchart_a <- fdqcd(mdata_a,"Monitoring curves")
 phase2.chart <- fdqcs.rank(fdchart,fdchart_a)
 plot(phase2.chart)
 summary(phase2.chart)

###################################################
### code chunk number 12: qcs.cp
###################################################
data("pistonrings")
xbar <- qcs.xbar(pistonrings[1:125, ], plot = FALSE)
limits <- c(lsl = 73.99, usl = 74.01)
# qcs.cp(object = xbar, parameters = c(0, 0), limits = limits, 
#        contour = FALSE)
# qcs.cp(object = xbar, parameters = c(1, 0), limits = limits, 
#        contour = FALSE)
# qcs.cp(object = xbar, parameters = c(0, 1), limits = limits, 
#        contour = FALSE)
qcs.cp(object = xbar, parameters = c(1, 1), limits = limits, 
       contour = FALSE)


###################################################
### code chunk number 13: cpplot (eval = FALSE)
###################################################
oldpar <- par(mfrow = c(2, 2))
qcs.cp(object = xbar, parameters = c(0, 0), limits = limits, 
       ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(1, 0), limits = limits,  
       ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(0, 1), limits = limits,  
       ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(1, 1), limits = limits,  
       ylim = c(0, 1))
par(oldpar)


###################################################
### code chunk number 14: cpplot-plot
###################################################
oldpar <- par(mfrow = c(2, 2))
qcs.cp(object = xbar, parameters = c(0, 0), limits = limits, ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(1, 0), limits = limits, ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(0, 1), limits = limits, ylim = c(0, 1))
qcs.cp(object = xbar, parameters = c(1, 1), limits = limits, ylim = c(0, 1))
par(oldpar)


###################################################
### code chunk number 15: cpplot2 (eval = FALSE)
###################################################
xbar <- qcs.xbar(pistonrings[1:125, ], plot = FALSE)
limits <- c(lsl = 73.99, usl = 74.01)
# qcs.hat.cpm(object = xbar, limits = limits, ylim = c(0,1))
mu <- xbar$center
std.dev <- xbar$std.dev
qcs.hat.cpm(limits = limits, mu = mu, std.dev = std.dev, ylim = c(0,1))


###################################################
### code chunk number 16: cpplot2-plot
###################################################
qcs.hat.cpm(object = xbar, limits = limits, ylim = c(0,1))


###################################################
### code chunk number 17: qcs.cpn
###################################################
xbar <- qcs.xbar(pistonrings[1:125, ], plot = FALSE)
limits <- c(lsl = 73.99, usl = 74.01)
# x <- xbar$statistics[[1]]
# median <- median(x)
# q = quantile(x, probs = c(0.00135, 0.99865)) # c(lq, uq)
# qcs.cpn(parameters = c(0, 0), limits = limits, median = median, q = q)
# qcs.cpn(object = xbar, parameters = c(0, 0), limits = limits)
# qcs.cpn(object = xbar, parameters = c(1, 0), limits = limits)
# qcs.cpn(object = xbar, parameters = c(0, 1), limits = limits)
qcs.cpn(object = xbar, parameters = c(1, 1), limits = limits)

###################################################
### code chunk number 18: caplot
###################################################
qcs.ca(xbar, limits = c(lsl = 73.99, usl = 74.01))

###################################################
### code chunk number 19: caplot-plot
###################################################
qcs.ca(xbar, limits = c(lsl = 73.99, usl = 74.01))



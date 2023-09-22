###############################################
##### Codes for Section: Package overview #####
###############################################
rm(list = ls())
library(FarmTest)
library(rstiefel)
library(mvtnorm)
n <- 120
p <- 400
K <- 5
set.seed(100)
B <- rustiefel(p, K) %*% diag(rep(sqrt(p), K))
FX <- rmvnorm(n, rep(0, K), diag(K))
p1 <- 100
strength <- 1
mu <- c(rep(strength, p1), rep(0, p - p1))
U <- rmvt(n, diag(p), 3)
X <- rep(1, n) %*% t(mu) + FX %*% t(B) + U
output <- farm.test(X)
output

FDP <- sum(output$reject > p1) / length(output$reject)
FDP
power <- sum(output$reject <= p1) / p1
power

names(output)

head(summary(output))

#### The following codes produce Figure 1, Page 8.
library(tikzDevice)
means = as.vector(output$means)
tstat = as.vector(output$tStat)
tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
hist(means, breaks = 15, main = "", xlab = "", ylab = "", xlim = c(-0.5, 1.5), col = "cornflowerblue")
title(main = "Histogram of estimated means", line = 1, cex.main = 2)
title(xlab = "Estimated means $\\hat{\\mu}$", line = 3, cex.lab = 2.5)
title(ylab = "Frequency", line = 2.2, cex.lab = 2.5)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
hist(tstat, breaks = 20, main = "", xlab = "", ylab = "", xlim = c(-2.5, 10), col = "coral")
title(main = "Histogram of test statistics", line = 1, cex.main = 2)
title(xlab = "Test statistics $T$", line = 3, cex.lab = 2.5)
title(ylab = "Frequency", line = 2.2, cex.lab = 2.5)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

ev = sort(as.vector(output$eigenVal), decreasing = TRUE)
prop = ev / sum(ev)
num = 20
tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(1:num, prop[1:num], type = "b", pch = 19, lwd = 5, cex = 1.5, col = "blue", axes = FALSE, main = "", xlab = "", ylab = "")
axis(1, 1:num)
axis(2, pretty(range(prop[1:num])))
lines(1:num, rep(prop[5], num), col = "red", lty = 2, lwd = 5)
title(main = "Scree plot of estimated covariance", line = 1, cex.main = 2)
title(xlab = "Top $20$ eigenvalues", line = 3, cex.lab = 2.5)
title(ylab = "Proportion of variance", line = 2.2, cex.lab = 2)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

ratio = as.vector(output$eigenRatio)
num = 20
tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(1:num, ratio[1:num], type = "b", pch = ifelse(ratio > 3, 19, 17), lwd = 4, cex = 1.5, axes = FALSE, col = ifelse(ratio > 3, "red", "blue"), 
     main = "", xlab = "", ylab = "")
axis(1, 1:num)
axis(2, pretty(range(ratio[1:num])))
title(main = "Eigenvalue ratios of estimated covariance", line = 1, cex.main = 2)
title(xlab = "Index", line = 3, cex.lab = 2.5)
title(ylab = "Eigenvalue ratios", line = 2.2, cex.lab = 2)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)
#### End of codes for Figure 1, Page 8.


output <- farm.test(X, fX = FX)
output

output <- farm.test(X, alternative = "greater", alpha = 0.01)
output

output <- farm.test(X, h0 = rep(1, p), alpha = 0.01)
output

output <- farm.test(X, KX = 2)
power <- sum(output$reject <= p1) / p1
power

output <- farm.test(X, KX = 0)
output

m <- 150
set.seed(200)
BY <- rustiefel(p, K) %*% diag(rep(sqrt(p), K))
FY <- rmvnorm(m, rep(0, K), diag(K))
uY <- rmvt(m, diag(p), 3)
Y <- FY %*% t(BY) + uY
output <- farm.test(X, Y = Y)
output

names(output$means)
head(as.vector(output$means$X.means))



##########################################
##### Codes for Section: Simulations #####
##########################################
#### The following codes produce testing results for Figure 2, Page 11.
rm(list = ls())
library(FarmTest)
library(rstiefel)
library(mvtnorm)
library(mutoss)

geneSigma = function(p, block) {
  Sigma = diag(3, p)
  bn = p / block
  for (i in 0:(bn - 1)) {
    for (j in 1:(block - 1)) {
      for (k in (j + 1):block) {
        Sigma[i * block + j, i * block + k] = Sigma[i * block + k, i * block + j] = runif(1)
      }
    }
  }
  return (Sigma)
}

compFDP = function(indices, p1) {
  if (is.character(indices)) {
    return (c(0, 0))
  } else {
    return (c(sum(indices > p1) / length(indices), sum(indices <= p1) / p1))
  }
}

n = 60 ## Modify sample size: 60, 80, 100, 120, 140
p = 600  ## Modify dimension: 200, 400, 600, 800, 1000
K = 5
M = 200
block = 5
p1 = 0.2 * p
strength = 4 * sqrt(log(p) / n)
set.seed(2019)
B = rustiefel(p, K) %*% diag(rep(sqrt(p), K))
mu = c(rep(strength, p1), rep(0, p - p1))
FDP = power = matrix(0, M, 4)

pb = txtProgressBar(style = 3)
for (i in 1:M) {
  set.seed(i)
  FX = rmvnorm(n, rep(0, K), diag(K))
  Sigma = geneSigma(p, block)
  U = rmvnorm(n, rep(0, p), Sigma)
  X = rep(1, n) %*% t(mu) + FX %*% t(B) + U 
  ## FarmTest
  output = farm.test(X)
  FDP[i, 1] = compFDP(output$reject, p1)[1]
  power[i, 1] = compFDP(output$reject, p1)[2]
  ## t-test
  pValues = apply(X, 2, function(x) t.test(x)$p.value)
  indices = which(adaptiveSTS(pValues, 0.05, silent = TRUE)$rejected == 1)
  FDP[i, 2] = compFDP(indices, p1)[1]
  power[i, 2] = compFDP(indices, p1)[2]
  ## Wilcoxon-Mann-Whitney test
  output = onesamp.marginal(t(X), robust = TRUE, alternative = "two.sided", psi0 = 0)
  indices = which(adaptiveSTS(output$pValues, 0.05, silent = TRUE)$rejected == 1)
  FDP[i, 3] = compFDP(indices, p1)[1]
  power[i, 3] = compFDP(indices, p1)[2]
  ## RmTest
  output = farm.test(X, KX = 0)
  FDP[i, 4] = compFDP(output$reject, p1)[1]
  power[i, 4] = compFDP(output$reject, p1)[2]
  
  setTxtProgressBar(pb, i / M)
}
colnames(FDP)=c("Farm_FDP", "t_FDP", "WMW_FDP", "RM_FDP")
colnames(power)=c("Farm_PW", "t_PW", "WMW_PW", "RM_PW")
df = data.frame(FDP, power)
df.mean = data.frame(colMeans(df))


#### The following codes produce testing results for Figure 3, Page 11.
rm(list = ls())
library(FarmTest)
library(rstiefel)
library(mutoss)
library(mvtnorm)
library(sgt)
library(EnvStats)

geneSigma = function(p, block) {
  Sigma = diag(p)
  bn = p / block
  for (i in 0:(bn - 1)) {
    for (j in 1:(block - 1)) {
      for (k in (j + 1):block) {
        Sigma[i * block + j, i * block + k] = Sigma[i * block + k, i * block + j] = runif(1) / 3
      }
    }
  }
  return (Sigma)
}

compFDP = function(indices, p1) {
  if (is.character(indices)) {
    return (c(0, 0))
  } else {
    return (c(sum(indices > p1) / length(indices), sum(indices <= p1) / p1))
  }
}

n = 60 ## Modify sample size: 60, 80, 100, 120, 140
p = 600  ## Modify dimension: 200, 400, 600, 800, 1000
K = 5
M = 50
block = 5
p1 = 0.2 * p
strength = 4 * sqrt(log(p) / n)
set.seed(2019)
B = rustiefel(p, K) %*% diag(rep(sqrt(p), K))
mu = c(rep(strength, p1), rep(0, p - p1))
FDP = power = matrix(0, M, 4)
pb = txtProgressBar(style = 3)
for (i in 1:M) {
  FX = rmvnorm(n, rep(0, K), diag(K))
  Sigma = geneSigma(p, block)
  U = rmvt(n, Sigma, 3)
  X = rep(1, n) %*% t(mu) + FX %*% t(B) + U
  ## FarmTest
  output = farm.test(X)
  FDP[i, 1] = compFDP(output$reject, p1)[1]
  power[i, 1] = compFDP(output$reject, p1)[2]
  ## t-test
  pValues = apply(X, 2, function(x) t.test(x)$p.value)
  indices = which(adaptiveSTS(pValues, 0.05, silent = TRUE)$rejected == 1)
  FDP[i, 2] = compFDP(indices, p1)[1]
  power[i, 2] = compFDP(indices, p1)[2]
  ## Wilcoxon-Mann-Whitney test
  output = onesamp.marginal(t(X), robust = TRUE, alternative = "two.sided", psi0 = 0)
  indices = which(adaptiveSTS(output$pValues, 0.05, silent = TRUE)$rejected == 1)
  FDP[i, 3] = compFDP(indices, p1)[1]
  power[i, 3] = compFDP(indices, p1)[2]
  ## RmTest
  output = farm.test(X, KX = 0)
  FDP[i, 4] = compFDP(output$reject, p1)[1]
  power[i, 4] = compFDP(output$reject, p1)[2]
  
  setTxtProgressBar(pb, i / M)
}
colnames(FDP)=c("Farm_FDP", "t_FDP", "WMW_FDP", "RM_FDP")
colnames(power)=c("Farm_PW", "t_PW", "WMW_PW", "RM_PW")
df = data.frame(FDP, power)
df.mean = data.frame(colMeans(df))


#### The following codes draw Figure 2 and 3, after we combine testing results from different sample size n and dimension p.

#### Codes for the upper panel of Figure 2 and 3.
library(tikzDevice)
size = c(60, 80, 100, 120, 140)
tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(size, FDP[, 1], type = "b", pch = 19, lwd = 5, ylim = c(0.04, 0.07), xlab = "", ylab = "", main = "", axes = FALSE)
lines(size, FDP[, 2], type = "b", pch = 19, lwd = 5, col = "blueviolet")
lines(size, FDP[, 3], type = "b", pch = 19, lwd = 5, col = "forestgreen")
lines(size, FDP[, 4], type = "b", pch = 19, lwd = 5, col = "goldenrod")
axis(1, pretty(range(size)))
axis(2, pretty(c(0.04, 0.09)))
title(main = "Empirical FDR versus sample size", line = 1, cex.main = 2)
title(xlab = "Sample size $n$", line = 3, cex.lab = 2.5)
title(ylab = "FDR", line = 2.2, cex.lab = 2.3)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(size, PW[, 1], type = "b", pch = 19, lwd = 5, ylim = c(0.8, 1), xlab = "", ylab = "", main = "", axes = FALSE)
lines(size, PW[, 2], type = "b", pch = 19, lwd = 5, col = "blueviolet")
lines(size, PW[, 3], type = "b", pch = 19, lwd = 5, col = "forestgreen")
lines(size, PW[, 4], type = "b", pch = 19, lwd = 5, col = "goldenrod")
axis(1, pretty(range(size)))
axis(2, pretty(c(0.75, 1)))
title(main = "Power versus sample size", line = 1, cex.main = 2)
title(xlab = "Sample size $n$", line = 3, cex.lab = 2.5)
title(ylab = "Power", line = 2.2, cex.lab = 2.3)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

#### Codes for the lower panel of Figure 2 and 3.
library(tikzDevice)
dimension = c(200, 400, 600, 800, 1000)
tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(dimension, FDP[, 1], type = "b", pch = 19, lwd = 5, ylim = c(0.04, 0.08), xlab = "", ylab = "", main = "", axes = FALSE)
lines(dimension, FDP[, 2], type = "b", pch = 19, lwd = 5, col = "blueviolet")
lines(dimension, FDP[, 3], type = "b", pch = 19, lwd = 5, col = "forestgreen")
lines(dimension, FDP[, 4], type = "b", pch = 19, lwd = 5, col = "goldenrod")
axis(1, pretty(range(dimension)))
axis(2, pretty(c(0.04, 0.08)))
title(main = "Empirical FDR versus dimension", line = 1, cex.main = 2)
title(xlab = "Dimension $p$", line = 3, cex.lab = 2.5)
title(ylab = "FDR", line = 2.2, cex.lab = 2.3)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

tikz('plot.tex', standAlone = TRUE, width = 7, height = 5)
plot(dimension, PW[, 1], type = "b", pch = 19, lwd = 5, ylim = c(0.7, 1), xlab = "", ylab = "", main = "", axes = FALSE)
lines(dimension, PW[, 2], type = "b", pch = 19, lwd = 5, col = "blueviolet")
lines(dimension, PW[, 3], type = "b", pch = 19, lwd = 5, col = "forestgreen")
lines(dimension, PW[, 4], type = "b", pch = 19, lwd = 5, col = "goldenrod")
axis(1, pretty(range(dimension)))
axis(2, pretty(c(0.7, 1)))
title(main = "Power versus dimension", line = 1, cex.main = 2)
title(xlab = "Dimension $p$", line = 3, cex.lab = 2.5)
title(ylab = "Power", line = 2.2, cex.lab = 2.3)
color = c("black", "goldenrod", "forestgreen", "blueviolet")
labels = c("FarmTest", "RmTest", "WMW-test", "$t$-test")
legend("bottomright", labels, col = color, lwd = 5, cex = 1.8)
dev.off()
tools::texi2dvi('plot.tex', pdf = T)



################################################
##### Codes for Section: Real data example #####
################################################
rm(list = ls())
library(fBasics)
library(mutoss)
library(FarmTest)
library(latex2exp)
library(tikzDevice)
sp500 = read.csv("sp500.prices.csv", na.strings=c(""," ","NA"))[,c(1:3)]
sp500 = sp500[complete.cases(sp500),]
date = unique(sp500$date)[-1]
sp500  = reshape(sp500, direction  = "wide", timevar = "date", idvar = "CUSIP")
indx <- sapply(sp500, is.factor)
sp500[indx] <- lapply(sp500[indx], function(x) as.numeric(as.character(x))) #warning is ok, data missing for some days, removed later
sp500 = sp500[,-1]
sp500.all = sp500[rowSums(is.na(sp500)) == 0, ]
sp500.all= sp500.all[rowSums((sp500.all)<=0) == 0, ]
dim(sp500.all)
sp500.ret.all = matrix(NA, NROW(sp500.all), NCOL(sp500.all)-1)
for ( i in 1: NROW(sp500.all)){
  sp500.ret.all[i,]  = diff(as.numeric(log(sp500.all[i,])))
}

# Rolling time window, each with one year's data
years = NCOL(sp500)
# Window length 12 months
subset = 12
# Matrix to store number of rejections
num = matrix(NA,years-subset, 4)
ind = 0
for ( time in subset:(years-1)){
  sp500.month = sp500[,(time-subset+1):(time+1)]
  print(paste(time, "of", years-1))
  ind = ind + 1
  sp500.keep = sp500.month[rowSums(is.na(sp500.month)) == 0, ]
  sp500.keep = sp500.keep[rowSums((sp500.keep)<=0) == 0, ]
  sp500.ret = matrix(NA, NROW(sp500.keep), NCOL(sp500.keep)-1)
  for ( i in 1: NROW(sp500.keep)){
    sp500.ret[i,]  = diff(as.numeric(log(sp500.keep[i,])))
  }
  X =as.matrix (sp500.ret)
  stock.names = rownames(sp500.ret)
  p = NROW(X)
  n = NCOL(X)
  H0 = rep(0, p)
  ## FARM----------------------------------------------------------
  output_FARM = farm.test(t(X),  alpha = 0.01)
  num[ind,1 ]= sum(output_FARM$significant == 1)
  ## t.test----------------------------------------------------------
  pvalue_t = NULL
  for ( j in 1:p){
    test =  t.test(X[j,])
    pvalue_t[j] = test$p.value
  }
  indices_t = which(adaptiveSTS(pvalue_t, 0.01, silent = TRUE)$rejected == 1)
  if (is.character(indices_t)) {
    num[ind,2 ] = 0
  } else {
    num[ind,2 ] = length(indices_t)
  }
  ## Wilcoxon-Mann-Whitney test -----------------------------------------------------
  output_WMW = onesamp.marginal(t(X), robust = TRUE, alternative = "two.sided", psi0 = 0)
  indices_WMW = which(adaptiveSTS(output_WMW$pValues, 0.01, silent = TRUE)$rejected == 1)
  if (is.character(indices_WMW)) {
    num[ind,3 ] = 0
  } else {
    num[ind,3 ] = length(indices_WMW)
  }
  ## RmTest --------------------------------------
  output_RM = farm.test(t(X),  alpha = 0.01, KX = 0)
  num[ind, 4]= sum(output_RM$significant == 1)
}
#how many stocks we're working with on average
ind = 0
how_many = NULL
for ( time in subset:(years-1)){
  sp500.month = sp500[,(time-subset+1):(time+1)]
  print(paste(time, "of", years-1))
  ind = ind + 1
  sp500.keep = sp500.month[rowSums(is.na(sp500.month)) == 0, ]
  sp500.keep= sp500.keep[rowSums((sp500.keep)<=0) == 0, ]
  sp500.ret = matrix(NA, NROW(sp500.keep), NCOL(sp500.keep)-1)
  for ( i in 1: NROW(sp500.keep)){
    sp500.ret[i,]  = diff(as.numeric(log(sp500.keep[i,])))
  }
  how_many[ind] = NROW(sp500.ret)
}
mean(how_many)
#plotting and printing summary
dates <- as.Date(as.character(date[(subset+1):(years-1)]), format = "%Y%m%d")
at = seq(1, years-subset, by = 12 )

##### The following codes produce Figure 4, Page 12.
data.plot  = data.frame(num[,c(1,2, 3, 4)])
tikz('plot.tex', standAlone = TRUE, pointsize = 8, width = 8,height = 4.5,)
mp = barplot(as.matrix(t(data.plot)), col=c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"), width=2,
             main= "", ylab= "Number of stocks chosen", xlab = "", cex.lab=1.2, border = NA, xaxt ='n')
axis(side=1, at=mp[at],    labels=format(dates[at], '%b-%Y'))
legend("top", fill=c("#d7191c", "#abd9e9", "#2c7bb6"),
       legend=c("FarmTest", "WMW-test", "RmTest"),bty = "n")
dev.off()
tools::texi2dvi('plot.tex', pdf = T)

##### he following codes produce Table 2, Page 12.
df.sum = data.frame( cbind(colMeans(num), colSds(num),apply(num,2,median),colMins(num),colMaxs(num)))
rownames(df.sum) = c("FarmTest", "t-test", "WMW-test", "RMtest")
colnames(df.sum) = c("Mean", "SD", "median", "min", "max")
print(df.sum)

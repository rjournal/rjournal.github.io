#QME Cauchy

myqme <- function(x)
  as.numeric(c(median(x), (quantile(x, probs=3/4)-quantile(x, probs=1/4))/2))

myqme2 <- function(x)
{
  q123 <- quantile(x, probs=1:3/4)
  M <- rbind(c(0,1,0), c(-1/2, 0, 1/2))
  M %*% q123
}

rbind(c(0,1,0), c(-1/2, 0, 1/2)) %*% qcauchy(1:3/4)
  


n <- 1e5
x <- rcauchy(n, 1, 2)
myqme(x)
myqme2(x)

f1 <- function(nbsimu, samplesize)
  replicate(nbsimu, myqme(rcauchy(samplesize)))


covtheo <- function(n)
{
  v1 <- 1/(4*n*dcauchy(qcauchy(1/2))^2)
  rho <- 0
  v2 <- 2/4*(1-2/4)/(4*n*dcauchy(qcauchy(1/4))^2)
  cbind(c(v1, rho), c(rho, v2))  
}

res1e3 <- t(f1(1e5, 1e3))
head(res1e3)
apply(res1e3, 2, mean)

cov(res1e3)*1e3

covtheo(1e3)*1e3

res1e4 <- t(f1(1e4, 1e4))
cov(res1e4)*1e4
covtheo(1e4)*1e4
apply(res1e4, 2, mean)

res1e5 <- t(f1(1e4, 1e5))
cov(res1e5)*1e5
covtheo(1e5)*1e5
apply(res1e5, 2, mean)




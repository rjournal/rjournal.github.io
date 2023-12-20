library(fnets)
energydata <- read.table("energydata.txt")
dt <- read.table("names.txt")

period_1 <- 1:(24*10)
data_1 <- as.matrix( t(energydata[period_1,]) )

# Fig 1:
Bartlett.weights <- function(x) 1 - abs(x)

eig_plot <- function(x, mode = "spectral"){
  p <- nrow(x)
  n <- ncol(x)
  if(mode == "spectral"){
    q.max <- round(min(50, sqrt(min(n - 1, p)))) #max number of factors/AR lags

    ## Step 1 :estimation of spectral density matrix of x
    mm <-  max(1, 2 * floor( (n/log(n))^(1/3) ) )
    len <- 2 * mm
    thetas <- 2 * pi * (0:len)/(len + 1)
    w <- Bartlett.weights(-mm:mm/mm)
    Gamma_x <- array(0, dim = c(p, p, 2 * mm + 1)) #spectral density array, slices correspond to l
    xx <- t(scale(t(x), scale = FALSE))
    for (h in 0:(mm - 1)){
      Gamma_x[, , h + 1] <- xx[, 1:(n - h) + h] %*% t(xx[, 1:(n - h)])/n * w[h + mm + 1]
      if(h != 0) Gamma_x[, , 2 * mm + 1 - h + 1] <- t(Gamma_x[, , h + 1])
    }

    Sigma_x <- aperm(apply(Gamma_x, c(1, 2), fft), c(2, 3, 1)) / (2 * pi)
    C <- diag(diag(Sigma_x[,,1])^(-.5)) %*% Sigma_x[,,1] %*%  diag(diag(Sigma_x[,,1])^(-.5))
  }
  else if (mode == "cov"){
    C <- cor(x)
    C <- diag(diag(C)^(-.5)) %*% C %*%  diag(diag(C)^(-.5))
  }
  return(C)
}


C_vec <- matrix(0,50, 100)
C_vec2 <- matrix(0,50, 100)
for (ii in 1:49) {
  for (jj in 1:100) {
    plot_index <- sample.int(50, ii+1)
    Cii <- eig_plot(data_1[plot_index,])
    Cii_svd <- svd(Cii, 0,0)
    C_vec[ii,jj] <- Cii_svd$d[1]
    C_vec2[ii,jj] <- Cii_svd$d[2]
  }
}
par(mfrow = c(1,1), mar = c(2.5, 2.5, 1, .5))
xtick <- 2:51
boxplot( (C_vec)[-50,] ~ (xtick-1)[-50], xlab = "Dimension", ylab = "Eigenvalue" )
lines(y= rowMeans(C_vec)[-50], x=(xtick-1)[-50], type = "p", pch=22, bg="blue");
boxplot(    (C_vec2)[-50,]  ~     (xtick-1)[-50] , add = TRUE  )
lines(y=  (rowMeans(  (C_vec2 )[-50,]  ) ), x = (xtick-1)[-50] , type = "p", pch=22, bg="red" )
legend(x="topleft", legend = c("First","Second"), fill = c("blue","red"))



## Fig 2:
model_1 <- fnets(data_1, center = FALSE, var.order = 1, q = 1, do.lrpc = FALSE)
model_1_q0 <- fnets.var(data_1, center = FALSE, var.order = 1)

par(mfrow=c(1,1))
plot(model_1_q0, groups = dt[,3])
plot(model_1,  groups = dt[,3])


par(mar = c(3, 3, 2, .5))
library(fnets)

## DATA GENERATION
set.seed(111)
n <- 500
p <- 50
common <- sim.unrestricted(n, p)
idio <- sim.var(n, p)
x <- common$data + idio$data

out <- fnets(x)
print(out)

## Fig 3:
fn <- factor.number(x, center=F)
plot(fn)
print(fn)

# Fig 4:
th <- threshold(out$idio.var$beta)
th

par(mar = c(4, 4, 2, .5))
plot(th, c(TRUE, FALSE, FALSE))
plot(th, c(FALSE, FALSE, TRUE))

# Fig 5:
par(mfrow = c(1, 1), mar = rep(2, 4))
g <- network(out)$network
plot(g, layout = igraph::layout_in_circle(g), vertex.color = grDevices::rainbow(1, alpha = 0.2), vertex.label = NA, main = "Granger causal network")
plot(out, display = 'network', type = 'granger')
plot(out, display = 'heatmap', type = 'lrpc')

# Fig 6:

set.seed(111)
n <- 500
p <- 10
x <- sim.var(n, p)$data
out <- fnets.var(x, var.order = 1:3,
             tuning.args = list(tuning = "cv"))
out$lrpc <- par.lrpc(out)
out$do.lrpc <- TRUE
plot(out, display = "tuning")

## Fig 9:

library(latex2exp)
tickers <- dt[,1]
grps <-c(rep(1, 11), rep(2, 37-11), rep(3, 40-37), rep(4, 50-40))
dates <- c(
  20210101, 20210719,
  20210220, 20210907,
  20210411, 20211027)
dates <- as.Date(paste(dates), format = '%Y%m%d')

## network plots

model_net <- fnets(data_1, center = FALSE, var.order = 1, q = 1, lrpc.adaptive = TRUE, do.threshold = TRUE)
par(mar = c(4, 4, 3, 1) + 0.1)
plot(model_net, display = "heatmap", type = "granger", names = tickers, groups = grps)
plot(model_net, display = "heatmap", type = "pc", names = tickers, groups = grps)
plot(model_net, display = "heatmap", type = "lrpc", names = tickers, groups = grps)




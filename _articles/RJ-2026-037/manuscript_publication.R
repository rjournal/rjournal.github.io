# This script contains code of all examples in the manuscript
# "psvmSDR: A Unified Algorithm for Sufficient Dimension Reduction via Principal"
# by Jungmin Shin, Seung Jun Shin, and Artemiou Andreas.
# This code was verified by "psvmSDR_3.0.1" R package.

#=============================#
#### Load "psvmSDR" package ####
#=============================#
#install.packages("psvmSDR") #Please install and load the attached psvmSDR_3.0.1.tar.gz file manually.
library("psvmSDR")


#====================================================#
#### Section 4.2 Functions for class 'psdr'      ####
#====================================================#

#===============================#
#### psdr() with loss='svm' #####
#===============================#
set.seed(100)
n <- 200;
p <- 5;
x <- matrix(rnorm(n*p, 0, 1), n, p)
y <-  x[,1] / (0.5 + (x[,2] + 1)^2) + 0.2*rnorm(n)
obj <- psdr(x, y)

#=====================#
#### print.psdr() #####
#=====================#
print(obj)
summary(obj)

#======================#
#### plot.psdr() #####
#=====================#
plot(obj)

#===============================#
#### psdr() with loss='wsvm' #####
#===============================#
y.binary <- sign(y)
obj_wsvm <- psdr(x, y.binary, loss="wsvm")
print(obj_wsvm)

#===============================================#
#### plot for viewing the result of 'wsvm' #####
#==============================================#
plot(obj_wsvm)


#===============================#
#### psdr() with loss='mylogit' #####
#===============================#
mylogistic <- function(u) log(1+exp(-u))
obj_mylogistic <- psdr(x, y, loss="mylogistic")
print(obj_mylogistic)


#=====================#
#### crBIC()      #####
#=====================#
d.hat <- psdr_bic(obj, rho_grid=seq(0.05, 0.1, length=5), cv_folds=5)
print(d.hat)


#=================================================#
#### Section 4.3 Functions for class 'npsdr'    ####
#=================================================#
set.seed(100)
n <- 200; p <- 5;
x <- matrix(rnorm(n*p, 0, 1), n, p)
y <- 0.5*sqrt((x[,1]^2+x[,2]^2))*(log(x[,1]^2+x[,2]^2))+ 0.2*rnorm(n)

#=====================#
#### psdr()  #####
#=====================#
#linear PSVM fails for recovering the true basis of the central subspace
#in case of the symmetric model.
obj_lin <- psdr(x, y)
print(obj_lin)


#=====================#
#### npsdr()      #####
#=====================#
#Depending on tuning results, the number of seeds, and the operating system environment, the sign of the estimated basis may change.
#Still, it captures the underlying linear relationship in the central subspace.
#set.seed(100)  #fix seed number for initial random parameter for npsdr()
obj_kernel <- npsdr(x, y, max.iter=200, eta=0.8, plot=FALSE) #it takes a couple of seconds
print(obj_kernel)

#=======================#
## generate test data ##
#=======================#
set.seed(200) #seed for generating new.x, new.y
new.x <- matrix(rnorm(n*p, 0, 1), n, p)
new.y <- 0.5*sqrt((new.x[,1]^2+new.x[,2]^2))*(log(new.x[,1]^2+new.x[,2]^2))+ 0.2*rnorm(n)
#=====================================================#

#===============================================================#
## sufficient plot for linear PSVM with test data for Figure 5 ##
#===============================================================#
lsvm <- obj_lin$evectors
x.lsvm <- new.x %*% lsvm
plot(x.lsvm[,1], new.y , type = "p", xlab = "Sufficient predictor 1", ylab=expression(paste(Y, "test")), cex=1, pch=16,
     main="Linear PSVM")
lines(lowess( x.lsvm[,1], new.y), col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = 1)

#=====================#
#### npsdr_x()    #####
#=====================#
reduced_data <- npsdr_x(object=obj_kernel, newdata=new.x, d=2)

#=====================================================================#
## sufficient plot for Nonlinear PSVM with test data for Figure 5.  ##
#=====================================================================#
#Based on which verion of R is used, the result may slightly differ.
#Kernel PSVM shows the better dimension reduction result compare to linear PSVM.
plot(reduced_data[,1], new.y , type = "p", xlab = "Sufficient predictor 1", ylab=expression(paste(Y, "test")), cex=1, pch=16,
     main="Nonlinear PSVM")
lines(lowess( reduced_data[,1], new.y), col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd=1)



#=================================================#
#### Section 4.4 Functions for class 'rtpsdr'   ####
#=================================================#
set.seed(1234)
p <- 5
m <- 500 # batch size
B <- 10  # number of batches
obj <- NULL

for (iter in 1:B){
  set.seed(iter)
  x <- matrix(rnorm(m*p, 0, 1), m, p)
  y <-  x[,1]/(0.5 + (x[,2] + 1)^2) + 0.2 * rnorm(m)
  obj <- rtpsdr(x = x, y = y, obj=obj)
  print(paste("iteration: ", iter))
}

#real-time dimension reduction result.
print(obj)
summary(obj)

#=================================================================#
#### Section 5.1 Real data application: Boston Housing Data    ####
#==================================================================#
#load "Bostonhousing" data from "mlbench" R package
#install.packages("mlbench")
data("BostonHousing", package = "mlbench")
attach(BostonHousing)

#Data preprocesing
BostonHousing <- BostonHousing[BostonHousing$crim < 3.2 , -c(4,9)]
X <- BostonHousing[,-12]; X <- as.matrix(X)
Y <- BostonHousing[,"medv"]

#apply psdr()
set.seed(1)
# PSVM
rslt <- psdr(X, Y)

#visualize the result. Figure 6.
lsvm <- rslt$evectors
x.lsvm <- X %*% lsvm
plot(x.lsvm[,1], Y, type = "p", xlab = expression(hat(b)[1]^T*X), ylab="medv", main="PSVM", pch=16)
lines(lowess( x.lsvm[,1], Y), col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd=1)
plot(x.lsvm[,2], Y, type = "p", xlab = expression(hat(b)[2]^T*X), ylab="medv", main="PSVM", pch=16)
lines(lowess(x.lsvm[,2], Y), col="blue", lwd=2)
grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd=1)



#apply psdr_bic()
bic_boston <- psdr_bic(rslt, rho_grid=seq(0.005, 0.05, length=5), cv_folds=5)
print(bic_boston$rho_star)
print(bic_boston$d_hat)


#========================================================#
#### Section 5.2 Real data application: WDBC data     ####
#========================================================#
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))
par(mar=c(5,5,5,5), oma=c(.5,.5,.5,.5))

#Data preprocessing
wisc <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep = ",")
names <- c('id_number', 'diagnosis', 'radius_mean',
           'texture_mean', 'perimeter_mean', 'area_mean',
           'smoothness_mean', 'compactness_mean',
           'concavity_mean','concave_points_mean',
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se',
           'area_se', 'smoothness_se', 'compactness_se',
           'concavity_se', 'concave_points_se',
           'symmetry_se', 'fractal_dimension_se',
           'radius_worst', 'texture_worst',
           'perimeter_worst', 'area_worst',
           'smoothness_worst', 'compactness_worst',
           'concavity_worst', 'concave_points_worst',
           'symmetry_worst', 'fractal_dimension_worst')
wisc$id_number <- NULL
x.wisc <- matrix(unlist(wisc[,-c(1,2)]), ncol = 30)
y.wisc <- 2*as.numeric(as.factor(unlist(wisc[,2]))) - 3 #transform the response to {-1,+1} style.

#apply psdr() for Figure 7(a).
psdr(x.wisc, y.wisc, loss="wlogit", h=20, lambda=0.1, eta=0.5, max.iter=30, plot=TRUE)

#apply npsdr() for Figure 7(b).
nonlinear.obj <- npsdr(x.wisc, y.wisc, loss="wlogit", h=20, lambda=5, eta=1, max.iter=100, plot=FALSE)
x.nsvm <- npsdr_x(nonlinear.obj, newdata=x.wisc, d=2)
boxplot(x.nsvm[y.wisc == 1,1], x.nsvm[y.wisc != 1,1], xlab = "Y", axes = F, ylab = expression(hat(phi)[1](x)))
axis(1, seq(0.5, 2.5, by = 0.5), c(NA, "+1", NA, "-1", NA)); axis(2, las = 1)


#=============================#
###  Section 3 Computation  ###
#=============================#
################################################################################################################
# The results are not greatly affected by the number of repetitions. If you have enough time and computing power,
# you can change the 'n.sim' argument to a higher number.
# The function 'lpsvm' uses the function 'ksvm' from the 'kernlab' package, which is a standard QP solver for SVM problems.
# The code for 'lpsvm' is appended to the top of the script.
################################################################################################################
# Please load below auxiliary functions in advance.
#========================================================#
# Helper functions
#========================================================#
lpsvm <- function(x, y, H, lambda)
{
  require(kernlab)
  n <- length(y)
  p <- ncol(x)
  bar.x <- apply(x, 2, mean)
  cov.x <- cov(x)

  qprob <- (1:(H-1))/H
  qy <- quantile(y, qprob)

  temp <- eigen(cov.x)
  D <- diag(sqrt(temp$values))
  V <- temp$vectors
  sd.x <-  V %*% D
  inv.sd.x <- diag(1/sqrt(temp$values)) %*% t(V)
  z <- t(inv.sd.x %*% (t(x) - bar.x))

  w <- matrix(0, p, length(qprob))
  for (h in 1:length(qprob)) {
    alpha <- rep(0, n)
    y.tilde <- rep(1, n)
    y.tilde[y < qy[h]] <- -1
    temp <- ksvm(x = z, y = as.factor(y.tilde), type = "C-svc", kernel = "vanilladot", kpar =  list(), C = lambda, scaled = T)
    alpha[temp@SVindex] <- unlist(temp@alpha)
    w[,h] <- apply(alpha * z * y.tilde, 2, sum)
  }

  psi <- solve(t(sd.x)) %*% w
  Mn <- matrix(0, p, p)
  for (h in 1:length(qprob)) Mn <- Mn + psi[,h, drop = F] %*% t(psi[,h, drop = F])

  eigen(Mn)
}

npsdr_no_gen_psi <- function(x, y, H=NULL, h=NULL, obj, lambda=NULL, delta=NULL,
                             eps=1.0e-4, max.iter=NULL, loss=NULL, a=NULL, c=NULL)
{
  psi.gen <- obj
  Psi.new <- psi.gen$w   #n*k
  n <- nrow(Psi.new)
  p <- ncol(Psi.new)

  init.theta <- rnorm(sd=1,n=p)
  step <- 1/H
  pi.grid <- seq(step, 1-step, by = step)

  qprob <- (1:(H-1))/H
  qy <- stats::quantile(y, qprob)

  theta.new <- rep(0,p)
  w.init <- matrix(init.theta, nrow=p, ncol=length(qprob))
  w.final <- matrix(0, nrow=p, ncol=length(qprob))
  eigen.mat <- diag(1,p,p)

  for (s in 1:length(qprob)) {
    y.tilde.new <- rep(1, nrow(Psi.new))
    y.tilde.new[y < qy[s]] <- -1  #s
    for(iter in 1:max.iter){
      Psi <- Psi.new
      y.tilde <- y.tilde.new
      n <- nrow(Psi)
      w <- w.init
      A <- t(Psi)%*%Psi
      for (k in 1:p){
        margin.v <- (Psi %*% w[,s]) * y.tilde #s
        deriv <- -Psi[,k]*y.tilde*as.numeric(I((1-margin.v)>0)) #k
        derivative.j <- lambda*mean(deriv) + 2*(1/nrow(Psi))*(A[k,]%*%w[,s])  ##k,s
        theta.new[k] <- w[k,s] -  delta*derivative.j  ##k,k,s
      }
      w[,s] <- theta.new
      w.init <- matrix(theta.new, nrow=p, ncol = length(qprob))
      if(mean(abs(deriv)) < eps)
        break
    }
    w.final[,s] <- w[,s]
  }

  Mn <- matrix(0, p, p)
  for (h in 1:length(qprob)) Mn <- Mn + w.final[,h, drop = F] %*% t(w.final[,h, drop = F])
  result <- eigen(Mn)
  v <- result$vectors
  u <- result$values
  obj <- list(evectors = v, evalues = u, obj.psi = psi.gen)
  return(obj)
}


npdr_no_gen_psi <- function(x, y, obj, lambda, H, k)
{
  n <- dim(x)[1]
  p <- dim(x)[2]

  obj.psi <- obj

  x <- obj.psi$scaled.x
  m <- attr(x, "scaled:center")
  s <- attr(x, "scaled:scale")

  bw <- obj.psi$bw

  w <- psi <- as.matrix(obj.psi$w) # evectors of Psi
  l <- as.vector(obj.psi$l)        # evalues  of Psi


  P.psi <- psi %*% solve(t(psi) %*% psi) %*% t(psi)
  qprob <- (1:(H-1))/H
  qy <- stats::quantile(y, qprob)

  Mn <- matrix(0, k, k)
  h <- 1
  for (h in 1:(H-1)) {
    y.tilde <- as.vector(ifelse(y >= qy[h], 1, -1))
    Q <- P.psi * outer(y.tilde, y.tilde)
    qp <- ipop(c = -matrix(rep(1, n)),
               H = (1/2) * Q,
               A = y.tilde, b = 0,
               r = 0, l = matrix(0, n), u = matrix(lambda, n))
    alpha <- qp@primal
    cvec <- 1/2 * solve(t(psi) %*% psi) %*% t(psi) %*% (alpha * y.tilde)
    Mn <- Mn + cvec %*% t(cvec)
  }

  result <- eigen(Mn)
  v <- result$vectors
  u <- result$values

  obj <- list(evector = v, evalue = u, obj.psi = obj.psi)
  return(obj)
}

get.psi <- function(x, y, b=floor(length(y)/3)) {
  n <- nrow(x)
  x <- scale(x)
  bw <- 1/mean(as.numeric(stats::dist(x)))^2 # bw parameter for kernel
  Kn <- kernel.function(x, y = x, param.kernel = bw)
  Qn <- diag(n) - matrix(1/n, n, n)

  eigen.psi <- eigen(Qn %*% Kn %*% Qn)
  Psi.new <- eigen.psi$vectors[,1:b, drop = F] # Psi
  l <- eigen.psi$values[1:b]
  tmp.obj <- list("w"=Psi.new, "l"=l, "scaled.x"= x, "bw" = bw, "b" = b)
  tmp.obj
}

kernel.function <- function (x, y = x, param.kernel = 1/p) {
  n <- nrow(x)
  m <- nrow(y)
  p <- ncol(x)
  normx <- drop((x^2) %*% rep(1, p))
  normy <- drop((y^2) %*% rep(1, p))
  a <- x %*% t(y)
  a <- (-2 * a + normx) + outer(rep(1, n), normy, "*")
  exp(-a * param.kernel)
}

# ======================================================== #
proj <- function(B) B %*% solve(t(B) %*% B) %*% t(B)
subspace_dist <- function(B_est, B_true) norm(proj(B_est) - proj(B_true), "F")
subspace_corr <- function(B_est, B_true) mean(svd(t(B_true) %*% B_est)$d)

pred_R2 <- function(y, S, idx_tr, idx_te) {
  fit <- lm(y[idx_tr] ~ S[idx_tr, ])
  pred <- predict(fit, newdata = data.frame(S = S[idx_te, ]))
  1 - mean((y[idx_te] - pred)^2) / var(y[idx_te])
}

pred_consistency <- function(S_qp, S_cgd, y, idx_te) {
  fit_qp <- lm(y ~ S_qp)
  fit_cgd <- lm(y ~ S_cgd)
  p_qp <- predict(fit_qp, newdata = data.frame(S_qp = S_qp[idx_te,]))
  p_cgd <- predict(fit_cgd, newdata = data.frame(S_cgd = S_cgd[idx_te,]))
  cor(p_qp, p_cgd)
}

#===============================================#
#### For Figure 1 left panel: Linear PSVM   #####
#===============================================#
library(kernlab)
library(Matrix)
library(tidyr)
library(ggplot2)
library(dplyr)
library(purrr)


#========================================================#
# Simulation setup
#========================================================#
n.sim <- 100
sample_size <- c(5e3, 6e3, 7e3, 8e3, 9e3, 1e4, 1.5e4, 2e4, 2.5e4, 3e4)
time_df <- matrix(0, ncol = length(sample_size), nrow = 2)
time_list <- vector("list", n.sim)

# accuracy storage
acc_list <- vector("list", n.sim)
true_B <- diag(1, 10)[, 1:2]

#========================================================#
# Main simulation
#========================================================#
for (j in 1:n.sim) {
  acc_df <- data.frame()
  for (ii in seq_along(sample_size)) {
    set.seed(j + ii)
    n <- sample_size[ii]; p <- 10; h <- 20; lambda <- 1
    max.iter <- 30; delta <- 0.5
    x <- matrix(rnorm(n * p), n, p)
    err <- rnorm(n, 0, .2)
    fx <- x[,1] / (0.5 + (x[,2] + 1)^2)
    y <- fx + err

    # QP solver
    tic0 <- Sys.time()
    obj_qp <- lpsvm(x, y, H = h, lambda)
    toc0 <- Sys.time()

    # CGD solver
    tic1 <- Sys.time()
    obj_gd <- psdr(x, y, h = h, lambda = 1, eta = 0.1, max.iter = 50, loss = "svm")
    toc1 <- Sys.time()

    # runtime
    time_df[1, ii] <- as.numeric(toc0 - tic0, units = "secs")
    time_df[2, ii] <- as.numeric(toc1 - tic1, units = "secs")

    # accuracy
    B_qp  <- obj_qp$vectors[, 1:2, drop = FALSE]
    B_cgd <- obj_gd$evectors[, 1:2, drop = FALSE]

    dist_qp  <- subspace_dist(B_qp, true_B)
    dist_cgd <- subspace_dist(B_cgd, true_B)
    corr_qp  <- subspace_corr(B_qp, true_B)
    corr_cgd <- subspace_corr(B_cgd, true_B)

    acc_df <- rbind(acc_df,
                    data.frame(n = n, method = "QP",  dist = dist_qp, corr = corr_qp),
                    data.frame(n = n, method = "CGD", dist = dist_cgd, corr = corr_cgd)
    )
    cat("Simulation", j, " | sample size =", n, "\n")
  }
  time_list[[j]] <- time_df
  acc_list[[j]]  <- acc_df
}



#========================================================#
# Timing summary
#========================================================#
time <- unlist(time_list)

qp.time <- matrix(time[seq(1, length(time), by = 2)], ncol = length(sample_size), byrow = TRUE)
gd.time <- matrix(time[seq(2, length(time), by = 2)], ncol = length(sample_size), byrow = TRUE)

time_mat <- data.frame(rbind(qp.time, gd.time))
time_mat <- data.frame(time_mat, rep(c("QP","CGD"), each = n.sim))
colnames(time_mat) <- c(paste0(sample_size / 1e3, "K"), "method")

time_long <- gather(time_mat, sample_size, time, 1:length(sample_size), factor_key = TRUE)
time_long$sample_size <- factor(time_long$sample_size, levels = paste0(sample_size / 1e3, "K"))

df_line_qp <- time_long %>% filter(method == "QP") %>% group_by(sample_size) %>%
  summarise(mean_qp = mean(time), sd_qp = sd(time))

df_line_cgd <- time_long %>% filter(method == "CGD") %>% group_by(sample_size) %>% summarise(mean_cgd = mean(time), sd_cgd = sd(time))

acc_df_all <- do.call(rbind, acc_list)
acc_long <- acc_df_all %>% mutate(sample_size = factor(paste0(n/1e3, "K"), levels = paste0(sample_size / 1e3, "K")))

#========================================================#
# Plotting: Execution time curve + boxplot of subspace distance difference (CGD - QP)
#========================================================#
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

diff_df <- acc_long %>%
  select(sample_size, method, dist) %>%
  group_by(sample_size, method) %>%
  summarise(dist_values = list(dist), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = method, values_from = dist_values) %>%
  mutate(diff_list = map2(CGD, QP, ~ unlist(.x) - unlist(.y))) %>%
  tidyr::unnest(diff_list) %>%
  rename(diff = diff_list)

x_pos <- 1:length(sample_size)

plot(x_pos, df_line_qp$mean_qp, col = 'blue', lty = 2, type = 'b',
     xlab = "Sample size", ylab = "Execution time (sec)",
     lwd = 2.2, xaxt = 'n', cex.lab = 1.2, pch = 19,
     ylim = c(0, max(df_line_qp$mean_qp, df_line_cgd$mean_cgd)))
lines(x_pos, df_line_cgd$mean_cgd, col = 'red', lty = 4, type = 'b', lwd = 2.2, pch = 9)

axis(1, at = x_pos, labels = paste0(sample_size / 1e3, "K"))
grid(nx = NULL, ny = NULL, lty = 2, col = "gray85", lwd = 1)
legend("topleft", legend = c("QP", "CGD", "Frobenius distance"),
       col = c("blue", "red", adjustcolor("gray50", alpha.f = 0.4)),
       lty = c(2,4,0), pch = c(19,9,15), cex = 1.1, lwd = c(2,2,6),
       pt.cex = c(1.3, 1.3, 1.5))

par(new = TRUE)
boxplot(diff ~ as.character(sample_size),
        data = diff_df,
        at = x_pos,
        width = rep(0.3, length(sample_size)),
        col = adjustcolor("gray60", alpha.f = 0.12),   # ✨ pastel transparent tone
        border = adjustcolor("gray70", alpha.f = 0.2), # thin, faint border
        yaxt = "n", xaxt = "n", ylab = "", xlab = "",
        outline = FALSE, boxwex = 0.8,
        ylim = range(diff_df$diff))

axis(4, las = 1, col.axis = "black")
mtext("Frobenius distance", side = 4, line = 3, col = "black")




# #===============================================#
# #### For Figure 1 right panel: Kernel PSVM #####
# #===============================================#

#========================================================#
# Simulation setup
#========================================================#
n.sim <- 100
sample_size <- c(300, 500, 700, 1000, 1200, 1500, 1800, 2000, 2500, 3000)

time_df <- matrix(0, ncol = length(sample_size), nrow = 2)
time_list <- vector("list", n.sim)
acc_list  <- vector("list", n.sim)

#========================================================#
# Main simulation
#========================================================#
for (j in 1:n.sim) {
  acc_df <- data.frame()
  for (ii in seq_along(sample_size)) {
    set.seed(j + ii)
    n <- sample_size[ii]; p <- 5; H <- 5; lambda <- 1
    eps <- 1e-5; max.iter <- 20; h <- 1e-5; delta <- 0.1
    x <- matrix(rnorm(n * p), n, p)
    err <- rnorm(n, 0, 0.2)
    fx <- x[, 1] / (0.5 + (x[, 2] + 1)^2)
    y  <- fx + err

    psi.gen <- get.psi(x, y, b = floor(length(y) / 4))
    Psi <- psi.gen$w; d <- 2

    #  QP
    tic_qp <- Sys.time()
    obj_qp <- npdr_no_gen_psi(x, y, obj = psi.gen, lambda, H=20, k = ncol(Psi))
    toc_qp <- Sys.time()

    # CGD
    tic_cgd <- Sys.time()
    obj_cgd <- npsdr_no_gen_psi(x, y, H, h, obj = psi.gen,
                                lambda, delta, eps, max.iter, loss = "svm")
    toc_cgd <- Sys.time()

    time_df[1, ii] <- as.numeric(toc_qp - tic_qp, units = "secs")
    time_df[2, ii] <- as.numeric(toc_cgd - tic_cgd, units = "secs")

    Z_qp  <- Psi %*% obj_qp$evector[, 1:d, drop=FALSE]
    Z_cgd <- Psi %*% obj_cgd$evectors[, 1:d, drop=FALSE]

    idx_tr <- sample(seq_len(n), floor(0.8 * n))
    idx_te <- setdiff(seq_len(n), idx_tr)

    R2_qp <- pred_R2(y, Z_qp, idx_tr, idx_te)
    R2_cgd <- pred_R2(y, Z_cgd, idx_tr, idx_te)
    consis <- pred_consistency(Z_qp, Z_cgd, y, idx_te)

    acc_df <- rbind(acc_df,
                    data.frame(n=n,
                               R2_QP=R2_qp, R2_CGD=R2_cgd,
                               Consistency=consis)
    )
    cat(sprintf("Rep %d | n=%d done (R2_QP=%.3f, R2_CGD=%.3f, corr=%.3f)\n",
                j, n, R2_qp, R2_cgd, consis))
  }
  time_list[[j]] <- time_df
  acc_list[[j]]  <- acc_df
}

#========================================================#
# Summary
#========================================================#
acc_all <- do.call(rbind, acc_list)

acc_summary <- acc_all %>%
  group_by(n) %>%
  summarise(
    mean_R2_QP  = mean(R2_QP),
    mean_R2_CGD = mean(R2_CGD),
    mean_consistency = mean(Consistency),
    sd_consistency = sd(Consistency)
  )

print(acc_summary)

#========================================================#
# Timing summary
#========================================================#
time <- unlist(time_list)
qp.time <- matrix(time[seq(1, length(time), by = 2)],
                  ncol = length(sample_size), byrow = TRUE)
gd.time <- matrix(time[seq(2, length(time), by = 2)],
                  ncol = length(sample_size), byrow = TRUE)

time_mat <- data.frame(rbind(qp.time, gd.time))
time_mat <- data.frame(time_mat, rep(c("QP", "CGD"), each = n.sim))
colnames(time_mat) <- c(as.character(sample_size), "method")

time_long <- tidyr::gather(time_mat, sample_size, time, 1:length(sample_size), factor_key = TRUE)
time_long$sample_size <- factor(time_long$sample_size, levels = as.character(sample_size))

df_line_qp <- time_long %>%
  filter(method == "QP") %>%
  group_by(sample_size) %>%
  summarise(mean_qp = mean(time), sd_qp = sd(time))

df_line_cgd <- time_long %>%
  filter(method == "CGD") %>%
  group_by(sample_size) %>%
  summarise(mean_cgd = mean(time), sd_cgd = sd(time))

#========================================================#
# Accuracy summary (for consistency)
#========================================================#
acc_df_all <- do.call(rbind, acc_list)
acc_long <- acc_df_all %>%
  mutate(sample_size = factor(as.character(n), levels = as.character(sample_size)))

##========================================================#
# Plotting: Execution time curve + boxplot of predictive consistency (CGD − QP)
#========================================================#
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

par(mar = c(5, 5, 5, 6.5))

x_pos <- 1:length(sample_size)

plot(x_pos, df_line_qp$mean_qp,
     col = 'blue', lty = 2, type = 'b',
     xlab = "Sample size", ylab = "Execution time (sec)",
     lwd = 2.2, xaxt = 'n', cex.lab = 1.2, pch = 19,
     ylim = c(0, max(df_line_qp$mean_qp, df_line_cgd$mean_cgd)))
lines(x_pos, df_line_cgd$mean_cgd,
      col = 'red', lty = 4, type = 'b', lwd = 2.2, pch = 9)

axis_labels <- paste0(sample_size / 1000, "K")
axis(1, at = x_pos, labels = axis_labels)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray85", lwd = 1)

legend("topleft",
       legend = c("QP", "CGD", "Correlation"),
       col = c("blue", "red", adjustcolor("gray40", alpha.f = 0.4)),
       lty = c(2, 4, 0), pch = c(19, 9, 15),
       cex = 1.1, lwd = c(2, 2, 6),
       pt.cex = c(1.3, 1.3, 1.5))

par(new = TRUE)

boxplot(Consistency ~ as.character(sample_size),
        data = acc_long,
        at = x_pos,
        width = rep(0.3, length(sample_size)),
        col = adjustcolor("gray50", alpha.f = 0.12),
        border = adjustcolor("gray50", alpha.f = 0.25),
        yaxt = "n", xaxt = "n", ylab = "", xlab = "",
        outline = FALSE, boxwex = 0.8,
        ylim = range(acc_long$Consistency, na.rm = TRUE))

right_ticks <- pretty(range(acc_long$Consistency, na.rm = TRUE))
right_labels <- format(right_ticks, scientific = FALSE, digits = 3, nsmall = 3)
axis(4, las = 1, col.axis = "black", at = right_ticks, labels = right_labels, cex.axis = 0.9)
mtext("Correlation", side = 4, line = 3, col = "black")




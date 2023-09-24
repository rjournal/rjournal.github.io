# Reproducible R code for Figure 1 on page 3
library(robflreg)
library(fda.usc)
set.seed(202)
# Generate a dataset with five functional predictors and 400
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the scalar-on-function regression model
sim.data <- generate.sf.data(n = 400, n.pred = 5, n.gp = 101, out.p = 0.1)
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
# Regression coefficient functions
coeffs <- sim.data$f.coef
# Plot the scalar response
out.indx <- sim.data$out.indx
plot(Y[-out.indx,], type = "p", pch = 16, xlab = "Index", ylab = "",
     main = "a)", ylim = range(Y))
points(out.indx, Y[out.indx,], type = "p", pch = 16, col = "grey")
# Plot the functional predictors
fX1 <- fdata(X[[1]], argvals = seq(0, 1, length.out = 101))
plot(fX1[-out.indx,], lty = 1, ylab = "", xlab = "", col = "black",
     main = "b)", mgp = c(2, 0.5, 0), ylim = range(fX1))
lines(fX1[out.indx,], lty = 1, col = "grey")
fX2 <- fdata(X[[2]], argvals = seq(0, 1, length.out = 101))
plot(fX2[-out.indx,], lty = 1, ylab = "", xlab = "", col = "black",
     main = "c)", mgp = c(2, 0.5, 0), ylim = range(fX2))
lines(fX2[out.indx,], lty = 1, col = "grey")
fX3 <- fdata(X[[3]], argvals = seq(0, 1, length.out = 101))
plot(fX3[-out.indx,], lty = 1, ylab = "", xlab = "Grid point", col = "black",
     main = "d)", mgp = c(2, 0.5, 0), ylim = range(fX3))
lines(fX3[out.indx,], lty = 1, col = "grey")
fX4 <- fdata(X[[4]], argvals = seq(0, 1, length.out = 101))
plot(fX4[-out.indx,], lty = 1, ylab = "", xlab = "Grid point", col = "black",
     main = "e)", mgp = c(2, 0.5, 0), ylim = range(fX4))
lines(fX4[out.indx,], lty = 1, col = "grey")
fX5 <- fdata(X[[5]], argvals = seq(0, 1, length.out = 101))
plot(fX5[-out.indx,], lty = 1, ylab = "", xlab = "Grid point", col = "black",
     main = "f)", mgp = c(2, 0.5, 0), ylim = range(fX5))
lines(fX5[out.indx,], lty = 1, col = "grey")






# Reproducible R code for Figure 2 on page 5
library(robflreg)
library(fda.usc)
set.seed(202)
# Generate a dataset with five functional predictors and 200
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the function-on-function regression model
sim.data <- generate.ff.data(n.pred = 5, n.curve = 200, n.gp = 101, out.p = 0.1)
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
# Regression coefficient functions
coeffs <- sim.data$f.coef
# Plot the scalar response
out.indx <- sim.data$out.indx
fY <- fdata(Y, argvals = seq(0, 1, length.out = 101))
plot(fY[-out.indx,], lty = 1, ylab = "", xlab = "",
     main = "Response", mgp = c(2, 0.5, 0), ylim = range(fY))
lines(fY[out.indx,], lty = 1, col = "grey")
# Plot the functional predictors
fX1 <- fdata(X[[1]], argvals = seq(0, 1, length.out = 101))
plot(fX1[-out.indx,], lty = 1, ylab = "", xlab = "",
     main = expression(X[1](s)), mgp = c(2, 0.5, 0), ylim = range(fX1))
lines(fX1[out.indx,], lty = 1, col = "grey")
fX2 <- fdata(X[[2]], argvals = seq(0, 1, length.out = 101))
plot(fX2[-out.indx,], lty = 1, ylab = "", xlab = "",
     main = expression(X[2](s)), mgp = c(2, 0.5, 0), ylim = range(fX2))
lines(fX2[out.indx,], lty = 1, col = "grey")
fX3 <- fdata(X[[3]], argvals = seq(0, 1, length.out = 101))
plot(fX3[-out.indx,], lty = 1, ylab = "", xlab = "Grid point",
     main = expression(X[3](s)), mgp = c(2, 0.5, 0), ylim = range(fX3))
lines(fX3[out.indx,], lty = 1, col = "grey")
fX4 <- fdata(X[[4]], argvals = seq(0, 1, length.out = 101))
plot(fX4[-out.indx,], lty = 1, ylab = "", xlab = "Grid point",
     main = expression(X[4](s)), mgp = c(2, 0.5, 0), ylim = range(fX4))
lines(fX4[out.indx,], lty = 1, col = "grey")
fX5 <- fdata(X[[5]], argvals = seq(0, 1, length.out = 101))
plot(fX5[-out.indx,], lty = 1, ylab = "", xlab = "Grid point",
     main = expression(X[5](s)), mgp = c(2, 0.5, 0), ylim = range(fX5))
lines(fX5[out.indx,], lty = 1, col = "grey")







# Reproducible R code for Figure 3 on page 7
library(robflreg)
# Generate a dataset with five functional predictors and 200
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the function-on-function regression model
set.seed(202)
sim.data <- generate.ff.data(n.pred = 5, n.curve = 200, n.gp = 101)
# Response variable
Y <- sim.data$Y
gpY <- seq(0, 1, length.out = 101) # grid points
# Perform robust functional principal component analysis on the response variable Y
rob.fpca <- getPCA(data = Y, nbasis = 20, ncomp = 5, gp = gpY, emodel = "robust")
# Principal components
PCs <- rob.fpca$PCAcoef
plot(PCs, xlab = "Grid point", ylab = "Values")







# Reproducible R code for Figure 4 on page 9
library(robflreg)
# Generate a dataset with three functional predictors and 400
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the scalar-on-function regression model
set.seed(202)
sim.data <- generate.sf.data(n = 400, n.pred = 3, n.gp = 101, out.p = 0.1)
# True parameter functions
true.b1 <- sim.data$f.coef[[1]]
true.b2 <- sim.data$f.coef[[2]]
true.b3 <- sim.data$f.coef[[3]]
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
gp <- rep(list(seq(0, 1, length.out = 101)), 3) # grid points of Xs
# Fit a functional principal component regression model for the generated data
# using the classical functional principal component analysis method:
classical.fit <- rob.sf.reg(Y, X, emodel = "classical", gp = gp)
# Fit a functional principal component regression model for the generated data
# using the robust functional principal component analysis method and tau estimator:
robust.fit <- rob.sf.reg(Y, X, emodel = "robust", fmodel = "MM", gp = gp)
# Estimated regression coefficient functions
classical.coefs <- get.sf.coeffs(classical.fit)
robust.coefs <- get.sf.coeffs(robust.fit)
# Estimated regression coefficient functions
plot_sf_coeffs(object = classical.coefs, b = 1)
lines(gp[[1]], robust.coefs$coefficients[[1]], col = "red")
lines(gp[[1]], true.b1, col = "blue", lwd = 2)
plot_sf_coeffs(object = classical.coefs, b = 2)
lines(gp[[2]], robust.coefs$coefficients[[2]], col = "red")
lines(gp[[2]], true.b2, col = "blue", lwd = 2)
plot_sf_coeffs(object = classical.coefs, b = 3)
lines(gp[[3]], robust.coefs$coefficients[[3]], col = "red")
lines(gp[[3]], true.b3, col = "blue", lwd = 2)







# Reproducible R code for Figure 5 on page 12
library(robflreg)
# Generate a dataset with three functional predictors and 200
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the function-on-function regression model
set.seed(202)
sim.data <- generate.ff.data(n.pred = 3, n.curve = 200, n.gp = 101)
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
gpY = seq(0, 1, length.out = 101) # grid points of Y
gpX <- rep(list(seq(0, 1, length.out = 101)), 3) # grid points of Xs
# Fit a functional principal component regression model for the generated data
# using the RFPCA and MM estimator:
model.fit <- rob.ff.reg(Y, X, model = "full", emodel = "robust",
                        fmodel = "MM", gpY = gpY, gpX = gpX)
# Estimated bivariate regression coefficient functions
coefs <- get.ff.coeffs(model.fit)
# Plot the bivariate regression coefficient function
plot_ff_coeffs(object = coefs, b = 1)
plot_ff_coeffs(object = coefs, b = 2)
plot_ff_coeffs(object = coefs, b = 3)








# Reproducible R code for Figure 6 on page 13
library(robflreg)
# Generate a dataset with five functional predictors and 200
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the function-on-function regression model
set.seed(202)
sim.data <- generate.ff.data(n.pred = 5, n.curve = 200, n.gp = 101, out.p = 0.1)
out.indx <- sim.data$out.indx
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
gpY = seq(0, 1, length.out = 101) # grid points of Y
gpX <- rep(list(seq(0, 1, length.out = 101)), 5) # grid points of Xs
# Perform classical functional principal component regression using least-squares
model.classical <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "classical",
                              gpY = gpY, gpX = gpX)
# Perform robust functional principal component regression using MM-estimator
model.MM <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust", fmodel = "MM",
                       gpY = gpY, gpX = gpX)
# Detect outliers using rob.out.detect function
rob.out.detect(object = model.classical, fplot = TRUE)
rob.out.detect(object = model.MM, fplot = TRUE)
sort(out.indx)









# Reproducible R code for the prediction of scalar-on-function linear
# regression model on page 14
library(robflreg)
# Generate a dataset with five functional predictors and 400
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the scalar-on-function regression model
set.seed(202)
sim.data <- generate.sf.data(n = 400, n.pred = 5, n.gp = 101, out.p = 0.1)
out.indx <- sim.data$out.indx
# Response variable
Y <- sim.data$Y
# Predictors
X <- sim.data$X
# Split the data into training and test samples.
indx.test <- sample(c(1:400)[-out.indx], 120)
indx.train <- c(1:400)[-indx.test]
Y.train <- Y[indx.train,]
Y.test <- Y[indx.test,]
X.train <- X.test <- list()
for(i in 1:5){
  X.train[[i]] <- X[[i]][indx.train,]
  X.test[[i]] <- X[[i]][indx.test,]
}
gp <- rep(list(seq(0, 1, length.out = 101)), 5) # grid points of Xs
# Perform classical functional principal component regression model using training samples
model.classical <- rob.sf.reg(Y.train, X.train, emodel = "classical", gp = gp)
# Perform robust functional principal component regression model
# using training samples and tau-estimator
model.tau <- rob.sf.reg(Y.train, X.train, emodel = "robust", fmodel = "tau", gp = gp)
# Predict the observations in Y.test using model.classical
pred.classical <- predict_sf_regression(object = model.classical, Xnew = X.test)
# Predict the observations in Y.test using model.tau
pred.tau <- predict_sf_regression(object = model.tau, Xnew = X.test)
# Compute mean squared errors for the test sample
round(mean((Y.test - pred.classical)^2), 4) # 2.49 (classical method)
round(mean((Y.test - pred.tau)^2), 4) # 1.1457 (tau method)









# Reproducible R code for the prediction of function-on-function linear
# regression model on page 15
library(robflreg)
# Generate a dataset with five functional predictors and 200
# observations at 101 equally spaced point in the interval [0, 1]
# for each variable for the function-on-function regression model
set.seed(202)
sim.data <- generate.ff.data(n.pred = 5, n.curve = 200, n.gp = 101, out.p = 0.1)
out.indx <- sim.data$out.indx
# Response variable
Y <- sim.data$Y
# Predictor variables
X <- sim.data$X
# Split the data into training and test samples.
indx.test <- sample(c(1:200)[-out.indx], 60)
indx.train <- c(1:200)[-indx.test]
Y.train <- Y[indx.train,]
Y.test <- Y[indx.test,]
X.train <- X.test <- list()
for(i in 1:5){
  X.train[[i]] <- X[[i]][indx.train,]
  X.test[[i]] <- X[[i]][indx.test,]
}
gpY = seq(0, 1, length.out = 101) # grid points of Y
gpX <- rep(list(seq(0, 1, length.out = 101)), 5) # grid points of Xs
# Perform classical functional principal component regression model using training samples
model.classical <- rob.ff.reg(Y = Y.train, X = X.train, model = "full",
                              emodel = "classical", gpY = gpY, gpX = gpX)
# Perform robust functional principal component regression
# using training samples and MM-estimator
model.MM <- rob.ff.reg(Y = Y.train, X = X.train, model = "full", emodel = "robust",
                       fmodel = "MM", gpY = gpY, gpX = gpX)
# Predict the functions in Y.test using model.classical
pred.classical <- predict_ff_regression(object = model.classical, Xnew = X.test)
# Predict the functions in Y.test using model.MM
pred.MM <- predict_ff_regression(object = model.MM, Xnew = X.test)
# Compute mean squared errors for the test sample
round(mean((Y.test - pred.classical)^2), 4) # 1.5705 (classical method)
round(mean((Y.test - pred.MM)^2), 4) # 0.8166 (MM method)







# Reproducible R code for Figure 7 on page 17
library(robflreg)
library(fda.usc)
data("MaryRiverFlow")
X <- MaryRiverFlow[1:2188,]
Y <- apply(MaryRiverFlow[2:2189,], 1, max)
Day <- seq(as.Date("2009/01/01"), as.Date("2014/12/28"), by="days")
plot(Day, Y, type = "p", pch = 16, ylab = "Level (m)", main = "Response")
X <- fdata(X, argvals = 1:24)
plot(X, lty = 1, ylab = "", xlab = "Hour", col = "black",
     main = "Predictor", mgp = c(2, 0.5, 0))








# Reproducible R code for the prediction of scalar-on-function linear
# regression model for the MaryRiverFlow dataset on page 17
library(robflreg)
data("MaryRiverFlow")
MaryRiverFlow <- as.matrix(MaryRiverFlow)
X <- list(MaryRiverFlow[1:2188,])
Y <- apply(MaryRiverFlow[2:2189,], 1, max)
gp <- rep(list(1:24), 1)
MSPE <- matrix(, ncol = 5, nrow = 10)
colnames(MSPE) <- c("classical","LTS","MM","S","tau")
starting_value <- 2178
for(i in 1:10){
  # Divide the data into training and test samples
  Y.train <- Y[1:starting_value]
  Y.test <- Y[(starting_value+1)]
  X.train <- list(X[[1]][1:starting_value,])
  X.test <- list(matrix(X[[1]][(starting_value+1),], nrow = 1))
  # Perform classical and robust functional principal component regression models
  model.classical <- rob.sf.reg(Y.train, X.train, emodel = "classical", gp = gp)
  model.LTS <- rob.sf.reg(Y.train, X.train, emodel = "robust", fmodel = "LTS", gp = gp)
  model.MM <- rob.sf.reg(Y.train, X.train, emodel = "robust", fmodel = "MM", gp = gp)
  model.S <- rob.sf.reg(Y.train, X.train, emodel = "robust", fmodel = "S", gp = gp)
  model.tau <- rob.sf.reg(Y.train, X.train, emodel = "robust", fmodel = "tau", gp = gp)
  # Predict the maximum river flow measurement of the current day
  pred.classical <- predict_sf_regression(object = model.classical, Xnew = X.test)
  pred.LST <- predict_sf_regression(object = model.LTS, Xnew = X.test)
  pred.MM <- predict_sf_regression(object = model.MM, Xnew = X.test)
  pred.S <- predict_sf_regression(object = model.tau, Xnew = X.test)
  pred.tau <- predict_sf_regression(object = model.tau, Xnew = X.test)
  # Record the MSPE values
  MSPE[i,1] <- (Y.test - pred.classical)^2
  MSPE[i,2] <- (Y.test - pred.LST)^2
  MSPE[i,3] <- (Y.test - pred.MM)^2
  MSPE[i,4] <- (Y.test - pred.S)^2
  MSPE[i,5] <- (Y.test - pred.tau)^2
  starting_value <- starting_value + 1
}
apply(MSPE, 2, mean); apply(MSPE, 2, sd)









# Reproducible R code for the prediction of function-on-function linear
# regression model for the MaryRiverFlow dataset on page 17
library(robflreg)
data("MaryRiverFlow")
MaryRiverFlow <- as.matrix(MaryRiverFlow)
# The following function is used to obtain the functional response and predictor
var_fun = function(data,order){
  n = dim(data)[1]
  y = data[((order+1):n),]
  x=list()
  a=1
  b=order
  for(i in 1:order){
    x[[i]] = data[(a:(n-b)),]
    a = a+1
    b = b-1
  }
  return(list(x=x,y=y))
}
# Grid points for the functional response and predictor
gpY <- 1:24 # grid points of Y
gpX <- rep(list(1:24), 1) # grid points of Xs
MSPE <- matrix(, ncol = 6, nrow = 10)
colnames(MSPE) <- c("classical","MCD","MLTS","MM","S","tau")
starting_value <- 2179
# In two cases (when i = 3 and i = 6) the covariance is not decomposed.
# Thus, try() is used to ignore these two cases.
for(i in 1:10){
  try({
    data.i <- MaryRiverFlow[1:starting_value,]
    # Obtain the functional response and predictor
    XY = var_fun(data=data.i, order=1)
    X = XY$x
    Y = XY$y
    # Perform classical and robust functional principal component regression models
    model.classical <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "classical",
                                  gpY = gpY, gpX = gpX)
    model.MCD <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust",
                            fmodel = "MCD", gpY = gpY, gpX = gpX)
    model.MLTS <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust",
                             fmodel = "MLTS", gpY = gpY, gpX = gpX)
    model.MM <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust",
                           fmodel = "MM", gpY = gpY, gpX = gpX)
    model.S <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust",
                          fmodel = "S", gpY = gpY, gpX = gpX)
    model.tau <- rob.ff.reg(Y = Y, X = X, model = "full", emodel = "robust",
                            fmodel = "tau", gpY = gpY, gpX = gpX)
    Xnew = list(t(as.matrix(Y[dim(Y)[1],])))
    # Predict the maximum river flow measurement of the current day
    predict.classical <- predict_ff_regression(object = model.classical, Xnew = Xnew)
    predict.MCD <- predict_ff_regression(object = model.MCD, Xnew = Xnew)
    predict.MLTS <- predict_ff_regression(object = model.MLTS, Xnew = Xnew)
    predict.MM <- predict_ff_regression(object = model.MM, Xnew = Xnew)
    predict.S <- predict_ff_regression(object = model.S, Xnew = Xnew)
    predict.tau <- predict_ff_regression(object = model.tau, Xnew = Xnew)
    # Record the MSPE values
    MSPE[i,1] <- mean((predict.classical - MaryRiverFlow[starting_value+1,])^2)
    MSPE[i,2] <- mean((predict.MCD - MaryRiverFlow[starting_value+1,])^2)
    MSPE[i,3] <- mean((predict.MLTS - MaryRiverFlow[starting_value+1,])^2)
    MSPE[i,4] <- mean((predict.MM - MaryRiverFlow[starting_value+1,])^2)
    MSPE[i,5] <- mean((predict.S - MaryRiverFlow[starting_value+1,])^2)
    MSPE[i,6] <- mean((predict.tau - MaryRiverFlow[starting_value+1,])^2)
    starting_value <- starting_value +1
  },silent=T)
}
apply(MSPE, 2, mean, na.rm=TRUE); apply(MSPE, 2, sd, na.rm=TRUE)

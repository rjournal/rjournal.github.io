#######################################################################################################################
# R code for the paper entitled: "A Software Tool for Sparese Estimation of a General Class of High-dimensional GLMs" #
# Authors: H. Pazira, L. Augugliaro and E.C. Wit                                                                      #
# submitted to "The R Journal"                                                                                        #
# R code author: H. Pazira (2021) <h.pazira@amsterdamumc.nl>                                                          #
#######################################################################################################################

###########################################
# Section 3.3. An example of a Gamma GLM  #
###########################################

# clear
rm(list=ls())
gc()

install.packages("dglars")
library("dglars")

set.seed(11235)
n <- 50
p <- 100
s <- 2
X <- matrix(runif(n = n * p), n, p)
bs <- rep(2, s)
Xs <- X[, 1:s]
eta <- drop(0.5 + Xs %*% bs)
mu <- Gamma("log")$linkinv(eta)
shape <- 1
phi <- 1 / shape
y <- rgamma(n, shape = shape, scale = mu * phi)
fit <- dglars(y ~ X, family = Gamma("log"), control = list(algorithm = "pc", method = "dgLARS", g0 = 0.5))

set.seed(11235)
summary(fit, type = "BIC", phi = "grcv", control = list(g0 = 0.5))

set.seed(11235)
g <- fit$g
phi.grcv <- phihat(fit, type = "grcv", control = list(g0 = 0.5))
phi.pear <- phihat(fit, type = "pearson")
phi.dev <- phihat(fit, type = "deviance")
phi.mle <- phihat(fit, type = "mle")
path <- cbind(g, phi.pear, phi.dev, phi.mle, phi.grcv)
print(path, digits = 4)

set.seed(11235)
new_g <- seq(range(fit$g)[2], range(fit$g)[1], by = -0.5)
phi.grcv <- phihat(fit, g = new_g, type = "grcv", control = list(g0 = 0.5))
phi.pear <- phihat(fit, g = new_g, type = "pearson")
phi.dev <- phihat(fit, g = new_g, type = "deviance")
phi.mle <- phihat(fit, g = new_g, type = "mle")
path <- cbind(new_g, phi.pear, phi.dev, phi.mle, phi.grcv)
print(path, digits = 4)

summary(fit, type = "BIC", phi = "pearson")

################################################
# Section 3.4. Comparing PC and IPC Algorithms #
################################################

set.seed(11235)
n <- 100
p <- 5
X <- matrix(abs(rnorm(n * p)), n, p)
b <- 1:2
eta <- drop(b[1] + (X[, 1] * b[2]))
mu <- poisson()$linkinv(eta)
y <- rpois(n, mu)

fit_ipc <- dglars(y ~ X, family = poisson, control = list(algorithm = "pc"))

detach(name = "package:dglars", unload = TRUE)
remove.packages(pkgs = "dglars")
Old_dglars <- "https://cran.r-project.org/src/contrib/Archive/dglars/dglars_1.0.5.tar.gz"
install.packages(Old_dglars, repos = NULL, type = "source")
library("dglars")

fit_pc <- dglars(y ~ X, family = "poisson", control = list(algorithm = "pc"))

fit_pc

fit_pc$np

fit_pc$g

fit_ipc

fit_ipc$np

fit_ipc$g

detach(name = "package:dglars", unload = TRUE)
remove.packages(pkgs = "dglars")


################################################
# Section 4. Simulation Studies                #
################################################

# clear
rm(list=ls())
gc()

install.packages("DescTools")
install.packages(pkgs = "dglars")
library(dglars)
library(MASS)
library(DescTools)

nsim <- 100L
n <- c(50L, 200L)
p <- c(100L, seq(from = 1000L, to = 7000L, length = 4L),10000L)
rho <- c(0, 0.5)
b0 <- 1
b <- seq_len(3L)
A <- seq_len(3L)
out_n <- array(data = 0, dim = c(length(n), length(p), length(rho), nsim, 2L, 2L),
               dimnames = list(n = n, p = p, rho = rho, i = seq_len(nsim), algorithm = c("ipc", "pc"), measures = c("time", "q")))

## simulation study with dglars v.2.1.6
set.seed(11235)
for(h in seq_len(length(n))) {
    nh <- n[h]
    for(k in seq_len(length(p))) {
        pk <- p[k]
        for(j in seq_len(length(rho))) {
            rhoj <- rho[j]
            mu <- rep(0, pk)
            Sigma <- outer(seq_len(pk), seq_len(pk), function(i, j) rhoj^abs(i - j))
            X <- mvrnorm(n = nh, mu = mu, Sigma = Sigma)
            eta <- b0 + drop(X[, A] %*% b)
            prob <- binomial()$linkinv(eta)
            for(i in seq_len(nsim)) {
                y <- rbinom(n = nh, size = 1L, prob = prob)
                out[h, k, j, i, "ipc", "time"] <- system.time(out.dglars <- dglars.fit(X = X, y = y, family = binomial, control = list(g0 = 0.2)))[3L]
                out[h, k, j, i, "ipc", "q"] <- out.dglars$np
                cat("simulation study with h =", h, "k =", k, "j =", j, "i =", i,"completed\n")
            }
        }
    }
}
save.image(file = "output_simulation.RData")

## simulation study with dglars v.1.0.5
detach(name = "package:dglars", unload = TRUE)
remove.packages(pkgs = "dglars")
Old_dglars <- "https://cran.r-project.org/src/contrib/Archive/dglars/dglars_1.0.5.tar.gz"
install.packages(Old_dglars, repos = NULL, type = "source")

library(dglars)

set.seed(11235)
for(h in seq_len(length(n))) {
    nh <- n[h]
    for(k in seq_len(length(p))) {
        pk <- p[k]
        for(j in seq_len(length(rho))) {
            rhoj <- rho[j]
            mu <- rep(0, pk)
            Sigma <- outer(seq_len(pk), seq_len(pk), function(i, j) rhoj^abs(i - j))
            X <- mvrnorm(n = nh, mu = mu, Sigma = Sigma)
            eta <- b0 + drop(X[, A] %*% b)
            prob <- binomial()$linkinv(eta)
            for(i in seq_len(nsim)) {
                y <- rbinom(n = nh, size = 1L, prob = prob)
                out[h, k, j, i, "pc", "time"] <- system.time(out.dglars <- dglars.fit(X = X, y = y, family = "binomial", control = list(g0 = 0.2)))[3L]
                out[h, k, j, i, "pc", "q"] <- out.dglars$np
                cat("simulation study with h =", h, "k =", k, "j =", j, "i =", i,"completed\n")
            }
          }
    }
}
save.image(file = "output_simulation.RData")
  
out2 <- array(data = 0, dim = c(length(n), length(p), length(rho), 2L, 2L, 2L),
             dimnames = list(n = n, p = p, rho = rho, algorithm = c("ipc", "pc"), 
             measures = c("time", "q"), stat = c("mean", "sd")))

for(h in seq_len(length(n))) {
	for(k in seq_len(length(p))) {
		for(j in seq_len(length(rho))) {
			out2[h, k, j, "ipc", "q", "mean"]    <- mean(out[h, k, j, , "ipc", "q"], trim = 0.05, na.rm = T)
			out2[h, k, j, "ipc", "q", "sd"]      <- sd(Trim(out[h, k, j, , "ipc", "q"], trim = 0.05), na.rm = T)
			out2[h, k, j, "ipc", "time", "mean"] <- mean(out[h, k, j, , "ipc", "time"], trim = 0.05 ,na.rm = T)
			out2[h, k, j, "ipc", "time", "sd"]   <- sd(Trim(out[h, k, j, , "ipc", "time"], trim = 0.05), na.rm = T)
			out2[h, k, j, "pc", "q", "mean"]	   <- mean(out[h, k, j, , "pc", "q"], trim = 0.05 ,na.rm = T)
			out2[h, k, j, "pc", "q", "sd"]       <- sd(Trim(out[h, k, j, , "pc", "q"], trim = 0.05), na.rm = T)
			out2[h, k, j, "pc", "time", "mean"]	 <- mean(out[h, k, j, , "pc", "time"], trim = 0.05, na.rm = T)
			out2[h, k, j, "pc", "time", "sd"]    <- sd(Trim(out[h, k, j, , "pc", "time"], trim = 0.05), na.rm = T)
		}
	}
}
save.image(file = "output_simulation.RData")

library("xtable")
ftable(round(out2,3), row.vars = c("rho", "p", "stat"), col.vars = c("n", "algorithm", "measures"))

# Plots
pdf(file = "n50_rho0.pdf")
matplot(p, out2[1, , 1, , "q", "mean"], 
		type = "b", pch = 20, col = 1,
		xlab = "Number of predictors",
		ylab = "Number of points",
		main = expression(paste("n = 50 and ", rho, " = 0")),
		axes = FALSE, xlim = c(100, 7000),
		cex = 2, cex.lab = 1.7, cex.main = 1.7)
axis(1, at = p, cex.axis = 1.5)
axis(2)
legend("bottomright", legend = c("IPC algorithm", " PC algorithm"), 
		lty = 1:2, cex = 1.5)
dev.off()

pdf(file = "n200_rho0.pdf")
matplot(p, out2[2, , 1, , "q", "mean"], 
		type = "b", pch = 20, col = 1,
		xlab = "Number of predictors",
		ylab = "Number of points",
		main = expression(paste("n = 200 and ", rho, " = 0")),
		axes = FALSE, xlim = c(100, 7000),
		cex = 2, cex.lab = 1.7, cex.main = 1.7)
axis(1, at = p, cex.axis = 1.5)
axis(2)
legend("bottomright", legend = c("IPC algorithm", " PC algorithm"), 
		lty = 1:2, cex = 1.5)
dev.off()

pdf(file = "n50_rho05.pdf")
matplot(p, out2[1, , 2, , "q", "mean"], 
		type = "b", pch = 20, col = 1,
		xlab = "Number of predictors",
		ylab = "Number of points",
		main = expression(paste("n = 50 and ", rho, " = 0.5")),
		axes = FALSE, xlim = c(100, 7000),
		cex = 2, cex.lab = 1.7, cex.main = 1.7)
axis(1, at = p, cex.axis = 1.5)
axis(2)
legend("bottomright", legend = c("IPC algorithm", " PC algorithm"), 
		lty = 1:2, cex = 1.5)
dev.off()

pdf(file = "n200_rho05.pdf")
matplot(p, out2[2, , 2, , "q", "mean"], 
		type = "b", pch = 20, col = 1,
		xlab = "Number of predictors",
		ylab = "Number of points",
		main = expression(paste("n = 200 and ", rho, " = 0.5")),
		axes = FALSE, xlim = c(100, 7000), ylim = c(60, 250),
		cex = 2, cex.lab = 1.7, cex.main = 1.7)
axis(1, at = p, cex.axis = 1.5)
axis(2)
legend("bottomright", legend = c("IPC algorithm", " PC algorithm"), 
		lty = 1:2, cex = 1.5)
dev.off()


###################################################
# Section 5. Application to Example Data          #
###################################################

install.packages(pkgs = "lars")
data("diabetes", package = "lars")

# the newest version of the dglars package is needed:
install.packages("dglars") 
library("dglars")

set.seed(11235)
cv_diabetes <- cvdglars(y ~ x, family = inverse.gaussian("log"), data = diabetes)
cv_diabetes

cv_diabetes$formula_cv

diabetes_dglars <- dglars(y ~ x, family = inverse.gaussian("log"), data = diabetes)
set.seed(11235)
summary(diabetes_dglars, type = "BIC", phi = "grcv")

set.seed(11235)
grcv(diabetes_dglars, type = "BIC")

system.time(diabetes_dglars_ipc <- dglars(y ~ x, family = inverse.gaussian("log"), data = diabetes, control = list(method = "dgLASSO")))

diabetes_dglars_ipc$np

##########
# The End!

#tools::texi2pdf("pazira.tex")

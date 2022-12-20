# Replication R codes

############
# Examples #
############

############################################################################*
# Logistic model with a single predictor ----
############################################################################*
library(ICAOD)
log1 <- locally(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
                predvars = "x", parvars = c("b0", "b1"),
                family = "binomial", lx = 0, ux = 6, iter = 40, k = 2,
                inipars = c(-4, 1.3333), ICA.control = list(rseed = 1))
print(log1)
log1$design
log1$out
pdf(file = "log1.pdf")
plot(log1)
dev.off()

leff(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
     predvars = "x", parvars = c("b0", "b1"),
     family = "binomial", inipars = c(-4, 1.3333),
     x1 = c(0:6), w1 = rep(1/7, 7),
     x2 = log1$evol[[40]]$x, w2 = log1$evol[[20]]$w)


log2 <- locally(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
                predvars = "x", parvars = c("b0", "b1"),
                family = "binomial", lx = 0, ux = 6, iter = 40, k = 2,
                inipars = c(-4, 1.3333),
                ICA.control = list(rseed = 1,
                                   checkfreq = 20,
                                   stop_rule = "equivalence",
                                   stoptol = .99))

print(log2)
log2$design

log3 <- locally(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
                predvars = "x", parvars = c("b0", "b1"),
                family = "binomial", lx = 0, ux = 6, iter = 40,
                x = c(1, 2, 3),
                inipars = c(-4, 1.3333),
                ICA.control = list(rseed = 1, checkfreq = Inf))
print(log3)
pdf(file = "log3.pdf")
plot(log3)
dev.off()

log4 <- minimax(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
                predvars = "x", parvars = c("b0", "b1"),
                family = "binomial",
                lx = 0, ux = 6, lp = c(-6, .5), up = c(-2, 2),
                iter = 200
                , k = 2,
                ICA.control = list(rseed = 1,
                                   checkfreq = 50,
                                   stop_rule = "equivalence",
                                   stoptol = .99),
                crt.minimax.control = list(optslist = list(maxeval = 200)))

print(log4)
pdf(file = "log4.pdf")
plot(log4)
dev.off()

log5 <- minimax(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
                predvars = "x", parvars = c("b0", "b1"),
                family = "binomial",
                lx = 0, ux = 6, lp = c(-6, .5), up = c(-2, 2),
                iter = 500,
                k = 3,
                ICA.control = list(rseed = 1,
                                   checkfreq = 50,
                                   stop_rule = "equivalence",
                                   stoptol = .99),
                crt.minimax.control = list(optslist = list(maxeval = 200)))
print(log5)
log5$design
pdf(file = "log5.pdf")
plot(log5)
dev.off()

meff(formula = ~exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)),
     predvars = "x", parvars = c("b0", "b1"),
     family = "binomial",
     lp = c(-6, .5), up = c(-2, 2),
     x1 = c(0:6), w1 = rep(1/7, 7),
     x2 = log5$evol[[20]]$x,
     w2 = log5$evol[[20]]$w)

############################################################################*
# Sigmiod Emax model----
############################################################################*
### uniform prior
prior1 <- uniform(lower = c(4, 11, 100, 5), upper = c(8, 15, 130, 9))
sig1 <- bayes(formula = ~b1 + (b2-b1) * x ^b4/(x^b4 + b3^b4),
              predvars = "x",
              parvars = c("b1", "b2", "b3", "b4"),
              lx = .001, ux = 1000, k = 5, iter = 400,
              prior = prior1,
              ICA.control = list(rseed = 1, checkfreq = Inf))
print(sig1)
sig1$design
pdf(file = "sig1.pdf")
plot(sig1)
dev.off()

beff(formula = ~b1 + (b2-b1) * x ^b4/(x^b4 + b3^b4),
     predvars = "x",
     parvars = c("b1", "b2", "b3", "b4"),
     prior = prior1,
     x1 = c(.001,seq(100, 1000, by = 100)),
     w1 = rep(1/11, 11),
     x2 = sig1$evol[[400]]$x, w2 = sig1$evol[[400]]$w)

#### robust
parset1 <- matrix(c(4, 11, 100, 5,
                    5, 12, 110, 6,
                    6, 13, 120, 7,
                    8, 15, 130, 9,
                    12, 30, 160, 13),
                  nrow = 5, byrow = TRUE)

sig2 <- robust(formula = ~b1 + (b2-b1) * x ^b4/(x^b4 + b3^b4),
               predvars = "x",
               parvars = c("b1", "b2", "b3", "b4"),
               lx = .001, ux = 1000, k = 6, iter = 400,
               parset = parset1,
               prob = rep(1/5, 5),
               ICA.control = list(rseed = 1, checkfreq = Inf))
print(sig2)
sig2$design
pdf(file = "sig2.pdf")
plot(sig2)
dev.off()

############################################################################*
# User-Specified optimality criteria----
############################################################################*
c_opt <-function(x, w, a, b, fimfunc){
  gam <- log(.95/(1-.95))
  M <- fimfunc(x = x, w = w, a = a, b = b)
  c <- matrix(c(1, -gam * b^(-2)), nrow = 1)
  B <- t(c) %*% c
  sum(diag(B %*% solve(M)))
}

c_sens <- function(xi_x, x, w, a, b, fimfunc){
  gam <- log(.95/(1-.95))
  M <- fimfunc(x = x, w = w, a = a, b = b)
  M_inv <- solve(M)
  M_x <- fimfunc(x = xi_x, w = 1, a = a, b = b)
  c <- matrix(c(1, -gam * b^(-2)), nrow = 1)
  B <- t(c) %*% c
  sum(diag(B %*% M_inv %*% M_x %*%  M_inv)) - sum(diag(B %*% M_inv))
}

twoPL1 <- locally(formula = ~1/(1 + exp(-b * (x-a))), predvars = "x",
                  parvars = c("a", "b"), family = "binomial",
                  lx = -1, ux = 1, inipars = c(0, 7),
                  iter = 100,
                  k = 2,
                  crtfunc = c_opt, sensfunc = c_sens,
                  ICA.control = list(rseed = 1, checkfreq = Inf))
print(twoPL1)
twoPL1$design
pdf(file = "twoPL1.pdf")
plot(twoPL1)
dev.off()

## bayesian
c_opt_vec <-function(x, w, a, b, fimfunc){
  gam <- log(.95/(1-.95))
  M <- fimfunc(x = x, w = w, a = a, b = b)
  B <- sapply(1:length(M), FUN = function(i)
    matrix(c(1, -gam * b[i]^(-2)), ncol= 1) %*%
      matrix(c(1, -gam * b[i]^(-2)), nrow = 1), simplify = FALSE)
  sapply(1:length(M), FUN = function(i)
    sum(diag(B[[i]] %*% solve(M[[i]]))))
}

c_sens_vec <- function(xi_x, x, w, a, b, fimfunc){
  gam <- log(.95/(1-.95)) # LD .95
  M <- fimfunc(x = x, w = w, a = a, b = b)
  M_inv <- lapply(M , FUN = function(FIM) solve(FIM))
  M_x <- fimfunc(x = xi_x, w = 1, a = a, b = b)
  B <- sapply(1:length(M), FUN = function(i)
    matrix(c(1, -gam * b[i]^(-2)), ncol= 1) %*%
      matrix(c(1, -gam * b[i]^(-2)), nrow = 1), simplify = FALSE)
  sapply(1:length(M), FUN = function(i)
    sum(diag(B[[i]] %*% M_inv[[i]] %*% M_x[[i]] %*% M_inv[[i]])) -
      sum(diag(B[[i]] %*% M_inv[[i]])))
}

twoPL2 <- bayes(formula = ~1/(1 + exp(-b * (x-a))), predvars = "x",
                parvars = c("a", "b"), family = "binomial",
                lx = -1, ux = 1,
                prior = uniform(lower = c(-.3, 6), upper  = c(.3, 8)),
                iter = 100,
                k = 3,
                crtfunc = c_opt_vec,
                sensfunc = c_sens_vec,
                ICA.control = list(rseed = 1, ncount = 60, checkfreq = Inf),
                sens.bayes.control = list(cubature = list(maxEval = 100)))
print(twoPL2)
twoPL2$design
pdf(file = "twoPL2.pdf")
plot(twoPL2,  sens.bayes.control = list(method= "cubature",cubature = list(maxEval = 10000)))
dev.off()

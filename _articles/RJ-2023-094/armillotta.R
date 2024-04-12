###############################################################################
## This file contains the code used in the paper "Inference for Network Count
## Time Series with the R Package PNAR"
## by Mirko Armillotta, Michail Tsagris and Konstantinos Fokianos.
##
## Contents:
##
## Section: Influenza data
## Section: Revisiting the influenza data
## Section: Simulating network count time series
##
## Note: R version 4.3.1 used throughout.
##
###############################################################################

###############################################################################
#
# Section: Influenza data
#
###############################################################################


## load required packages
library(PNAR)
library(surveillance)

## load the data
data(fluBYBW)

flu <- fluBYBW@observed      ### data matrix time series 416 X 140
A_flu <- fluBYBW@neighbourhood                   ### neighbourhood matrix 140 X 140

pop <- as.matrix(t(fluBYBW@populationFrac)[,1])  ### fraction of population for each district 140 X 1


# Plots in Figure 1

plot(fluBYBW, type = observed ~ unit, tps = 1:12, main = "Total cases per 100,000 inhabitats during 2007:Q1",
     total.args = list(label = "", x = 10, y = -10),
     population = fluBYBW@map$X31_12_07 / 100000 )

plot(fluBYBW, type = observed ~ unit, tps = 13:24, main = "Total cases per 100,000 inhabitats during 2007:Q2",
     total.args = list(label = "", x = 10, y = -10),
     population = fluBYBW@map$X31_12_07 / 100000 )


plot(fluBYBW, type = observed ~ unit, tps = 25:36, main = "Total cases per 100,000 inhabitats during 2007:Q3",
     total.args = list(label = "", x = 10, y = -10),
     population = fluBYBW@map$X31_12_07 / 100000 )


plot(fluBYBW, type = observed ~ unit, tps = 37:48, main = "Total cases per 100,000 inhabitats during 2007:Q4",
     total.args = list(label = "", x = 10, y = -10),
     population = fluBYBW@map$X31_12_07 / 100000 )


## Estimation of linear PNAR model with 1-2 lags

est1.z <- lin_estimnarpq(y = flu, W = A_flu, p = 1, Z = pop)
est2.z <- lin_estimnarpq(y = flu, W = A_flu, p = 2, Z = pop)

summary(est1.z) # to see estimated coefficients and information criteria in Table 1
summary(est2.z) # to see estimated coefficients and information criteria in Table 1


# Lag order selection


# Plot of Figure 2

par(mfrow=c(1,1))
lin_ic_plot(y = flu, W = A_flu, p = 1:10, Z = pop, ic = "AIC")

est9.z <- lin_estimnarpq(y = flu, W = A_flu, p = 9, Z = pop)

summary(est9.z) # to see estimated coefficients in the Appendix


# Comparison of estimation with surveillance package

# PNAR
est1 <- lin_estimnarpq(y = flu, W = A_flu, p = 1)
summary(est1)

# surveillance
stsObj <- fluBYBW
ni <- rowSums(neighbourhood(stsObj))
control <- list( ar = list(f = ~ 1),
                 ne = list(f = ~ 1, weights = neighbourhood(stsObj) == 1,
                           offset = matrix(1/ni, nrow(stsObj), ncol(stsObj), byrow=TRUE)),
                 end = list(f = ~ 1),
                 family = "Poisson", keep.terms = TRUE)
fit1 <- hhh4(stsObj, control)
coefSE <- coef(fit1, idx2Exp = TRUE, se = TRUE)
coefSE



###############################################################################
#
# Section: Revisiting the  influenza data
#
###############################################################################

# Linearity test versus ID-PNAR

id2.z <- score_test_nonlinpq_h0(b = est2.z$coefs[, 1], y = flu, W = A_flu,
                                p = 2, d = 1, Z = pop)
id2.z



# Linearity test versus ST-PNAR with Davies' bound

dv2.z <- score_test_stnarpq_DV(b = est2.z$coefs[, 1], y = flu, W = A_flu,
                               p = 2, d = 1, Z = pop)

dv2.z


# Linearity test versus ST-PNAR with bootstrap

go1.z2 <- global_optimise_LM_stnarpq(b = est2.z$coefs[, 1], y = flu, W = A_flu,
                                     p = 2, d = 1, Z = pop)

go1.z2$gama
go1.z2$supLM

boot1.z2 <- score_test_stnarpq_j(supLM = go1.z2$supLM, b = est2.z$coefs[, 1],
                                 y = flu, W = A_flu, p = 2, d = 1, Z = pop,
                                 J = 499, ncores = 7, seed = 1234)

boot1.z2$pJ
boot1.z2$cpJ


# Linearity test versus T-PNAR with bootstrap

tgo1.z2 <- global_optimise_LM_tnarpq(b = est2.z$coefs[, 1], y = flu, W = A_flu,
                                     p = 2, d = 1, Z = pop)

tgo1.z2$gama
tgo1.z2$supLM


tboot1.z2 <- score_test_tnarpq_j(supLM = tgo1.z2$supLM, b = est2.z$coefs[, 1],
                                 y = flu, W = A_flu,  p = 2, d = 1, Z = pop,
                                 J = 499, ncores = 7, seed = 1234)

tboot1.z2$pJ
tboot1.z2$cpJ


# ----- Computing average computation times reported in Table 2 ------

# (of course these cannot be equal to the values in the paper)


## Estimation of linear PNAR model with 1-4 lags

avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  est1.z <- lin_estimnarpq(y = flu, W = A_flu, p = 1, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  est2.z <- lin_estimnarpq(y = flu, W = A_flu, p = 2, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  est4.z <- lin_estimnarpq(y = flu, W = A_flu, p = 4, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  est9.z <- lin_estimnarpq(y = flu, W = A_flu, p = 9, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


# Comparison of estimation with sureveillance package

avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  est1 <- lin_estimnarpq(y = flu, W = A_flu, p = 1)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


stsObj = fluBYBW
ni <- rowSums(neighbourhood(stsObj))
control <- list(
  ar = list(f = ~ 1),
  ne = list(f = ~ 1,
            weights = neighbourhood(stsObj) == 1,  # this is the default anyway
            offset = matrix(1/ni, nrow(stsObj), ncol(stsObj), byrow=TRUE)), # this does the standardization as in the paper
  end = list(f = ~ 1),
  family = "Poisson", keep.terms = TRUE
)
avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  fit1 <- hhh4(stsObj, control)
  coefSE <- coef(fit1, idx2Exp = TRUE, se = TRUE)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)



## Linearity test versus ID-PNAR

avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  id2.z <- score_test_nonlinpq_h0(b = est2.z$coefs[,1], y = flu, W = A_flu, p = 2 , d = 1, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


## Linearity test versus ST-PNAR

avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  dv2.z <- score_test_stnarpq_DV(b = est2.z$coefs[,1], y = flu, W = A_flu, p = 2 , d = 1, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)



avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  go1.z2 <- global_optimise_LM_stnarpq(b = est2.z$coefs[,1], y = flu, W = A_flu, p = 2 , d = 1, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)


avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  boot1.z2 <- score_test_stnarpq_j(supLM = go1.z2$supLM, b = est2.z$coefs[,1],
                                   y = flu, W = A_flu, p = 2 , d = 1, Z = pop,
                                   J = 499, ncores = 7, seed = 1234)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)



## Linearity test versus T-PNAR

avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  tgo1.z2 <- global_optimise_LM_tnarpq(b = est2.z$coefs[,1], y = flu,
                                       W = A_flu, p = 2 , d = 1, Z = pop)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)



avtime = vector()
for(i in 1:10){
  tic <- proc.time()
  tboot1.z2 <- score_test_tnarpq_j(supLM = tgo1.z2$supLM, b = est2.z$coefs[,1],
                                   y = flu, W = A_flu, p = 2 , d = 1, Z = pop,
                                   J = 499, ncores = 7, seed = 1234)
  proc.time() - tic
  avtime[i] = (proc.time() - tic)[3]
}
mean(avtime)

#----------------------------------------------------------------------



###############################################################################
#
# Section: Simulating network count time series
#
###############################################################################

set.seed(1234)  # for reproducibility

W_SBM <- adja(N = 10, K = 2, alpha = 0.7, directed = TRUE)

sim1 <- poisson.MODpq(b = c(0.2,0.2,0.4), W = W_SBM, p = 1, TT = 100, N = 10,
              copula = "gaussian", corrtype = "equicorrelation", rho = 0.5)
sim1$y


# Plots of Figure 3

par(mfrow=c(3,3))
par(mar=c(3,3.1,2.1,1.1))
for(i in 1:9){
  plot(sim1$y[,i],ylab="")
}



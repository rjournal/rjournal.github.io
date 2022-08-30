## ----Start
library(tvReg)

## Example 1: Usage of the tvSURE formula
data("Kmenta", package = "systemfit")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand = eqDemand, supply = eqSupply)

##Example 1 (continued): tvSURE estimation for models with an identity 
##variance-covariance matrix in the error term and with a non-identity matrix
OLS.fit <- systemfit::systemfit(system, data = Kmenta)
FGLS1.fit <- systemfit::systemfit(system, data = Kmenta, method = "SUR")
TVOLS.fit <- tvSURE(system, data = Kmenta)
TVFGLS1.fit <- tvSURE(system, data = Kmenta, method = "tvFGLS")

##Example 1 (continued 2): comparison of the iterative FGLS and 
##time-varying FGLS using the argument control
FGLS2.fit <- systemfit::systemfit(system, data = Kmenta, method = "SUR", 
                                  maxiter = 100)
TVFGLS2.fit <- tvSURE(system, data = Kmenta, method = "tvFGLS",
                      control = list(tol = 0.001, maxiter = 100))

##Example 1 (continued 3): usage of restrictions in the tvSURE
Rrestr <- matrix(0, 2, 7)
Rrestr[1, 7] <- 1
Rrestr[2, 2] <- 1
Rrestr[2, 5] <- -1
qrestr <- c(0, 0.5)
TVFGLS.rest <- tvSURE(system, data = Kmenta, method = "tvFGLS",
                      R = Rrestr, r = qrestr, 
                      bw = TVFGLS1.fit$bw, bw.cov = TVFGLS1.fit$bw.cov)

##Example 2: tvSURE estimation with coefficients as the rescaled time. 
##Application to portfolio management 
data("FF5F")
eqNA <- NA.SMALL.LoBM - NA.RF ~ NA.Mkt.RF + NA.SMB + NA.HML + NA.RMW + NA.CMA
eqJP <- JP.SMALL.LoBM - JP.RF ~ JP.Mkt.RF + JP.SMB + JP.HML + JP.RMW + JP.CMA
eqAP <- AP.SMALL.LoBM - AP.RF ~ AP.Mkt.RF + AP.SMB + AP.HML + AP.RMW + AP.CMA
eqEU <- EU.SMALL.LoBM - EU.RF ~ EU.Mkt.RF + EU.SMB + EU.HML + EU.RMW + EU.CMA
system2 <- list(NorthA = eqNA, JP = eqJP, AP = eqAP, EU = eqEU)
TVFF5F <- tvSURE(system2, data = FF5F, method = "tvFGLS", 
                 bw = c(0.56, 0.27, 0.43, 0.18), bw.cov = 0.12)

##Example 2 (continued): tvSURE estimates confidence intervals.
##Application to portfolio management
TVFF5F.90 <- confint(TVFF5F, level = 0.90)
TVFF5F.95 <- confint(TVFF5F.90)

##Example 2 (continued 2): tvSURE estimates plot. 
##Application to portfolio management 
plot(TVFF5F.95, vars = 1)

##Example 2 (continued 3): tvSURE estimates plot. 
##Application to portfolio management
plot(TVFF5F.95, vars = c(2, 3), eqs = 2)

##Example 3: Usage of the tvPLM formula
data("OECD")
elast.fe <- plm::plm(lhe ~ lgdp + pop65 + pop14 + public, data = OECD, 
                     index = c("country", "year"), model = "within")
elast.tvfe <- tvPLM (lhe ~ lgdp + pop65 + pop14 + public, data = OECD, 
                     index = c("country", "year"), method = "within",
                     bw = 0.67)
elast.fe <- confint(elast.fe)
elast.tvfe <- confint(elast.tvfe)
#Comparison plot of the within estimators
plot(elast.tvfe, vars = 1, ylim = c(0.5, 1.3))
graphics::par(mfrow = c(1, 1), 
              mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
x.axis <- 1:elast.tvfe$obs
par(new = TRUE)
plot(x.axis, rep(mean(elast.fe[1,]), elast.tvfe$obs), ylim = c(0.5, 1.3),
     ylab ="", xlab ="", type = "l", xaxt ="n", lty = 2)
graphics::polygon(c(rev(x.axis), x.axis), 
                  c(rep(rev(elast.fe[1, 2]), elast.tvfe$obs),
                    rep(elast.fe[1, 1], elast.tvfe$obs)),
                  col = "grey60", border = NA, fillOddEven = TRUE)
lines(x.axis, rep(mean(elast.fe[1,]), elast.tvfe$obs), lty=2)
legend("bottom", c("FE", "TVFE"), lty = 2:1, col = 1, ncol = 2, bty = "n")

##Example 4:  tvVAR estimation with coefficients as the rescaled time. 
##Application to monetary policy. 
##Comparison of the VAR, TVVAR and TVP-VAR models
data(usmacro, package = "bvarsv")
VAR.usmacro <- vars::VAR(usmacro, p = 4, type = "const")
TVVAR.usmacro <- tvVAR(usmacro, p = 4, bw = c(1.14, 20, 20), 
                       type = "const")
TVPVAR.usmacro <- bvarsv::bvar.sv.tvp(usmacro, p = 4, pdrift = TRUE, 
                                      nrep = 1000, nburn = 1000, 
                                      save.parameters = TRUE)


##Example 4 (continued): tvVAR estimation plot.
##Application to monetary policy. 
plot(TVVAR.usmacro)

##Example 4 (continued 2):  tvIRF estimation. Comparison with other irf estimation.
##Application to monetary policy. 
IRF.usmacro <- vars::irf(VAR.usmacro, impulse = "tbi", response ="inf", 
                         n.ahead = 20)
TVIRF.usmacro <- tvIRF(TVVAR.usmacro, impulse = "tbi", response="inf", 
                       n.ahead = 20)
TVPIRF.usmacro <- bvarsv::impulse.responses(TVPVAR.usmacro, 
                                            impulse.variable = 3,
                                            response.variable = 1, 
                                            draw.plot = FALSE)
##Example 4 (continued 3): graphical comparison of the irf, TVIRF and TVP-IRF 
##for time index 150 corresponding to 1990Q2
##Application to monetary policy. 
irf1 <- IRF.usmacro$irf[["tbi"]]
irf2 <- TVIRF.usmacro$irf[["tbi"]]
irf3 <- TVPIRF.usmacro$irf
par (mar = c(4, 4, 2, 1))
ylim <- range(irf1, irf2[150,,], irf3[50,])
plot(1:20, irf1[-1], ylim = ylim, main = "Impulse variable: tbi from 1990Q2", 
     xlab ="horizon", ylab ="inf", type ="l", lwd = 2)
lines(1:20, irf2[150,,-1], lty = 2, lwd = 2)
lines(1:20, irf3[50,], lty = 3, lwd = 2)
legend("bottomleft", c("IRF", "TVIRF", "TVP-IRF"), lty = 1:3, col = 1, 
       bty ="n")


##Example 5:  Nonparametric estimation of variance-covariance matrix. 
##Application to portfolio management
data(CEES)
VaR <- numeric(nrow(CEES))
Ht <- tvCov(CEES[, -1], bw = 0.12)
e <- rep (1, ncol(CEES)-1)
for (t in 1:nrow(CEES)){
  omega <- solve(Ht[,,t])%*%e/((t(e)%*%solve(Ht[,,t])%*%e)[1])
  VaR[t] <- abs(qnorm(0.05))*sqrt(max(t(omega)%*%Ht[,,t]%*%omega,0))
}
plot(as.Date(CEES[, "Date"]), VaR, type ="l", xlab = "year", 
             ylab = expression(VaR[t]), main="VaR of CEES over time")


## Example 6: tvLM estimation with coefficients as function of the rescaled time
set.seed (42)
N <-1000
tau <- seq(1:N)/N
d <- data.frame(tau, beta1 = sin(2 * pi * tau), beta2 = 2 * tau,
                x1 = rt(N, df = 2), x2 = rchisq(N, df = 4))
error.cov <- exp(-as.matrix(dist(tau))/10)
L <- t(chol(error.cov))
error <-  L %*% rchisq(N, df = 2)
d <- transform(d, y = x1 * beta1 + x2 * beta2 + error)
## LM
lm1 <- stats::lm(y ~ x1 + x2, data = d)
## TVLM
TVLM1 <- tvLM(y ~ x1 + x2,  data = d, bw = 0.05, est ="ll")
## GAM
library("mgcv")
gam1 <- mgcv::gam(y ~ s(tau, by = x1) + s(tau, by = x2), 
                  data = d)
par(mar = c(4, 2, 1, 1))
lm_beta <- coef(lm1)[c("x1", "x2")]
TVLM_beta <- coef(TVLM1)[, c("x1", "x2")]
gam_beta <- predict(gam1, type = "terms")/d[, c("x1", "x2")]
for(i in 1:2)
{
  betai <- d[[paste0("beta", i)]]
  ylim <- range(betai, TVLM_beta[, i], gam_beta[,i])
  plot(d$tau, betai, type = "l", ylim = ylim, lwd = 2, 
       xlab = expression(tau), ylab ="", lty = 2)
  abline(h = lm_beta[i], col = 2)
  lines(d$tau, TVLM_beta[, i], col =3)
  lines(d$tau, gam_beta[, i], col =4)
  if (i==2)
    legend("topleft", c(expression(beta[2*t]), "lm",  "tvLM", "gam"), 
           col =1:4, lty = c(2,1, 1, 1), bty = "n")
  else
    legend("topright", c(expression(beta[1*t]), "lm",  "tvLM", "gam"), 
           col =1:4, lty = c(2,1, 1, 1), bty = "n")
}

## Example 7: tvAR estimation with coefficients as functions of a random variable. 
##Application to risk management
data("RV")
RV2 <- head(RV, 2000)
HAR <- with(RV2, arima(RV, order = c(1, 0, 0), 
                       xreg = cbind(RV_week, RV_month)))
TVHAR<- with(RV2, tvAR(RV, p = 1, bw = 0.8,
                       exogen = cbind(RV_week, RV_month)))

## Example 7 (continued 2): tvAR estimation with coefficients as functions of a random process, the realized quarticity.
##Application to risk management
HARQ <- with(RV2, lm(RV ~ RV_lag + I(RV_lag*RQ_lag_sqrt) + RV_week + 
                       RV_month))
TVHARQ <- with(RV2, tvAR(RV, p = 1, exogen = cbind(RV_week, RV_month), 
                         z = RQ_lag_sqrt, cv.block = 10))


##Example 7 (continued 3):  Usage of forecast for the  model 
newexogen <- cbind(RV$RV_week[2001:2003], RV$RV_month[2001:2003])
forecast(TVHAR, n.ahead = 3, newexogen = newexogen)

## Example 7 (continued 4):  Usage of predict for the TVHARQ model 
newdata <- RV$RV_lag[2001:2003]
newexogen <- cbind(RV$RV_week[2001:2003], RV$RV_month[2001:2003])
newz <- RV$RQ_lag_sqrt[2001:2003]
predict(TVHARQ, newdata, newz, newexogen = newexogen)


## Example 2 (continued 4): Usage of forecast for the tvsure class
newKmenta <- data.frame(consump = c(95, 100, 102), price = c(90, 100, 103),
                        farmPrice = c(70, 95, 103), income = c(82, 94, 115), 
                        trend = c(21:23))
forecast(TVOLS.fit, newdata = newKmenta, n.ahead = 3)

## Example 2 (continued 5):  Usage of predict for the class tvsure
set.seed(42)
nobs <- nrow (Kmenta)
smoothing <- arima.sim(n = nobs + 3, list(ar = c(0.8897, -0.4858), 
                                          ma = c(-0.2279, 0.2488)), 
                       sd = sqrt(0.1796))
smoothing <- as.numeric(smoothing)
tvOLS.z.fit <- tvSURE(system, data = Kmenta,  z = smoothing[1:nobs])
newSmoothing <- tail(smoothing, 3)
predict(tvOLS.z.fit, newdata = newKmenta, newz = newSmoothing)

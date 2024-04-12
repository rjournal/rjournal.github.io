## Global options
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


## Package 
require("PLreg")

# Figure 1 Section 3.1

## Histogram with densities of some PL-Hyp(1.2) distributions
par(mfrow = c(3, 3), mar = c(2, 2, 2, 2), cex = 0.55)
n <- 5000
x <- seq(0.01, 0.99, 0.01)
mu <- c(0.1, 0.5, 0.9)
sigma <- 1; lambda <- 1.5
for(i in 1:length(mu)){
  hist(rPL(n, mu[i], sigma, lambda, 1.2, "Hyp"),
       xlim = c(0, 1), ylim = c(0, max(dPL(x, mu[i], sigma, 
                                           lambda, 1.2, "Hyp")) 
                                + 0.5),   
       freq = F, breaks  =  20, xlab = " ", ylab = " ", main = NULL, col="gray")
  lines(x, dPL(x, mu[i], sigma, lambda, 1.2, "Hyp"), type  =  "l", 
        col = "blue", lwd=2)
}

sigma <- c(0.2, 1, 3)
mu <- 0.5; lambda <- 1.5
for(i in 1:length(sigma)){
  hist(rPL(n, mu, sigma[i], lambda, 1.2, "Hyp"),
       xlim = c(0, 1), ylim = c(0, max(dPL(x, mu, sigma[i], 
                                           lambda, 1.2, "Hyp")) 
                                + 0.5),   
       freq = F, breaks  =  20, xlab = " ", ylab = " ", main = NULL, col="gray")
  lines(x, dPL(x, mu, sigma[i], lambda, 1.2, "Hyp"), type  =  "l", 
        col = "blue", lwd=2)
}

lambda <- c(0.01, 1, 5)
mu <- 0.3; sigma <- 1
for(i in 1:length(lambda)){
  hist(rPL(n, mu, sigma, lambda[i], 1.2, "Hyp"),
       xlim = c(0, 1), ylim = c(0, max(dPL(x, mu, sigma, 
                                           lambda[i], 1.2, "Hyp")) 
                                + 0.5),   
       freq = F, breaks  =  20, xlab = " ", ylab = " ", main = NULL, col="gray")
  lines(x, dPL(x, mu, sigma, lambda[i], 1.2, "Hyp"), type  =  "l", 
        col = "blue", lwd=2)
}


# Examples Section 4

## Section 4.1 bodyfat_Aeolus IID setting
data("bodyfat_Aeolus", package = "PLreg")

par(mfrow = c(1, 2), mar = c(5, 5, 5, 5), cex = 0.8)
hist(bodyfat_Aeolus$percentfat, main = " ",
     xlab = "proportion of body fat")
boxplot(bodyfat_Aeolus$percentfat, main = " ", 
        ylab = "proportion of body fat")

summary(bodyfat_Aeolus$percentfat)

PLNO <- PLreg(percentfat ~ 1, data = bodyfat_Aeolus, family = "NO")
PLSN.aux <- PLreg(percentfat ~ 1, data = bodyfat_Aeolus, family = "SN",
                  zeta = 1)
extra.parameter(PLSN.aux, lower = 1, upper = 4)
PLSN <- PLreg(percentfat ~ 1, data = bodyfat_Aeolus, family = "SN",
              zeta = 1.67)

# IC for mu
PLSN$fitted.values[1] - 1.96*0.05416*exp(-2.11477)/((1 + exp(-2.11477))^2)


PL_NO <- round(c(PLNO$Upsilon.zeta, AIC(PLNO)), 3)
PL_SN <- round(c(PLSN$Upsilon.zeta, AIC(PLSN)), 3)

measures <- rbind(PL_NO, PL_SN)
colnames(measures) <- c("Upsilon", "AIC")
measures

summary(PLSN)

set.seed(180123)
envelope(PLSN)
hist(bodyfat_Aeolus$percentfat, main = " ",
     xlab = "proportion of body fat", prob = TRUE, ylim = c(0, 10))
curve(dPL(x, PLSN$fitted.values[1], exp(PLSN$coefficients$dispersion),
          PLSN$coefficients$skewness, zeta = 1.67, family = "SN"), 0 , 1, add = TRUE, lwd = 2, col= "blue")

## Section 4.2 Firmcost data PL slash regression model
data("Firm", package = "PLreg")

Firm_slash2 <- PLreg(firmcost ~ indcost + sizelog | indcost + sizelog,
                     data = Firm, family = "SLASH", zeta = 2)
extra.parameter(Firm_slash2, lower = 1, upper = 2.5, grid = 30)

Firm_slash <- PLreg(firmcost ~ indcost + sizelog | indcost + sizelog,
                    data = Firm, family = "SLASH", zeta = 1.88)
summary(Firm_slash)

sand.matrix <- sandwich(Firm_slash)
se <- sqrt(diag(sand.matrix)) 
se

Firm_slash.CD <- PLreg(firmcost ~ indcost + sizelog,
                       data = Firm, family = "SLASH", zeta = 2.29)
summary(Firm_slash.CD)

par(mfrow = c(2, 3), mar = c(5, 5, 5, 5), cex = 0.8)
envelope(Firm_slash.CD, type = "quantile")
envelope(Firm_slash.CD, type = "deviance")
envelope(Firm_slash.CD, type = "standardized")
influence(Firm_slash.CD)

par(mar = c(2, 2, 2, 2), cex = 0.8)
plot(Firm_slash.CD, which = 7)


measures <- sapply(c("logit", "probit", "cloglog"),
                    function(x){
                      fit <- update(Firm_slash.CD, link = x)
                      round(c(fit$pseudo.r.squared, fit$Upsilon.zeta),3)
                    })
rownames(measures) <- c("pseudo R-squared", "Upsilon_zeta")
measures
             
## Section 4.3.1 PeruVotes data: GJS regression model
data("PeruVotes", package = "PLreg")
PV_GJSt <- PLreg(votes ~ HDI | HDI, data = PeruVotes, family = "TF",
                 zeta = 4, control = PLreg.control(lambda = 1))
summary(PV_GJSt)
plot(PV_GJSt, 1:7)

PV_PLt <- PLreg(votes ~ HDI | HDI, data = PeruVotes, family = "TF",
                zeta = 4)
coefficients(PV_PLt)
CI.lambda(PV_PLt, conf.coef = 0.9)

## Section 4.3.2 bodyfat_Aeolus data: log-log regression models

bodyf_PL <- PLreg(percentfat ~ days + sex + year | days + sex + year,
                  data = bodyfat_Aeolus, family = "NO")
bodyf_PL$coefficients$skewness



bodyf_loglog <- PLreg(percentfat ~ days + sex + year | days + sex + year,
                      data = bodyfat_Aeolus, family = "NO", 
                      control = PLreg.control(lambda = 0))
summary(bodyf_loglog)

## Section 4.4 inflated PL regression models

# Generating data

n <- 300
kappa <- c(-2, 0.5)
beta <- c(-1.0, -2.0)
sigma <- 0.5
lambda <- 2

set.seed(25012023)
x1 <- runif(n)

X <- Z <- matrix(c(rep(1,n), x1), ncol = 2, byrow = FALSE)

alpha <- exp(Z%*%kappa)/(1 + exp(Z%*%kappa))
mu <- exp(X%*%beta)/(1 + exp(X%*%beta))

prob <- runif(n)
y <- ifelse((prob <= alpha), 0, rPL(n, mu, sigma, lambda, family = "NO"))

par(mfrow = c(1, 2), mar = c(5, 5, 5, 5), cex = 0.8)
hist(y, main = " ", xlab = "y", ylim = c(0, 100))
boxplot(y, main = " ",ylab = "y")


Ind <- ifelse(y == 0, 1, 0)
fit.glm <- glm(Ind ~ x1, family = binomial())
fit.PL <- PLreg(y[Ind == 0] ~ x1[Ind == 0], family = "NO", type = "ML")

coefficients(fit.glm)
coefficients(fit.PL)

alpha  <- fit.glm$fitted.values
mu     <- fit.PL$link$median$linkinv(X%*%fit.PL$coefficients$median)
sigma  <- fit.PL$link$dispersion$linkinv(fit.PL$coefficients$dispersion)
lambda <- fit.PL$coefficients$skewness

cdf <- alpha*as.numeric(y >= 0) + (1 - alpha)*pPL(y, mu, sigma, lambda, family = "NO")
res <- ifelse(y == 0, qnorm(runif(length(y),0, alpha)), qnorm(cdf))
plot(res, ylab = "Randomized quantile residuals", pch = "+", ylim = c(-4, 4))
abline(h = 2.5, col = "gray", lty = 2)
abline(h = -2.5, col = "gray", lty = 2)
abline(h = 0, col = "gray", lty = 2)
qqnorm(res)
qqline(res, col = "gray")

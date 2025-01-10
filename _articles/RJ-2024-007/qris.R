## #########################################
## Requiared packages
## #########################################

library(qris)
library(ggplot2)
library(knitr)
library(kableExtra)

## #########################################
## Section: Package implementation
## #########################################

## Introducing new functions
args(qris)
args(qris.control)
argsAnywhere(plot.qris)
args(qris.extend)

## #########################################
## Section: Simulated data
## #########################################

## Data generation
data.gen <- function(n, t0, cen = .3, Q = .5) {
  if (!(t0 %in% 0:2))
    stop("T0 is limited to three specific values: 0, 1, or 2.")
  if (!(cen %in% c(0, .1, .3)))
    stop("Censoring is limited to three specific values: 0%, 10%, or 30%.")
  if (!(Q %in% c(.25, .5)))
    stop("Q is limited to two specific values: 0.25, or 0.50.")
  censoring <- Inf
  if (t0 == 0) {
    if (cen == .1) censoring <- runif(n, 0, 125.1)
    if (cen == .3) censoring <- runif(n, 0, 25.49)
    beta0 <- log(5); beta1 <- log(2)
  }
  if (t0 == 1) {
    if (cen == .1) censoring <- runif(n, 0, 120.8)
    if (cen == .3) censoring <- runif(n, 0, 23.41)
    beta0 <- 1.410748; beta1 <- 0.7974189
  }
  if (t0 == 2) {
    if (cen == .1) censoring <- runif(n, 0, 120.6)
    if (cen == .3) censoring <- runif(n, 0, 26.20)
    beta0 <- 1.219403; beta1 <- 0.9070615
  }
  dat <- data.frame(censoring,
                    Time0 = sqrt(-log(1 - runif(n))),
                    X1 = runif(n),
                    X2 = rbinom(n, 1, .5),
                    X3 = rnorm(n),
                    X4 = runif(n),
                    X5 = rexp(n, 1))
  rho <- (-log(1 - Q))^0.5 * (((exp(beta0 + beta1 * dat$X1) + t0)^2 - t0^2)^-0.5)
  dat$Time0 <- dat$Time0 / rho
  dat$Time <- pmin(dat$Time0, dat$censoring)
  dat$status <- 1 * (dat$Time0 < dat$censoring)
  subset(dat, select = c(Time, status, X1, X2, X3, X4, X5))
}

## Data illustration
set.seed(3)
head(data.gen(200, 0))

## Function to run the simulation with se = fmb

do_fmb <- function(n, t0, cen, Q, nB) {
  dat <- data.gen(n, t0, cen, Q)
  fm <- Surv(Time, status) ~ X1 + X2 + X3 + X4 + X5
  stamp <- NULL
  stamp[1] <- Sys.time()
  f1 <- qris(fm, data = dat, t0 = t0, Q = Q, nB = nB, method = "smooth", se = "fmb")
  stamp[2] <- Sys.time()
  f2 <- qris(fm, data = dat, t0 = t0, Q = Q, nB = nB, method = "nonsmooth", se = "fmb")
  stamp[3] <- Sys.time()
  f3 <- qris(fm, data = dat, t0 = t0, Q = Q, nB = nB, method = "iterative", se = "fmb")
  stamp[4] <- Sys.time()
  list(smooth = c(f1$coef, f1$std),
       nonsmooth = c(f2$coef, f2$std),
       iter = c(f3$coef, f3$std),
       times = diff(stamp))
}

## Example codes to run replications
## Simulation for other scenarios are carried out separately

B <- 200
set.seed(2)
sims0_fmb <- mapply(function(n, t0)
    replicate(B, do_fmb(n, t0 = t0, cen = .3, Q = .5, nB = 200)),
    n = c(200, 400, 1000), t0 = c(0, 0, 0), SIMPLIFY = F)
sim1_fmb <- mapply(function(n, t0)
    replicate(B, do_fmb(n, t0 = t0, cen = .3, Q = .5, nB = 200)),
    n = c(200, 400, 1000), t0 = c(1, 1, 1), SIMPLIFY = F)

## Example codes to create simulation tables
## Tables for full simulation results are created separately

makeTab_fmb <- function(...) {
    d <- rbind(...)
    c(colMeans(d), apply(d[,1:6], 2, sd))
}

tmp0_fmb <- sapply(sims0_fmb, function(s)
    apply(s[1:3,], 1, do.call, what = makeTab_fmb))

tab0_fmb <- data.frame(t0 = rep(0, each = 18),
                       n = rep(c(200, 400, 1000), 1, each = 6),
                       b = c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$", "$\\beta_3$",
                             "$\\beta_4$", "$\\beta_5$"),
                       do.call(rbind, apply(tmp0_fmb, 2, matrix, 6, simplify = F)))

kable(tab0_fmb, digits = 3, 'latex', booktabs = T, escape = F,
      col.names = c("$t_0$", "n", "$\\beta$", rep(c("PE", "ESE", "ASE"), 3)),
      caption = "Result $t_0=0$ and se=fmb") %>%
  add_header_above(c("", "", "", "Smooth+fmb" = 3, "Nonsmooth+fmb" = 3, "Iterative+fmb" = 3)) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, valign = "top", latex_hline = "none")

times0_fmb <- sapply(sims0_fmb, function(s) Reduce("+", s[4,]) / B)
rownames(times0_fmb) <- c("Smooth+fmb", "Nonsmooth+fmb", "Iterative+fmb")
kable(times0_fmb, digits = 3, 'latex', booktabs = T, row.names = T,
      col.names = c("200", "400", "1000"),
      caption = "Runtimes when $t_0=0$ and se=fmb") %>%
  add_header_above(c("", "$t_0 = 0$" = 3), escape = F)


## Function to run the simulation with se = pmb

do_pmb <- function(n, t0, cen, Q, nB) {
  dat <- data.gen(n, t0, cen, Q)
  fm <- Surv(Time, status) ~ X1 + X2 + X3 + X4 + X5
  stamp <- NULL
  stamp[1] <- Sys.time()
  f1 <- qris(fm, data = dat, t0 = t0, Q = Q, nB = nB, method = "smooth", se = "pmb")
  stamp[2] <- Sys.time()
  f2 <- qris(fm, data = dat, t0 = t0, Q = Q, nB = nB, method = "iterative", se = "pmb")
  stamp[3] <- Sys.time()
  list(smooth = c(f1$coef, f1$std),
       iter = c(f2$coef, f2$std),
       times = diff(stamp))
}

set.seed(2)
sims0_pmb <- mapply(function(n, t0)
    replicate(B, do_pmb(n, t0 = t0, cen = .3, Q = .5, nB = 200)),
    n = c(200, 400, 1000), t0 = c(0, 0, 0), SIMPLIFY = F)
sims1_pmb <- mapply(function(n, t0)
    replicate(B, do_pmb(n, t0 = t0, cen = .3, Q = .5, nB = 200)),
    n = c(200, 400, 1000), t0 = c(1, 1, 1), SIMPLIFY = F)

## Simplified simulation
rho0 <- .2 * sqrt(log(2))
rho1 <- .1 * sqrt(log(2))
data.gen <- function(n) {
    dat <- data.frame(censoring = runif(n, 0, 23.41),
                      Time0 = sqrt(-log(1 - runif(n))),
                      X = rbinom(n, 1, .5))
    dat$Time0 <- ifelse(dat$X > 0, dat$Time0 / rho1, dat$Time0 / rho0)
    dat$Time <- pmin(dat$Time0, dat$censoring)
    dat$status <- 1 * (dat$Time0 < dat$censoring)
    subset(dat, select = c(Time, status, X))
}
set.seed(10)
head(dat <- data.gen(200))

## Initial value illustration
(random <- rnorm(2))
f1 <- qris(Surv(Time, status) ~ X, data = dat, t0 = 1, init = "rq", nB = 0)
f2 <- update(f1, init = c(1, 1))
f3 <- update(f1, init = random)
all.equal(f1$coef, f2$coef)
all.equal(f2$coef, f3$coef)

## More sophisticated coefficient effect plots
fit <- qris(Surv(Time, status) ~ X, data = dat, t0 = 1, se = "pmb")
fit2 <- qris.extend(fit, Qs = 4:7 / 10)
class(fit2)
names(fit)
setdiff(names(fit2), names(fit))
plot(fit2)

## Prepare true values to overlay on covariate effect plots
r <- 2:1 * sqrt(log(2)) / 10
k <- 2
trueB <- function(t0, tau) {
    b <- log(1 / r * ((r * t0) ^ k - log(1 - tau))^(1 / k) - t0)
    c(b[1], b[2] - b[1])
}
true_Q <- c(t(sapply(4:7 / 10, trueB, t0 = 1)))
true_t0 <- c(t(sapply(1:3, trueB, tau = .5)))

## Demonstrate ggplot options
plot(fit2) + theme(legend.position = "bottom") + 
    geom_line(aes(x = Qs, y = true_Q, col = variable, linetype = "True value")) +
    scale_linetype_manual(name = "", values = c("True value" = "dotdash"))
b <- plot(fit2, t0s = 1:3, byQs = F)
b + theme(legend.position = "bottom") +
    geom_line(aes(x = t0s, y = true_t0, col = variable,
                  linetype = "True value")) +
    scale_linetype_manual(name = "", values = c("True value" = "dotdash"))



## #########################################
## Section: Lung Cancer Data
## #########################################
## Load data
data(cancer, package = "survival")
str(subset(lung, select = c(time, status, sex, wt.loss)))

## Prepare data and fit 
lung$male <- factor(lung$sex, 1:2, c("Male", "Female"))
lung$std.wt.loss <- scale(lung$wt.loss)
fit1 <- qris(Surv(time, status) ~ male + std.wt.loss,
             data = lung, t0 = 30, Q = .5, nB = 100,
             method = "smooth", se = "pmb")
summary(fit1)
coef(fit1)
vcov(fit1)
confint(fit1)

## summaries and compare to nonsmooth fit
summary(fit2 <- update(fit1, method = "nonsmooth", se = "fmb"))
summary(update(fit1, method = "iterative"))
summary(update(fit1, Q = 0.25))
summary(update(fit1, Q = 0.75))
summary(update(fit1, t0 = 180))

## predict and residuals
lung.new <- data.frame(male = c("Male", "Female"), std.wt.loss = 0)
predict(fit2, newdata = lung.new)
head(residuals(fit2), 5)

## plots
hide <- theme(legend.position = "none")
plot(fit1, Qs = 2:5 / 10, byQs = TRUE, ggextra = hide)
plot(fit2, Qs = 2:5 / 10, byQs = TRUE, ggextra = hide)
plot(fit1, Qs = 2:5 / 10, t0s = 5:8 * 10, byQs = TRUE, ggextra = hide)
plot(fit1, Qs = 2:5 / 10, t0s = 5:8 * 10, byQs = FALSE, ggextra = hide)

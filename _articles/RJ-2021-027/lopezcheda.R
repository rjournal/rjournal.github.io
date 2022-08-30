## Replication code for the paper:
## "npcure: an R Package for Nonparametric Inference in Mixture Cure Models"

library("npcure")

## This ensures that the default random number generator of R 3.6.0 is used
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", 
        sample.kind = "Rejection")

## Section 4 -----------------------------------------------------------
## Subsection 4.1 ------------------------------------------------------

## Simulated data
set.seed(123)
n <- 50
x <- runif(n, -2, 2)
y <- rweibull(n, shape = 0.5 * (x + 4), scale = 1)
c <- rexp(n, rate = 1)
p <- exp(2 * x)/(1 + exp(2 * x))
u <- runif(n)
t <- ifelse(u < p, pmin(y, c), c)
d <- ifelse(u < p, ifelse(y < c, 1, 0), 0)
data <- data.frame(x = x, t = t, d = d)

## Estimation of the cure rate with local bootstrap bandwidth
set.seed(123)
x0 <- seq(-1.5, 1.5, by = 0.1)
hb <- probcurehboot(x, t, d, data, x0 = x0,
   bootpars = controlpars(B = 2000, hsmooth = 15))
q1 <- probcure(x, t, d, data, x0 = x0, h = hb$hsmooth, conflevel = 0.95,
   bootpars = controlpars(B = 2000))
q1

## The same 'in one step'
set.seed(123)
q2 <- probcure(x, t, d, data, x0 = x0, conflevel = 0.95,
   bootpars = controlpars(B = 2000, hsmooth = 15))

## Plot
plot(q1$x0, q1$q, type = "l", ylim = c(0, 1), xlab = "Covariate X",
   ylab = "Cure probability")
lines(q1$x0, q1$conf$lower, lty = 2)
lines(q1$x0, q1$conf$upper, lty = 2)
lines(q1$x0, 1 - exp(2 * q1$x0)/(1 + exp(2 * q1$x0)), col = 2)
legend("topright", c("Estimate", "95% CI limits", "True"),
   lty = c(1, 2, 1), col = c(1, 1, 2))

## Subsection 4.2 ------------------------------------------------------

## Estimation of the latency:
## a) with fixed local bandwidths
set.seed(123)
S0 <- latency(x, t, d, data, x0 = c(0, 0.5), h = c(0.8, 0.5),
   conflevel = 0.95, bootpars = controlpars(B = 500))

## b) with local bootstrap bandwidths
set.seed(123)
b <- latencyhboot(x, t, d, data, x0 = c(0, 0.5))
S0 <- latency(x, t, d, data, x0 = c(0, 0.5), h = b$h, conflevel = 0.95)
S0

## c) same as b) 'in one step'
set.seed(123)
S0 <- latency(x, t, d, data, x0 = c(0, 0.5), conflevel = 0.95)

## Plot
plot(S0$testim, S0$S$x0, type = "s", xlab = "Time", ylab = "Latency",
   ylim = c(0, 1))
lines(S0$testim, S0$conf$x0$lower, type = "s", lty = 2)
lines(S0$testim, S0$conf$x0$upper, type = "s", lty = 2)
lines(S0$testim, pweibull(S0$testim, shape = 0.5 * (S0$x0[1] + 4), 
   scale = 1, lower.tail = FALSE), col = 2)
legend("topright", c("Estimate", "95% CI limits", "True"),
   lty = c(1, 2, 1), col = c(1, 1, 2))

## Subsection 4.3 ------------------------------------------------------

## Covariate significance test for the cure rate:
set.seed(123)
## a) continuous covariate
testcov(x, t, d, data, bootpars = controlpars(B = 2500))

## b) categorical covariate
data$z <- rep(factor(letters[1:5]), each = 10)
testcov(z, t, d, data, bootpars = controlpars(B = 2500))

## Subsection 4.4 ------------------------------------------------------

## Estimation of the conditional survival function
## With local cross-validation bandwidths
set.seed(123)
x0  <- c(0, 0.5)
hcv <- berancv(x, t, d, data, x0 = x0,
   cvpars = controlpars(hbound = c(0.2, 2), hl = 200, hsave = TRUE))
S <- beran(x, t, d, data, x0 = x0,  h = hcv$h, conflevel = 0.95)
S

## The same 'in one step'
set.seed(123)
S <- beran(x, t, d, data, x0 = x0, conflevel = 0.95,
   cvbootpars = controlpars(hbound = c(0.2, 2), hl = 200, hsave = TRUE))

## Plot
plot(S$testim, S$S$x0.5, type = "s", xlab = "Time", ylab = "Survival",
   ylim = c(0, 1))
lines(S$testim, S$conf$x0.5$lower, type = "s", lty = 2)
lines(S$testim, S$conf$x0.5$upper, type = "s", lty = 2)
p0 <- exp(2 * x0[2])/(1 + exp(2 * x0[2]))
lines(S$testim, 1 - p0 + p0 * pweibull(S$testim,
   shape = 0.5 * (x0[2] + 4), scale = 1, lower.tail = FALSE), col = 2)
legend("topright", c("Estimate", "95% CI limits", "True"),
   lty = c(1, 2, 1), col = c(1, 1, 2))

## Subsection 4.5 ------------------------------------------------------

## Maller-Zhou test
testmz(t, d, data)

## Section 5 -----------------------------------------------------------

## Example: Bone Marrow Transplantation data
data("bmt", package = "KMsurv")

## Maller-Zhou test
testmz(t2, d3, bmt)

## Kaplan-Meier estimate of disease-free survival
library("survival")
surv <- survfit(Surv(t2, d3) ~ 1, conf.type="log", conf.int = 0.95, 
                type = "kaplan-meier", error = "greenwood", data = bmt)
plot(surv, mark.time = FALSE, xlab = "Time (days)", ylab = "Survival")
legend("topright", c("Estimate", "95% CI limits"), lty = 1:2)

## Estimates of the cure rate conditional on a) age and b) waiting time 
## to transplant:
## a) age
x0 <- seq(quantile(bmt$z1, 0.05), quantile(bmt$z1, 0.95), length.out = 100)
q.age <- probcure(z1, t2, d3, bmt, x0 = x0, conflevel = 0.95,
   bootpars = controlpars(hsmooth = 10))

## Plot
par(mar = c(5, 4, 4, 5) + 0.1)
plot(q.age$x0, q.age$q, type = "l", ylim = c(0, 1),
   xlab = "Patient age (years)", ylab = "Cure probability")
lines(q.age$x0, q.age$conf$lower, lty = 2)
lines(q.age$x0, q.age$conf$upper, lty = 2)
par(new = TRUE)
d.age <- density(bmt$z1)
plot(d.age, xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = 2,
   main = "", zero.line = FALSE)
mtext("Density", side = 4, col = 2, line = 3) 
axis(4, ylim = c(0, max(d.age$y)), col = 2, col.axis = 2)
legend("topright", c("Estimate", "95% CI limits", "Covariate density"),
   lty = c(1, 2, 1), col = c(1, 1, 2), cex = 0.8)

## b) waiting time to transplant
x0 <- seq(quantile(bmt$z7, 0.05), quantile(bmt$z7, 0.95), length.out = 100)
q.wtime <- probcure(z7, t2, d3, bmt, x0 = x0, conflevel = 0.95,
   bootpars = controlpars(hsmooth = 10))

## Plot
par(mar = c(5, 4, 4, 5) + 0.1)
plot(q.wtime$x0, q.wtime$q, type = "l", ylim = c(0, 1),
   xlab = "Waiting time to transplant (days)", ylab = "Cure probability")
lines(q.wtime$x0, q.wtime$conf$lower, lty = 2)
lines(q.wtime$x0, q.wtime$conf$upper, lty = 2)
par(new = TRUE)
d.wtime <- density(bmt$z7)
plot(d.wtime, xaxt= "n", yaxt = "n", xlab = "", ylab = "", col = 2,
   main = "", zero.line = FALSE)
mtext("Density", side = 4, col = 2, line = 3) 
axis(4, ylim = c(0, max(d.wtime$y)), col = 2, col.axis = 2)
legend("topright", c("Estimate", "95% CI limits", "Covariate density"),
   lty = c(1, 2, 1), col = c(1, 1, 2), cex = 0.8)

## Testing the effect of one covariate:
## a) age
set.seed(123)
testcov(z1, t2, d3, bmt,  bootpars = controlpars(B = 2500))

## b) waiting time to transplant
set.seed(123)
testcov(z7, t2, d3, bmt,  bootpars = controlpars(B = 2500))

## c) gender
## Estimated survival
bmt$z3 <- factor(bmt$z3, labels = c("Male", "Female"))
bmt$z10 <- factor(bmt$z10, labels = c("MTX", "No MTX"))
summary(bmt[, c("z3", "z10")]) 

Sgender <- survfit(Surv(t2, d3) ~ z3, data = bmt)
Sgender

##Plot
par(mar = c(5, 4, 4, 2) + 0.1)
plot(Sgender, col = 1:2, mark.time = FALSE, xlab = "Time (days)",
   ylab = "Disease-free survival")
legend("topright", legend = c("Male", "Female"), title = "Gender",
   lty = 1, col = 1:2)

## Estimated probability of cure
qgender <- c(min(Sgender[1]$surv), min(Sgender[2]$surv))
qgender

## Test
set.seed(123)
testcov(z3, t2, d3, bmt, bootpars = controlpars(B = 2500))

## d) use of methotrexate
## Estimated survival
Smtx <- survfit(Surv(t2, d3) ~ z10, data = bmt)
plot(Smtx, col = 1:2, mark.time = FALSE, xlab = "Time (days)", 
     ylab = "Disease-free survival")
legend("topright", legend = c("Yes", "No"), lty = 1, title = "MTX", col = 1:2)

## Estimated probability of cure
qmtx <- c(min(Smtx[1]$surv), min(Smtx[2]$surv))
qmtx

## Test
set.seed(123)
testcov(z10, t2, d3, bmt, bootpars = controlpars(B = 2500))

## Estimation of the latency conditional on age
set.seed(123)
S0 <- latency(z1, t2, d3, bmt, x0 = c(25, 40), conflevel = 0.95,
   bootpars = controlpars(B = 500))

## Plot
plot(S0$testim, S0$S$x25, type = "s", ylim = c(0, 1),
   xlab = "Time (days)", ylab = "Latency")
lines(S0$testim, S0$conf$x25$lower, type = "s", lty = 2)
lines(S0$testim, S0$conf$x25$upper, type = "s", lty = 2)
lines(S0$testim, S0$S$x40, type = "s", col = 2)
lines(S0$testim, S0$conf$x40$lower, type = "s", lty = 2, col = 2)
lines(S0$testim, S0$conf$x40$upper, type = "s", lty = 2, col = 2)
legend("topright", c("Age 25: Estimate", "Age 25: 95% CI limits",
   "Age 40: Estimate", "Age 40: 95% CI limits"), lty = 1:2,
   col = c(1, 1, 2, 2))

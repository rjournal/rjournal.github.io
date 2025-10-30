## M. Besalú, K. Langohr, M. Francisco, A. Garcia, G. Gómez Melis:
## Goodness-of-Fit for Right-Censored Data: Package GofCens
## ===============================================================
## Generation of right-censored sample from a log-normal distribution
## ------------------------------------------------------------------
set.seed(123)
survt <- round(rlnorm(300, 2, 1), 2)
censt <- round(rexp(300, 1 / 20), 2)
times <- pmin(survt, censt)
delta <- as.numeric(survt <= censt)
table(delta)

## Figure 1
## --------
library("GofCens")
## Left panel
pdf("Fig1Left.pdf", width = 8, height = 8)
probPlot(times, delta, distr = "lognormal", cex.lab = 1.3)
invisible(dev.off())
## Right panel
pdf("Fig1Right.pdf", width = 8, height = 8)
probPlot(times, delta, distr = "weibull", ggp = TRUE)
invisible(dev.off())

## Figure 2
## --------
pdf("Fig2.pdf", width = 20, height = 5)
probPlot(Surv(times, delta) ~ 1, distr = "lognormal", m = matrix(1:4, nrow = 1),
         params0 = list(location = 2, scale = 1.5), ggp = TRUE, prnt = TRUE)
invisible(dev.off())

## Figure 3
## --------
pdf("Fig3.pdf", width = 12, height = 8)
cumhazPlot(times, delta, font.lab = 4, cex.lab = 1.3, degs = 2, prnt = TRUE)
invisible(dev.off())

## Figure 4
## --------
pdf("Fig4.pdf", width = 15, height = 5)
cumhazPlot(times, delta, distr = c("exponential",  "beta", "lognormal"),
           betaLimits = c(0, 100), ggp = TRUE)
invisible(dev.off())

## Figure 5
## --------
pdf("Fig5.pdf", width = 12, height = 8)
kmPlot(times, delta, ggp = TRUE)
invisible(dev.off())


## Examples with KScens()
## ----------------------
set.seed(123)
KScens(times, delta, distr = "lognormal")
summary(KScens(times, delta, distr = "weibull", boot = FALSE))

## Examples with CvMcens()
## -----------------------
set.seed(123)
summary(CvMcens(times, delta, distr = "lognormal"))
set.seed(123)
summary(CvMcens(times, delta, distr = "weibull"))
set.seed(123)
system.time(CvMcens(times, delta, distr = "lognormal"))

## Examples with ADcens()
## ----------------------
set.seed(123)
summary(ADcens(times, delta, distr = "lognormal"))
set.seed(123)
summary(ADcens(times, delta, distr = "weibull"))
set.seed(123)
summary(ADcens(times, delta, distr = "lognormal",
               params0 = list(location = 2, scale = 1.5)), outp = "table")

## Examples with gofcens()
## -----------------------
set.seed(123)
gofcens(times, delta, distr = "lognormal")
set.seed(123)
system.time(gofcens(times, delta, distr = "lognormal"))
set.seed(123)
summary(gofcens(times, delta, distr = "weibull"), print.AIC = FALSE,
        print.BIC = FALSE, print.infoBoot = TRUE)

## Examples with chisqcens()
## -------------------------
set.seed(123)
chisqcens(times, delta, M = 8, distr = "lognormal")
set.seed(123)
summary(chisqcens(times, delta, M = 8, distr = "weibull"))
set.seed(123)
result <- summary(chisqcens(times, delta, M = 8, distr = "gamma"))
result
class(result)


## Illustration with NBA data
## --------------------------
library(rms)
data(nba)
npsurv(Surv(survtime, cens) ~ 1, nba)


## Figure 6
## --------
pdf("Fig6.pdf", width = 12, height = 7)
par(las = 1, cex.lab = 1.3, cex.axis = 1.2, font.lab = 4, font.axis = 2,
    mar = c(5, 5, 2, 2), yaxs = "i", xaxs = "i")
survplot(npsurv(Surv(survtime, cens) ~ 1, nba), lwd = 3,
         xlab = "Years after NBA career", time.inc = 5, col.fill = grey(0.6),
         ylab = "Estimated survival probability", xlim = c(0, 75))
abline(h = 0.5, lwd = 2, lty = 2)
invisible(dev.off())


## Figure 7
## --------
pdf("Fig7.pdf", width = 12, height = 8)
cumhazPlot(Surv(survtime, cens) ~ 1, nba, font.lab = 4, cex.lab = 1.3,
           lwd = 3, colour = "blue")
invisible(dev.off())

## Figure 8
## --------
## Left panel
pdf("Fig8left.pdf", width = 8, height = 8)
probPlot(Surv(survtime, cens) ~ 1, nba, distr = "logistic", ggp = TRUE)
invisible(dev.off())
## Right panel
pdf("Fig8right.pdf", width = 8, height = 8)
probPlot(Surv(survtime, cens) ~ 1, nba, distr = "normal", ggp = TRUE)
invisible(dev.off())

## Application of gofcens() function
## ---------------------------------
set.seed(123)
summary(gofcens(Surv(survtime, cens) ~ 1, nba, distr = "logistic"))
set.seed(123)
summary(gofcens(Surv(survtime, cens) ~ 1, nba, distr = "normal"))

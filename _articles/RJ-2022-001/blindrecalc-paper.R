## ----setup, echo=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------
library(blindrecalc)


## ----setup-design---------------------------------------------------------------------------------------------------------------------
design <- setupChiSquare(alpha = 0.025, beta = 0.2, delta = 0.2)


## ----fixed-sample-size----------------------------------------------------------------------------------------------------------------
n_fix(design, nuisance = c(0.2, 0.3, 0.4, 0.5))


## ----type-1-error, cache=TRUE---------------------------------------------------------------------------------------------------------
n <- n_fix(design, nuisance = 0.2)
p <- seq(0.1, 0.9, by = 0.01)
toer_fix <- toer(design, n1 = n, nuisance = p, recalculation = FALSE)
toer_ips <- toer(design, n1 = n/2, nuisance = p, recalculation = TRUE)


## ----adjust-alpha, cache=TRUE---------------------------------------------------------------------------------------------------------
adj_sig <- adjusted_alpha(design, n1 = n/2, nuisance = p, precision = 0.0001,
                          recalculation = TRUE)
design@alpha <- adj_sig
toer_adj <- toer(design, n1 = n/2, nuisance = p, recalculation = TRUE)


## ----plot-type-1-error, cache=TRUE, echo=FALSE, fig=TRUE, fig.height=4, fig.width=6, fig.cap="\\label{fig:toer}Type 1 error rate for different nuisance parameters."----
par(mfrow = c(1, 2))
plot(p, toer_ips, type = "l", ylab = "actual level without adjustment", ylim = c(0.023, 0.03))
lines(p, toer_fix, lty = 2)
abline(a = 0.025, b = 0, col = "red")
legend(0.25, 0.0245, legend = c("fix", "recalculation"), lty = c(2, 1), cex=0.7,
       bty="n")


plot(p, toer_adj, type = "l", ylab = "actual level with adjustment", ylim = c(0.023, 0.03))
abline(a = 0.025, b = 0, col = "red")

par(mfrow = c(1, 1))


## ----power, cache=TRUE----------------------------------------------------------------------------------------------------------------
pow_fix <- pow(design, n1 = n, nuisance = p, recalculation = FALSE)
pow_ips <- pow(design, n1 = n/2, nuisance = p, recalculation = TRUE)


## ----plot-power, cache=TRUE, echo=FALSE, fig=TRUE, fig.height=4, fig.width=5, fig.cap="\\label{fig:pow}Power values for different nuisance parameters."----
plot(p, pow_fix, type = "l", ylab = "power", lty = 2)
lines(p, pow_ips, lty = 1)
abline(a = 0.8, b = 0, col = "red")
legend(0.35, 1, legend = c("fix", "recalculation"), lty = c(2, 1), cex=0.9,
       bty="n")



## ----sample-size-distribution, cache=TRUE, fig=TRUE, fig.height=4, fig.width=5, fig.cap="\\label{fig:dist}Sample size distribution boxplots produced by blindrecalc."----
p <- seq(0.2, 0.8, by = 0.1)
n_dist(design, n1 = n/2, nuisance = p, plot = TRUE)


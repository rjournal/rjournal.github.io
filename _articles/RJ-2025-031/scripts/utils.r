#
# Wrapper for all models used in the evaluation study
#

library(XICOR)
library(moonboot)

get.model <- function(model) {
  models <- list("mean" = mean.boot, "max" = max.boot, "shorth" = shorth.boot, "xicor" = xicor.boot, "mean.first" = mean.first.boot, "poisson" = poisson.boot)
  if (is.numeric(model))
    return(models[[model]])
  return(models[[model]])
}

generate.xi <- function(n) {
  x <- runif(n, -1, 1)
  eps <- rnorm(n, sd = 0.5)
  y <- x + eps
  return(cbind(x = x, y = y))
}

xicor.statistic <- function(data, indices) {
  return(xicor(data[indices, "x"], data[indices, "y"]))
}

xicor.boot <- c(generate = generate.xi, statistic = xicor.statistic, real.value = 0.3818147, name = "xicor", tau = function(x) x^(1 / 2))

mean.statistic <- function(data, indices) {
  return(mean(data[indices]))
}

mean.generate <- function(n) {
  return(rpower(n, pow = 2, min = 0, max = 1))
}

mean.boot <- c(generate = mean.generate, statistic = mean.statistic, real.value = 3 / 4, tau = sqrt, name = "mean")

max.generate <- function(n) {
  return(runif(n, min = 0, max = 1))
}

max.statistic <- function(data, indices) {
  return(max(data[indices]))
}

max.boot <- c(generate = max.generate, statistic = max.statistic, real.value = 1, name = "max", tau = function(x) x)

shorth.boot <- c(generate = function(n) return(rnorm(n)), statistic = shorth, real.value = 0, name = "shorth",
                 tau = function(n) n^(1 / 3))

# additional models

mean.first.statistic <- function(data, indices){
  return(data[indices[1]])
}

mean.first.generate <- function (n){
  return(rpower(n, pow = 2, min = 0, max = 1))
}

mean.first.boot <- c(generate = mean.first.generate, statistic = mean.first.statistic, real.value = 3/4, name = "mean_first",
                tau = function(n) n^0)

# trying to estimate lambda
poisson.statistic <- function(data, indices){

  x <- data[indices]
  return(sum(x) - (length(x)-1)* var(x))

}

poisson.generate <- function(n){

  lambda <- 2
  return(rpois(n, lambda = lambda))

}

poisson.boot <- c(generate = poisson.generate, statistic = poisson.statistic, real.value = 2, name = "poisson", tau = function (n) n^(-0.5))

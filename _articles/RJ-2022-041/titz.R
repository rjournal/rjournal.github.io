## ----setup, include = FALSE--------------------------------------------
library(knitr)
opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = TRUE,
	cache = TRUE,
	cache.extra = rand_seed
)


## ----sim1--------------------------------------------------------------
library(fastpos)
set.seed(20200219)
find_critical_pos(rho = 0.6, precision_absolute = 0.1, confidence_levels = .8,
                  sample_size_min = 20, sample_size_max = 1e3, n_studies = 1e4)


## ----sim2--------------------------------------------------------------
find_critical_pos(rho = seq(.1, .7, .1), n_studies = 1e5)


## ----takesmin, include=FALSE-------------------------------------------
tictoc::tic()
result <- find_critical_pos(rho = 0.1, sample_size_max = 1e3, n_studies = 1e6)
res <- tictoc::toc()
takesmin <- round((res$toc-res$tic) * 100 / 60)


## ----sim3--------------------------------------------------------------
result <- find_critical_pos(rho = rep(0.1, 100), sample_size_max = 1e3,
                            n_studies = 1e6)


## ----select------------------------------------------------------------
result <- result[, c("pos.80%", "pos.90%", "pos.95%")]
colMeans(result)
round(apply(result, 2, sd), 3)


## ----simulatepos-------------------------------------------------------
pop <- create_pop(rho = 0.1, size = 1e6)
pos <- simulate_pos(x_pop = pop[, 1], y_pop = pop[, 2], n_studies = 1e6,
                    sample_size_min = 20, sample_size_max = 1e3,
                    replace = TRUE, lower_limit = 0, upper_limit = 0.2,
                    progress = FALSE)


## ----quantiles, cache=FALSE--------------------------------------------
quantile(pos, c(.8, .9, .95), na.rm = TRUE)
pos2 <- ifelse(is.na(pos), 1e3, pos)
quantile(pos2, c(.8, .9, .95))


## ----summary2----------------------------------------------------------
result <- find_critical_pos(rho = rep(0.1, 100), sample_size_max = 5e3,
                            n_studies = 1e6)
result <- result[, c("pos.80%", "pos.90%", "pos.95%")]
colMeans(result)
round(apply(result, 2, sd), 3)


## ----correct10k--------------------------------------------------------
result <- find_critical_pos(rho = rep(0.1, 100), sample_size_max = 1e4,
                            n_studies = 1e6)
result <- result[, c("pos.80%", "pos.90%", "pos.95%")]
colMeans(result)
round(apply(result, 2, sd), 3)


## git -C corEvol pull || git clone --single-branch --branch benchmark \

##   https://github.com/johannes-titz/corEvol


## ----bm, warning=FALSE, eval=FALSE-------------------------------------
## library(microbenchmark)
## setwd("corEvol")
## corevol <- function() {
##   source("01-simdata.R")
##   source("02-analyse.R")
## }
## fastpos <- function() {
##   find_critical_pos(rho = .1, sample_size_max = 1e3, n_studies = 1e4,
##                     progress = FALSE)
## }
## bm <- microbenchmark(corevol = corevol(), fastpos = fastpos(), times = 10,
##                      unit = "s")
## summary(bm)

## ----bmmanually, echo=FALSE, cache=FALSE, warning=FALSE----------------
library(microbenchmark)
library(fastpos)
corevol <- function() {
  source("01-simdata.R")
  source("02-analyse.R")
}
fastpos <- function() {
  find_critical_pos(rho = .1, sample_size_max = 1e3, n_studies = 1e4,
                    progress = FALSE)
}
# cache that can be used in html and latex to have the same numbers
if (file.exists("titz_cache/bm.rds")) {
  bm <- readRDS("titz_cache/bm.rds")
} else {
  setwd("corEvol")
  bm <- microbenchmark(corevol = corevol(), fastpos = fastpos(), times = 10, unit = "s")
  setwd("../")
  saveRDS(bm, "titz_cache/bm.rds")
}
summary(bm)

## ----speedup, include=FALSE, cache=FALSE-------------------------------
speedup <- round(summary(bm)$mean[1] / summary(bm)$mean[2])
corevol <- round(summary(bm)$mean[1])


## ----es----------------------------------------------------------------
r <- effectsize::d_to_r(0.5)
lower_limit <- effectsize::d_to_r(0.4)
upper_limit <- effectsize::d_to_r(0.6)
pos <- find_critical_pos(rho = r, sample_size_max = 11e3, n_studies = 1e5,
                         lower_limit = lower_limit, upper_limit = upper_limit)
pos


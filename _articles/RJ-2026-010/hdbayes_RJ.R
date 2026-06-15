################################# installation of hdbayes #################################
## Note: It is important to set type = "source" when installing the package.
## Otherwise, the CmdStan models included in hdbayes may not be compiled during installation.

## install.packages("hdbayes", type = "source")

################################### hdbayes Replication Code ####################################

library(hdbayes)
library(posterior)
library(bayesplot)
library(dplyr)
library(parallel)
library(latex2exp)
library(ggplot2)
library(scales)
library(ggthemes)

########################### Section 4.1: Logistic Regression Example ###########################

data(actg019)
data(actg036)
## center and scale the continuous covariates (age and cd4) based on the current data
age_stats <- with(actg036,
                  c('mean' = mean(age), 'sd' = sd(age)))
cd4_stats <- with(actg036,
                  c('mean' = mean(cd4), 'sd' = sd(cd4)))
actg036$age <- ( actg036$age - age_stats['mean'] ) / age_stats['sd']
actg019$age <- ( actg019$age - age_stats['mean'] ) / age_stats['sd']
actg036$cd4 <- ( actg036$cd4 - cd4_stats['mean'] ) / cd4_stats['sd']
actg019$cd4 <- ( actg019$cd4 - cd4_stats['mean'] ) / cd4_stats['sd']

formula <- outcome ~  age + race + treatment + cd4
p       <- length(attr(terms(formula), "term.labels")) ## number of predictors
family  <- binomial('logit')
## frequentist analysis of current and historical data separately using GLM
fit.mle.cur  <- glm(formula, family, actg036)
fit.mle.hist <- glm(formula, family, actg019)

the.data <- list(actg036, actg019)
## MLEs and 95% confidence intervals from frequentist analysis 
round(confint(fit.mle.hist), 3)
round(confint(fit.mle.cur), 3)

## set up computation parameters
ncores        <- 4 ## maximum number of MCMC chains to run in parallel
nchains       <- 4 ## number of Markov chains
iter_warmup   <- 1000 ## warmup per chain for MCMC sampling
iter_sampling <- 2500 ## number of samples post warmup per chain

## implement each historical data prior in hdbayes:

## fit Bayesian hierarchical model (BHM)
fit.bhm <- glm.bhm(
  formula, family, the.data,
  meta.mean.mean = 0, meta.mean.sd = 10,
  meta.sd.mean = 0, meta.sd.sd = 0.5,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)

## function to pull out the posterior summaries in a convenient form
get_summaries <- function(fit, pars.interest, digits = 3) {
    fit %>%
      select(all_of(pars.interest)) %>%
      summarise_draws(mean, sd, ~quantile(.x, probs = c(0.025, 0.5, 0.975)),
                      .num_args = list(digits = digits))
}
base.pars      <- c("(Intercept)", "age", "race", "treatment", "cd4")
base.pars.hist <- paste(base.pars, "hist", "1", sep="_")
get_summaries(fit = fit.bhm, pars.interest = c(base.pars, base.pars.hist))

## check MCMC diagnostics
fit.bhm %>%
  select(all_of(c(base.pars, base.pars.hist))) %>%
  summarise_draws(rhat, ess_bulk, ess_tail)

## create trace plots and ACF plots
bayesplot::mcmc_trace(fit.bhm, pars = c("age", "race", "treatment", "cd4"))
bayesplot::mcmc_acf(fit.bhm, pars = c("age", "race", "treatment", "cd4"))

## fit commensurate prior
fit.commensurate <- glm.commensurate(
  formula = formula, family = family, data.list = the.data,
  p.spike = 0.1, spike.mean = 200, spike.sd = 0.1,
  slab.mean = 0, slab.sd = 5,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
base.pars.hist <- paste(base.pars, "hist", sep="_")
get_summaries(fit = fit.commensurate, pars.interest = c(base.pars, base.pars.hist))


## fit robust meta-analytic predictive prior (RMAP)
res.rmap <- glm.rmap(
  formula = formula, family = family, data.list = the.data,
  w = 0.1,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
fit.rmap <- res.rmap[["post.samples"]]
get_summaries(fit.rmap, pars.interest = base.pars)


## fit power prior (PP)
n0      <- nrow(actg019)
n       <- nrow(actg036)
a0.star <- (n/n0) * 1/2
fit.pp  <- glm.pp(
  formula = formula, family = family, data.list = the.data,
  a0 = a0.star, ## discounting parameter
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
get_summaries(fit.pp, pars.interest = base.pars)


## generate the plot of prior density on a_{01} (Figure 1 Panel A)

#' Elicit hyperparameters of a Beta distribution from mean and variance or
#' mean and coefficient of variation
#'
#' @param m0 the desired mean, a scalar between 0 and 1.
#' @param v0 the desired mean, a scalar between 0 and 1.
#' @param cv the desired coefficient of variation, a scalar larger than 0.
#'
#' @return a vector containing the elicited shape parameters.
#'
elicit_beta_mean_cv <- function(m0, v0 = NULL, cv = 1) {
  if (!is.null(v0)) {
    a <- -(m0 * v0 + m0 ^ 3 - m0 ^ 2) / v0
    b <- ((m0 - 1) * v0 + m0 ^ 3 - 2 * m0 ^ 2 + m0) / v0
  } else{
    a <- -(m0 * (cv * m0) ^ 2 + m0 ^ 3 - m0 ^ 2) / (cv * m0) ^ 2
    b <- ((m0 - 1) * (cv * m0) ^ 2 + m0 ^ 3 - 2 * m0 ^ 2 + m0) / (cv * m0) ^
      2
  }
  if (a < 0 || b < 0) {
    warning("Warning: at least one of the obtained parameters is not valid")
  }
  return(list(a = a, b = b))
}

beta.pars <- elicit_beta_mean_cv(m0 = a0.star, cv = 1)
curve(dbeta(x, shape1 = beta.pars$a, shape2 = beta.pars$b),
      lwd = 3,
      main = TeX("Prior on $a_{0 1}$"),
      ylab = "Density",
      xlab = TeX("$a_{0 1}$"))
abline(v = a0.star, lwd = 2, lty = 3)
legend(x = "topright",
       legend = c(TeX("$\\pi(a_{0 1})$"),
                  TeX("$a_{0 1} = (1/2) (n/n_0)$")),
       lwd = 2, lty = c(1, 3),
       bty = 'n')


## compute log normalizing constants for PP for a range of a0 values
a0       <- seq(0, 1, length.out = 21)
histdata <- the.data[[2]]

## wrapper to obtain log normalizing constant in parallel package
logncfun <- function(a0, ...){
  hdbayes::glm.npp.lognc(
    formula = formula, family = family, histdata = histdata, a0 = a0, ...
  )
}
cl <- makeCluster(10)
clusterSetRNGStream(cl, 123)
clusterExport(cl, varlist = c('formula', 'family', 'histdata'))
## call created function
a0.lognc <- parLapply(
  cl = cl, X = a0, fun = logncfun, iter_warmup = 2*iter_warmup,
  iter_sampling = 2*iter_sampling, chains = nchains, refresh = 0
)
stopCluster(cl)

a0.lognc <- data.frame( do.call(rbind, a0.lognc) )
head(a0.lognc) %>%
  mutate(across(where(is.numeric), round, 3))

## generate the plot of log normalizing constant versus a_{01} (Figure 1 Panel B)
ggplot(data = a0.lognc, aes(x = a0, y = lognc)) +
  geom_point(alpha = 0.75) +
  labs(x = TeX("$a_{0 1}$"),
       y = "log normalizing constant"
  ) +
  theme_bw(base_size = 15)


## fit normalized power prior (NPP)
fit.npp <- glm.npp(
  formula = formula, family = family, data.list = the.data,
  a0.lognc = a0.lognc$a0,
  lognc = matrix(a0.lognc$lognc, ncol = 1),
  a0.shape1 = beta.pars$a, a0.shape2 = beta.pars$b, ## beta prior on a_{01}
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
get_summaries(fit = fit.npp, pars.interest = c(base.pars, "a0_hist_1"))


## fit normalized asymptotic power prior (NAPP)
fit.napp <- glm.napp(
  formula = formula, family = family, data.list = the.data,
  a0.shape1 = beta.pars$a, a0.shape2 = beta.pars$b,
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
get_summaries(fit = fit.napp, pars.interest = c(base.pars, "a0_hist_1"))


## fit latent exchangeability prior (LEAP)
fit.leap <- glm.leap(
  formula = formula, family = family, data.list = the.data,
  K = 2, prob.conc = rep(1, 2),
  iter_warmup = iter_warmup, iter_sampling = iter_sampling,
  chains = nchains, parallel_chains = ncores,
  refresh = 0
)
get_summaries(fit = fit.leap, pars.interest = c(base.pars, "gamma"))


## generate the plot of posterior estimates for each of the models (Figure 2)
fit.list <- list('BHM' = fit.bhm,
                 'Commensurate' = fit.commensurate,
                 'RMAP' = fit.rmap,
                 'NAPP' = fit.napp,
                 'NPP' = fit.npp,
                 'PP' = fit.pp,
                 'LEAP' = fit.leap)

## function to get current data regression estimates (posterior mean, sd, and 95% CI) for each model
get_quants <- function(x, name){
  pars <- names(coef(fit.mle.cur))
  summs <- get_summaries(x, pars.interest = pars)
  out <- tibble::tibble(
    parameter = pars,
    mean = as.numeric(unlist(summs[, "mean"])),
    sd = as.numeric(unlist(summs[, "sd"])),
    lwr = as.numeric(unlist(summs[, "2.5%"])),
    upr = as.numeric(unlist(summs[, "97.5%"]))
  )
  out$model <- name
  return(out)
}
model.names  <- names(fit.list)
results.list <- lapply(seq_along(fit.list), function(i){
  get_quants(x = fit.list[[i]], name = model.names[i])
})
results      <- do.call(rbind, results.list)

## obtain MLE estimates of current and historical data models
mle.estimates.past <- data.frame(
  parameter = names(coef(fit.mle.hist)),
  value = coef(fit.mle.hist),
  MLE = "Historical"
)
mle.estimates.curr <- data.frame(
  parameter = names(coef(fit.mle.cur)),
  value = coef(fit.mle.cur),
  MLE = "Current"
)
mle.estimates <- rbind(mle.estimates.past,
                       mle.estimates.curr)

results$model <- as.factor(results$model)
LL <- unique(results$model)

## Figure 2: compare posterior estimates from different models along with the MLE estimates
ggplot() +
  geom_pointrange(data = results,
                  mapping = aes(x = model,
                                y = mean,
                                ymin = lwr, ymax = upr,
                                colour = model)) +
  scale_color_colorblind() +
  geom_hline(data = mle.estimates,
             aes(yintercept = value, linetype = MLE)) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  scale_y_continuous("") +
  scale_x_discrete("") +
  facet_wrap(parameter~., scales = "free_y") +
  ggtitle("AIDS example - logistic regression") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



########################## Appendix A: Link selection in binary regression ##########################

library(hdbayes)
library(devtools)
library(tidyverse)
library(ggplot2)

## set up computation parameters
ncores        <- 4 ## maximum number of MCMC chains to run in parallel
nchains       <- 4 ## number of Markov chains
iter_warmup   <- 1000 ## warmup per chain for MCMC sampling
iter_sampling <- 2500 ## number of samples post warmup per chain

## we use the same data sets as in Section 4.1
data("actg019")
data("actg036")

## center and scale the continuous covariates (age and cd4) based on the current data
age_stats <- with(actg036,
                  c('mean' = mean(age), 'sd' = sd(age)))
cd4_stats <- with(actg036,
                  c('mean' = mean(cd4), 'sd' = sd(cd4)))
actg036$age <- ( actg036$age - age_stats['mean'] ) / age_stats['sd']
actg019$age <- ( actg019$age - age_stats['mean'] ) / age_stats['sd']
actg036$cd4 <- ( actg036$cd4 - cd4_stats['mean'] ) / cd4_stats['sd']
actg019$cd4 <- ( actg019$cd4 - cd4_stats['mean'] ) / cd4_stats['sd']

the.data <-  list(actg036,
                  actg019)

### MLE analysis
formula  <- outcome ~  age + race + treatment + cd4
P        <- length(attr(terms(formula), "term.labels"))
## we will compare results from using two different link functions
family1  <- binomial('logit')
family2  <- binomial('probit')
base.pars <- c("(Intercept)", "age", "race", "treatment", "cd4")

get_summaries <- function(fit, pars.interest,
                          digits = 2) {
  ## A little function to pull out the summaries in a convenient form
  out <- subset(posterior::summarise_draws(fit,
                                           .num_args = list(
                                             sigfig = digits,
                                             notation = "dec"
                                           )),
                variable %in% pars.interest)
  
  return(out)
}

## M1: using logit link
## M2: using probit link
fit.mle.cur.m1  <- glm(formula, family1, actg036)
fit.mle.hist.m1 <- glm(formula, family1, actg019)

fit.mle.cur.m2  <- glm(formula, family2, actg036)
fit.mle.hist.m2 <- glm(formula, family2, actg019)

fit.mle.both.m1 <- glm(formula, family1,
                       data = do.call(rbind, the.data))
fit.mle.both.m2 <- glm(formula, family2,
                       data = do.call(rbind, the.data))

summary(fit.mle.hist.m1)
summary(fit.mle.hist.m2)

summary(fit.mle.cur.m1)
summary(fit.mle.cur.m2)

summary(fit.mle.both.m1)
summary(fit.mle.both.m2)

## wrapper function for fitting power prior and computing the log marginal likelihood under each model
fit_pp_both <-  function(a){
  
  cat("Doing a_0:", a, "\n")
  
  ## fit power prior
  f1 <- glm.pp(
    formula,
    family1,
    data.list = the.data,
    a0 = a,
    chains = nchains, 
    parallel_chains = ncores,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, 
    adapt_delta = .999,
    max_treedepth = 13,
    refresh = 0
  )
  
  f2 <- glm.pp(
    formula,
    family2,
    data.list = the.data,
    a0 = a,
    chains = nchains, 
    parallel_chains = ncores,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, 
    adapt_delta = .999,
    max_treedepth = 13,
    refresh = 0
  )
  
  ## compute log marginal likelihood
  marglike.m1 <-   glm.logml.pp(
    post.samples = f1,
    bridge.args = list(silent = TRUE),
    chains = nchains, 
    parallel_chains = ncores,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, 
    adapt_delta = .999,
    max_treedepth = 13,
    refresh = 0
  )
  
  marglike.m2 <- glm.logml.pp(
    post.samples = f2,
    bridge.args = list(silent = TRUE),
    chains = nchains, 
    parallel_chains = ncores,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, 
    adapt_delta = .999,
    max_treedepth = 13,
    refresh = 0
  )
  
  ml.tab <- data.frame(a_0 = a, 
                       m1_marglik = marglike.m1$logml,
                       m2_marglik = marglike.m2$logml,
                       log_BF = marglike.m1$logml - marglike.m2$logml)
  
  out <- list(
    m1_fit = f1,
    m2_fit = f2,
    marglik_res = ml.tab
  )
  
  return(out)
}

## compute log Bayes factor comparing M1 (logit link) and M2 (probit link) for various values of the discounting parameter a0 (between 0 and 1) 
a0.grid <- (0:10)/10 

run.time <- system.time(
  all.fits <- lapply(a0.grid, fit_pp_both)
)
run.time

BF.tab <- do.call(rbind, lapply(all.fits,
                                function(x) x$marglik_res))

## generate the plot of log Bayes factor for a0 values varying from 0 to 1 (Figure S1)
pp <- ggplot(data = BF.tab,
             aes(x = a_0, y = log_BF)) +
  geom_point(size = 2.5) +
  scale_x_continuous(expression(a[0])) + 
  scale_y_continuous("log Bayes factor") + 
  geom_hline(yintercept = 0, linetype = "longdash",
             linewidth = 1) + 
  geom_hline(yintercept = log(3), linetype = "dotted",
             linewidth = 1) + 
  theme_bw(base_size = 20)
pp

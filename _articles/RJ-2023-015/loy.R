## -----------------------------------------------------------------------------
## Code supplement for "Bootstrapping Clustered Data in R using lmeresampler"
## -----------------------------------------------------------------------------


## Setup
library(lmeresampler)
library(HLMdiag)
library(lme4)
library(nlme)
library(ggplot2)
library(nullabor)


## A two-level example: JSP data -----------------------------------------------

# Examine the data
tibble::as_tibble(jsp728)


# Fit the model
jsp_mod <- lmer(mathAge11 ~ mathAge8 + gender + class + (1 | school), data = jsp728)


# Run the residual bootstrap
(jsp_boot <- bootstrap(jsp_mod, .f = fixef, type = "residual", B = 2000))


# Calculate confidence intervals
confint(jsp_boot)


## User-specified statistics: Estimating repeatibility -------------------------

# Load the data and fit the model
data("BeetlesBody", package = "rptR")
(beetle_mod <- lmer(BodyL ~ (1 | Population), data = BeetlesBody))


# Function to calculate the repeatability
repeatability <- function(object) {
  vc <- as.data.frame(VarCorr(object))
  vc$vcov[1] / (sum(vc$vcov))
}


# Calculate observed repeatability
repeatability(beetle_mod)


# Run the parametric bootstrap
(beetle_boot <- bootstrap(beetle_mod, .f = repeatability, type = "parametric", B = 2000))

# Calculate basic bootstrap confidence interval
(beetle_ci <- confint(beetle_boot, type = "basic"))


# Density plot of the bootstrap distribution
plot(beetle_boot, .width = c(.5, .9)) + 
  ggplot2::labs(
    title = "Bootstrap repeatabilities",
    y = "density",
    x = "repeatability"
  )


## Bootstrap tests for a single parameter --------------------------------------

# Observed summary for fixed-effects coefficients
summary(jsp_mod)$coefficients


# Fit reduced model without the class binary variable
reduced_model <- update(jsp_mod, . ~ . - class)

# Bootstrap the reduced model via Wild bootstrap
reduced_boot <- bootstrap(reduced_model, type = "wild", B = 1000, hccme = "hc2", 
                          aux.dist = "mammen", .refit = FALSE)


# Function to extract t-values
extract_t <- function(model, term) {
  coef(summary(model))[term, "t value"]
}

# Refit model and extract t-vales for the class variable
tstats <- purrr::map_dbl(
  reduced_boot, 
  ~refit(jsp_mod, .x) %>% extract_t(., term = "classnonmanual")
)


# Calculate bootstrap p-value
(sum(abs(tstats) >= extract_t(jsp_mod)) + 1) / (1000 + 1)


# Use bootstrap_pvals to iterate through each fixed effect
bootstrap_pvals(jsp_mod, type = "wild", B = 1000, hccme = "hc2", aux.dist = "mammen")


## Bootstrap tests for model comparison

# Load data
data("Machines", package = "nlme")


# Fit the reduced model
reduced_mod <- lmer(score ~ Machine + (1 | Worker), data = Machines, REML = FALSE)


# Fit the full model
full_mod <- lmer(score ~ Machine + (1 | Worker/Machine), data = Machines, REML = FALSE)


# Observed F statistic
observed <- anova(full_mod, reduced_mod)$Chisq[2]

# Bootstrap the reduced model
reduced_boot <- bootstrap(reduced_mod, type = "residual", B = 1000, .refit = FALSE)


# Function to compare the full and observed models
compare_models <- function(full, reduced, newdata) {
  full_mod <- refit(full, newdata, 
                    control = lmerControl(check.conv.singular = "ignore", 
                                          check.conv.grad = "ignore"))
  reduced_mod <- refit(reduced, newdata)
  anova(full_mod, reduced_mod)$Chisq[2]
}

# Iterate through the simulated data sets from the reduced model and compare models
chisq_stats <- purrr::map_dbl(reduced_boot, 
                              ~compare_models(full_mod, reduced_mod, newdata = .x))

# Calculate bootstrap p-value
(sum(chisq_stats >= observed) + 1) / (1000 + 1)


## Simulation-based model diagnostics ------------------------------------------

# Fit the model
dialyzer_mod <- lme(
  rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4)) * QB, 
  data = Dialyzer, 
  random = ~ pressure + I(pressure^2)
)


# Render the residual plot
ggplot(Dialyzer, aes(x = pressure, y = resid(dialyzer_mod))) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_point(shape = 1) +
  theme_bw() +
  labs(x = "Transmembrane pressure (dmHg)", y = "Residuals (ml/hr)")


# Simulate residual plots via the parametric bootstrap
set.seed(1234)
sim_resids <- bootstrap(dialyzer_mod, .f = hlm_resid, type = "parametric", B = 19)


# Check the simulated data sets
dplyr::glimpse(sim_resids$replicates)


# Format data for lineup of residual plots
lineup_data <- lineup(true = hlm_resid(dialyzer_mod), 
                      n = 19, 
                      samples = sim_resids$replicates)

# Check the data structure
dplyr::glimpse(lineup_data)


# Create a lineup of residual plots
ggplot(lineup_data, aes(x = pressure, y = .resid)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_point(shape = 1) +
  facet_wrap(~.sample) +
  theme_bw() +
  labs(x = "Transmembrane pressure (dmHg)", y = "Residuals (ml/hr)")


# Use Wild bootstrap to cope with heteroscedasticity
wild_dialyzer <- bootstrap(dialyzer_mod, .f = fixef, type = "wild", B = 1000, 
                           hccme = "hc2", aux.dist = "webb")

confint(wild_dialyzer, type = "perc")


## Bootstrapping in parallel ---------------------------------------------------

# Load packages
library(foreach)
library(doParallel)

# Run 1000 resamples on each core
nboot <- rep(1000, 2)

set.seed(5678)

# Starting a cluster with 2 cores
no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cores = no_cores)

# Run 1000 bootstrap iterations on each core
ptime <- system.time({
  boot_parallel <- foreach(
    B = nboot, 
    .combine = combine_lmeresamp,
    .packages = c("lmeresampler", "lme4")
  ) %dopar% {
    bootstrap(jsp_mod, .f = fixef, type = "parametric", B = B)
  }
})

# Stop the cluster
stopCluster(cl)

# Sequential timing (i.e., not parallel)
stime <- system.time({
  boot_parallel <- foreach(
    B = nboot, 
    .combine = combine_lmeresamp,
    .packages = c("lmeresampler", "lme4")
  ) %do% {
    bootstrap(jsp_mod, .f = fixef, type = "parametric", B = B)
  }
})


## Timing in parallel
# Starting a cluster with 2 cores
no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cores = no_cores)

# Run 1000 bootstrap iterations on each core
boot_parallel <- foreach(
  B = rep(1000, 2), 
  .combine = combine_lmeresamp,
  .packages = c("lmeresampler", "lme4")
) %dopar% {
  bootstrap(jsp_mod, .f = fixef, type = "parametric", B = B)
}

# Stop the cluster
stopCluster(cl)


# Print sequential times
stime


# Print parallel times
ptime


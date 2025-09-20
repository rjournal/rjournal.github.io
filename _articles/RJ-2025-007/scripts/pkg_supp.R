# Supplemental information with additional code for 
# Structured Bayesian Regression Tree Models for Estimating Distributed Lag Effects: The R Package dlmtree
# Authors: Seongwon Im, Ander Wilson, Daniel Mork

# Table of contents
# 1. Data pivoting from time-series format to wide format
# 2. Fitting nonlinear tree structured DLMs
#   2-1. Tree structured distributed lag nonlinear model (TDLNM)
#   2-2. monotone-TDLNM
# 3. Using 'diagnose' function for assessing MCMC convergence

# Environment
library(dlmtree)
library(dplyr)
library(ggplot2)
set.seed(1)


# ---------------------------------------------- #
# 1. Data pivoting
# ---------------------------------------------- #
# An example of a data frame with time-series format
ts.data <- data.frame(
  date = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 100),
  outcome = rpois(100, lambda = 5),
  e1 = rnorm(100, mean = 35, sd = 5),     # simulated exposure 1
  e2 = rgamma(100, shape = 2, rate = 1)   # simulated exposure 2
)
head(ts.data)

# An example of format converting function 
# Notes: 
# 1. The original data must have a complete set of dates
# 2. The lag columns of the first few dates of the resulting data frame will contain NA values,
#    because the exposure measurements of their lags are not available due to the earlier dates. 
#    The function below automatically removes the earlier dates with NA values.
to_lagged_form <- function(data, num_lags, exp_names){
  for(exp in exp_names){
    for (i in 1:num_lags) {
      # Name a new column
      lag_name <- paste0(exp, "_", i) 
      
      # Create lagged columns
      data <- data %>% mutate(!!lag_name := lag(get(exp), n = i)) 
    }
  }
  
  # Remove the exposure columns from the ts.data
  data <- data %>% select(!all_of(exp_names)) %>% filter(complete.cases(.))
  
  return(data)
}

# Dataset in lagged format (Total lag of 3)
lag.data <- to_lagged_form(ts.data, 3, c("e1", "e2")) 
head(lag.data)




# ---------------------------------------------- #
# 2. Model fitting preparation
# ---------------------------------------------- #
# Data
sbd <- get_sbd_dlmtree()

# Response and covariates
sbd_cov <- sbd %>% select(bwgaz, ChildSex, MomAge, GestAge, MomPriorBMI, Race, 
                          Hispanic, MomEdu, SmkAny, Marital, Income, 
                          EstDateConcept, EstMonthConcept, EstYearConcept) 

# Exposure data
sbd_exp <- list(PM25 = sbd %>% select(starts_with("pm25_")),
                TEMP = sbd %>% select(starts_with("temp_")),
                SO2 = sbd %>% select(starts_with("so2_")),
                CO = sbd %>% select(starts_with("co_")),
                NO2 = sbd %>% select(starts_with("no2_")))
sbd_exp <- sbd_exp %>% lapply(as.matrix)


# ---------------------------------------------- #
# 2-1. TDLNM
# ---------------------------------------------- #
# Splits specified with an integer
tdlnm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI + 
                       Race + Hispanic + SmkAny + EstMonthConcept,
                     data = sbd_cov,
                     exposure.data = sbd_exp[["TEMP"]],
                     dlm.type = "nonlinear",
                     family = "gaussian",
                     control.tdlnm = list(exposure.splits = 20),
                     control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# Summary
tdlnm.sum <- summary(tdlnm.fit)

# exposure concentration - time effect surface
plot(tdlnm.sum, plot.type = "mean")

# slicing on exposure-concentration
plot(tdlnm.sum, plot.type = "slice", val = 1, main = "Slice at concentration 1") 
plot(tdlnm.sum, plot.type = "slice", val = 2, main = "Slice at concentration 2")

# slicing on time lag
plot(tdlnm.sum, plot.type = "slice", time = 7, main = "Slice at time 7")
plot(tdlnm.sum, plot.type = "slice", time = 15, main = "Slice at time 15")
plot(tdlnm.sum, plot.type = "slice", time = 33, main = "Slice at time 33")

# different plot.type options
# Standard error, credible intervals
plot(tdlnm.sum, plot.type = "se", main = "Standard error")  
plot(tdlnm.sum, plot.type = "ci-min", main = "Credible interval lower bound")
plot(tdlnm.sum, plot.type = "ci-max", main = "Credible interval upper bound")

# Cumulative effect and significance
plot(tdlnm.sum, plot.type = "cumulative", main = "Cumulative effect per exposure-concentration")
plot(tdlnm.sum, plot.type = "effect", main = "Significant effects with directions")


# Extracting cumulative effect
tdlnm.sum$cumulative.effect


# Comparing varying cumulative effect for exposure-concentration to average
temp.mean <- mean(sbd_exp[["TEMP"]])
tdlnm.sum$cumulative.effect %>% 
  mutate(dev = vals - temp.mean) %>% 
  ggplot(., aes(x = dev, y = mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_bw() +
  labs(title = "TDLNM: Cumulative effect per exposure-concentration relative to average",
       x = "Deviation from the average temperature", y = "Exposure effect")



# ---------------------------------------------- #
# 2-2. monotone-TDLNM
# ---------------------------------------------- #
monotone.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI + 
                          Race + Hispanic + SmkAny + EstMonthConcept,
                        data = sbd_cov,
                        exposure.data = sbd_exp[["TEMP"]],
                        dlm.type = "monotone",
                        family = "gaussian",
                        control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# Summary
monotone.sum <- summary(monotone.fit)

# exposure concentration - time effect surface
plot(monotone.sum, plot.type = "mean")

# slicing on exposure-concentration
plot(monotone.sum, plot.type = "slice", val = 1, main = "Slice at concentration 1") 
plot(monotone.sum, plot.type = "slice", val = 2.5, main = "Slice at concentration 2.5")

# slicing on time lag
plot(monotone.sum, plot.type = "slice", time = 5, main = "Slice at time 5")
plot(monotone.sum, plot.type = "slice", time = 25, main = "Slice at time 25")
plot(monotone.sum, plot.type = "slice", time = 35, main = "Slice at time 33")

# different plot.type options
# Standard error, credible intervals
plot(monotone.sum, plot.type = "se", main = "Standard error")  
plot(monotone.sum, plot.type = "ci-min", main = "Credible interval lower bound")
plot(monotone.sum, plot.type = "ci-max", main = "Credible interval upper bound")

# Cumulative effect and significance
plot(monotone.sum, plot.type = "cumulative", main = "Cumulative effect per exposure-concentration")
plot(monotone.sum, plot.type = "effect", main = "Significant effects with directions")

# extracting cumulative effect
monotone.sum$cumulative.effect

# Comparing varying cumulative effect for exposure-concentration to average
temp.mean <- mean(sbd_exp[["TEMP"]])
monotone.sum$cumulative.effect %>% 
  mutate(dev = vals - temp.mean) %>% 
  ggplot(., aes(x = dev, y = mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  theme_bw() +
  labs(title = "TDLNM-monotone: Cumulative effect per exposure-concentration relative to average",
       x = "Deviation from the average temperature", y = "Exposure effect")




# ---------------------------------------------- #
# 3. 'diagnose' function
# ---------------------------------------------- #

# We use the same code for fitting TDLM used in the main text. 
# Same process can be used for any model embedded in dlmtree package
tdlm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI +
                      Race + Hispanic + SmkAny + EstMonthConcept,
                    data = sbd_cov,
                    exposure.data = sbd_exp[["PM25"]], 
                    family = "gaussian",
                    dlm.type = "linear",
                    control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# Apply summary function with 'mcmc = T'
tdlm.sum <- summary(tdlm.fit, mcmc = T)

# Launch a shiny app
diagnose(tdlm.sum)
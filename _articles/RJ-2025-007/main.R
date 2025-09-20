# Code for reproducing results in the main text of
# Structured Bayesian Regression Tree Models for Estimating Distributed Lag Effects: The R Package dlmtree
# Authors: Seongwon Im, Ander Wilson, Daniel Mork

# Section 4.2
library(dlmtree)
library(dplyr)
set.seed(1)

# Load data
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


# Section 4.3
# TDLM fitting
tdlm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI +
                      Race + Hispanic + SmkAny + EstMonthConcept,
                    data = sbd_cov,
                    exposure.data = sbd_exp[["PM25"]], # A single numeric matrix
                    family = "gaussian",
                    dlm.type = "linear",
                    control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# TDLM summary
tdlm.sum <- summary(tdlm.fit)
print(tdlm.sum)

# TDLM cumulative effect
tdlm.sum$cumulative.effect

# TDLM plot
plot(tdlm.sum, main = "Estimated effect of PM2.5", xlab = "Time", ylab = "Effect") 

# Section 4.4
# TDLMM fitting
tdlmm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI +
                       Race + Hispanic + SmkAny + EstMonthConcept,
                     data = sbd_cov,
                     exposure.data = sbd_exp,
                     family = "gaussian",
                     dlm.type = "linear",
                     mixture = TRUE,
                     control.mix = list(interactions = "noself"),
                     control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# TDLMM summary with marginalization options
tdlmm.sum <- summary(tdlmm.fit, marginalize = "mean") 
tdlmm.sum.percentile <- summary(tdlmm.fit, marginalize = 25)
tdlmm.sum.level <- summary(tdlmm.fit, marginalize = c(1, 1, 1, 1, 1))                     
print(tdlmm.sum)

# TDLMM adjusting for expected co-exposure
tdlmm.coexp <- adj_coexposure(sbd_exp, tdlmm.fit, contrast_perc = c(0.25, 0.75))

# TDLMM cumulative effect
tdlmm.sum$DLM$PM25$cumulative

# TDLMM plotting
# Main effects
library(gridExtra)
p1 <- plot(tdlmm.sum, exposure1 = "PM25", main = "PM2.5") 
p2 <- plot(tdlmm.sum, exposure1 = "TEMP", main = "Temperature") 
p3 <- plot(tdlmm.sum, exposure1 = "NO2", main = "NO2")
grid.arrange(p1, p2, p3, nrow = 1)

# Interaction effects
plot(tdlmm.sum, exposure1 = "PM25", exposure2 = "TEMP")


# Section 4.5
# HDLM
hdlm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI +
                      Race + Hispanic + SmkAny + EstMonthConcept,
                    data = sbd_cov,
                    exposure.data = sbd_exp[["PM25"]],
                    family = "gaussian",
                    dlm.type = "linear",
                    het = TRUE,
                    control.het = list(
                      modifiers = c("ChildSex", "MomAge", "MomPriorBMI", "SmkAny"),
                      modifier.splits = 10),
                    control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# HDLM summary
hdlm.sum <- summary(hdlm.fit)
print(hdlm.sum)

# HDLMM
hdlmm.fit <- dlmtree(formula = bwgaz ~ ChildSex + MomAge + MomPriorBMI +
                       Race + Hispanic + SmkAny + EstMonthConcept,
                     data = sbd_cov,
                     exposure.data = sbd_exp,
                     family = "gaussian",
                     dlm.type = "linear",
                     mixture = TRUE,
                     het = TRUE,
                     control.het = list(
                       modifiers = c("ChildSex", "MomAge", "MomPriorBMI", "SmkAny"),
                       modifier.splits = 10),
                     control.mcmc = list(n.burn = 2500, n.iter = 10000, n.thin = 5))

# HDLM shiny interface
shiny(hdlm.fit)

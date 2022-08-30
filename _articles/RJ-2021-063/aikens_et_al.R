#----------------------------------------------------------
### CONTAINS: 
# R script to replicate all examples from Aikens 2021
# Expects Installed: stratamatch (v0.1.7)
#                    glmnet (v4.0)
#                    dplyr (v1.0.1)
#
# For sample code to replicate runtime results in Figure 5, see
# figure_5_replication.R
#----------------------------------------------------------

#----------------------------------------------------------
### 4.1 Simulated Example
#----------------------------------------------------------
library("stratamatch")
library("dplyr")
set.seed(125)
dat <- make_sample_data(n = 10000)
head(dat)

# 4.1.1 Automatic Stratification
#----------------------------------------------------------
a.strat <- auto_stratify(dat, treat = "treat", prognosis = outcome ~ X1 + X2,
                         pilot_fraction = 0.1, size = 500)
print(a.strat)

# 4.1.2 Diagnostics
#----------------------------------------------------------
a.strat$issue_table

# Figure 1
## A
plot(a.strat, type = "SR")
## B
plot(a.strat, type = "hist",
     propensity = treat ~ X2 + X1 + B1 + B2,
     stratum = 1)

# Figure 2
## A
plot(a.strat, type = "AC",
     propensity = treat ~ X2 + X1 + B1 + B2)
## B
plot(a.strat, type = "AC",
     propensity = treat ~ X2 + X1 + B1 + B2,
     stratum = 1)

# Residual plots (not shown in paper)
# plot(a.strat, type = "residual")

# Prognostic model summary
prog_model <- a.strat$prognostic_model
summary(prog_model)

# 4.1.2 Matching
#----------------------------------------------------------
mymatch <- strata_match(a.strat, model = treat ~ X1 + X2 + B1 + B2)

# Summary (not shown in paper)
summary(mymatch)

#----------------------------------------------------------
### 4.2 Simulated Example
#----------------------------------------------------------
set.seed(123)

# 4.2.1 Automatic Stratification
#----------------------------------------------------------
ICU_astrat <- auto_stratify(data = ICU_data, 
                            treat = "surgicalTeam",
                            prognosis = DNR ~ Birth.preTimeDays + Female.pre +
                              RaceAsian.pre + RaceUnknown.pre + RaceOther.pre +
                              RaceBlack.pre + RacePacificIslander.pre +
                              RaceNativeAmerican.pre + all_latinos,
                            pilot_fraction = 0.1,
                            size = 500)
print(ICU_astrat)
summary(ICU_astrat)

# 4.2.2 Manual Stratification
#----------------------------------------------------------
ICU_mstrat <- manual_stratify(data = ICU_data,
                              strata_formula = surgicalTeam ~ Female.pre +
                                RaceAsian.pre + RaceUnknown.pre + RaceOther.pre +
                                RaceBlack.pre + RacePacificIslander.pre +
                                RaceNativeAmerican.pre + all_latinos)

summary(ICU_mstrat)

# 4.2.3 Diagnostics
#----------------------------------------------------------
ICU_mstrat$issue_table

# Figure 3
## A
plot(ICU_mstrat, type = "SR")
## B
plot(ICU_astrat, type = "SR")

# Figure 4
plot(ICU_astrat, type = "AC", propensity = surgicalTeam ~ Birth.preTimeDays +
       Female.pre + RaceAsian.pre + RaceUnknown.pre + RaceOther.pre +
       RaceBlack.pre + RacePacificIslander.pre + RaceNativeAmerican.pre,
     stratum = 2)

# with jitter (not shown in paper)
plot(ICU_astrat, type = "AC", propensity = surgicalTeam ~ Birth.preTimeDays +
       Female.pre + RaceAsian.pre + RaceUnknown.pre + RaceOther.pre +
       RaceBlack.pre + RacePacificIslander.pre + RaceNativeAmerican.pre +
       all_latinos, stratum = 2, jitter_propensity = 0.005)

# 4.2.4 Matching
#----------------------------------------------------------
ICU_match <- strata_match(ICU_astrat, 
                          model = surgicalTeam ~ Birth.preTimeDays + 
                            Female.pre + RaceAsian.pre + RaceUnknown.pre +
                            RaceOther.pre + RaceBlack.pre +
                            RacePacificIslander.pre + RaceNativeAmerican.pre + 
                            all_latinos, 
                          k = 2)
summary(ICU_match)

#----------------------------------------------------------
### 5.1 Designing the selection of the pilot set
#----------------------------------------------------------

ICU_split <- split_pilot_set(ICU_data, treat = "surgicalTeam",
                             pilot_fraction = 0.1,
                             group_by_covariates = c("Female.pre", "self_pay"))

ICU_astrat2 <- auto_stratify(data = ICU_split$analysis_set,
                             treat = "surgicalTeam",
                             prognosis = DNR ~ Birth.preTimeDays + Female.pre +
                               RaceAsian.pre + RaceUnknown.pre + RaceOther.pre +
                               RaceBlack.pre + RacePacificIslander.pre +
                               RaceNativeAmerican.pre + all_latinos,
                             pilot_sample = ICU_split$pilot_set,
                             size = 500)

#----------------------------------------------------------
### 5.1 Fitting the prognostic model
#----------------------------------------------------------

library(glmnet)
x_pilot <- ICU_split$pilot_set %>%
  dplyr::select(Birth.preTimeDays, Female.pre, RaceAsian.pre, RaceUnknown.pre,
                RaceOther.pre, RaceBlack.pre, RacePacificIslander.pre,
                RaceNativeAmerican.pre, all_latinos) %>%
  as.matrix()

y_pilot <- ICU_split$pilot_set %>%
  dplyr::select(DNR) %>%
  as.matrix()

cvfit <- cv.glmnet(x_pilot, y_pilot, family = "binomial")

x_analysis <- ICU_split$analysis_set %>% 
  dplyr::select(Birth.preTimeDays, Female.pre, RaceAsian.pre, RaceUnknown.pre,
                RaceOther.pre, RaceBlack.pre, RacePacificIslander.pre,
                RaceNativeAmerican.pre, all_latinos) %>%
  as.matrix()

lasso_scores <- predict(cvfit, newx = x_analysis, s = "lambda.min", type = "response")

ICU_astrat3 <- auto_stratify(data = ICU_split$analysis_set,
                             treat = "surgicalTeam",
                             outcome = "DNR",
                             prognosis = lasso_scores,
                             pilot_sample = ICU_split$pilot_set,
                             size = 500)

## Note: Code to generate runtime results as shown in Figure 5 is included in
## figure_5_replication.R
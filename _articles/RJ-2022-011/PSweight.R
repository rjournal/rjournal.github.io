##install package, it is on CRAN
#install.packages("PSweight") 

set.seed(2020)
library(PSweight)

#could check the covariate name
#str(NCDS)

########################################################################################################################
### Estimating generalized propensity scores and balance assessment ####################################################
########################################################################################################################


# Define the propensity formula for multiple treatment.
ps.mult <- Dmult ~ white + maemp + as.factor(scht) + as.factor(qmab)+ as.factor(qmab2) + as.factor(qvab) +
  as.factor(qvab2) + paed_u + maed_u+ agepa + agema + sib_u + paed_u * agepa + maed_u * agema

# Generate the balance check statistics for IPW, overlap, matching and entropy weights.
bal.mult <- SumStat(ps.formula = ps.mult, weight = c("IPW", "overlap", "matching", "entropy"), data = NCDS)

# For multiple treatment group, the histogram is disabled, instead, the propensity distribution is illustrated with density plot.
plot(bal.mult, type = "density")

# Use the love plot to assess the covariate balance for each covariate based on ASD.
plot(bal.mult,metric = "ASD")


########################################################################################################################
### Generalized propensity score trimming ##############################################################################
########################################################################################################################


# Generate the balance check statistics for IPW, overlap, matching and entropy weights with trimming threshold 0.067.
bal.mult.trim <- SumStat(ps.formula = ps.mult, weight = c("IPW", "overlap", "matching", "entropy"), data = NCDS,delta = 0.067)

# Info about how many sample on each group get trimmed.
bal.mult.trim

# Use the love plot to access the covariate balance for each covariate based on ASD on trimmed data.
plot(bal.mult.trim,metric = "ASD")

# Info about how many sample on each group get trimmed under the optimal trimming criterion.
PStrim(ps.formula = ps.mult, data = NCDS, optimal = TRUE)


########################################################################################################################
### Estimation and inference of pairwise (weighted) average treatment effects ##########################################
########################################################################################################################


# Specify the contrast of interest for the three treatment groups.
contrasts.mult <- rbind(c(1,-1, 0), c(1, 0,-1), c(0, -1, 1))

# The IPW estimator in multiple treatment case with clsoed-form variance estimation.
ate.mult <- PSweight(ps.formula = ps.mult, yname = "wagebin", data = NCDS, weight = "IPW")

# The inference on combined population: risk ratio with the specified contrast (the inference is in log form).
sum.ate.mult.rr <- summary(ate.mult, type = "RR", contrast = contrasts.mult)

sum.ate.mult.rr

# The inference on combined population: risk ratio with the specified contrast (the inference is in original form).
exp(sum.ate.mult.rr$estimates[,c(1,4,5)])

# The overlap estimator in multiple treatment case with closed-form variance estimation.
ato.mult <- PSweight(ps.formula = ps.mult, yname = "wagebin", data = NCDS,weight = "overlap")

# The inference on overlap population: risk ratio with the specified contrast(the inference is in original form).
sum.ato.mult.rr <- summary(ato.mult, type = "RR", contrast = contrasts.mult)
exp(sum.ato.mult.rr$estimates[,c(1,4,5)])

# The test on the ratio group3/group2=group2/group1 is also possible with the following contrast.
summary(ato.mult, type = "RR", contrast = c(1, 1, -2), CI = FALSE)


# We could also study the inference on odds ratio with the specified contrast (the inference is in both log and original forms).
sum.ato.mult.or <- summary(ato.mult, type = "OR", contrast = contrasts.mult)
exp(sum.ato.mult.or$estimates[,c(1,4,5)])


# The overlap estimator in multiple treatment case with clsoed-form variance estimation with augmentation.
# Outcome regression formula for binary wage.
out.wagebin <- wagebin ~ white + maemp + as.factor(scht) + as.factor(qmab)+ as.factor(qmab2) + as.factor(qvab) + as.factor(qvab2) +
  paed_u + maed_u+ agepa + agema + sib_u + paed_u * agepa + maed_u * agema

# The overlap estimator in multiple treatment case with clsoed-form variance estimation and augmentation. This may take 2 minute.
ato.mult.aug <- PSweight(ps.formula = ps.mult, yname = "wagebin", data = NCDS,  augmentation = TRUE, out.formula = out.wagebin,
                         family = "binomial")

# The inference on overlap population: risk ration, with the specified contrast (the inference is in both log and original forms).
sum.ato.mult.aug.rr<- summary(ato.mult.aug, type = "RR", contrast = contrasts.mult)
sum.ato.mult.aug.rr
exp(sum.ato.mult.aug.rr$estimates[,c(1,4,5)])




########################################################################################################################
### Using machine learning to estimate propensity scores and potential outcomes ########################################
########################################################################################################################

# Specify the propensity model using gbm. Interaction is removed,
# because boosted regression is already capable of capturing non-linear effects and interactions.
ps.any.gbm <- Dany ~ white + maemp + as.factor(scht) + as.factor(qmab) + as.factor(qmab2) +
  as.factor(qvab) + as.factor(qvab2) + paed_u + maed_u + agepa + agema + sib_u


# The balance assessment is based on ASD.
set.seed(2020)
bal.any.gbm <-SumStat(ps.formula = ps.any.gbm, data= NCDS, weight = "overlap", method = "gbm", ps.control = list(distribution = "adaboost"))
#plot(bal.any.gbm, metric = "ASD") #balance could also be checked through plot


# Augmented estimator with propensity scores and potential outcomes both estimated through gbm
set.seed(2020)
out.wage.gbm <- wage ~ white + maemp + as.factor(scht) + as.factor(qmab)+ as.factor(qmab2) +
  as.factor(qvab) + as.factor(qvab2) + paed_u + maed_u+ agepa+ agema + sib_u
ato.any.aug.gbm <- PSweight(ps.formula = ps.any.gbm, zname = "Dmult",yname = "wagebin", data = NCDS, weight = "overlap",
                             augmentation = TRUE, out.formula = out.wage.gbm, ps.method = "gbm", ps.control = list(distribution = "adaboost"),out.method = "gbm")
# including between-group difference, estiamted standard error, test statistics and p-value.
summary(ato.any.aug.gbm, CI = FALSE)









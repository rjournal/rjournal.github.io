# Main article: Bayesian analysis (using shinybrms) -----------------------

library("shinybrms")
launch_shinybrms(launch.browser = TRUE)
# Now follow the instructions from the paper. The dataset to be read into the
# app is appended here (file 'CAP.csv').

# Supplement: Frequentist analysis ----------------------------------------

# Read data and fit the model:
CAP <- read.csv("CAP.csv")
library(lme4)
lmm <- lmer(TWI ~ age + anticoagulation + diabetes + day * trt + (1 | patID),
            data = CAP)

# Summary:
print(summary(lmm),
      correlation = FALSE,
      ranef.comp = "Std.Dev.",
      show.resids = FALSE)

# 95% profile CIs:
( prof_cis <- confint(lmm, quiet = TRUE, oldNames = FALSE) )

# Custom summary for interactions:
coefs <- fixef(lmm)
ncoefs <- length(coefs)
contr_mat <- matrix(0, nrow = length(unique(CAP$day)), ncol = ncoefs,
                    dimnames = list(paste0("CAP_", unique(CAP$day)),
                                    names(coefs)))
contr_mat[, "trtCAP"] <- 1
for (day_i in setdiff(unique(CAP$day), "d1")) {
  contr_mat[paste0("CAP_", day_i), paste0("day", day_i, ":trtCAP")] <- 1
}
library(multcomp)
linhyp <- glht(lmm, linfct = contr_mat)
confint(linhyp, calpha = univariate_calpha())

# Conditional-effects plot:
library(emmeans)
rg <- ref_grid(lmm, at = list(anticoagulation = "no", diabetes = "no"))
em <- emmeans(rg, specs = c("day", "trt"))
plem <- plot(em, plotit = FALSE)
library(ggplot2)
dg <- position_dodge(width = 0.2)
ggplot(plem, aes(x = day, y = the.emmean, color = trt, group = trt)) +
  geom_point(position = dg) +
  ylab("TWI") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = dg,
                width = 0.25)

# Predictive check (i.e., the frequentist version of the PPC):
library(performance)
theme_set(theme_bw())
set.seed(847299)
check_predictions(lmm, iterations = 8)
check_predictions(lmm, iterations = 4000, check_range = TRUE)

# Visualization of the CIs:
point_estims <- c(sd_patID = attr(VarCorr(lmm)$patID, "stddev"),
                  sigma = sigma(lmm),
                  fixef(lmm))
prof_cis_gg <- cbind(as.data.frame(prof_cis),
                     point_est = point_estims,
                     Parameter = rownames(prof_cis))
pars_sel <- grep("^sigma$|^sd_|:trthealthy",
                 row.names(prof_cis_gg),
                 invert = TRUE)
prof_cis_gg <- prof_cis_gg[pars_sel, ]
prof_cis_gg <- within(prof_cis_gg, {
  Parameter <- factor(Parameter, levels = rev(unique(Parameter)))
})
ggplot(prof_cis_gg, aes(y = Parameter)) +
  geom_point(aes(x = point_est)) +
  xlab("") +
  geom_errorbar(aes(xmin = `2.5 %`, xmax = `97.5 %`),
                width = 0.25)

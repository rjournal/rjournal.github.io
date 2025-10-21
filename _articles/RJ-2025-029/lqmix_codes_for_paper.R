install.packages("lqmix")

# load libraries
library(ggplot2)
library(lqmix)
library(lme4)


# --- spaghetti plot - Figure 1 -----
# longitudinal trajectories
wh = which(pain$id %in% 1:24)
ggplot(data=pain[wh,], aes(y = meas, x = time, group = id)) +
  geom_line() +
  xlab("Time Occasion") +
  ylab("Response")


# pain data
head(pain)


# estimate a linear mixed model and plot residuals
outLME = lmer(meas ~ time + trt + trt:time + (1 | id), data = pain)
par(mfrow = c(1,2))
qqnorm(residuals(outLME))
qqline(residuals(outLME))
qqnorm(unlist(ranef(outLME)$id))
qqline(unlist(ranef(outLME)$id))


# lqmix function
args(lqmix)

# TC linear quantile mixture model
outTC = lqmix(formula = meas ~ time + trt + trt:time, randomTC = ~1,
                time = "time", group = "id",G = 2, data = pain)
outTC


# TV linear quantile mixture model
outTV = lqmix(formula = meas ~ time + trt + trt:time,
                randomTV = ~1, time = "time",group = "id", m = 2, data = pain)
outTV


# TCTV linear quantile mixture model
outTCTV = lqmix(formula = meas ~ trt + time + trt:time, randomTC = ~ time,
  randomTV = ~1, time = "time", group = "id", m = 2, G = 2,
  data = pain, se = TRUE, start = 1, seed = 11, parallel = TRUE)
outTCTV

# summary method # --------
summary(outTCTV)


# search_lqmix function
args(search_lqmix)
sTCTV = search_lqmix(formula = meas ~ trt + time + trt:time, randomTC = ~time,
                     randomTV = ~1, nran = 2,
                     group = "id", time = "time", mv = 1:2, Gv = 1:2, data = pain,
                     seed = 11, se = TRUE)
sTCTV


# lqr function
args(lqr)

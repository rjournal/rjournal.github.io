################
# Installation #
################
install.packages("metapack") # default choice
library(metapack)

#####################
# Replication Code  #
# for Data Analysis #
#####################
###############################################
# Real data analysis with metapack            #
# > Primary hypercholesterolemia data         #
###############################################
data("cholesterol")
set.seed(2797542)
data("cholesterol")
f_1 <- 'pldlc + phdlc + ptg | sdldl + sdhdl + sdtg ~ 0 + bldlc + bhdlc + btg +
  age + durat + white + male + dm + ns(n) | treat | treat + trial + onstat'
out_ma <- bmeta_analyze(formula(f_1), data = cholesterol,
  prior = list(model="NoRecovery"),
  mcmc = list(ndiscard = 1000, nskip = 1, nkeep = 1000),
  control=list(scale_x = TRUE, verbose=TRUE))

# >>> Posterior inference
summary(out_ma, HPD = TRUE, level = 0.95)
print(out_ma, HPD = TRUE, level = 0.95)
dic <- model.comp(out_ma, "dic")
dic
lpml <- model.comp(out_ma, "lpml")
lpml
plot(out_ma) # generate trace plots

###############################################
# Real data analysis with metapack            #
# > Triglycerides Network Meta data           #
###############################################
data("TNM")
TNM$group <- factor(match(TNM$treat, c("PBO", "R"), nomatch = 0))
f_2 <- 'ptg | sdtg ~
  0 + bldlc + bhdlc + btg + age + white + male + bmi +
  potencymed + potencyhigh + durat + ns(n) | 
  scale(bldlc) + scale(btg) + group | treat  + trial'
set.seed(2797542)
out_nma <- bmeta_analyze(formula(f_2), data = TNM,
  mcmc = list(ndiscard = 1000, nskip = 1, nkeep = 1000),
  control=list(scale_x = TRUE, verbose=TRUE))

# >>> Posterior inference
summary(out_nma, HPD = TRUE, level = 0.95)
print(out_nma, HPD = TRUE, level = 0.95)
dic <- model.comp(out_nma, "dic", verbose=TRUE)
dic
lpml <- model.comp(out_nma, "lpml", verbose=TRUE)
lpml
plot(out_nma) # generate trace plots
s <- sucra(out_nma)
s
plot(s) # generate SUCRA plot


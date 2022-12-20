data("cholesterol")
out <- bmeta_analyze(pldlc + phdlc + ptg | sdldl + sdhdl + sdtg ~
          0 + bldlc + bhdlc + btg + age + durat + white + male + dm + ns(Npt) | trt | trt + Trial + onstat, data = cholesterol, control=list(model="NoRecovery", scale_x = TRUE, verbose=TRUE))

data("TNM")
out <- bmeta_analyze(ptg | sdtg ~
        0 + bldlc + bhdlc + btg + age + white + male + bmi + potencymed + potencyhigh + durat + ns(Npt) | bldlc + btg | Treat  + Trial, data = TNM, control=list(scale_x = TRUE, verbose=TRUE))


set.seed(2797542)
data("cholesterol")
f_1 <- 'pldlc + phdlc + ptg | sdldl + sdhdl + sdtg ~ 0 + bldlc + bhdlc + btg +
  age + durat + white + male + dm + ns(n) | treat | treat + trial + onstat'
out_ma <- bmeta_analyze(formula(f_1), data = cholesterol,
  prior = list(model="NoRecovery"),
  mcmc = list(ndiscard = 1000, nskip = 1, nkeep = 1000),
  control=list(scale_x = TRUE, verbose=TRUE))


# f_2 <- 'ptg | sdtg ~
#   0 + bldlc + bhdlc + btg + age + white + male + bmi +
#   potencymed + potencyhigh + durat + ns(Npt) | Treat  + Trial'

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



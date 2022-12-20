install.packages("likelihoodR")
library(likelihoodR)

# one sample t test
# one sample Gosset's original additional hours of sleep data Cahusac (2020) p 29 
mysample <- c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0)
a1=L_ttest(mysample, d=0.5, alt.2=2, L.int=2)

# 2-way factorial ANOVA
time <- c(6.4, 4.6, 6.4, 5.6, 5.9, 6.1, 6.3, 4.5, 4.8, 6.6, 7, 9.3, 7.9, 9.4, 8.2,
4.4, 4.2, 5, 6.9,4.5, 4, 4.3, 6.9, 5.5, 5.8, 4.4, 4.2, 5.1, 6.9, 4.5)
Treatment = gl(3,5,30, labels=c("T1","T2","T3"))
Health = gl(2,15,30, labels=c("Hemophiliac","Normal"))
contrast1 <- c(-1, -1, 5, -1, -1, -1) # interaction Hemo T3 higher than others
contrast2 <- c(-1, -1, -1, 1, 1, 1)    # main effect of health status (Hemo higher than Normal)
m1=L_2way_Factorial_ANOVA(time, Treatment, Health, contrast1, contrast2, verb=TRUE)

# correlation
m200 <- c(22.6,23.7,23.1,23.6,23.6,23.6,25.5,23.9,24.5,23.9,24.9,24.8,24.7,
25.0,24.6,24.9,25.0,25.6,24.8,25.5,25.7,24.9,26.6,25.2,26.2)
m800 <- c(128.5,126.1,124.2,132.5,134.7,132.5,138.5,127.9,133.7,132.2,136.1,142.8,
125.8,131.5,137.1,134.9,146.7,133.9,146.4,144.0,133.4,138.0,139.2,137.3,163.4)
m2=L_corr(m200, m800, null=0, exp.r=0.5, L.int=3, alpha=.05, verb=TRUE)

# 1-way categorical (binomial)
obs <- c(18,5); exp.p <- c(0.7, 0.3)  # observed and expected values
m3 <- L_1way_cat(obs, exp.p, verb = TRUE)


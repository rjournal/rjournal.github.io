set.seed(42)

# JZS-BF for Bayesian ANOVA and traditional ANOVA for the ToothGrowth dataset
library(datasets)
data(ToothGrowth)
head(ToothGrowth,n=3)


ToothGrowth$dose = factor(ToothGrowth$dose) 
levels(ToothGrowth$dose) = c("Low", "Medium", "High")


library(BayesFactor)
bf = anovaBF(len ~ supp*dose, data=ToothGrowth)
bf


summary(aov(len ~ supp*dose, data=ToothGrowth))


# Tooth growth of guinea pigs treated with vitamin c
library(datasets)
data(ToothGrowth)
head(ToothGrowth,n=3)

library(dplyr)
library(tidyr)
library(bayesanova)
grp1 = (ToothGrowth %>% filter(dose==0.5) %>% select(len))$len 
grp2 = (ToothGrowth %>% filter(dose==1.0) %>% select(len))$len 
grp3 = (ToothGrowth %>% filter(dose==2.0) %>% select(len))$len

assumption.check(grp1, grp2, grp3, conf.level=0.95)

set.seed(42)
res = bayes.anova(first = grp1, second = grp2, third = grp3)

anovaplot(res)

# Heart rate data for runners
library(dplyr)
hr=read.csv("heartrate.csv",sep=",")
head(hr)

femaleRunners = (hr %>% filter(Gender=="Female") %>% filter(Group=="Runners")  %>% select(Heart.Rate))$Heart.Rate
maleRunners = (hr %>% filter(Gender=="Male") %>% filter(Group=="Runners") %>% select(Heart.Rate))$Heart.Rate
femaleControl = (hr %>% filter(Gender=="Female") %>% filter(Group=="Control") %>% select(Heart.Rate))$Heart.Rate
maleControl = (hr %>% filter(Gender=="Male") %>% filter(Group=="Control") %>% select(Heart.Rate))$Heart.Rate

assumption.check(femaleRunners, maleRunners, femaleControl, maleControl)

set.seed(42)
resRunners = bayes.anova(first = femaleRunners, second = maleRunners, third = femaleControl, fourth = maleControl)

anovaplot(resRunners)
anovaplot(resRunners, type="diff")

post.pred.check(anovafit = resRunners, ngroups = 4, out = hr$Heart.Rate , reps = 50, eta = c(1/4,1/4,1/4,1/4))	

# Pleasantness ratings after watching artistic or erotic pictures
feelings=read.csv("feelings.csv",sep=",")
head(feelings)

femaleArtistic = (feelings %>% filter(Gender=="Female") %>% filter(Condition=="Abstract Art"))$AveragePleasantness
maleArtistic = (feelings %>% filter(Gender=="Male") %>% filter(Condition=="Abstract Art"))$AveragePleasantness
femaleNude = (feelings %>% filter(Gender=="Female") %>% filter(Condition=="Nudes"))$AveragePleasantness
maleNude = (feelings %>% filter(Gender=="Male") %>% filter(Condition=="Nudes"))$AveragePleasantness

assumption.check(femaleArtistic, maleArtistic, femaleNude, maleNude)

set.seed(42)
resFeelings = bayes.anova(first = femaleArtistic, second = maleArtistic, third = femaleNude, fourth = maleNude)

anovaplot(resFeelings)

post.pred.check(anovafit = resFeelings, ngroups = 4, out = feelings$AveragePleasantness , reps = 100, eta = c(58/223,64/223,41/223,60/223))

# Traditional ANOVA solution
summary(aov(AveragePleasantness ~ Gender * Condition, data = feelings))
# JZS-BF ANOVA solution
set.seed(42)
bfFeelings = generalTestBF(AveragePleasantness ~ Gender * Condition, data = feelings)
bfFeelings

# Amyloid data
library(Stat2Data)
library(dplyr)
data(Amyloid)
head(Amyloid)

NCI = (Amyloid %>% filter(Group=="NCI"))$Abeta
MCI = (Amyloid %>% filter(Group=="MCI"))$Abeta
mAD = (Amyloid %>% filter(Group=="mAD"))$Abeta

assumption.check(NCI, MCI, mAD)



# Simulations
set.seed(42)
Nsim=500
mu1 = mu2 = mu3 = numeric(Nsim)
sigma1 = sigma2 = sigma3 = numeric(Nsim)
mu2minusMu1 = mu3minusMu1 = mu3minusMu2 = numeric(Nsim)
sigma2minusSigma1 = sigma3minusSigma1 = sigma3minusSigma2 = numeric(Nsim)
delta12 = delta13 = delta23 = numeric(Nsim)
for(i in 1:Nsim){
  x1=rnorm(50,1,1)
  x2=rnorm(50,2,1)
  x3=rnorm(50,3,1)
  res = bayes.anova(first = x1, second = x2, third = x3)
  mu1[i] = mean(res$mu1)
  mu2[i] = mean(res$mu2)
  mu3[i] = mean(res$mu3)
  sigma1[i] = mean(res$sigma1Sq)
  sigma2[i] = mean(res$sigma2Sq)
  sigma3[i] = mean(res$sigma3Sq)
  mu2minusMu1[i] = mean(res$diffOfMeansMu2MinusMu1)
  mu3minusMu1[i] = mean(res$diffOfMeansMu3MinusMu1)
  mu3minusMu2[i] = mean(res$diffOfMeansMu3MinusMu2)
  sigma2minusSigma1[i] = mean(res$diffOfVariancesS2MinusS1)
  sigma3minusSigma1[i] = mean(res$diffOfVariancesS3MinusS1)
  sigma3minusSigma2[i] = mean(res$diffOfVariancesS3MinusS2)
  delta12[i] = mean(res$effectSize12)
  delta13[i] = mean(res$effectSize13)
  delta23[i] = mean(res$effectSize23)
}
par(mfrow=c(5,3), oma=c(2,2,2,2), mar=c(2,2,2,2))
hist(mu1, col="cornflowerblue", freq = FALSE, main=expression(mu[1]))
hist(mu2, col="cornflowerblue", freq = FALSE, main=expression(mu[2]))
hist(mu3, col="cornflowerblue", freq = FALSE, main=expression(mu[3]))
hist(sigma1, col="cornflowerblue", freq = FALSE, main=expression(sigma[1]))
hist(sigma2, col="cornflowerblue", freq = FALSE, main=expression(sigma[2]))
hist(sigma3, col="cornflowerblue", freq = FALSE, main=expression(sigma[3]))
hist(mu2minusMu1, col="cornflowerblue", freq = FALSE, main=expression(mu[2]-mu[1]))
hist(mu3minusMu1, col="cornflowerblue", freq = FALSE, main=expression(mu[3]-mu[1]))
hist(mu3minusMu2, col="cornflowerblue", freq = FALSE, main=expression(mu[3]-mu[2]))
hist(sigma2minusSigma1, col="cornflowerblue", freq = FALSE, main=expression(sigma[2]-sigma[1]))
hist(sigma3minusSigma1, col="cornflowerblue", freq = FALSE, main=expression(sigma[3]-sigma[1]))
hist(sigma3minusSigma2, col="cornflowerblue", freq = FALSE, main=expression(sigma[3]-sigma[2]))
hist(delta12, col="cornflowerblue", freq = FALSE, main=expression(delta[12]))
hist(delta13, col="cornflowerblue", freq = FALSE, main=expression(delta[13]))
hist(delta23, col="cornflowerblue", freq = FALSE, main=expression(delta[23]))





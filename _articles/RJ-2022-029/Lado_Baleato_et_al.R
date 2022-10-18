library(refreg)

##CASE 1 
#----- Reference region for two glycemia tests conditioned by age

?aegis # glycemic test results dataset
summary(aegis)

#1) Define healthy and DM patients samples
dm_no = subset(aegis,aegis$dm == "no")
dm_yes = subset(aegis,aegis$dm == "yes")

#2) Fit the age effect on the ref region shape
# by means of a bivariate location-scale model
mu1 = fpg ~ s(age)
mu2 = hba1c ~ s(age)
var1 = ~ s(age)
var2 = ~ s(age)
rho = ~ s(age)
formula = list(mu1,mu2,var1,var2,rho)

fit = bivRegr(formula,data=dm_no)

#2.1) Represent age estimated effect with 95% CI
fit_boot = summary_boot(fit, B=250, parallel = TRUE )
plot(fit_boot,eq=1)
plot(fit_boot,eq=2)
plot(fit_boot,eq=3)
plot(fit_boot,eq=4)
plot(fit_boot,eq=5)

#3) Reference region, non-parametrically estimated
region = bivRegion(fit,tau=c(0.50,0.95),
                   H_choice = "Hcov")

summary(region)

#4) Reference region representation
par(mfrow=c(1,2))
plot(region,tau = c(0.50,0.95),col="grey",reg.lwd=3,pch="*",
     xlab = "FPG, mg/dL", ylab = "HbA1c, %",main="Healthy patients")
plot(region,tau = c(0.50,0.95),col="grey",reg.lwd=3,pch="*",ylim=c(-3,15),
     xlab = "FPG, mg/dL", ylab = "HbA1c, %",main="Patients with diabetes",
     newdata=dm_yes)


par(mfrow=c(1,2))
plot(region, xlab = "FPG, mg/dL", ylab = "HbA1c, %",cond=T,
     newdata = data.frame(age = c(20,30,40,50,60,70)),tau=0.95,reg.lwd=2,
     pch="*",col="grey")

plot(region,xlab = "FPG, mg/dL", ylab = "HbA1c, %",cond=T,
     newdata = data.frame(age = c(20,60)),tau=c(0.50,0.95),reg.lwd=2,
     pch="*",col="grey")

par(mfrow=c(1,3))
plot(dm_no[dm_no$age==20,"fpg"],dm_no[dm_no$age==20,"hba1c"],main="20 years",
     xlim=c(50,140),ylim=c(4.2,7.5),xlab = "FPG, mg/dL", ylab = "HbA1c, %",
     pch=20,cex=2)
plot(region,cond=T,newdata = data.frame(age = 20),add=T,legend=T,
     tau=c(0.50,0.95),reg.lty=c(1,2))

plot(dm_no[dm_no$age==40,"fpg"],dm_no[dm_no$age==40,"hba1c"],main="40 years",
     xlim=c(50,140),ylim=c(4.2,7.5),xlab = "FPG, mg/dL", ylab = "HbA1c, %",
     pch=20,cex=2)
plot(region,cond=T,newdata = data.frame(age = 40),add=T,legend=F,
     tau=c(0.50,0.95),reg.lty=c(1,2))

plot(dm_no[dm_no$age==60,"fpg"],dm_no[dm_no$age==60,"hba1c"],main="60 years",
     xlim=c(50,140),ylim=c(4.2,7.5),xlab = "FPG, mg/dL", ylab = "HbA1c, %",
     pch=20,cex=2)
plot(region,cond=T,newdata = data.frame(age = 60),add=T,legend=F,
     tau=c(0.50,0.95),reg.lty=c(1,2))

## CASE 1 extension trivariate response
#------ Conditional reference region beyond the bivariate case
#(proof of concept)

#1) Estimate age effect on the means vector, and variance covariance matrix
dm_no = subset(aegis,aegis$dm == "no")
mu1 = fpg ~ s(age)
mu2 = hba1c ~ s(age)
mu3 = fru ~ s(age)
var1 = ~ s(age)
var2 = ~ s(age)
var3 = ~ s(age)
rho12 = ~ s(age)
rho13 = ~ s(age)
rho23 = ~ s(age)
formula = list(mu1,mu2,mu3,var1,var2,var3,rho12,rho13,rho23)
fit = trivRegr(formula,data=dm_no)

#2) Trivariate region estimation and representation
region = trivRegion(fit,tau=0.95)
plot(region,planes = F,size=5, col="red", incol = "grey", xlab="FPG, mg/dl",
        ylab="HbA1c, %", zlab="Fru, mg/dL")
plot(region,planes = T,size=5,col="red",incol = "grey", xlab="FPG, mg/dl",
        ylab="HbA1c, %", zlab="Fru, mg/dL")
plot(region,cond=T,newdata=data.frame(age=c(20,70)), xlab="FPG, mg/dl",
        ylab="HbA1c, %", zlab="Fru, mg/dL", legend=T)

## CASE 2
#------ Joint forecasting of two air pollutants
?pollution #historical records
?pollution_episode # a specific pollution episode

#1) Fit the location-scale model
mu1 = Nox~s(Nox_0)+s(Nox_2)
mu2 = So2~s(So2_0)+s(So2_2)
var1 = ~s(Nox_0)+s(Nox_2)
var2 = ~s(So2_0)+s(So2_2)
rho = ~s(Nox_0)+s(So2_0)
f = list(mu1,mu2,var1,var2,rho)
fit = bivRegr(f,data=pollution)

#2) Probabilistic region estimation
region = bivRegion(fit,tau=c(0.950,0.975),shape=10)

#3) Joint forecasting
par(mfrow=c(3,3))
for(k in c(150, 160, 165, 175, 180, 185, 190, 200,210)){
  plot(pollution_episode[,3:2],type="l",lty=2,ylim=c(0,600),xlim=c(0,45),
       main=pollution_episode[k,1])
  points(pollution_episode[k,3:2],col="black",pch=19)
  plot(region, cond = T, newdata = pollution_episode[k,], add = T,
       tau=c(0.95, 0.975),legend=F)
}


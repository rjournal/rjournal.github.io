# This file provides R code for the paper "Comparing Multiple Survival Functions with Crossing Hazards in R"  
# by H.-w. Chang, P.-Y. Tsai, J.-T. Kao and G.-Y. Lan

##################### User guide and numerical examples ##################
##Program description
#(see p.5)
install.packages("survELtest")
library(survELtest)

##Application of supELtest to threearm data
#(see p.7)
library(survival)
dat = Surv(threearm[, 1], threearm[, 2])
logrank = survdiff(dat ~ threearm[, 3])
score_vec = 3 : 1
logrankteststat = matrix(score_vec, nrow = 1, ncol = 3) %*% (logrank$obs - logrank$exp) / sqrt(matrix(score_vec, nrow = 1, ncol = 3) %*% (logrank$var) %*% matrix(score_vec, nrow = 3, ncol = 1))
if(logrankteststat < 0){
	pval = 2 * pnorm(logrankteststat)
}else{
	pval = 2 * (1 - pnorm(logrankteststat))
}
round(pval, 2)
#(see p.8)
nocrossings(Surv(threearm$time, threearm$censor) ~ threearm$group, group_order = c(3, 2, 1), sided = 1)
#
supELtest(Surv(threearm$time, threearm$censor) ~ threearm$group, group_order = c(3, 2, 1), sided = 1)
#
ptwise = ptwiseELtest(Surv(threearm$time, threearm$censor) ~ threearm$group, group_order = c(3, 2, 1), sided = 1)
round(ptwise$result_dataframe$time_pts[ptwise$result_dataframe$decision == 1], 2)


##Application of intELtest to hepatitis data
#(see p.9)
library(survival)
dat = Surv(hepatitis[, 1], hepatitis[, 2])
logrank = survdiff(dat ~ hepatitis[, 3])
round(1 - pchisq(logrank$chisq, df = 1), 2)
#
intELtest(Surv(hepatitis$time, hepatitis$censor) ~ hepatitis$group)

##################### Figures #####################
library(survival)
library(survELtest)
setwd("E:\\R_program\\figures")
label_size = 1.8
tcl_size = 0.3
axis_size = 1.5
position_lab = 3.7
bot_left_mar = 5
lwd = 1.8 

## Figure 1
pdf("paper_survELtest_hep_surv_20191220.pdf", width = 5.2, height = 5) 
par(mfrow = c(1, 1), mar = c(bot_left_mar, bot_left_mar, 1, 0), oma = c(0, 0, 0, 1), las = 1)
require(graphics)
KM.est <- survfit(Surv(hepatitis[, 1], hepatitis[, 2]) ~ strata(hepatitis[, 3]), data = hepatitis, type = "kaplan-meier")
plot(KM.est, lty = 1 : 2, lwd = lwd, mark.time = FALSE, conf.int = FALSE, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlim = c(0, 180), ylim = c(0, 1), las = 1, cex = 0.7, tcl = tcl_size, frame.plot = FALSE)
axis(1, at = seq(0, 182, by = 30),las = 1, tcl = tcl_size, cex.axis = axis_size) 
axis(2, at = seq(0, 1, by = 0.2), las = 1, tcl = tcl_size, cex.axis = axis_size) 
title(ylab = "Survival probability", cex.lab = label_size, line = position_lab)
title(xlab = "Days", cex.lab = label_size)
dev.off()

## Figure 2 is a flow chart which is readily reproducible

## Figure 3
install.packages("muhaz")
library(muhaz)
KM.est <- survfit(Surv(threearm[, 1], threearm[, 2]) ~ strata(threearm[, 3]), data = threearm, type = "kaplan-meier")
sfit_1 <- survfit(Surv(threearm[threearm[, 3] == 1, 1], threearm[threearm[, 3] == 1, 2]) ~ 1)
sfit_2 <- survfit(Surv(threearm[threearm[, 3] == 2, 1], threearm[threearm[, 3] == 2, 2]) ~ 1)
sfit_3 <- survfit(Surv(threearm[threearm[, 3] == 3, 1], threearm[threearm[, 3] == 3, 2]) ~ 1)
mufit_1 <- muhaz(threearm[threearm[, 3] == 1, 1], threearm[threearm[, 3] == 1, 2])  
mufit_2 <- muhaz(threearm[threearm[, 3] == 2, 1], threearm[threearm[, 3] == 2, 2]) 
mufit_3 <- muhaz(threearm[threearm[, 3] == 3, 1], threearm[threearm[, 3] == 3, 2]) 
ylim_L = min(c(mufit_1$haz.est, mufit_2$haz.est, mufit_3$haz.est))
ylim_U = max(c(mufit_1$haz.est, mufit_2$haz.est, mufit_3$haz.est))
xlim_L = 0
xlim_U = max(c(max(sfit_1$time), max(sfit_2$time), max(sfit_3$time)))
pdf("paper_survELtest_3arm_surv_haz_20191220.pdf", height = 6, width = 13.2)
par(mfrow = c(1, 2), mar = c(bot_left_mar, bot_left_mar, 1, 1), oma = c(0, 0, 0, 0), las = 1) 
require(graphics)
plot(KM.est, lty = c(1, 2, 6), lwd = lwd, mark.time = FALSE, conf.int = FALSE, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlim = c(0, 80), ylim = c(0, 1), las = 1, cex = 0.7, tcl = tcl_size, frame.plot = FALSE)
axis(1, at = seq(0, 80, by = 10), las = 1, tcl = tcl_size, cex.axis = axis_size) 
axis(2, at = seq(0, 1, by = 0.2), las = 1, tcl = tcl_size, cex.axis = axis_size) 
title(ylab = "Survival probability", cex.lab = label_size, line = position_lab - 0.6)
title(xlab = "Time to remission (days)", cex.lab = label_size, line=position_lab - 0.3)
plot(mufit_1, lwd = lwd, xaxs = "i", yaxs = "i", xaxt = "n", yaxt="n", ylim = c(0, 0.06), xlim = c(xlim_L, 80), xlab = '', ylab = '', main = '', las = 1, cex = 0.7, tcl = tcl_size, frame.plot = FALSE)
lines(mufit_2, lwd = lwd, lty = 2)
lines(mufit_3, lwd = lwd, lty = 6)
axis(1, at = seq(xlim_L, 80, by = 10), las = 1, tcl = tcl_size, cex.axis = axis_size)
axis(2, at = seq(0, 0.06, by = 0.02), las = 1, tcl = tcl_size, cex.axis = axis_size) 
title(xlab = "Time to remission (days)", cex.lab = label_size, line = position_lab - 0.3)
title(ylab = "Hazard function", cex.lab = label_size, line = position_lab)
dev.off()



## Figure 4
# install.packages("muhaz")
library(muhaz)
sfit_1 <- survfit(Surv(hepatitis[hepatitis[, 3] == 1, 1], hepatitis[hepatitis[, 3] == 1, 2]) ~ 1)
sfit_2 <- survfit(Surv(hepatitis[hepatitis[, 3] == 2, 1], hepatitis[hepatitis[, 3] == 2, 2]) ~ 1)
mufit_1 <- muhaz(hepatitis[hepatitis[, 3] == 1, 1], hepatitis[hepatitis[, 3] == 1, 2], max.time = max(sfit_1$time))  
mufit_2 <- muhaz(hepatitis[hepatitis[, 3] == 2, 1], hepatitis[hepatitis[, 3] == 2, 2], max.time = max(sfit_2$time)) 
ylim_L = min(c(mufit_1$haz.est, mufit_2$haz.est))
ylim_U = max(c(mufit_1$haz.est, mufit_2$haz.est))
xlim_L = 0
xlim_U = max(c(max(sfit_1$time), max(sfit_2$time)))
pdf("paper_survELtest_hep_haz_20191220.pdf", width = 5.2, height = 5)
par(mfrow = c(1, 1), mar = c(bot_left_mar, bot_left_mar, 1, 0), oma = c(0, 0, 0, 1), las = 1)
require(graphics)
plot(mufit_1, lwd = lwd, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", ylim = c(0, 0.01), xlim = c(xlim_L, xlim_U), xlab='', ylab = '', main = '', las = 1, cex = 0.7, tcl = tcl_size, frame.plot = FALSE)
lines(mufit_2, lwd = lwd, lty = 2)
axis(1, at=seq(xlim_L, xlim_U, by = 30), las = 1, tcl = tcl_size, cex.axis = axis_size * 0.95) 
axis(2, at=seq(0, 0.01, by = 0.005), las = 1, tcl = tcl_size, cex.axis = axis_size * 0.95) 
title(xlab = "Days", cex.lab = label_size * 0.95)
title(ylab = "Hazard function", cex.lab = label_size * 0.95, line = position_lab)
dev.off()


## Figure 5
install.packages("eha")
library(eha)
pdf("paper_EL2Surv_2egs.pdf", width = 13.2, height = 6) 
par(mfrow = c(1, 2), mar = c(bot_left_mar, bot_left_mar, 1, 1), oma = c(0, 0, 0, 0), las = 1) 
require(graphics)
plotxsequb = 10
x = seq(0, plotxsequb, by = 0.001)
y1 = 1 - ppch(x, cuts = c(3, 6), levels = c(0.1, 0.3, 0.2))
y2 = 1 - ppch(x, cuts = c(3, 6), levels = c(0.3, 0.1, 0.2))
plot(x, y1, type = "l", lwd = lwd, cex.axis = axis_size, xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlim = c(0, plotxsequb), ylim = c(0, 1), las = 1 , cex = 0.7, tcl = tcl_size, frame.plot = FALSE)
points(x, y2, type = "l",lwd = lwd, lty = 2)
axis(1, at = seq(0, plotxsequb, by = 5),las = 1, tcl = tcl_size, cex.axis = axis_size) 
axis(2, at = seq(0, 1, by = 0.5), las = 1,tcl = tcl_size, cex.axis = axis_size) 
title(ylab = "Survival probability", cex.lab = label_size, line = position_lab)
title(xlab = "Time", cex.lab = label_size, line = position_lab)
a1b1_result = c(2, 6)          
a2b2_result = c(1.6, 5.2) 
y1 = 1 - pweibull(x, shape = a1b1_result[1], scale = a1b1_result[2])
y2 = 1 - pweibull(x, shape = a2b2_result[1], scale = a2b2_result[2])
plot(x, y1, type = "l", lwd = lwd, cex.axis = axis_size, xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt="n",xlim=c(0,plotxsequb),ylim=c(0,1),las=1,cex=0.7,tcl=tcl_size,frame.plot=FALSE)
points(x, y2, type = "l", lwd = lwd, lty = 2)
axis(1, at=seq(0, plotxsequb, by = plotxsequb / 2), las = 1, tcl = tcl_size, cex.axis = axis_size)
axis(2, at=seq(0, 1, by = 0.5), las = 1, tcl = tcl_size, cex.axis = axis_size)
title(xlab = "Time", cex.lab = label_size, line = position_lab)
dev.off()


##################### Table 1 #####################
## hazardcross
supELtest(Surv(hazardcross$time, hazardcross$censor) ~ hazardcross$group) 
# logrank
dat = Surv(hazardcross[, 1], hazardcross[, 2])
(logrank = survdiff(dat ~ hazardcross[, 3])) 
1 - pchisq(logrank$chisq, df = 1)
# PP
(PP = survdiff(dat ~ hazardcross[, 3], rho = 1))
PPteststat = abs(PP$obs[1] - PP$exp[1]) / sqrt(PP$var[1, 1])
(1 - pnorm(PPteststat)) * 2
# YP
install.packages("YPmodel")
library(YPmodel)
hazardcross0 = hazardcross
hazardcross0[hazardcross0[, 3] == 2, 3] = 0
colnames(hazardcross0) = c('V1', 'V2', 'V3')
Adlgrk <- YPmodel.adlgrk(data = hazardcross0)
Adlgrk$pval
# RMST
install.packages('survRM2')
library(survRM2)
hg = hazardcross$group
hg[hg == 2] = 0
b = rmst2(hazardcross$time, hazardcross$censor, hg)
print(b)

## hazardcross_Weibull
intELtest(Surv(hazardcross_Weibull$time, hazardcross_Weibull$censor) ~ hazardcross_Weibull$group) 
# logrank
dat = Surv(hazardcross_Weibull[, 1], hazardcross_Weibull[, 2])
(logrank = survdiff(dat ~ hazardcross_Weibull[, 3])) 
1 - pchisq(logrank$chisq, df = 1)
# PP
(PP = survdiff(dat ~ hazardcross_Weibull[, 3], rho = 1))
PPteststat = abs(PP$obs[1] - PP$exp[1]) / sqrt(PP$var[1, 1])
(1 - pnorm(PPteststat)) * 2
# YP
# install.packages("YPmodel")
library(YPmodel)
hazardcross_Weibull0 = hazardcross_Weibull
hazardcross_Weibull0[hazardcross_Weibull0[, 3] == 2, 3] = 0
colnames(hazardcross_Weibull0) = c('V1', 'V2', 'V3')
Adlgrk <- YPmodel.adlgrk(data = hazardcross_Weibull0)
Adlgrk$pval
# RMST
# install.packages('survRM2')
library(survRM2)
hg = hazardcross_Weibull$group
hg[hg == 2] = 0
b = rmst2(hazardcross_Weibull$time, hazardcross_Weibull$censor, hg)
print(b)

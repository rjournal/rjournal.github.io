# load libraries
library(slgf)
library(hiddenf)
data(locknut)
data(textile)
data(roadwear)
data(smell)
data(lymphoma)
data(bottles)

# Figure 1

# png("intro4.png", width=6, height=6, unit="in", res=300)
par(mfrow=c(2,2))
# smell
boxplot(olf~agecat, data=smell,cex.lab=1.25, cex.main=1.25, cex.axis=1.25,
        col=c("white", "white", "white", "red", "red"), pch=16, names=1:5, 
        ylab="Olfactory Function", xlab="Age Category", main="Smell Data\nOne-way Layout",
        border=c("#666666","#666666","#666666","red","red"), 
        ylim=c(0.4, 1.5))
segments(3.6, median(smell$olf[smell$agecat==4]), 
         4.4, median(smell$olf[smell$agecat==4]), 
         lwd=3, col="white")
segments(4.6, median(smell$olf[smell$agecat==5]), 
         5.4, median(smell$olf[smell$agecat==5]), 
         lwd=3, col="white")
text(2.2, 0.5, "P(model|data)=61%", cex=1)

# textile
plot(textile$film, textile$strength, xlab="Film Thickness (unit)", 
     ylab="Breaking Strength (unit)", 
     main="Textile Data\nANCOVA", 
     pch=16, col="white",cex.axis=1.25, cex.lab=1.25,
     cex.main=1.25, ylim=c(0, 1650))

points(textile$film[textile$starch=="canna"], 
       textile$strength[textile$starch=="canna"], pch=17, 
       col="#666666", cex=1.2)
points(textile$film[textile$starch=="potato"], 
       textile$strength[textile$starch=="potato"], pch=0, 
       col="#666666", cex=1.2)
points(textile$film[textile$starch=="corn"], 
       textile$strength[textile$starch=="corn"], pch=16, 
       col="red", cex=1.2)
legend(x="topleft", legend=c("Canna", "Potato", "Corn"),cex=1,
       pch=c(17,0,16), col=c("#666666", "#666666", "red"), 
       bty="n")
text(8.2, 100, "P(model|data)=59%", cex=1)
abline(232.89260-983.93005, 59.39661+129.51081, col="red", lwd=2)
abline(232.89260, 59.39661, col="#666666", lwd=2)

# locknut
plot(c(0.25,2.25), c(10,55), main="Locknut Data\nTwo-way Layout", pch=16, col="white", xaxt="n", ylab="Torque (foot-pounds)", xlab="Fixture",cex.lab=1.25, cex.main=1.25, cex.axis=1.25, 
     ylim=c(5, 52))
axis(1, at=c(1,2), labels=c("Bolt", "Mandrel"), cex.axis=1.25)
points(rep(1,10), locknut$Torque[locknut$Fixture=="bolt"&locknut$Plating=="CW"], pch=16, col="black")
points(rep(2,10), locknut$Torque[locknut$Fixture=="mandrel"&locknut$Plating=="CW"], pch=16, col="black")
points(rep(.95,10), locknut$Torque[locknut$Fixture=="bolt"&locknut$Plating=="HT"], pch=0, col="black")
points(rep(1.95,10), locknut$Torque[locknut$Fixture=="mandrel"&locknut$Plating=="HT"], pch=15, col="black")
points(rep(1.05,10), locknut$Torque[locknut$Fixture=="bolt"&locknut$Plating=="PO"], pch=2, col="black")
points(rep(2.05,10), locknut$Torque[locknut$Fixture=="mandrel"&locknut$Plating=="PO"], pch=17, col="black")
lines(c(1,2), c(17.4, 16.9), lwd=2, col="black")
lines(c(.95,1.95), c(34.7, 29.4), lwd=2, col="black")
lines(c(1.05,2.05), c(30.5, 14.1), lwd=2, col="black")
legend(x="topleft", legend=c("CW","HT","PO"), pch=c(16, 15, 17), 
       cex=1, col=c("black", "black", "black"), bty="n")
text(0.9, 9, "P(model|data)=85%", cex=1)

# bottles
plot(c(.5,5),c(min(bottles[,1]), max(bottles[,1])), pch=16, col="white", 
     xlab="Time", ylab="Weight (ounces)", main="Bottles Data\nTwo-way Unrep. Layout",
     cex.lab=1.25, cex.main=1.25, cex.axis=1.25, 
     ylim=c(20,90))
lines(1:5, lty=1, bottles[bottles[,3]==1,1], lwd=2, col="#666666")
text(.9, bottles[bottles[,3]==1 & bottles[,2]==1,1], "1")
lines(1:5, lty=1, bottles[bottles[,3]==2,1], lwd=2, col="#666666")
text(.9, bottles[bottles[,3]==2 & bottles[,2]==1,1], "2")
lines(1:5, lty=1, bottles[bottles[,3]==3,1], lwd=2, col="#666666")
text(.9, bottles[bottles[,3]==3 & bottles[,2]==1,1]+1, "3")
lines(1:5, lty=1, bottles[bottles[,3]==4,1], lwd=2, col="#666666")
text(.9, bottles[bottles[,3]==4 & bottles[,2]==1,1], "4")
lines(1:5, lty=1, bottles[bottles[,3]==5,1], lwd=2, col="red")
text(.9, bottles[bottles[,3]==5 & bottles[,2]==1,1], "5")
lines(1:5, lty=1, bottles[bottles[,3]==6,1], lwd=2, col="#666666")
text(.9, bottles[bottles[,3]==6 & bottles[,2]==1,1]+2, "6")
text(1, 83, "Nozzle", cex=1)
text(2, 25, "P(model|data)>99%", cex=1)

# Smell Analysis

smell$agecat <- as.factor(smell$agecat)
smell_null <- lm(olf~1, data=smell)
smell_full <- lm(olf~agecat, data=smell)
print(smell_null)
print(smell_full)
anova(smell_null, smell_full)

summary(smell_null)$sigma^2
summary(smell_full)$sigma^2

smell_out <- ms_slgf(dataf=smell, response="olf", lgf_beta="agecat", min_levels_beta=1, 
                     lgf_Sigma="agecat", min_levels_Sigma=1, same_scheme=TRUE, 
                     usermodels=list("olf~1", "olf~agecat", "olf~group"), 
                     het=c(1,1,1), prior="flat", m0=9)
smell_out$models[1:2,c(1,2,3,5,7)]

smell_out$coefficients[[62]]
smell_out$variances[[62]]
smell_out$scheme_probabilities_Sigma
smell_out$scheme_probabilities_beta
smell_out$class_probabilities

# textile Analysis

par(mfrow=c(1,3))
plot(textile$film, textile$strength, xlab="Film Thickness (unit)", 
     ylab="Breaking Strength (unit)", 
     main="Observed Data", 
     pch=16, col="white",cex.axis=1.25, cex.lab=1.25,
     cex.main=1.25)
points(textile$film[textile$starch=="canna"], 
       textile$strength[textile$starch=="canna"], pch=17, 
       col="black", cex=1.2)
points(textile$film[textile$starch=="potato"], 
       textile$strength[textile$starch=="potato"], pch=15, 
       col="black", cex=1.2)
points(textile$film[textile$starch=="corn"], 
       textile$strength[textile$starch=="corn"], pch=16, 
       col="black", cex=1.2)
legend(x="topleft", legend=c("Canna", "Potato", "Corn"),cex=1.25,
       pch=c(17,15,16), col=c("black", "black", "black"))

m_ancova <- lm(strength~film+starch, data=textile)
plot(textile$film, textile$strength, xlab="Film Thickness (unit)", 
     ylab="Breaking Strength (unit)", 
     main="ANCOVA Model Fit", 
     pch=16, col="white",cex.axis=1.25, cex.lab=1.25,
     cex.main=1.25)
points(textile$film[textile$starch=="canna"], 
       textile$strength[textile$starch=="canna"], pch=17, 
       col="black", cex=1.2)
points(textile$film[textile$starch=="potato"], 
       textile$strength[textile$starch=="potato"], pch=15, 
       col="black", cex=1.2)
points(textile$film[textile$starch=="corn"], 
       textile$strength[textile$starch=="corn"], pch=16, 
       col="black", cex=1.2)
abline(m_ancova$coefficients[1], m_ancova$coefficients[2])
abline(m_ancova$coefficients[1]+m_ancova$coefficients[3], m_ancova$coefficients[2])
abline(m_ancova$coefficients[1]+m_ancova$coefficients[4], m_ancova$coefficients[2])

flurrymod <- lm(textile$strength~textile$film*as.factor(textile$starch=="corn"))
plot(textile$film, textile$strength, xlab="Film Thickness (unit)", 
     ylab="Breaking Strength (unit)", 
     main="Group-Based Interaction\nand Variances Fit", 
     pch=16, col="white",cex.axis=1.25, cex.lab=1.25,
     cex.main=1.25)
points(textile$film[textile$starch=="canna"], 
       textile$strength[textile$starch=="canna"], pch=17, 
       col="#666666", cex=1.2)
points(textile$film[textile$starch=="potato"], 
       textile$strength[textile$starch=="potato"], pch=0, 
       col="#666666", cex=1.2)
points(textile$film[textile$starch=="corn"], 
       textile$strength[textile$starch=="corn"], pch=16, 
       col="red", cex=1.2)
legend(x="topleft", legend=c("Canna", "Potato", "Corn"),cex=1.25,
       pch=c(17,0,16), col=c("#666666", "#666666", "red"))
abline(flurrymod$coefficients[1],flurrymod$coefficients[2],
       col="#666666",lwd=2)
abline(flurrymod$coefficients[1]+
         flurrymod$coefficients[3],
       flurrymod$coefficients[2]+
         flurrymod$coefficients[4],
       col="red",lwd=2)

# textile analysis
out_textile <- ms_slgf(dataf = textile, response = "strength", 
                     lgf_beta = "starch", lgf_Sigma = "starch",
                     same_scheme=FALSE, min_levels_beta=1, min_levels_Sigma=1, 
                     usermodels = list("strength~film+starch", "strength~film*starch",
                                       "strength~film+group", "strength~film*group"), 
                     het=c(1,1,1,1), prior="flat", m0=9)
out_textile$models[1:2,c(1,2,3,5,7)]
out_textile$scheme_probabilities_Sigma

textile_ancova <- lm(strength~film+starch, data=textile)
summary(textile_ancova)
anova(textile_ancova, out_textile$model_fits[[8]])

# Locknut Analysis

locknut$Interaction <- paste0(locknut$Fixture, "*", locknut$Plating)
out_locknut <- ms_slgf(dataf=locknut, response="Torque", 
                       lgf_beta="Plating", min_levels_beta=1,
                       lgf_Sigma="Interaction", min_levels_Sigma=1, 
                       same_scheme=FALSE, 
                       usermodels=list("Torque~Fixture+Plating+Fixture*Plating", 
                                       "Torque~Fixture+group+Fixture*group"), 
                       het=c(1,1), prior="zs", m0=2)
out_locknut$models
out_locknut$coefficients[[13]]

locknut_mpm <- lm(as.formula(out_locknut$models[1,]$Model), data=locknut)
locknut_mpm

lm(Torque~Fixture+Plating+Fixture*Plating, data=locknut)

# bottles analysis
data(bottles)
names(bottles)

mbottles <- lm(weight~time+heads, data=bottles)
mbottles$fitted.values

bottles_me <- lm(weight~time+heads, data=bottles)

bottles2 <- data.frame(weight=bottles$weight, time=as.factor(bottles$time),
                       heads=as.factor(bottles$heads))
bottles_out <- ms_slgf(dataf=bottles2, response="weight", 
                       lgf_beta="heads",
                         min_levels_beta=1, lgf_Sigma=NA, min_levels_Sigma=NA, same_scheme=FALSE,
                         usermodels=list("weight~time+group:time", "weight~time+heads"),
                         het=c(0,0), prior="zs", m0=2)
bottles_out$models[c(1,3),c(1,2,4,5)]

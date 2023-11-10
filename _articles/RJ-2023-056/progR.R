library(glmtoolbox)
library(robustbase)
library(nlme)

############################ Growth patterns of trees under two types of atmosphere 
data(spruces)
dev.new()
adjbox(size ~ days, data=subset(spruces,treat=="normal"), at=c(1:5,8:15) - 0.2, ylab="Tree size", xlim=c(0.8,15.2),
        col="white", boxwex=0.3, xaxt="n", xlab="Number of days after the experiment began", pch=20, cex=0.8)
adjbox(size ~ days, data=subset(spruces,treat=="ozone-enriched"), add=TRUE, 
        at=c(1:5,8:15) + 0.2, col="gray", boxwex=0.3, xaxt="n", pch=20, cex=0.8)
axis(1, at=c(1:5,8:15), labels=unique(spruces$days))
axis(2, at=seq(0,2000,250), labels=seq(0,2000,250))
legend("topleft", legend=c("normal","ozone-enriched"), fill=c("white","gray"),
       title="Atmosphere", bty="n")

m1 <- glmgee(size ~ poly(days,4) + treat, id=tree, family=Gamma(log), data=spruces)
m2 <- update(m1, corstr="Exchangeable")
m3 <- update(m1, corstr="AR-M-dependent(1)")
m4 <- update(m1, corstr="AR-M-dependent(2)")
m5 <- update(m1, corstr="AR-M-dependent(3)")

a <- CIC(m1, m2, m3, m4, m5, verbose=FALSE)
b <- QIC(m1, m2, m3, m4, m5, verbose=FALSE)
c <- GHYC(m1, m2, m3, m4, m5, verbose=FALSE)
d <- RJC(m1, m2, m3, m4, m5, verbose=FALSE)
e <- AGPC(m1, m2, m3, m4, m5, verbose=FALSE)
f <- SGPC(m1, m2, m3, m4, m5, verbose=FALSE)
cbind(a,QIC=b[,"QIC"],GHYC=c[,"GHYC"],RJC=d[,"RJC"],AGPC=e[,"AGPC"],SGPC=f[,"SGPC"])

summary(m3)

m3a <- update(m3, . ~ . + poly(days,4):treat)
anova(m3a, test="wald")
anova(m3a, test="score")
cbind(model=diag(vcov(m3, type="model")),robust=diag(vcov(m3, type="robust")),
      bias.corrected=diag(vcov(m3, type="bias-corrected")),jackknife=diag(vcov(m3, type="jackknife")))

stepCriterion(m3, direction="forward", criterion="p-value", test="score",
             scope=list(lower=~1, upper=~poly(days,4)*treat), levels=c(0.10,0.05))

dev.new()
residuals(m3, type="deviance", plot.it=TRUE, pch=16)

dev.new()
r1 <- residuals(m3, type="mahalanobis")
plot(1:length(r1),r1,type="h",xlab="Cluster index(i)",ylab=expression(r[ij]^M))
text(1:length(r1),r1,ifelse(m3$ids %in% c("O1T09","O1T17","O2T14"),m3$ids,""),pos=4)

dev.new()
d1 <- dfbeta(m3, method="full")
plot(1:length(d1[,6]),d1[,6],type="h",xlab="Cluster index(i)",ylab=expression(hat(beta)-hat(beta)[("- i")]))
text(1:length(d1[,6]),d1[,6],ifelse(rownames(d1) %in% c("N1T10","N1T02","N1T07","N2T07"),rownames(d1),""),pos=4)

dev.new()
l1 <- localInfluence(m3, type="total", perturbation="cw-clusters", coefs="treat")
plot(1:length(l1),l1,type="h",xlab="Cluster index(i)",ylab=expression(C[i]))
text(1:length(l1),l1,ifelse(rownames(l1) %in% c("N1T02","N1T07","N2T07","N1T10"),rownames(l1),""),pos=4)

############################ Treatment of severe postnatal depression
data(depression)
dev.new()
propor <- aggregate(depressd ~ visit + group, data=subset(depression,visit!=-1), mean)
propor <- within(propor,depressd <- log(depressd/(1-depressd)))
with(propor,plot(visit,depressd,pch=ifelse(group=="placebo",1,16),
            xlab="Number of months since the experiment began",
			ylab="logit(proportion)"))
legend("bottomleft", legend=c("Placebo patch","Oestrogen patch"), pch=c(1,16),
       title="Treatment", bty="n")

m1 <- glmgee(depressd ~ visit + group, id=subj, family=binomial(logit), data=depression)
m2 <- update(m1, corstr="Exchangeable")
m3 <- update(m1, corstr="AR-M-dependent(1)")
m4 <- update(m1, corstr="AR-M-dependent(2)")
m5 <- update(m1, corstr="AR-M-dependent(3)")

a <- CIC(m1, m2, m3, m4, m5, verbose=FALSE)
b <- QIC(m1, m2, m3, m4, m5, verbose=FALSE)
c <- AGPC(m1, m2, m3, m4, m5, verbose=FALSE)
d <- SGPC(m1, m2, m3, m4, m5, verbose=FALSE)
cbind(a,QIC=b[,"QIC"],AGPC=c[,"AGPC"],SGPC=d[,"SGPC"])

summary(m3)

m3a <- update(m3, . ~ . + visit:group)
anova(m3a, test="wald")
cbind(model=diag(vcov(m3, type="model")),robust=diag(vcov(m3, type="robust")), 
     bias.corrected=diag(vcov(m3, type="bias-corrected")),jackknife=diag(vcov(m3, type="jackknife")))
stepCriterion(m3a, direction="forward", criterion="qic")

dev.new()
r2 <- residuals(m3, type="mahalanobis")
plot(1:length(r2),r2,type="h",xlab="Cluster Index (i)",ylab=expression(r^M[ij]))
text(1:length(r2),r2,ifelse(m3$ids %in% c(10,14,20),m3$ids,""),pos=4)

dev.new()
d2 <- dfbeta(m3, method="full")[,3]
plot(1:length(d2),d2,type="h",xlab="Cluster Index (i)",ylab=expression(hat(beta)-hat(beta)[("- i")]))
text(1:length(d2),d2,ifelse(m3$ids %in% c(5,18,26,31,39,51),m3$ids,""),pos=4)

dev.new()
l2 <- localInfluence(m3, type="total", perturbation="cw-clusters", coefs="group")
plot(1:length(l2),l2,type="h",xlab="Cluster Index (i)",ylab=expression(C[i]))
text(1:length(l2),l2,ifelse(m3$ids %in% c(5,18,20,26),m3$ids,""),pos=4)

dev.new()
l2a <- localInfluence(m3, type="total", perturbation="cw-observations", coefs="group")
plot(1:length(l2a),l2a,type="h",xlab="Observation Index (i)",ylab=expression(C[ij]))
text(1:length(l2a),l2a,ifelse(rownames(l2a) %in% c("5(3)","10(2)","20(2)","14(2)","18(3)","20(4)","26(2)"),rownames(l2a),""),pos=4)

############################ Growth patterns of two soybean genotypes
data(Soybean)
dev.new()
boxplot(log(weight) ~ Time, data=subset(Soybean,Year=="1989" & Variety=="F"), at=c(1:8) - 0.2, ylab="log(average leaf weight per plant)",
       xlim=c(0.8,8.2), col="white", boxwex=0.25, xaxt="n", xlab="Days after planting", pch=20, cex=0.8, ylim=c(-4,3.5))
boxplot(log(weight) ~ Time, data=subset(Soybean,Year=="1989" & Variety=="P"), add=TRUE, 
        at=c(1:8) + 0.2, col="gray", boxwex=0.25, xaxt="n", pch=20, cex=0.8)
axis(1, at=c(1:8), labels=unique(subset(Soybean,Year=="1989")$Time))
axis(2, at=seq(0,25,5), labels=seq(0,25,5))
legend("topleft", legend=c("Commercial","Experimental"), fill=c("white","gray"),
       title="Variety", bty="n")

Soybean2 <- subset(Soybean,Year=="1989")
Soybean2 <- within(Soybean2,x <- ifelse(Variety=="P",1,0))

m0 <- gnmgee(weight ~ SSlogis(Time,b1,b2,b3), id=Plot, family=Gamma(identity), data=Soybean2)
start <- c(coef(m0),rep(0,3))
names(start) <- paste0("b",1:6)

m1 <- gnmgee(weight ~ (b1 + b4*x)/(1 + exp(-(Time - b2 - b5*x)/(b3 + b6*x))), start=start, id=Plot, family=Gamma(identity), data=Soybean2)
m2 <- update(m1, corstr="Exchangeable")
m3 <- update(m1, corstr="AR-M-dependent(1)")
m4 <- update(m1, corstr="AR-M-dependent(2)")
m5 <- update(m1, corstr="AR-M-dependent(3)")
m6 <- update(m1, corstr="AR-M-dependent(4)")

a <- CIC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
b <- QIC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
c <- GHYC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
d <- PAC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
e <- AGPC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
f <- SGPC(m1, m2, m3, m4, m5, m6, verbose=FALSE)
cbind(a,QIC=b[,"QIC"],GHYC=c[,"GHYC"],PAC=d[,"PAC"],AGPC=e[,"AGPC"],SGPC=f[,"SGPC"])

summary(m5)

############################ Amenorrhea rates over time
data(amenorrhea)
dev.new()
propor <- aggregate(amenorrhea ~ Time + Dose, data=amenorrhea, mean)
propor <- within(propor,amenorrhea <- log(amenorrhea/(1-amenorrhea)))
with(propor,plot(Time,amenorrhea,pch=ifelse(Dose=="100mg",1,16),
            xlab="Number of 90-day intervals since the trial began",
			ylab="logit(proportion)"))
legend("topleft", legend=c("100 mg","150 mg"), pch=c(1,16),
       title="Dose", bty="n")	   

amenorrhea2 <- within(amenorrhea,{Ctime <- factor(Time)
                                  Ctime <- relevel(Ctime,ref="1")
                                  ylag1 <- c(0,amenorrhea[-length(ID)])
                                  ylag1 <- ifelse(Time==0,0,ylag1)})

fit1 <- wglmgee(amenorrhea ~ poly(Time,2) + Dose | Ctime + Dose + ylag1, 
                family=binomial, data=amenorrhea2, id=ID, corstr="AR-M-dependent(1)",
                scale.fix=TRUE, scale.value=1, level="observations")
summary(fit1)


################################################################################
#Replication File
################################################################################
library(ggplot2)
library(rankFD)
library(MANOVA.RM)
library(nparcomp)




#Two independent samples-------------------------------------------------------#
data(reaction)
reaction<-subset(reaction,Group<2)
reaction$Group=factor(reaction$Group)


ggplot(reaction, aes(x=Group, y=Time)) + 
  geom_boxplot(cex=1.3)+
  geom_point(cex=3)+
  xlab("Dose")+
  ylab("Time [sec]")+
 theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 22))+             
  theme(aspect.ratio=1)
  

A<-rank.two.samples(Time~Group,data=reaction,method="logit",wilcoxon="asymptotic",shift.int=FALSE)
A
plot(A,cex.axis=1.8,cex.lab=1.8,cex=2)
#******************************************************************************#
#One Way Design
#******************************************************************************#


data(EEGwide)

ggplot(EEGwide, aes(x=diagnosis, y=complexity_central)) + 
  geom_boxplot(cex=1.3)+
  geom_point(cex=3)+
  xlab("Diagnosis")+
  ylab("Complexity")+
 theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 22))              # Axis titles
  
 B=rankFD(complexity_central~diagnosis,data=EEGwide,CI.method="logit",
 effect="unweighted",hypothesis="H0p",contrast=list("diagnosis","Tukey"))
B
plot(B,cex.axis=1.8,cex.lab=1.8,xlab="Diagnosis",cex=2,lwd=6,cex.ci=6)





#******************************************************************************#
#Factorial Design
#******************************************************************************#

data(nms)
freq<-aggregate(score~conc*subst,data=nms,length)

nms$conc <- factor(nms$conc, levels = c("1", "2", "5"),
                  labels = c("Con1", "Con2", "Con5"))
                  
nms$subst <- factor(nms$subst, levels = c("1", "2"),
                  labels = c("subst 1", "subst 2"))
boxplot(score~subst*conc,data=nms )
table(nms)
nms1=subset(nms,subst=="subst 1")
nms2=subset(nms,subst=="subst 2")
  

ggplot(nms1, aes(score, group = conc)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "", x="subst 1",fill="Score") +
    facet_grid(~conc) +
    scale_y_continuous(labels = scales::percent,limits=c(0,1))+
    theme(axis.title = element_text(size = 20))+              # Axis titles
    theme(legend.text = element_text(size=15, face="bold")) +
    theme(legend.title = element_text( size=22, face="bold")) +
    theme(legend.position="bottom")+
    theme(axis.text = element_text(size = 22))+
    theme(strip.background = element_rect(colour = "black", fill = "white",size=2))+ 
    theme(strip.text.x = element_text(face = "bold",size=18)) 
  
 
ggplot(nms2, aes(score, group = conc)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "", x="subst 2",fill="Score") +
    facet_grid(~conc) +
    scale_y_continuous(labels = scales::percent,limits=c(0,1))+
    theme(axis.title = element_text(size = 20))+              # Axis titles
    theme(legend.text = element_text(size=15, face="bold")) +
    theme(legend.title = element_text( size=22, face="bold")) +
    theme(legend.position="bottom")+
    theme(axis.text = element_text(size = 22))+
    theme(strip.background = element_rect(colour = "black", fill = "white",size=2))+ 
    theme(strip.text.x = element_text(face = "bold",size=18)) 

 
D=rankFD(score ~ conc * subst, data = nms, hypothesis = "H0F")
D
plot(D,xlab="Concentration",cex.lab=1.8,cex.axis=2)
conc

plot(D,xlab="subst",cex.lab=1.8,cex.axis=2)
subst

plot(D,xlab="",cex.lab=1.8,cex.axis=2)
conc:subst



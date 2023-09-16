
###################################################################### 
## nlmeVPC: Visual Model Diagnosis for a Nonlinear Mixed Effect Model
###################################################################### 

library(nlmeVPC)
library(ggplot2)
data(origdata)
data(simdata)

VPCgraph(origdata,simdata,N_xbin=8,type="scatter")+
  labs(title="(A) Visual Predictive Check : scatter",caption="")
VPCgraph(origdata,simdata,N_xbin=8,type="percentile")+
  labs(title="(B) Visual Predictive Check : percentile",caption="")
VPCgraph(origdata,simdata,N_xbin=8,type="CI")+
  labs(title="(C) Visual Predictive Check : CI",caption="")
  
# Figure 2

aqrVPC(origdata,simdata) +labs(caption="")

# Figure 3

bootVPC(origdata,simdata,N_xbin=8)
 
# Figure 4

asVPC(origdata,simdata,type="CI",N_xbin=8,N_hist=3,weight_method="bin") +labs(caption="")
asVPC(origdata,simdata,type="CI",N_xbin=8,N_hist=3,weight_method="distance")+labs(caption="")
  
# Numerical Predictive Check
 
NumericalCheck(origdata,simdata,N_xbin=8,pred.level=c(0,0.2,0.4,0.6,0.8,0.9))$NPC

# Figure 5
 
coverageplot(origdata,simdata,N_xbin=8) +ggtitle("(A) Coverage Plot")
coverageDetailplot(origdata,simdata,N_xbin=8,predL=0.8) +
   ggtitle("(B) Coverage detailed plot: PI = 80")
   
# Figure 6
 
quantVPC(origdata,simdata)
 
# simulation
 
library(nlme)
library(nlraa)
library(nlmeVPC)
data(origdata)
origdataT <- groupedData(DV~TIME|ID,origdata)

# Model 1 (True) 
T.nlme <- nlme(DV ~ exp(lKe+lKa-lCl)*AMT*
               (exp(-exp(lKe)*TIME) - exp(-exp(lKa)*TIME))/
               (exp(lKa)-exp(lKe)), data=origdataT,
             fixed=lKa+lKe+lCl~1,
             random=lKa+lCl~1,
             start=c(lKe=-2,lKa=1.5,lCl=-3))
set.seed(123456)
sim.T <- simulate_nlme(T.nlme,nsim=100,psim=3,level=1,value="data.frame")
simdata.T <- matrix(sim.T$sim.y,ncol=100)

# Model 2 (Wrong)
F.nlme <- nlme(DV ~ exp(lKe+lKa-lCl)*AMT*
                (exp(-exp(lKe)*TIME) - exp(-exp(lKa)*TIME))/
                (exp(lKa)-exp(lKe)), data=origdataT,
              fixed=lKa+lKe+lCl~1,
              random=lKe~1,
              start=c(lKe=-2,lKa=1.5,lCl=-3))

sim.F <- simulate_nlme(F.nlme,nsim=100,psim=3,level=1,value="data.frame")
simdata.F <- matrix(sim.F$sim.y,ncol=100)
  
# Figure 7
 
VPCgraph(origdata,simdata.T,type="CI",N_xbin=8)+labs(title="Model 1",caption="")
VPCgraph(origdata,simdata.F,type="CI",N_xbin=8)+labs(title="Model 2",caption="")
  
# Figure 8
 
aqrVPC(origdata,simdata.T)+labs(title="Model 1",caption="")
aqrVPC(origdata,simdata.F)+labs(title="Model 2",caption="")
  
# Figure 9
 
asVPC(origdata,simdata.T,type="CI",weight_method="distance",N_xbin=8)+labs(title="Model 1",caption="")
asVPC(origdata,simdata.F,type="CI",weight_method="distance",N_xbin=8)+labs(title="Model 2",caption="")

# Figure 10
 
bootVPC(origdata,simdata.T,N_xbin=8)+labs(title="Model 1",caption="")
bootVPC(origdata,simdata.F,N_xbin=8)+labs(title="Model 2",caption="")

  
# Figure 11
 
coverageplot(origdata,simdata.T,conf.level=0.9,N_xbin=8)+labs(title="Model 1")
coverageplot(origdata,simdata.F,conf.level=0.9,N_xbin=8)+labs(title="Model 2")
  
# Figure 12
 
coverageDetailplot(origdata,simdata.T,predL=0.5,N_xbin=8)+labs(title="Model 1")
coverageDetailplot(origdata,simdata.F,predL=0.5,N_xbin=8)+labs(title="Model 2")
  
# Figure 13
 
coverageDetailplot(origdata,simdata.T,predL=0.8,N_xbin=8)+labs(title="Model 1")
coverageDetailplot(origdata,simdata.F,predL=0.8,N_xbin=8)+labs(title="Model 2")
  
# Figure 14
 
quantVPC(origdata,simdata.T,N_xbin=8)+labs(title="Model 1")
quantVPC(origdata,simdata.F,N_xbin=8)+labs(title="Model 2")

  
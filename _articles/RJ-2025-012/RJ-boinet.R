
## Figure 1(a) ##

toxprob <- c(0.05,0.15,0.25,0.35,0.45,0.55)
effprob <- c(0.05,0.30,0.55,0.57,0.59,0.61)

plot(toxprob,pch=16,type="o",ylim=c(0,1),main="scenario1",cex=0.8,frame=FALSE)
points(1:6,effprob,pch=17,type="o",lty=3,cex=0.8)
legend(1,0.95,legend=c("Efficacy","Toxicity"),pch=c(17,16),lty=c(3,1),cex=0.8)

## Figure 1(b) ##

toxprob <- c(0.10,0.18,0.31,0.33,0.38,0.43)
effprob <- c(0.18,0.26,0.59,0.60,0.61,0.66)

plot(toxprob,pch=16,type="o",ylim=c(0,1),main="scenario1",cex=0.8,frame=FALSE)
points(1:6,effprob,pch=17,type="o",lty=3,cex=0.8)
legend(1,0.95,legend=c("Efficacy","Toxicity"),pch=c(17,16),lty=c(3,1),cex=0.8)

#####

library(boinet)

n.dose      <- 6   # Six dose levels
start.dose  <- 1   # Starting with the lowest dose level
size.cohort <- 3   # Three patients assigned to each cohort
n.cohort    <- 12  # Twelve cohorts at the maximum

# Target (quasi-Bernoulli) toxicity and efficacy probability
phi   <- 0.33
delta <- 0.60

tau.T   <- 30  # Thirty days of toxicity assessment windows
tau.E   <- 45  # Forty-five days of efficacy assessment windows
accrual <- 10  # Ten days of accrual rate

## BOIN-ET ##

# True toxicity and efficacy probabilities for the six dose levels
toxprob <- c(0.05,0.15,0.25,0.35,0.45,0.55)
effprob <- c(0.05,0.30,0.55,0.57,0.59,0.61)

# Method to estimate the efficacy probability and to select the optimal biological dose
estpt.method <- "obs.prob"
obd.method   <- "max.effprob"

boinet(
  n.dose=n.dose, start.dose=start.dose,
  size.cohort=size.cohort, n.cohort=n.cohort,
  toxprob=toxprob, effprob=effprob,
  phi=phi, delta=delta,
  tau.T=tau.T, tau.E=tau.E, accrual=accrual,
  estpt.method=estpt.method, obd.method=obd.method)

## TITE-BOIN-ET ##

# True toxicity and efficacy probabilities for the six dose levels
toxprob <- c(0.05,0.15,0.25,0.35,0.45,0.55)
effprob <- c(0.05,0.30,0.55,0.57,0.59,0.61)

# Method to estimate the efficacy probability and to select the optimal biological dose
estpt.method <- "obs.prob"
obd.method   <- "max.effprob"

tite.boinet(
  n.dose=n.dose, start.dose=start.dose,
  size.cohort=size.cohort, n.cohort=n.cohort,
  toxprob=toxprob, effprob=effprob,
  phi=phi, delta=delta,
  tau.T=tau.T, tau.E=tau.E, accrual=accrual,
  estpt.method=estpt.method, obd.method=obd.method)

## gBOIN-ET ##

toxprob <- rbind(c(0.82,0.65,0.41,0.42,0.34,0.26),
                 c(0.10,0.20,0.34,0.28,0.31,0.34),
                 c(0.05,0.10,0.15,0.18,0.21,0.24),
                 c(0.03,0.05,0.10,0.12,0.14,0.16))

effprob <- rbind(c(0.30,0.20,0.05,0.05,0.05,0.05),
                 c(0.35,0.30,0.25,0.20,0.15,0.10),
                 c(0.30,0.40,0.20,0.25,0.30,0.30),
                 c(0.05,0.10,0.50,0.50,0.50,0.55))

sev.weight <- c(0.00,0.50,1.00,1.50)
res.weight <- c(0.00,0.25,1.00,3.00)

gboinet(
  n.dose=n.dose, start.dose=start.dose,
  size.cohort=size.cohort, n.cohort=n.cohort,
  toxprob=toxprob, effprob=effprob,
  sev.weight=sev.weight, res.weight=res.weight,
  phi=phi, delta=delta,
  tau.T=tau.T, tau.E=tau.E, accrual=accrual,
  estpt.method=estpt.method, obd.method=obd.method)

## TITE-gBOIN-ET ##

toxprob <- rbind(c(0.82,0.65,0.41,0.42,0.34,0.26),
                 c(0.10,0.20,0.34,0.28,0.31,0.34),
                 c(0.05,0.10,0.15,0.18,0.21,0.24),
                 c(0.03,0.05,0.10,0.12,0.14,0.16))

effprob <- rbind(c(0.30,0.20,0.05,0.05,0.05,0.05),
                 c(0.35,0.30,0.25,0.20,0.15,0.10),
                 c(0.30,0.40,0.20,0.25,0.30,0.30),
                 c(0.05,0.10,0.50,0.50,0.50,0.55))

sev.weight <- c(0.00,0.50,1.00,1.50)
res.weight <- c(0.00,0.25,1.00,3.00)

tite.gboinet(
  n.dose=n.dose, start.dose=start.dose,
  size.cohort=size.cohort, n.cohort=n.cohort,
  toxprob=toxprob, effprob=effprob,
  sev.weight=sev.weight, res.weight=res.weight,
  phi=phi, delta=delta,
  tau.T=tau.T, tau.E=tau.E, accrual=accrual,
  estpt.method=estpt.method, obd.method=obd.method)



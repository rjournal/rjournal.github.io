### How to use the different files included in this submission ####
#Author: Camille J. Hochheimer, PhD

## Includes: 

## Hochheimer-Sabo.R    -- examples used in the manuscript
## simstudyfns.RData    -- functions to run simulation
## resultsfns.RData     -- functions for processing results
## plotcode.R           -- create tables and figures used in publication
## simulation templates -- see below

### How to reproduce examples ####
# Run Hochheimer-Sabo.R


### Templates for running simulation #####

## Example: Exponential with one change-point
library(cpsurvsim) #load R package
load("simstudyfns.RData") #load simulation functions

# create output object
param1cp.exp <- cbind(n = c(rep(50, 3), rep(100, 3), rep(500,3)),
                      endtime = as.numeric(100),
                      theta1 = c(rep(0.001, 9), rep(0.01, 9)),
                      theta2 = c(rep(0.01, 9), rep(0.001, 9)),
                      tau = c(20, 50, 80))

sim <- 10000 #set number of simulations
set.seed(4546) # set seed

#run simulation
exp1cpout <- apply(X = param1cp.exp, MARGIN = 1, FUN = test_cp,
                   cp = 1, dist = "exp", nsim = sim) 

#save to use in tables and analysis 
save(exp1cpout, file="exp1cpoutput.RData")

### Template files included in this submission: #####

# exp1cp_sim.R
# exp2cp_sim.R
# exp3cp_sim.R
# exp4cp_sim.R
# weib1cp_sim.R
# weib2cp_sim.R
# weib3cp_sim.R
# weib4cp_sim.R

### How to reproduce results from simulation study ####
# (1) Run all of the simulation template files
# (2) Run plotcode.R
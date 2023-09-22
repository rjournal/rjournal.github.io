rm(list = ls())
library(ggplot2)
library(reshape2)
library(xtable)
library(dplyr)
#Be sure to use correct data directory
# .../RJournal_Rfiles/sect 

# i = 1; n = 100, 000
# i = 2; n = 500, 000
# i = 3; n = 50, 000

i = 3

load("results/addtl_simulation_crr_no_var_", i, ".RData")
load("results/addtl_simulation_crr__var_", i, ".RData")
load("results/addtl_simulation_fast_no_var_", i, ".RData")
load("results/addtl_simulation_fast_var_", i, ".RData")


# Timing estimates (Table 2)
fast_time_no_var
fast_time_var
crr_time_no_var
crr_time_var

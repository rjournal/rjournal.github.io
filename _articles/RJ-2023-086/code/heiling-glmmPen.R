

# If necessary, set working directory to directory "glmmPen_code" folder
# where the "Tables ... Full Data.RData" objects are stored
# setwd("glmmPen_code")

# Libraries
library(glmmPen)
library(stringr)
library(ggplot2)

# Code to run an abbreviated version of the glmmPen fit for the basal dataset 
## In Rashid et al. (2020) paper, used 50 covariates. 
## However, this took well over an hour to run. Therefore,
## we instead provide an illustration using 10 covariates to illustrate the process,
## which can finish in under an hour.

# basal data from glmmPen package
data("basal")

# Extract response
y = basal$y
# Select a sampling of 10 TSP covariates from the total 50 covariates
set.seed(1618)
idx = sample(1:50, size = 10, replace = FALSE)
# Selected column index values:
(idx = idx[order(idx)])
X = basal$X[,idx]
# Selected predictors:
colnames(X)
group = basal$group
# Levels of the grouping variable:
levels(group)

# If have run this code before, this will have resulted in saved posterior samples.
# To completely replicate code, remove these files before running basal variable selection code
if(file.exists("Basal_Posterior_Draws.bin")) file.remove("Basal_Posterior_Draws.bin")
if(file.exists("Basal_Posterior_Draws.desc")) file.remove("Basal_Posterior_Draws.desc")

# Note: In code provided below, output includes iteration-level information
# for each iteration of the MCECM algorithm. In order to suppress this information
# to reduce some of the output, add argument `progress = FALSE` (see glmmPen
# function documentation for more details).

start_basal = proc.time()

set.seed(1618)
fitB = glmmPen(formula = y ~ X + (X | group), 
               family = "binomial", covar = "independent", 
               optim_options = optimControl(),
               tuning_options = selectControl(BIC_option = "BICq", pre_screen = T, 
                                              search = "abbrev"),
               BICq_posterior = "Basal_Posterior_Draws")

# Save output object for use in "glmmPen_FineSearch()" function used in "_code_extra.Rmd"
save(fitB, file = "fit_basal.RData")

end_basal = proc.time()
# Time needed to complete the algorithm
end_basal - start_basal

# Code to examine the output from the glmmPen fit for the basal dataset.
## (a) Illustration that methods such as summary, fixef, ranef ... run and work as expected
## (b) Code to create Figure 1

# Illustration of use of methods
summary(fitB)
print(fitB)
## fixed effects coefficients
fixef(fitB)
## random effects coefficients for each covariate for each level of the grouping factor
ranef(fitB)
## random effects covariance matrix
sigma(fitB)
summary(residuals(fitB, type = "deviance"))
plot(fitB)
## log-likelihood using corrected arithmetic mean estimator with importance sampling weights (the Pajor method discussed in paper)
logLik(fitB)
## BIC-derived quantities
BIC(fitB)

# Figure 1 (sample path plots and autocorrelation)

fitB_covar = sigma(fitB)$group
fitB_var = diag(fitB_covar)[-1]
TSP = names(fitB_var[which(fitB_var != 0)])[1:3]
# TSP = c("XGPR160_CD109", "XSPDEF_MFI2", "XPLEK2_HSD17B2")
plot_diag = plot_mcmc(object = fitB, plots = c("sample.path","autocorr"), 
                      grps = "all", vars = TSP)
# Figure 1a sample path plot
plot_diag$sample_path + theme(axis.text.x = element_text(angle = 270))
ggsave(filename = "Figures/Figure1A Sample Path.pdf",
       width = 7, height = 5, units = "in")
# Figure 1b autocorrelation plot
plot_diag$autocorr
ggsave(filename = "Figures/Figure1B Autocorrelation.pdf",
       width = 7, height = 5, units = "in")

## prediction using fixed effects only
head(predict(object = fitB, newdata = NULL, type = "link", fixed.only = T))
## the fitted linear predictor object using fixed effects only
head(fitted(object = fitB, fixed.only = T))

# Code to run a single replicate of one of the variable selection simulations (p=10)
## Note: In paper, Tables 3-4 show results from 8 sets of simulations, each with 100 replicates.
## The p=50 simulations especially can take a long time. 
## Therefore, we provide here an example of running a single p=10 simulation, which can finish
## in under an hour.
## The code to run all 100 replicates of all 8 sets of simulations in serial (which would take
## multiple days) is provided in the folder "sims_serial/".
## We ran these simulations on a cluster, allowing us to run all individual replicates in parallel.
## Instructions on running these simulations on a cluster are given in the README file within
## the folder "sims_on_cluster/".

# Situation
# Total predictors: 10
# Covariates: 2 non-zero slopes, one intercept
# Random effects: sd_ranef = 1.0 or 2.0 for intercept and non-zero slopes 

N = 500
sd_ranef = 1.0
K = 5

dat = sim.data(n = N, ptot = 10, pnonzero = 2, nstudies = K,
               sd_raneff = sd_ranef, family = 'binomial',
               seed = 3213, imbalance = 1, 
               pnonzerovar = 0, beta = c(0, 1, 1))

y = dat$y
X = dat$X[,-1]
group = dat$group

# If have run this code before, this will have resulted in saved posterior samples.
# To completely replicate code, remove these files before running simulated data 
# variable selection code
if(file.exists("BICq_Post_SingleSim.bin")) file.remove("BICq_Post_SingleSim.bin")
if(file.exists("BICq_Post_SingleSim.desc")) file.remove("BICq_Post_SingleSim.desc")

start_sim = proc.time()

start1 = proc.time()
set.seed(3213)
fit_glmmPen = glmmPen(formula = y ~ X + (X | group), family = "binomial",
                      covar = "independent", optim_options = optimControl(),
                      tuning_options = selectControl(BIC_option = "BICq", pre_screen = T,
                                                     search = "abbrev", lambda.min.presc = 0.01),
                      BICq_posterior = "BICq_Post_SingleSim")
end1 = proc.time()

end_sim = proc.time()
# Time needed to complete the algorithm
end_sim - start_sim

# From saved RData objects of saved simulation results, extract summary information

# RData object description: List with the following elements:
## Matrix of fixed effects (beta_mat)
## Matrix of variances from the random effect covariance matrices (vars_mat)
## Vector of times to completion (time)
## Matrix of prescreening results (PreSc)

# Code to create rows of Table 3

# file_name: name of saved RData object
Table3_out = function(file_name){
  
  # load results list object
  load(file_name)
  
  # Extract relevant output
  beta_mat = results$beta_mat
  vars_mat = results$vars_mat
  time = results$time
  # PreSc = results$PreSc
  
  # Number of finished simulations
  n = nrow(beta_mat)
  
  # Initializations: 
  ## true positives for fixed effects
  tp_f = 0
  ## false positives for fixed effects
  fp_f = 0
  ## true positives for random effects (non-zero variances in random effect covariance matrix)
  tp_r = 0
  ## false positives for random effects
  fp_r = 0
  ## Fixed effects for predictors 1 and 2 (the truely non-zero fixed effects)
  b1 = 0
  b2 = 0
  
  # Take average of the fixed effects for predictors 1 and 2 (average of the non-zero values)
  b1 = sum(beta_mat[which(beta_mat[,2] != 0),2]) / sum(beta_mat[,2] != 0)
  b2 = sum(beta_mat[which(beta_mat[,3] != 0),3]) / sum(beta_mat[,3] != 0)
  
  # Calculate true positives and false positives
  for(f in 1:n){
    
    beta = beta_mat[f,]
    vars = vars_mat[f,]
    
    tp_f = tp_f + sum(beta[2:3] != 0)
    fp_f = fp_f + sum(beta[-c(1:3)] != 0)
    
    tp_r = tp_r + sum(vars[2:3] != 0)
    fp_r = fp_r + sum(vars[-c(1:3)] != 0)
    
  }
  
  # Note: need to calculate percentages for true positive and false positive values
  # out = round(c(b1, b2, c(tp_f, fp_f, tp_r, fp_r) / n, median(time)), 2)
  out = c(round(c(b1, b2), 2), round(c(tp_f/(2*n)*100, fp_f/((length(beta)-3)*n)*100, 
                                       tp_r/(2*n)*100, fp_r/((length(vars)-3)*n)*100), 1),
          round(median(time), 2))
  names(out) = c("Beta 1","Beta 2","TP % Fixef","FP % Fixef","TP % Ranef","FP % Ranef","Median Time (hrs)")
  
  return(out)
}

# Code to create rows of Table 4

Table4_out = function(file_name){
  
  # load RData list object
  load(file_name)
  
  # Extract pre-screening information
  PreSc = results$PreSc
  
  # Find true and false positives
  pre_pos = numeric(2)
  names(pre_pos) = c("TP %","FP %")
  
  pre_pos[1] = mean(rowSums(PreSc[,c(2,3)])) / 2 * 100
  pre_pos[2] =  mean(rowSums(PreSc[,-c(1:3)])) / (ncol(PreSc) - 3) * 100
  
  
  return(round(pre_pos, 1))
}

# Table output

N = 500
K = rep(rep(c(5,10), each=2), times = 2)
sigma = rep(rep(c(1,sqrt(2)), times=2), times = 2)

# file names of simulation output for Tables 3 and 4
output_T34 = list.files(pattern = "Tables 3 and 4", full.names = T)

# Create Table 3 - coefficient and timing results

Table3 = NULL
for(i in 1:length(output_T34)){
  
  # run Table3_out function sourced from "sim_output.R"
  row = Table3_out(output_T34[i])
  if(i == 1){
    Table3 = row
  }else{
    Table3 = rbind(Table3, row)
  }
}

(Table3 = cbind(N, K, sigma, Table3))

# Create Table 4 - pre-screening results

Table4 = NULL
for(i in 1:length(output_T34)){
  
  # run Table4_out function sourced from "sim_output.R"
  row = Table4_out(output_T34[i])
  if(i == 1){
    Table4 = row
  }else{
    Table4 = rbind(Table4, row)
  }
}

(Table4 = cbind(N, K, sigma, Table4))


sessionInfo()



################################################################################################
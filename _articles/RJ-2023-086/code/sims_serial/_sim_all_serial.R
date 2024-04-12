

# Run all 800 simulation replicates (100 replicates for each of 8 simulation set-ups) in serial

# Call relevant libraries
library(glmmPen) 
library(stringr)

#########################################################################################
# Consistent variable values used for all simulations
#########################################################################################
# Total number of simulation replicates for each simulation set-up
sim_total = 100
# Total number of observations
N = 500

# Optional: Set prefix0 to a specific directory, format "directory_name/"
## Directories for simulation output will be placed in this directory
## If unspecified, will save in current working directory
prefix0 = ""

#########################################################################################
# Run simulations in serial
# 2 main sets of simulations: total covariates p = {10,50}, 
#   fixed effect coefficients = c(0,1,1)
# Within each of these sets, 4 set-ups: Number groups K = {5,10},
#   standard deviation of the random effects covariance matrix = {1.0, sqrt(2)}

# General steps within each set of simulations:
## Select appropriate set-up (number groups, standard deviation of random effects
##    covariance matrix), and define appropriate directory to store the output
## Simulate relevant dataset, extract response y, covariates X (removing
##    intercept), and grouping factor
## Perform variable selection on dataset using glmmPen function
## Save output object and time to complete model fit
#########################################################################################

# Specify all combinations of simulations:
combos = expand.grid(c(1,sqrt(2)),c(5,10),c(10,50),1)
colnames(combos) = c("sd_raneff","K","p","Beta")
# Output directories
combos$out_dir = c(str_c("paper_select_10",LETTERS[1:4],"_B1"),
                   str_c("paper_select_50",LETTERS[1:4],"_B1"))
# Directories saving posterior samples for BIC-ICQ calculation
combos$post_dir = c(str_c("post_10",LETTERS[1:4],"_B1"),
                    str_c("post_50",LETTERS[1:4],"_B1"))
combos

# Run all simulations in serial
for(j in 1:nrow(combos)){

  # Extract relevant parameters from the combo data.frame
  Beta = combos$Beta[j]
  p_tot = combos$p[j]
  K = combos$K[j]
  sd_ranef = combos$sd_raneff[j]
  
  # Extract output directory and posterior sample directory from combo data.fram
  prefix = str_c(prefix0, combos$out_dir[j])
  prefix_BICq = str_c(prefix0, combos$post_dir[j])
  
  if(!dir.exists(prefix)){dir.create(prefix, recursive = T)}
  if(!dir.exists(prefix_BICq)){dir.create(prefix_BICq, recursive = T)}
  
  # Define 100 random seeds to be used for each of the 100 simulation replicates
  set.seed(2021) 
  seeds = sample(1000:9999, size = sim_total, replace = F)
  
  # Run all 100 simulations for given simulation condition
  for(i in 1:sim_total){
    
    batch = i
    
    if(batch <= 9){
      batch_label = str_c("00",batch)
    }else if(batch <= 99){
      batch_label = str_c("0",batch)
    }else{
      batch_label = as.character(batch)
    }
    
    dat = sim.data(n = N, ptot = p_tot, pnonzero = 2, nstudies = K,
                   sd_raneff = sd_ranef, family = 'binomial',
                   seed = seeds[batch], imbalance = 1, 
                   pnonzerovar = 0, beta = c(0, rep(Beta,2)))
    
    y = dat$y
    X = dat$X[,-1]
    group = dat$group
    
    start1 = proc.time()
    set.seed(seeds[batch])
    fit_glmmPen = glmmPen(formula = y ~ X + (X | group), family = "binomial",
                          covar = "independent", optim_options = optimControl(),
                          tuning_options = selectControl(BIC_option = "BICq", pre_screen = TRUE,
                                                         search = "abbrev", lambda.min.presc = 0.01),
                          BICq_posterior = sprintf("%s/BICq_post_Batch_%s", prefix_BICq, batch_label))
    end1 = proc.time()
    
    
    
    output = list(fit_glmmPen = fit_glmmPen, time_select = (end1 - start1))
    
    save(output, file = sprintf("%s/Output_%s.RData", prefix, batch_label))
    
  }
  
}

################################################################################################
# Convert output into the tables given in the Simulation section of the paper
################################################################################################ 

# direct_out: From saved output files, extract fixed effects, random effect variances,
#   pre-screening information, and timing information
# files: vector of file names
# q: number of input predictors plus 1 (input predictors plus intercept)
direct_out = function(files, q = 11){
  
  time_vec = numeric(length(files))
  
  PreSc = matrix(0, nrow = length(files), ncol = q)
  
  beta_mat = matrix(0, nrow = length(files), ncol = q)
  vars_mat = matrix(0, nrow = length(files), ncol = q)
  
  
  for(f in 1:length(files)){
    load(files[f])
    fit_glmmPen = output$fit_glmmPen
    if(!is.null(output$time_select)){
      time_vec[f] = output$time_select[3] / 3600 # Record in hours instead of seconds
    }else if(!is.null(output$time)){
      time_vec[f] = output$time[3] / 3600 # Record in hours instead of seconds
    }
    
    
    beta = fit_glmmPen$fixef
    vars = diag(fit_glmmPen$sigma)
    
    beta_mat[f,] = beta
    vars_mat[f,] = vars
    
    PreSc[f,] = fit_glmmPen$penalty_info$prescreen_ranef
    
  }
  
  colnames(PreSc) = names(fit_glmmPen$penalty_info$prescreen_ranef)
  
  return(list(beta_mat = beta_mat, vars_mat = vars_mat, PreSc = PreSc, time = time_vec))
}


# Directories for Tables 3 and 4 (beta = c(0,1,1))
directories = combos$out_dir

for(i in 1:length(directories)){
  files = list.files(path = sprintf("%s%s", prefix0, directories[i]), full.names = TRUE)
  situation = combos[which(combos$out_dir == directories[i]),]
  q = situation["p"] + 1
  results = direct_out(files, q=q)
  save(results, file = sprintf("%sTables 3 and 5 Row %i Full Data.RData", prefix0, i))
}



################################################################################################

################################################################################################ 
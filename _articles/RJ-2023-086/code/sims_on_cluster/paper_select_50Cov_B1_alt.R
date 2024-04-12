# Test accuracy of glmmPen for Binomial family
## Paper simulations for model selection with 50 Covariates
## True Beta: (0,1,1)

# Situation
# Total predictors: 50
# Covariates: 2 non-zero slopes, one intercept
# Random effects: sd_ranef = 1.0 or sqrt(2.0) for intercept and non-zero slopes 

## Alternative approach: first find 'best' random effect model, then find best fixed effect model.

library(glmmPen)
library(stringr)

# Arrays 1-400 
array_val <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# Can change this directory as desired
prefix0 = "~/"
if(!dir.exists(prefix0)){dir.create(prefix0, recursive = T)}


if(array_val <= 100){
  batch <- array_val
  sd_ranef = 1.0
  K = 5
  prefix = str_c(prefix0, "paper_select_50A_B1")
  prefix_BICq = str_c(prefix0, "selectpost_50A_B1")
}else if(array_val <= 200){
  batch <- array_val - 100
  sd_ranef = sqrt(2.0)
  K = 5
  prefix = str_c(prefix0, "paper_select_50B_B1")
  prefix_BICq = str_c(prefix0, "selectpost_50B_B1")
}else if(array_val <= 300){
  batch <- array_val - 200
  sd_ranef = 1.0
  K = 10
  prefix = str_c(prefix0, "paper_select_50C_B1")
  prefix_BICq = str_c(prefix0, "selectpost_50C_B1")
}else if(array_val <= 400){
  batch <- array_val - 300
  sd_ranef = sqrt(2.0)
  K = 10
  prefix = str_c(prefix0, "paper_select_50D_B1")
  prefix_BICq = str_c(prefix0, "selectpost_50D_B1")
}


if(!dir.exists(prefix)){dir.create(prefix, recursive = T)}
if(!dir.exists(prefix_BICq)){dir.create(prefix_BICq, recursive = T)}


if(batch <= 9){
  batch_label = str_c("00",batch)
}else if(batch <= 99){
  batch_label = str_c("0",batch)
}else{
  batch_label = as.character(batch)
}




sim_total = 100
N = 500

set.seed(2021) 
seeds = sample(1000:9999, size = sim_total, replace = F)

dat = sim.data(n = N, ptot = 50, pnonzero = 2, nstudies = K,
               sd_raneff = sd_ranef, family = 'binomial',
               seed = seeds[batch], imbalance = 1, 
               pnonzerovar = 0, beta = c(0, 1, 1))

y = dat$y
X = dat$X[,-1]
group = dat$group



# Default optimControl arguments
start1 = proc.time()
set.seed(seeds[batch])
fit_glmmPen = glmmPen(formula = y ~ X + (X | group), family = "binomial",
                      covar = "independent", optim_options = optimControl(),
                      tuning_options = selectControl(BIC_option = "BICq", pre_screen = T,
                                                     search = "abbrev", lambda.min.presc = 0.05),
                      BICq_posterior = sprintf("%s/BICq_post_Batch_%s", prefix_BICq, batch_label))
end1 = proc.time()



output = list(fit_glmmPen = fit_glmmPen, time_select = (end1 - start1))

save(output, file = sprintf("%s/Output_%s.RData", prefix, batch_label))

################################################################################################

print(gc())

q(save="no")

################################################################################################ 
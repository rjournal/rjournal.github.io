

# From completed simulations, create RData objects

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

# Specify path where simulation results are kept, change as needed
prefix = "~/"
# Directories: 
## 10x_B1_abbrev gives the beta = c(0,1,1) results for p=10
## 50x_B1 gives the beta = c(0,1,1) results for p=50


# Directories for Tables 3 and 4 results (beta = c(0,1,1))
directories_T3 = c("paper_select_10A_B1_abbrev","paper_select_10B_B1_abbrev",
                   "paper_select_10C_B1_abbrev","paper_select_10D_B1_abbrev",
                   "paper_select_50A_B1","paper_select_50B_B1","paper_select_50C_B1",
                   "paper_select_50D_B1")

for(i in 1:length(directories_T3)){
  files = list.files(path = sprintf("%s/%s", prefix, directories_T3[i]), full.names = T)
  if(i <= 4){
    q = 11
  }else{
    q = 51
  }
  results = direct_out(files, q=q)
  save(results, file = sprintf("Tables 3 and 4 Row %i Full Data.RData", i))
}


####################################################################################################

#----------------------------------------------------------
### CONTAINS: 
# Sample R script to replicate runtime results as shown in Figure 5 of Aikens et al
# Expects Installed: stratamatch (v0.1.6)
#                    dplyr (v1.0.1)
# Arguments: ID for naming result file
#----------------------------------------------------------

#----------------------------------------------------------
# NOTE:  This script collects one replicate of runtime data.  10 replicates of
# data are shown in Figure 5.  Each iteration of this script may take several
# hours on a personal computer.
#----------------------------------------------------------

library("stratamatch")
library("dplyr")

args = commandArgs(trailingOnly=TRUE)
fileid <- args[1]

#----------------------------------------------------------
### Helper function to match without Stratification
#----------------------------------------------------------

#' Match without Stratification
#'
#' @param object a strata object
#' @param propensity (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
strata_match_nstrat <- function(object, model = NULL, k = 1) {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    stop("optmatch package is required.  Please install it.")
  }
  
  check_inputs_matcher(object, model, k)
  
  message("This function makes essential use of the optmatch package, which has an academic license.")
  message("For more information, run optmatch::relaxinfo()")
  
  if (is.null(model)) {
    # match on all variables, stratified by stratum
    model <- formula(paste(
      object$treat, "~ . -", object$outcome,
      "- stratum"
    ))
  }
  
  message(paste(
    "Fitting propensity model:",
    Reduce(paste, deparse(model))
  ))
  
  # build propensity model
  propensity_model <- glm(model,
                          data = object$analysis_set,
                          family = binomial()
  )
  
  # optmatch issues incorrect warning.  Catch this and suppress it.
  withCallingHandlers(matchdist <- optmatch::match_on(propensity_model,
                                                      data = object$analysis_set), 
                      warning = function(w) {
                        if(grepl("Error gathering complete data", w$message)){
                          message(w$type)
                          invokeRestart("muffleWarning")
                        } 
                      })
  
  return(optmatch::pairmatch(matchdist,
                             data = object$analysis_set,
                             controls = k
  )) 
}

#----------------------------------------------------------
### Helper function for running stratified and unstratified matching
#----------------------------------------------------------

#' Run Strata Match
#' 
#' Run stratified and unstratified matching, recording runtimes
#'
#' @param N number of samples (minimum 3000 for this application)
#'
#' @return dataframe of run times
run_strata_match <- function(N){
  n_dat <- make_sample_data(N)
  
  # stratify 
  t1 <- proc.time()
  a.strat <- auto_stratify(data = n_dat, treat = "treat", 
                           outcome = "outcome", 
                           prognosis = outcome ~ X1 + X2 + B1 + B2 + C1,
                           size = 2500)
  strat_time <- proc.time() - t1
  print(paste("Stratified sample of ", N))
  
  # match within strata
  t2 <- proc.time()
  strata_match(a.strat, propensity = treat ~ X1 + X2 + B1 + B2 + C1)
  strata_match_time <- proc.time() - t2
  print(paste("stratamatched sample of ", N))
  
  # match without stratification
  t3 <- proc.time()
  strata_match_nstrat(a.strat, propensity = treat ~ X1 + X2 + B1 + B2 + C1)
  nstrata_match_time = proc.time() - t3
  print(paste("ordinary matched sample of ", N))
  
  return(data.frame(rbind(strat_time, strata_match_time, nstrata_match_time)))
}

#----------------------------------------------------------
### Run strata match for a sequence of sample sizes
#----------------------------------------------------------

options("optmatch_max_problem_size" = Inf)
#n_samples <- c(3000, 5000, 10000, 15000, 20000, 25000, 30000)
n_samples <- c(3000, 5000)
time_list <- lapply(n_samples, run_strata_match)

bind_rows(time_list, .id = "column_label")

trials <- length(n_samples)

time_df <- bind_rows(time_list, .id = "column_label") %>%
  mutate(n_samples = rep(n_samples, each = 3)) %>% 
  mutate(process = rep(c("Stratification", "Stratified Match", "Unstratified Match"), trials)) %>%
  select(c(process, n_samples, user.self, sys.self, elapsed))

#----------------------------------------------------------
### Save data as .csv
#----------------------------------------------------------

write.csv(time_df, paste("performance_",fileid,".csv", sep = ""))

############################################################
# Reproducibility Script for BayesianLasso

#
############################################################

# if (!requireNamespace("BayesianLasso", quietly = TRUE)) {
#   devtools::load_all(".")
# }
# reproduce_all.R

script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
setwd(script_dir)

helpers_dir <- file.path(script_dir, "helpers")

helper_files <- c("generate_data.R",
                  "effective_sample_size.R",
                  "mcmc_stats.R",
                  "normalize.R")
library(BayesianLasso)


helper_paths <- file.path(helpers_dir, helper_files)
missing <- helper_paths[!file.exists(helper_paths)]
if (length(missing) > 0) {
  stop("Missing helper file(s):\n", paste(missing, collapse = "\n"))
}

invisible(lapply(helper_paths, source, local = globalenv()))




results_dir <- "results"
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

datasets <- c("simulated") #, "diabetes2", "Kakadu2", "Crime", "riboflavin", "eyedata")


set.seed(123)
time_val_total_ngtp <- system.time({
  for (d in datasets) {
    dataset_name <- d
    if(dataset_name!= "riboflavin" && dataset_name!= "eyedata"){
      if(dataset_name!= "simulated"){
        res = generate_data(dataset_name)
        
        x = res$mX
        y = res$vy
        p = res$p
        n = res$n
        
        rm(res)
      }else if(dataset_name == "simulated"){
      x = NULL
      y = NULL
      p = NULL
      n = NULL
      }
      ngtp_env <- new.env(parent = globalenv())
      ngtp_env$dataset_name <- dataset_name
      ngtp_env$x            <- x
      ngtp_env$y            <- y
      ngtp_env$p            <- p
      ngtp_env$n            <- n
      source("Benchmarks_ngtp.R", local = ngtp_env)
    }
    if(dataset_name == "riboflavin" || dataset_name ==  "eyedata"){
      res = generate_data(dataset_name)
      
      mX = res$mX
      vy = res$vy
      p = res$p
      n = res$n
      
      rm(res)
      rmarkdown::render(
        "benchmarks_pgtn.Rmd",
        params = list(dataset_name = d,
                      results_dir = file.path(script_dir, "results")),
        envir = new.env(parent = globalenv())
      )
      
    }
  }
})[3]

print(time_val_total_ngtp)

# datasets_pgtn <- c("cookie", "eyedata")
# 
# 
# 
# for (d in datasets_pgtn) {
#   print(d)
#   rmarkdown::render(
#     "benchmarks_pgtn.Rmd",
#     params = list(dataset_name = d, results_dir = "results"),
#     envir = new.env(parent = globalenv())
#   )
# }
# 
# 
# load(file.path(results_dir,"eyedata_results_pgtn.Rdata"))
# 
# print(colMeans(res_PC$mStat))
# print(colMeans(res_hans$mStat))
# print(colMeans(res_bayesreg$mStat))
# print(time_val_total_pgtn)
# 
# 


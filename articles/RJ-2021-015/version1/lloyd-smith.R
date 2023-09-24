
# Load packages
library(dplyr)
library(rmdcev)
#knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
#opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)


## ----data-------------------------------------------------------
data(data_rec, package = "rmdcev")


## ----summary----------------------------------------------------
data_rec %>%
	group_by(alt) %>%
	summarise(mean_quant = mean(quant),
			      mean_price = mean(price))


## ----mdcev.data_test--------------------------------------------
data_mdcev <- mdcev.data(data_rec,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")


## ----mdcev------------------------------------------------------
args(mdcev)


## ----formula----------------------------------------------------
formula = ~ z1 + z2 + z3


## ----set_data---------------------------------------------------
data_model <- mdcev.data(data_rec, subset = id < 201,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")  

formula <- ~ alt - 1



## ----estimation_mdcev_hybrid------------------------------------
mdcev_mle <- mdcev(formula,
                  data = data_model,
                  model = "hybrid",
                  algorithm = "MLE",
                  std_errors = "deltamethod",
                  print_iterations = FALSE)


## ----summary_mdcev_hybrid----------------------------------------
summary(mdcev_mle)


## ---- estimation_mdcev_alpha, message = FALSE, warning = FALSE-----------
mdcev_mle <- mdcev(formula,
                    data = data_model,
                    model = "alpha",
                    algorithm = "MLE",
				  	std_error = "deltamethod",
                    print_iterations = FALSE)


## ----summary_mdcev_alpha---------------------------------
summary(mdcev_mle)


## ---- estimation_mdcev_les, message = FALSE, warning = FALSE-------------
mdcev_mle <- mdcev(formula,
                    data = data_model,
                    model = "gamma",
                    algorithm = "MLE",
				  	std_error = "deltamethod",
                    print_iterations = FALSE)


## ----  echo=T,summary_mdcev_les------------------------------------------
summary(mdcev_mle)


## ----estimation_mdcev_bayes, message = FALSE, warning = FALSE----
mdcev_bayes <- mdcev(formula,
                        data = data_model,
                        model = "hybrid0",
                        algorithm = "Bayes",
                        n_iterations = 200,
                        n_chains = 4,
                        print_iterations = FALSE)


## ----summary_mdcev_bayes---------------------------------
	summary(mdcev_bayes)


## ---- estimation_mdcev_lc, message = FALSE, warning = FALSE--------------

data_model <- mdcev.data(data_rec, subset = id < 501,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")  

formula <- ~ alt - 1 | university + ageindex + urban

mdcev_lc <- mdcev(formula,
                  data = data_model,
                  n_classes = 2,
                  model = "hybrid0",
                  algorithm = "MLE",
                  print_iterations = FALSE)


## ---- summary_mdcev_lc---------------------------------------------------
summary(mdcev_lc)


## ----estimation_mdcev_rp, message = FALSE, warning = FALSE----
data_model <- mdcev.data(data_rec, subset = id < 201,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant") 

mdcev_rp <- mdcev(formula = ~ 1,
					data = data_model,
					model = "hybrid0",
					algorithm = "Bayes",
					n_chains = 4,
					fixed_scale1 = 1,
					n_iterations = 200,
					random_parameters = "uncorr",
					print_iterations = FALSE)


## ----summary_mdcev_rp-------------------------------------------
summary(mdcev_rp)


## ---- sim_mdcev_welfare_policy-------------------------------------------
nalts <- mdcev_mle$stan_data[["J"]]
npols <- 2

policies<-  CreateBlankPolicies(npols = npols,
								nalts = nalts,
								mdcev_mle$stan_data[["dat_psi"]],
								price_change_only = TRUE)

policies$price_p[[1]] <- c(0, rep(1, nalts))
policies$price_p[[2]][10:13] <- rep(10, 4)


## ---- sim_mdcev_welfare_policy_psi---------------------------------------

policies$dat_psi[[1]][3] <- policies$dat_psi[[1]][3]*1.2


## ---- sim_mdcev_welfare_prepare------------------------------------------
df_sim <- PrepareSimulationData(mdcev_mle, policies)


## ----sim_mdcev_welfare------------------------------------------
welfare <- mdcev.sim(df_sim$df_indiv,
					 df_common = df_sim$df_common,
					 sim_options = df_sim$sim_options,
					 cond_err = 1,
					 nerrs = 15,
					 sim_type = "welfare")
summary(welfare)


## ----sim_mdcev_demand-------------------------------------------
policies <-	CreateBlankPolicies(npols = 2,
								nalts = mdcev_mle$stan_data[["J"]],
								mdcev_mle$stan_data[["dat_psi"]],
								price_change_only = TRUE)

policies$price_p[[1]] <- c(0, rep(1, nalts))
policies$price_p[[2]][10:13] <- rep(10, 4)
df_sim <- PrepareSimulationData(mdcev_mle, policies)

demand <- mdcev.sim(df_sim$df_indiv,
						df_common = df_sim$df_common,
						sim_options = df_sim$sim_options,
					 	cond_err = 1,
						nerrs = 20,
						sim_type = "demand")
summary(demand)

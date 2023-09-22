
# Load packages
library(rmdcev)
#knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
#opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)


## ---- echo=T, data-----------------------------------------------------------------
data(data_rec, package = "rmdcev")


## ---- echo=T, summary--------------------------------------------------------------
aggregate(cbind(quant, price) ~ alt, data = data_rec, FUN = mean )


## ---- echo=T, mdcev.data_test------------------------------------------------------
data_mdcev <- mdcev.data(data_rec,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")


## ---- echo=T, mdcev----------------------------------------------------------------
args(mdcev)


## ---- echo=T-----------------------------------------------------------------------
f1 = ~ 0


## ---- echo=T-----------------------------------------------------------------------
f2 = ~ university + ageindex


## ---- echo=T-----------------------------------------------------------------------
f2 = ~ z1 + z2


## ---- echo=T-----------------------------------------------------------------------
f3 = ~ 0 | university + ageindex


## ---- echo=T-----------------------------------------------------------------------
f4 = ~ 0 | 0 | q1


## ---- echo=T, set_data-------------------------------------------------------------
data_model <- mdcev.data(data_rec, subset = id <= 200,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")  


## ---- echo=T-----------------------------------------------------------------------
data_model$age_garden = ifelse(data_model$alt == "garden",
							   data_model$ageindex,0)
f5 = ~ age_garden


## ---- echo=T, estimation_mdcev_gamma-----------------------------------------------
mdcev_mle <- mdcev(~ age_garden,
                  data = data_model,
                  model = "gamma",
                  algorithm = "MLE",
                  print_iterations = FALSE)


## ---- echo=T,summary_mdcev_gamma---------------------------------------------------
summary(mdcev_mle)


## ---- estimation_mdcev_alpha, message = FALSE, warning = FALSE---------------------
mdcev_mle <- mdcev(~ age_garden,
                   data = data_model,
                   model = "alpha",
                   algorithm = "MLE",
                   print_iterations = FALSE)


## ---- echo=T, echo=T,summary_mdcev_alpha-------------------------------------------
summary(mdcev_mle)


## ---- echo=T, estimation_mdcev_hybrid----------------------------------------------
mdcev_mle <- mdcev(~ age_garden,
                  data = data_model,
                  model = "hybrid",
                  algorithm = "MLE",
                  print_iterations = FALSE)


## ----  echo=T,summary_mdcev_hybrid-------------------------------------------------
summary(mdcev_mle)


## ---- estimation_mdcev_kt, message = FALSE, warning = FALSE------------------------
kt_mle <- mdcev(~ age_garden | 0 | 0,
                   data = data_model,
                   model = "kt_ee",
                   algorithm = "MLE",
                   print_iterations = FALSE)


## ----  echo=T,summary_mdcev_kt-----------------------------------------------------
summary(kt_mle)


## ----eval=T, echo=T, estimation_mdcev_bayes, message = FALSE, warning = FALSE------
mdcev_bayes <- mdcev(~ age_garden,
                        data = data_model,
                        model = "gamma",
                        algorithm = "Bayes",
                        n_iterations = 200,
                        n_chains = 4,
						n_cores = 4,
                        print_iterations = FALSE)


## ----eval=T, echo=T, summary_mdcev_bayes-------------------------------------------
	summary(mdcev_bayes)


## ---- estimation_mdcev_lc, message = FALSE, warning = FALSE------------------------

data_model <- mdcev.data(data_rec, subset = id <= 500,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant")  

mdcev_lc <- mdcev(~ 0 | university + ageindex + urban,
                  data = data_model,
                  n_classes = 2,
                  model = "gamma",
				  fixed_scale1 = 1,
                  algorithm = "MLE",
                  print_iterations = FALSE)


## ---- summary_mdcev_lc-------------------------------------------------------------
summary(mdcev_lc)


## ----eval=T, echo=T, estimation_mdcev_rp, message = FALSE, warning = FALSE---------
data_model <- mdcev.data(data_rec, subset = id <= 200,
					   id.var = "id",
					   alt.var = "alt",
					   choice = "quant") 

mdcev_rp <- mdcev(~ 0,
					data = data_model,
					model = "gamma",
					algorithm = "Bayes",
					n_chains = 4,
					psi_ascs = 0,
					fixed_scale1 = 1,
					n_iterations = 200,
					random_parameters = "uncorr",
					print_iterations = FALSE)


## ---- eval=T, summary_mdcev_rp-----------------------------------------------------
summary(mdcev_rp)


## ---- sim_mdcev_welfare_estimate---------------------------------------------------
mdcev_mle <- mdcev(~0,
                  data = data_model,
                  model = "hybrid",
                  algorithm = "MLE",
				  std_errors = "mvn",
                  print_iterations = FALSE)


## ---- sim_mdcev_welfare_policy-----------------------------------------------------
nalts <- mdcev_mle$stan_data[["J"]]
npols <- 2

policies<-  CreateBlankPolicies(npols = npols,
								model = mdcev_mle,
								price_change_only = TRUE)

policies$price_p[[1]] <- c(0, rep(1, nalts))
policies$price_p[[2]][10:13] <- rep(10, 4)


## ---- sim_mdcev_welfare_policy_psi-------------------------------------------------

policies$dat_psi[[1]][3] <- policies$dat_psi[[1]][3]*1.2


## ---- sim_mdcev_welfare_prepare----------------------------------------------------
df_sim <- PrepareSimulationData(mdcev_mle, policies)


## ---- eval=T, sim_mdcev_welfare----------------------------------------------------
welfare <- mdcev.sim(df_sim$df_indiv,
					 df_common = df_sim$df_common,
					 sim_options = df_sim$sim_options,
					 cond_err = 1,
					 nerrs = 25,
					 sim_type = "welfare")
summary(welfare)


## ---- eval=T, sim_mdcev_demand-----------------------------------------------------
policies <-	CreateBlankPolicies(npols = 2, model = mdcev_mle)

policies$price_p[[1]] <- c(0, rep(1, nalts))
policies$price_p[[2]][10:13] <- rep(10, 4)

df_sim <- PrepareSimulationData(mdcev_mle, policies)

demand <- mdcev.sim(df_sim$df_indiv,
						df_common = df_sim$df_common,
						sim_options = df_sim$sim_options,
					 	cond_err = 1,
						nerrs = 25,
						sim_type = "demand")
summary(demand)


## ---- eval=T, sim_mdcev_data-------------------------------------------------------
model = "gamma" 
nobs = 1000
nalts = 10
sim.data <- GenerateMDCEVData(model = model, 
							  nobs = nobs, 
							  nalts = nalts,
							  psi_j_parms = c(-5, 0.5, 2), # alternative-specific variables
							  psi_i_parms = c(-1.5, 3, -2, 1, 2), # individual-specific variables
							  gamma_parms = stats::runif(nalts, 1, 10),
							  alpha_parms = 0.5,
							  scale_parms = 1)


## ---- fake_estimate, message = FALSE, warning = FALSE------------------------------
mdcev_mle <- mdcev(formula = ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8,
        				   data = sim.data$data,
        				   model = model,
						   psi_ascs = 0,
        				   algorithm = "MLE",
        				   print_iterations = FALSE)


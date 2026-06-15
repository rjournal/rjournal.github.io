#set working directory 
#setwd("")
setwd("C:/Users/nguye/OneDrive - Hitotsubashi University/R Journal/MarchSubmission/DeepLearningCausal")


#set save_files to TRUE to save output results
save_files = FALSE

#devtools::install_github("hknd23/DeepLearningCausal", force = TRUE)
#package installation from CRAN if not already installed
if (!requireNamespace("DeepLearningCausal")) {
  install.packages("DeepLearningCausal")
}
TimeA <- Sys.time()
library(DeepLearningCausal)

#set envname to desired environment name or path 
python_ready(envname  = "r-reticulate")

#Import Datasets
data("exp_data_full")
str(exp_data_full)
data("pop_data_full")
str(pop_data_full)

#Model Spefication
response_formula <- support_war ~ age + female + education + income + employed +
  job_loss + hindu + political_ideology

# Splitting experimental data into training and testing sets
set.seed(1234)
exp_split <- rsample::initial_split(exp_data_full,)
train_data <- rsample::training(exp_split)
test_data <- rsample::testing(exp_split)
#Meta Learners with Deep Learning
### S Learner
slearner_deep <- metalearner_deeplearning(cov.formula = response_formula, 
                                          train.data = train_data,
                                          test.data = test_data,
                                          treat.var = "strong_leader", meta.learner.type = "S.Learner",
                                          algorithm = "adam", hidden.layer = c(4,2), hidden_activation = "relu",
                                          output_activation = "sigmoid", output_units = 1,
                                          loss = "binary_crossentropy", metrics = "accuracy", epoch = 100,
                                          batch_size = 32, validation_split = 0.2, dropout_rate = 0.1, patience = 20, 
                                          conformal = TRUE,
                                          alpha = 0.1,
                                          calib_frac = 0.5, prob_bound = TRUE,
                                          verbose = 1, seed = 1234)

print(slearner_deep)

#Figure 2
slearner_trace <- plot(slearner_deep$ml_model_history[[1]])

#Figure 3a
s_conformal <- conformal_plot(slearner_deep,
                              binary.outcome = TRUE,
                              prop = .2,
                              seed = 1234)

# T Learner
tlearner_deep <- metalearner_deeplearning(cov.formula = response_formula, 
                                          train.data = train_data,
                                          test.data = test_data,
                                          treat.var = "strong_leader", 
                                          meta.learner.type = "T.Learner",
                                          algorithm = "adam", hidden.layer = c(4,2), hidden_activation = "relu",
                                          output_activation = "sigmoid", output_units = 1,
                                          loss = "binary_crossentropy", metrics = "accuracy", epoch = 100,
                                          batch_size = 32, validation_split = 0.2, dropout_rate = 0.1, patience = 20, 
                                          conformal = TRUE,
                                          alpha = 0.1,
                                          calib_frac = 0.5, prob_bound = TRUE,
                                          verbose = 1, seed = 1234)
print(tlearner_deep)
plot(tlearner_deep$ml_model_history[[1]])

#Figure 3b
t_conformal <- conformal_plot(tlearner_deep,
                              binary.outcome = TRUE,
                              prop = .2,
                              seed = 1234)

#### Other Meta Learners 

xlearner_deep <- metalearner_deeplearning(cov.formula = response_formula, 
                                          train.data = train_data,
                                          test.data = test_data,
                                          treat.var = "strong_leader", 
                                          meta.learner.type = "X.Learner",
                                          algorithm = "adam", hidden.layer = c(4,2), hidden_activation = "relu",
                                          output_activation = "sigmoid", output_units = 1,
                                          loss = "binary_crossentropy", metrics = "accuracy", epoch = 100,
                                          batch_size = 32, validation_split = 0.2, dropout_rate = 0.1, patience = 20, 
                                          conformal = FALSE, seed = 1234)
# R Learner
rlearner_deep <- metalearner_deeplearning(cov.formula = response_formula, 
                                          train.data = train_data,
                                          test.data = test_data,
                                          treat.var = "strong_leader", 
                                          meta.learner.type = "R.Learner",
                                          algorithm = "adam", hidden.layer = c(4,2), hidden_activation = "relu",
                                          output_activation = "sigmoid", output_units = 1,
                                          loss = "binary_crossentropy", metrics = "accuracy", epoch = 100,
                                          batch_size = 32, validation_split = 0.2, dropout_rate = 0.1, patience = 20, 
                                          conformal = FALSE, seed = 1234)
### HTE plots for meta learners 
#Figure 4a
slearner_hte <- hte_plot(slearner_deep, selected_vars = c( "employed", "female", "political_ideology"), 
                         cut_points = c(.5,.5,5),  custom_labels= c( "Employed", "Unemployed",
                                                                     "Male", "Female", "Centrist", "Right-wing Partisan"))
#Figure 4b
tlearner_hte <- hte_plot(tlearner_deep, selected_vars = c( "employed", "female", "political_ideology"), 
                         cut_points = c(.5,.5,5),  custom_labels= c( "Employed", "Unemployed",
                                                                     "Male", "Female", "Centrist", "Right-wing Partisan"))
#Figure 4c
xlearner_hte <- hte_plot(xlearner_deep, selected_vars = c( "employed", "female", "political_ideology"), 
                         cut_points = c(.5,.5,5),  custom_labels= c( "Employed", "Unemployed",
                                                                     "Male", "Female", "Centrist", "Right-wing Partisan"))
### Comparing 4 learners:

allCATEs_deep <- data.frame("S_learner" = slearner_deep$CATEs,
                            "T_learner" = tlearner_deep$CATEs,
                            "X_learner" = xlearner_deep$CATEs,
                            "R_learner" = rlearner_deep$CATEs)
#Figure 5
psych::pairs.panels(allCATEs_deep, breaks = 30)

#### PATT-C

deeppattc <- pattc_deeplearning(response.formula = response_formula,
                                exp.data = exp_data_full, pop.data = pop_data_full,
                                treat.var = "strong_leader", compl.var = "compliance",
                                compl.algorithm = "adam", response.algorithm = "adam",
                                compl.hidden.layer = c(4,2), response.hidden.layer = c(4,2),
                                compl.hidden_activation = "relu", response.hidden_activation = "relu",
                                response.output_activation = "sigmoid", response.output_units = 1, 
                                response.loss = "binary_crossentropy", response.metrics = "accuracy",
                                compl.epoch = 100, response.epoch = 300, verbose = 1, batch_size = 32, 
                                compl.validation_split = 0.2, response.validation_split = 0.2, 
                                compl.dropout_rate = 0.1, response.dropout_rate = 0.1,
                                compl.patience = 20, response.patience = 20,
                                nboot = 1000, 
                                seed = 1234)

#Figure 6
pattc_plot <- plot(deeppattc)

#Figure 7
pattc_hte <- hte_plot(deeppattc, selected_vars = c( "employed", "female", "political_ideology"), 
                      cut_points = c(.5,.5,5),  custom_labels= c( "Employed", "Unemployed", 
                                                                  "Male", "Female", "Centrist", 
                                                                  "Right-wing Partisan"))

TimeB <- Sys.time()

#Time elapsed
TimeA - TimeB
# Save output results to RDS 
if (save_files){
  if (!dir.exists("Results")) {
    dir.create("Results")
  }
  setwd("Results")
  saveRDS(slearner_deep, paste0("slearner_deep",Sys.Date(),".RDS"))
  saveRDS(tlearner_deep, paste0("tlearner_deep",Sys.Date(),".RDS"))
  saveRDS(xlearner_deep, paste0("xlearner_deep",Sys.Date(),".RDS"))
  saveRDS(rlearner_deep, paste0("rlearner_deep",Sys.Date(),".RDS"))
  saveRDS(deeppattc, paste0("deeppattc",Sys.Date(),".RDS"))
  ggsave(paste0("pattc_hte",Sys.Date(),".png"), 
         plot = pattc_hte, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("slearner_trace",Sys.Date(),".png"), 
         plot = slearner_trace, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("slearner_hte",Sys.Date(),".png"), 
         plot = slearner_hte, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("tlearner_hte",Sys.Date(),".png"), 
         plot = tlearner_hte, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("xlearner_hte",Sys.Date(),".png"), 
         plot = xlearner_hte, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("s_conformal",Sys.Date(),".png"), 
         plot = s_conformal, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("t_conformal",Sys.Date(),".png"), 
         plot = t_conformal, width = 8, height = 6, dpi = 300, bg = "white")
  ggsave(paste0("deeppattc",Sys.Date(),".png"), 
         plot = pattc_plot, width = 8, height = 6, dpi = 300, bg = "white")
  png(paste0("corr_plot.png",Sys.Date(),".png"), width = 800, height = 600)
  psych::pairs.panels(allCATEs_deep, breaks = 30)
  dev.off()
}
# End of DeepLearningCausal.R

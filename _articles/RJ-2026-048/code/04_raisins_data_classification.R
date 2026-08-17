if (!requireNamespace("LBBNN", quietly = TRUE)) {
  install.packages("LBBNN")
}
library(LBBNN)


seed <- 42
torch::torch_manual_seed(seed)

loaders_raisin <- get_dataloaders(raisin_dataset, train_proportion = 0.8, 
                           train_batch_size = 720, test_batch_size = 180)
train_loader_raisin <- loaders_raisin$train_loader
test_loader_raisin <- loaders_raisin$test_loader

problem <- 'binary classification'
sizes <- c(7, 10, 10, 10, 1) 
inclusion_priors <-c(0.5, 0.5, 0.5, 0.5) 
stds <- c(1, 1, 1, 1) 

#possible initializations: 
#polarized, polarized_mild, polarized_dense, polarized_sparse, 
#dense, sparse, balanced
inclusion_inits <- 'balanced'
device <- 'cpu' #can also be mps or gpu.

model_raisins <- lbbnn_net(problem_type = problem,sizes = sizes,
                           prior = inclusion_priors, 
                           inclusion_inits = inclusion_inits,input_skip = TRUE,
                           std = stds, flow = FALSE, device = device)


results_raisins <- train_lbbnn(epochs = 3000, LBBNN = model_raisins, lr = 0.005,
                               train_dl = train_loader_raisin, device = device,
                               min_density = NULL)

val_raisins <- validate_lbbnn(LBBNN = model_raisins,num_samples = 100, 
               test_dl = test_loader_raisin, device)
plot(model_raisins, type = 'global', 
     vertex_size = 6, edge_width = 0.2, label_size = 0.7)

#compile results
history <- data.frame(
  density = results_raisins$density,
  loss = results_raisins$loss,
  accuracy = val_raisins$accuracy_full_model,
  sparse_accuracy = val_raisins$accuracy_sparse,
  density_active_path = val_raisins$density_active_path,
  initialization = inclusion_inits
)

#save results
saveRDS(
  history,
  file = file.path(
    "tests_current",
    "results",
    paste0("raisin_", inclusion_inits, ".rds")
  )
)






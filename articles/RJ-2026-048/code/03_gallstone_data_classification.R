#Tutorial 3: Real data classification experiment

#check if the package is already installed.
if (!requireNamespace("LBBNN", quietly = TRUE)) {
  install.packages("LBBNN")
}
library(LBBNN)

seed <- 42
torch::torch_manual_seed(seed)
loaders_gs <- get_dataloaders(gallstone_dataset, train_proportion = 0.70,
                              train_batch_size = 223, test_batch_size = 96,
                              standardize = TRUE, seed = seed)
train_loader_gs <- loaders_gs$train_loader
test_loader_gs <- loaders_gs$test_loader

#this paper reports approx 85% accuracy using gradient boosting
#https://pmc.ncbi.nlm.nih.gov/articles/PMC11309733/#T2


problem <- "binary classification"
sizes <- c(40, 3, 3, 1)
inclusion_priors <- c(0.5, 0.5, 0.5) #one prior probability per weight matrix.
stds <- c(1, 1, 1) #prior standard deviation for each layer.


inclusion_inits <- 'polarized_dense'
device <- "cpu"


model_gs <- lbbnn_net(problem_type = problem, sizes = sizes,
                      prior = inclusion_priors,
                      inclusion_inits = inclusion_inits,
                      input_skip = TRUE, std = stds, flow = TRUE,
                      dims = c(10, 10 ,10, 10), device = device)


results_gs <- train_lbbnn(epochs = 1000, LBBNN = model_gs,
                          lr = 0.005, train_dl = train_loader_gs,
                          device = device, scheduler = "step",
                          sch_step_size = 1000)

validate_lbbnn(LBBNN = model_gs, num_samples = 100,
               test_dl = test_loader_gs, device)

x_gs <- train_loader_gs$dataset$tensors[[1]] #grab the dataset
y_gs <- train_loader_gs$dataset$tensors[[2]]
ind <- 42
data <- x_gs[ind, ] #plot this specific data-point
output <- y_gs[ind]
print(output$item())

plot(model_gs, type = "local", data = data)
plot(model_gs, type = "global", vertex_size = 4,
     edge_width = 0.1, label_size = 0.2)
summary(model_gs)
coef(model_gs, train_loader_gs)
predictions_gs <- predict(model_gs, newdata = test_loader_gs,
                          draws = 100, mpm = TRUE)

dim(predictions_gs)
print(predictions_gs)
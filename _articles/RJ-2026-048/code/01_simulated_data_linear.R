### Tutorial 1: Simulation study with linear effects

#check if the package is already installed.
if (!requireNamespace("LBBNN", quietly = TRUE)) {
  install.packages("LBBNN")
}
library(LBBNN)

i <- 1000
j <- 15

set.seed(42)
torch::torch_manual_seed(42)
#generate some data
X <- matrix(rnorm(i * j, mean = 0, sd = 1), ncol = j)
#make some X relevant for prediction
y_base <- c()
y_base <-  0.6 * X[, 1] - 0.4 * X[, 2] + 0.5 * X[, 3] + rnorm(n = i, sd = 0.1)
sim_data <- as.data.frame(X)
sim_data <- cbind(sim_data, y_base)

loaders <- get_dataloaders(sim_data, train_proportion = 0.9,
                           train_batch_size = 450, test_batch_size = 100,
                           standardize = FALSE)
train_loader <- loaders$train_loader
test_loader  <- loaders$test_loader

problem <- "regression"
sizes <- c(j, 5, 5, 1) # 2 hidden layers, 5 neurons in each
incl_priors <- c(0.5, 0.5, 0.5) #prior inclusion probability
stds <- c(1, 1, 1) #prior for the standard deviation of the weights
incl_inits <- matrix(rep(c(-10, 10), 3), nrow = 2, ncol = 3) #inclusion inits
device <- "cpu" #can also be 'gpu' or 'mps'

model_linear <- lbbnn_net(problem_type = problem, sizes = sizes,
                              prior = incl_priors, inclusion_inits = incl_inits,
                              std = stds, input_skip = TRUE, flow = FALSE,
                              num_transforms = 2, dims = c(10, 10, 10),
                              raw_output = FALSE, custom_act = NULL,
                              link = NULL, nll = NULL,
                              bias_inclusion_prob = FALSE, device = device)

train_lbbnn(epochs = 300, LBBNN = model_linear,
            lr = 0.05, train_dl = train_loader, device = device)
validate_lbbnn(LBBNN = model_linear, num_samples = 10, test_dl = test_loader,
              device = device)

summary(model_linear)

coef(model_linear, dataset = train_loader, inds = NULL,
     output_neuron = 1, num_data = 5, num_samples = 10)

x <- train_loader$dataset$tensors[[1]] #grab the dataset
y <- train_loader$dataset$tensors[[2]]
ind <- 42
data <- x[42, ] #plot this specific data-point
output <- y[ind]
print(output$item())
plot(model_linear, type = "local", data = data)
plot(model_linear, type = "global", vertex_size = 7,
     edge_width = 0.4, label_size = 0.4)

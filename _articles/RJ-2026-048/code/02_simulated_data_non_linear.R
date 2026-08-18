### Tutorial 2: Simulation study with non-linear effects

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
X_nl <- matrix(runif(i * j, 0, 0.5), ncol = j)

#make some X relevant for prediction
y_nl <- (- 3 +  0.1 * log(abs(X_nl[, 1])) + 3 * cos(X_nl[, 2]) 
         + 2 * X_nl[, 3] * X_nl[, 4] + X_nl[, 5] - 
           X_nl[, 6] ** 2 + rnorm(i, sd = 0.1))
hist(y_nl)
y <- c()
# change y to 0 and 1
y[y_nl > median(y_nl)] <- 1
y[y_nl <= median(y_nl)] <- 0

sim_data_nl <- as.data.frame(X_nl)
sim_data_nl <- cbind(sim_data_nl, y)

loaders_nl <- get_dataloaders(sim_data_nl, train_proportion = 0.9,
                              train_batch_size = 450, test_batch_size = 100,
                              standardize = FALSE)
train_loader_nl <- loaders_nl$train_loader
test_loader_nl  <- loaders_nl$test_loader

problem <- "binary classification"
sizes <- c(j, 5, 5, 1) # 2 hidden layers, 5 neurons in each
incl_priors <- c(0.5, 0.5, 0.5) #prior inclusion probs for each weight matrix
stds <- c(1, 1, 1) #prior distribution for the standard deviation of the weights
incl_inits <- matrix(rep(c(-10, 10), 3), nrow = 2, ncol = 3) #initializations for inclusion params
device <- "cpu" #can also be 'gpu' or 'mps'


model_nl <- lbbnn_net(problem_type = problem, sizes = sizes,
                      prior = incl_priors,
                      inclusion_inits = incl_inits, input_skip = TRUE,
                      std = stds, flow = TRUE, dims = c(10, 10, 10),
                      device = device, bias_inclusion_prob = FALSE)

train_lbbnn(epochs = 300, LBBNN = model_nl,
            lr = 0.01, train_dl = train_loader_nl, device = device)

validate_lbbnn(LBBNN = model_nl, num_samples = 100, test_dl = test_loader_nl,
               device = device)

plot(model_nl, type = "global", vertex_size = 9,
     edge_width = 0.4, label_size = 0.4)
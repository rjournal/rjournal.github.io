### Tutorial 4: extending the LBBNN to a convolutional architecture

#check if the package is already installed.
if (!requireNamespace("LBBNN", quietly = TRUE)) {
  install.packages("LBBNN")
}
library(LBBNN)
library(torch)  # initialize torch and its backend

#install torchvision in order to download KMNIST dataset
if(!requireNamespace("torchvision"))
  install.packages("torchvision")

torch::torch_manual_seed(42)
dir <- "./dataset/kmnist"

kmnist_transform <- function(x) {
  d <- dim(x)
  if (length(d) == 3 && d[3] > 1 && d[1] == d[2]) {#if shape [28,28,batch] as on windows and linux(?)
    x <- torchvision::transform_to_tensor(x) #now shape should be [batch, 28,28]
    x <- x$unsqueeze(2) #add the channel dimension - > [batch,1,28,28]
  }
  else{ #on mac, everything is fine and easy
    x <- torchvision::transform_to_tensor(x)
  }
  return(x)
}


train_ds <- torchvision::kmnist_dataset(
  dir,
  download = TRUE,
  transform = kmnist_transform)

test_ds <- torchvision::kmnist_dataset(
  dir,
  train = FALSE,
  transform = kmnist_transform)

train_loader_kmnist <- torch::dataloader(train_ds, batch_size = 100, shuffle = TRUE)
test_loader_kmnist <- torch::dataloader(test_ds, batch_size = 100)





### create the convolutional network for MNIST

device <- "cpu"
conv_layer_1 <- lbbnn_conv2d(in_channels = 1, out_channels = 32, kernel_size = 5,
                             prior_inclusion = 0.5, standard_prior = 1,
                             density_init = c(-10, 10), num_transforms = 2,
                             flow = FALSE, hidden_dims = c(200, 200),
                             device = device)
conv_layer_2 <- lbbnn_conv2d(in_channels = 32, out_channels = 64, kernel_size = 5,
                             prior_inclusion = 0.5, standard_prior = 1,
                             density_init = c(-10, 15), num_transforms = 2,
                             flow = FALSE, hidden_dims = c(200, 200),
                             device = device)

linear_layer_1 <- lbbnn_linear(in_features = 1024, out_features = 300,
                               prior_inclusion = 0.5, standard_prior = 1,
                               density_init = c(-10, 10), num_transforms = 2,
                               flow = FALSE, hidden_dims = c(200, 200), device = device,
                               bias_inclusion_prob = FALSE, conv_net = TRUE)

linear_layer_2 <- lbbnn_linear(in_features = 300, out_features = 10,
                               prior_inclusion = 0.5, standard_prior = 1,
                               density_init = c(-5, 15),num_transforms = 2,
                               flow = FALSE, hidden_dims = c(200, 200), device = device,
                               bias_inclusion_prob = FALSE, conv_net = TRUE)

LBBNN_ConvNet <- torch::nn_module(
  "LBBNN_ConvNet",
  
  initialize = function(conv1, conv2, fc1 ,fc2 ,device = device) {
    self$problem_type <- "multiclass classification"
    self$input_skip <- FALSE
    self$conv1 <- conv1
    self$conv2 <- conv2
    self$fc1 <- fc1
    self$fc2 <- fc2
    
    
    self$pool <- torch::nn_max_pool2d(2)
    self$act <- torch::nn_leaky_relu()
    self$out <- torch::nn_log_softmax(dim = 2)
    self$pout <- torch::nn_softmax(dim = 2)
    self$loss_fn <- torch::nn_nll_loss(reduction = "sum")
  },
  
  forward = function(x, MPM = FALSE, predict = FALSE) {
    x = self$act(self$conv1(x, MPM))
    x = self$pool(x)
    x = self$act(self$conv2(x, MPM))
    x = self$pool(x)
    x = torch::torch_flatten(x,start_dim = 2)
    x = self$act(self$fc1(x, MPM))
    if(!predict)
      x = self$out(self$fc2(x ,MPM))
    else
      x = self$pout(self$fc2(x ,MPM))
  },
  kl_div = function(){
    kl <- self$conv1$kl_div() + self$conv2$kl_div() +
      self$fc1$kl_div() + self$fc2$kl_div()
    return(kl)
  },
  density = function(){
    alphas <- NULL
    l1 <- self$conv1$lambda_l$clone()$detach()
    l2 <- self$conv1$lambda_l$clone()$detach()
    l3 <- self$fc1$lambda_l$clone()$detach()
    l4 <- self$fc2$lambda_l$clone()$detach()
    alphas <- c(as.numeric(torch::torch_sigmoid(l1)),
                as.numeric(torch::torch_sigmoid(l2)),
                as.numeric(torch::torch_sigmoid(l3)), 
                as.numeric(torch::torch_sigmoid(l4)))
    return(mean(alphas > 0.5))
  },
  compute_paths = function(){
    NULL
  },
  density_active_path = function(){
    NA
  }
)

model_kmnist <- LBBNN_ConvNet(conv_layer_1, conv_layer_2, linear_layer_1,
                       linear_layer_2, device)
model_kmnist$to(device = device)


num_eps <- 20
train_lbbnn(epochs = num_eps, LBBNN = model_kmnist, lr = 0.001, 
            train_dl = train_loader_kmnist, device = device)


validate_lbbnn(model_kmnist, num_samples = 10, test_dl = test_loader_kmnist, 
               device = device)

print(model_kmnist)


draws <- 20 # how many samples from posterior to use
out_dim <- 10 # dimensionality of the output
mpm <- TRUE # if to use the MPM
model_kmnist$eval() # to avoid gradient computations
predictions_kmnist <- NULL
torch::with_no_grad({ 
  coro::loop(for (b in test_loader_kmnist)# go through all data
  { 
    outputs <- torch::torch_zeros(draws,dim(b[[1]])[1],out_dim)$to(device=device)
    for(i in 1:draws)# go through all draws 
    {
      data <- b[[1]]$to(device = device)
      outputs[i]<- model_kmnist(data, MPM = mpm, predict = TRUE)
    }
    predictions_kmnist <- torch::torch_cat(c(predictions_kmnist, outputs), dim = 2) #combine all
    
  })  
})

dim(predictions_kmnist)

idx1 <- min(255, dim(predictions_kmnist)[2])
print(torch::torch_round(predictions_kmnist[1:5, idx1, ], 4))

idx2 <- min(258, dim(predictions_kmnist)[2])
print(torch::torch_round(predictions_kmnist[1:5, idx2, ], 4))






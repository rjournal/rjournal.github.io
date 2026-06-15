
## INSTALL PACKAGES
install.packages("priorityelasticnet")
install.packages("survival")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("glmSparseNet")

library(priorityelasticnet)
library(survival)
library(glmSparseNet)



## -----------------------------------------------------------------------------
# Priority Elastic Net for a Gaussian family
## -----------------------------------------------------------------------------

# Simulate some data
set.seed(123)
n <- 100  # Number of observations
p <- 500   # Number of predictors

## -----------------------------------------------------------------------------
# Create a matrix of predictors
X <- matrix(rnorm(n * p), n, p)

## -----------------------------------------------------------------------------
# Generate a response vector based on a linear combination of some predictors
beta <- rnorm(10)  # Coefficients for the first 10 predictors
Y <- X[, 1:10] %*% beta + rnorm(n)  # Linear model with added noise

## -----------------------------------------------------------------------------
# Define predictor blocks
blocks <- list(
  block1 = 1:10,    # First block includes the first 10 predictors
  block2 = 11:30,   # Second block includes the next 20 predictors
  block3 = 31:500   # Third block includes the last predictors
  )
## -----------------------------------------------------------------------------
# Fit a priorityelasticnet model
fit_gaussian <- priorityelasticnet(
  X = X, 
  Y = Y, 
  family = "gaussian", 
  blocks = blocks,
  block1.penalization = TRUE,
  standardize = TRUE,
  lambda.type = "lambda.min",
  max.coef = c(Inf, Inf, Inf),  
  nfolds = 10,
  type.measure = "mse",
  alpha = 0.6,
  cvoffset = TRUE,
  cvoffsetnfolds=10,
  adaptive = FALSE,
  initial_global_weight = FALSE)

## -----------------------------------------------------------------------------

# Fit the model using cvm_priorityelasticnet
fit_cvm <- cvm_priorityelasticnet(X = X,
                        Y = Y,
                        weights = rep(1, n),
                        foldid = sample(rep(1:10, length.out = n)),
                        family = "gaussian",
                        type.measure = "mse",
                        blocks.list = list(
                          list(bp1 = 1:10, bp2 = 11:30, bp3 = 31:500),
                          list(bp1 = 1:10, bp2 = 31:500, bp3 = 11:30)),
                        max.coef.list <- list(
                          c(Inf, Inf, Inf),
                          c(Inf, 100, Inf)),
                        block1.penalization = TRUE,
                        lambda.type = "lambda.min",
                        standardize = TRUE,
                        nfolds = 10,
                        cvoffset = TRUE,
                        cvoffsetnfolds = 10,
                        alpha = 0.6,
                        adaptive = FALSE,
                        initial_global_weight = FALSE)
## -----------------------------------------------------------------------------
fit_cvm$best.blocks
fit_cvm$best.blocks.indices
fit_cvm$best.max.coef

## -----------------------------------------------------------------------------
# Handling missing data 
missing.control(
  handle.missingdata = c("none", "ignore", "impute.offset"),
  offset.firstblock = c("zero", "intercept"),
  impute.offset.cases = c("complete.cases", "available.cases"),
  nfolds.imputation = 10,
  lambda.imputation = c("lambda.min", "lambda.1se"),
  perc.comp.cases.warning = 0.3,
  threshold.available.cases = 30,
  select.available.cases = c("maximise.blocks", "max"))

## -----------------------------------------------------------------------------

mcontrol <-missing.control(
     handle.missingdata = "impute.offset", nfolds.imputation = 10)

fit_missing <- priorityelasticnet(
  X,
  Y,
  family = "gaussian",
  type.measure = "mse",
  blocks = blocks,
  mcontrol = mcontrol)



## -----------------------------------------------------------------------------
# Priority Elastic Net for a Multinomial Logistic Regression
## -----------------------------------------------------------------------------
set.seed(123)
# Number of observations and predictors
n <- 100  # Number of observations
p <- 500   # Number of predictors
k <- 3    # Number of classes

# Simulate a matrix of predictors
x <- matrix(rnorm(n * p), n, p)

# Simulate a response vector with three classes
y <- factor(sample(1:k, n, replace = TRUE))

blocks <- list(
  block1 = 1:10,   # First block with predictors 1 to 10
  block2 = 11:30,  # Second block with predictors 11 to 30
  block3 = 31:500   # Third block with predictors 31 to 500
)

## -----------------------------------------------------------------------------
# Fit the Priority Elastic net model
fit_multinom <- priorityelasticnet(
  X = x, 
  Y = y, 
  family = "multinomial", 
  alpha = 0.5, 
  type.measure = "class", 
  blocks = blocks,
  block1.penalization = TRUE,
  lambda.type = "lambda.min",
  standardize = TRUE,
  nfolds = 10,
  adaptive = FALSE,
  initial_global_weight = FALSE)

## -----------------------------------------------------------------------------
# Priority Elastic Net for a Cox Regression
## -----------------------------------------------------------------------------


# Simulate Data
set.seed(123)
n <- 200 
p <- 600  
nzc <- trunc(p / 10)

# Predictors
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(nzc)
fx <- x[, seq(nzc)] %*% beta / 3

# Hazard function
hx <- exp(fx)

# Time
ty <- rexp(n, hx)

tcens <- rbinom(n = n, prob = .3, size = 1)

# Response variable
y <- Surv(ty, 1 - tcens)

# Split into training and test sets

train_index <- sample(1:n, size = n * 0.7)
x_train <- x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]



## -----------------------------------------------------------------------------
blocks <- list(
     bp1 = 1:20,    # First block with predictors 1 to 20
     bp2 = 21:300,  # Second block with predictors 21 to 300
     bp3 = 301:600  # Third block with predictors 301 to 600
   )

## -----------------------------------------------------------------------------
# Fit the Priority Elastic net model (train set) 

fit_cox <- priorityelasticnet(
  x_train, 
  y_train, 
  family = "cox", 
  alpha = 0.5, 
  type.measure = "deviance", 
  blocks = blocks,
  block1.penalization = TRUE,
  lambda.type = "lambda.min",
  standardize = TRUE,
  nfolds = 10,
  adaptive = FALSE,
  initial_global_weight = FALSE,
  cvoffset = TRUE)

## -----------------------------------------------------------------------------
# Extract coefficients from the fitted Cox model
y_train_df <- data.frame(
  time = ty[train_index],          # Survival times
  status = 1 - tcens[train_index]  # Event indicator
)


## -----------------------------------------------------------------------------
# Group patients and plot Kaplan-Meier survival curves (train set)
separate2GroupsCox(
  chosen.btas = fit_cox$coefficients,# Coefficients from the model
  xdata = x_train,                   # Predictor matrix (x_train)
  ydata = y_train_df,                # Survival data (y_train as Surv object)
  probs = c(0.4, 0.6),               # Median split (adjust if necessary)
  no.plot = FALSE,                   # Plot the Kaplan-Meier curve
  plot.title = "Training Set Survival Curves",  # Plot title
  xlim = NULL, # Automatic x-axis limits 
  ylim = NULL, # Automatic y-axis limits 
  expand.yzero = FALSE, # Don't force y-axis to start at zero 
  legend.outside = FALSE # Keep legend inside the plot 
)

## -----------------------------------------------------------------------------
# Prediction on the test set
pred_test <- predict(fit_cox, newx = x_test, type = "link")

y_test_df <- data.frame(
  time = ty[-train_index],
  status = 1 - tcens[-train_index]
)

# Group patients and plot Kaplan-Meier survival curves (train set)
separate2GroupsCox(
  chosen.btas = fit_cox$coefficients,# Coefficients from the model
  xdata = x_test,                   # Predictor matrix (x_train)
  ydata = y_test_df,                # Survival data (y_train as Surv object)
  probs = c(0.4, 0.6),               # Median split (adjust if necessary)
  no.plot = FALSE,                   # Plot the Kaplan-Meier curve
  plot.title = "Test Set Survival Curves",  # Plot title
  xlim = NULL, # Automatic x-axis limits 
  ylim = NULL, # Automatic y-axis limits 
  expand.yzero = FALSE, # Don't force y-axis to start at zero 
  legend.outside = FALSE # Keep legend inside the plot 
)


## -----------------------------------------------------------------------------
#Priority-Adaptive Elastic Net
# Fit the Priority Elastic net (PEN)  model (with adaptive equals to true)

## -----------------------------------------------------------------------------
# Gaussian model
## -----------------------------------------------------------------------------

# Set seed for reproducibility
set.seed(123)


n <- 100  # Number of observations
p <- 500   # Number of predictors

# Create a matrix of predictors
X <- matrix(rnorm(n * p), n, p)

# Generate a response vector based on a linear combination of some predictors
beta <- rnorm(10)  # Coefficients for the first 10 predictors
Y <- X[, 1:10] %*% beta + rnorm(n)  # Linear model with added noise

blocks <- list(
  block1 = 1:10,   # First block with predictors 1 to 10
  block2 = 11:30,  # Second block with predictors 11 to 30
  block3 = 31:500   # Third block with the remaining predictors
)

## -----------------------------------------------------------------------------
# Fit PEN Adaptive (gaussian model)
fit_gaussian <- priorityelasticnet(
  X = X, 
  Y = Y, 
  family = "gaussian", 
  blocks = blocks,
  block1.penalization = TRUE,
  standardize = TRUE,
  lambda.type = "lambda.min",
  max.coef = c(Inf, Inf, Inf),  
  nfolds = 10,
  adaptive = TRUE,
  initial_global_weight = FALSE,
  type.measure = "mse",
  alpha = 0.6,
  cvoffset = TRUE,
  cvoffsetnfolds=10)


## -----------------------------------------------------------------------------
cvm_priorityelasticnet(
  X = matrix(rnorm(100 * 500), 100, 500), 
  Y = rnorm(100),
  weights = rep(1, n),
  foldid = sample(rep(1:10, length.out = n)),
  family = "gaussian", 
  alpha = 0.6,
  type.measure = "mse", 
  lambda.type = "lambda.min", 
  nfolds = 10,
  block1.penalization = TRUE,
  adaptive = TRUE,              # Enables adaptive weight calculation
  initial_global_weight = FALSE, # Skips global weight calculation
  standardize = TRUE, 
  cvoffset = TRUE,  
  cvoffsetnfolds = 10,
  blocks.list = list(
    list(bp1 = 1:10, bp2 = 11:30, bp3 = 31:500),
    list(bp1 = 1:10, bp2 = 31:500, bp3 = 11:30) ),
  max.coef.list = list( c(Inf, Inf, Inf),  c(Inf, 100, Inf)))                     


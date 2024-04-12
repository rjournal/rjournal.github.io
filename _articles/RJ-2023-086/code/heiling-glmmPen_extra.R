
# Libraries
library(glmmPen)
library(stringr)

# Example of running glmm() function on basal dataset - fit a single model with
#   no variable selection

# basal data from glmmPen package
data("basal")

# Extract response
y = basal$y
# Select a sampling of 10 TSP covariates from the total 50 covariates
set.seed(1618)
idx = sample(1:50, size = 10, replace = FALSE)
# Selected column index values:
(idx = idx[order(idx)])
X = basal$X[,idx]
# Selected predictors:
colnames(X)
group = basal$group
# Levels of the grouping variable:
levels(group)

start_glmm = proc.time()
set.seed(1618)
fit_glmm = glmm(formula = y ~ X + (X | group), 
                family = "binomial", covar = "independent", 
                optim_options = optimControl())
end_glmm = proc.time()
end_glmm - start_glmm


sessionInfo()
################################################################################################

################################################################################################ 
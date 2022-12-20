# Reproducible examples of paper "dbcsp: User-friendly R package for Distance-Based Common Spacial Patterns"
# install.packages("dbcsp")
library(dbcsp)
library(caret)

### Basic/classic analysis
x1 <- AR.data$come
x2 <- AR.data$five
mydbcsp <- new('dbcsp', X1=x1, X2=x2, q=15, labels=c("C1", "C2"))
summary(mydbcsp)

mydbcsp <- new('dbcsp', X1=x1, X2=x2, q=3, labels=c("C1", "C2"), training=TRUE, fold = 10, seed = 19)
summary(mydbcsp)

# Accuracy in each fold
mydbcsp@out$folds_acc

# Intances belonging to each fold
mydbcsp@out$used_folds

### Basic/classic analysis selecting the value of $q$
mydbcsp <- new('dbcsp', X1=x1, X2=x2, labels=c("C1", "C2"))
selectDim <- selectQ(mydbcsp, seed=30, CV=TRUE, fold = 10)
selectDim

plot(mydbcsp, index=1, class=1, vectors=1:2)

boxplot(mydbcsp, vectors=1:2)

### Basic/classic analysis new unit classification
mydbcsp <- train(mydbcsp, selected_q=2, verbose=FALSE)
xtest <- x1[1:5]
outpred <- predict(mydbcsp, X_test=xtest)

outpred <- predict(mydbcsp, X_test=xtest, true_targets= rep("C1", 5))


# Distance DTW
mydbcsp.dtw <- new('dbcsp', X1=x1, X2=x2, labels=c("C1", "C2"), type="dtw")

# Custom distance




### Extending the example
# Establish training and test data
n1 <- length(x1)
trainind1 <- rep(TRUE, n1)
n2 <- length(x2)
trainind2 <- rep(TRUE, n2)
set.seed(19)
trainind1[sample(1:n1, 10, replace=FALSE)] <- FALSE
trainind2[sample(1:n2, 10, replace=FALSE)] <- FALSE
x1train <- x1[trainind1]
x2train <- x2[trainind2]

# Extract the interesting directions
vectors <- new('dbcsp', X1=x1train, X2=x2train, q=5, labels=c("C1", "C2"))@out$vectors

# Function to calculate the desired characteristics from signals
calc_info <- function(proj_X, type){
  values <- switch(type,
                   'var' = values <-  plyr::laply(proj_X, function(x){apply(x,1,var)}),
                   'max' = values <-  plyr::laply(proj_X, function(x){apply(x,1,max)}),
                   'min' = values <- plyr::laply(proj_X, function(x){apply(x,1,min)}),
                   'iqr' = values <- plyr::laply(proj_X, function(x){
                     apply(x,1,function(y){
                       q <- quantile(y, probs = c(0.25, 0.75))
                       q[2] -q[1]
                     })
                   })
  )
  return(values)
}

# Project units of class C1 and
projected_x1 <- plyr::llply(x1, function(x,W) t(W)%*%x, W=vectors)

# Extract the characteristics
logvar_x1 <- log(calc_info(projected_x1,'var'))
iqr_x1 <- calc_info(projected_x1,'iqr')
new_x1 <- data.frame(logvar=logvar_x1, iqr=iqr_x1)

# Similarly for units of class C2
projected_x2 <- plyr::llply(x2, function(x,W) t(W)%*%x, W=vectors)
logvar_x2 <- log(calc_info(projected_x2,'var'))
iqr_x2 <- calc_info(projected_x2,'iqr')
new_x2 <- data.frame(logvar=logvar_x2, iqr=iqr_x2)


# Create dataset for classification
labels <- rep(c('C1','C2'), times=c(n1,n2))
new_data <- rbind(new_x1,new_x2)
new_data$label <- factor(labels)
new_data_train <- new_data[c(trainind1, trainind2), ]
new_data_test <- new_data[!c(trainind1, trainind2), ]

# Random forest
trControl <- caret::trainControl(method = "none")
rf_default <- caret::train(label~.,
                           data = new_data_train,
                           method = "rf",
                           metric = "Accuracy",
                           trControl = trControl)
rf_default

# K-NN
knn_default <- caret::train(label~.,
                            data = new_data_train,
                            method = "knn",
                            metric = "Accuracy",
                            trControl = trControl)
knn_default

# Predictions and accuracies on test data
# Based on random forest classifier
pred_labels <- predict(rf_default, new_data_test)
predictions_rf <- caret::confusionMatrix(table(pred_labels,new_data_test$label))
predictions_rf

# Based on knn classifier
pred_labels <- predict(knn_default, new_data_test)
predictions_knn <- caret::confusionMatrix(table(pred_labels,new_data_test$label))
predictions_knn

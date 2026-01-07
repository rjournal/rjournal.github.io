## ---- Using the ASML package
## -------------------------------------------------------------
set.seed(1234)
library(ASML)
data(branching)
features<-branching$x
KPI<-branching$y
lab_rules<-c("max", "sum", "dual", "range", "eig-VI", "eig-CMI")

## ---- Data partition
data <- partition_and_normalize(features, KPI, family_column = 1, split_by_family =TRUE, better_smaller = TRUE)
names(data)

## ---- Boxplots of instance-normalized KPI for each algorithm across instances in the train setlots of instance-normalized KPI for each algorithm across instances in the train set.", fig.alt = "Boxplots of instance-normalized KPI for each algorithm across instances in the train set."----
boxplots(data,test=FALSE,by_families=FALSE,labels=lab_rules)

## ---- Ranking of algorithms based on the instance-normalized KPI for the training sample, categorized by family. 
ranking(data, test=FALSE, by_families = TRUE, labels=lab_rules)

## ---- Preprocess caret
preProcValues <- caret::preProcess(data$x.train, method = "YeoJohnson")
data$x.train <- predict(preProcValues, data$x.train)
data$x.test <- predict(preProcValues, data$x.test)

## ---- AMSL train
library(quantregForest)
tune_grid <- expand.grid(mtry = 10) 
training <- AStrain(data, method = "qrf", tuneGrid = tune_grid)

## ---- AMSL prediction
predict_test <- ASpredict(training, newdata = data$x.test)

## ---- AMSL summary tables 
KPI_table(data, predictions =predict_test)
KPI_summary_table(data, predictions =predict_test)

## ---- Boxplots of instance-normalized KPI for each algorithm, including the ML algorithm, across instances in the test set.
boxplots(data, predictions = predict_test, labels=c(lab_rules,"ML"))

## ---- Boxplots of instance-normalized KPI for each algorithm, including the ML algorithm, across instances in the test set, categorized by family.
boxplots(data, predictions = predict_test,labels=c(lab_rules,"ML"),by_families=TRUE)

## ---- Ranking of algorithms, including the ML algorithm, based on the instance-normalized KPI for the test sample, categorized by family. 
ranking(data, predictions = predict_test,labels=c("ML",lab_rules),by_families=TRUE)

## ---- Comparison of the best-performing rules
figure_comparison(data, predictions = predict_test,by_families=FALSE,labels=lab_rules)

## ---- Custom user-defined methods
qrf_q_predict <- function(modelFit, newdata, what = 0.5, submodels = NULL) {
  out <- predict(modelFit$finalModel, newdata, what = what)
  if (is.matrix(out))
    out <- out[, 1]
  out
}
predict_test_Q1 <- ASpredict(training, newdata = data$x.test, f="qrf_q_predict", what=0.25)
KPI_summary_table(data, predictions=predict_test_Q1)

## ---- Model interpretability with DALEX explainers
explainers_qrf <- ASexplainer(training, data = data$x.test, y = data$y.test, labels = lab_rules)
mp_qrf <- lapply(explainers_qrf, DALEX::model_performance)
do.call(plot, unname(mp_qrf))
vi_qrf <- lapply(explainers_qrf, DALEX::model_parts)
do.call(plot, c(unname(vi_qrf), list(max_vars = 5)))
pdp_qrf <- lapply(explainers_qrf, DALEX::model_profile, variable = "degree", type = "partial")
do.call(plot, unname(pdp_qrf))


## ---- Example on a larger dataset
## -------------------------------------------------------------
set.seed(1234)
data(SpMVformat)
features <- SpMVformat$x
KPI <- SpMVformat$y
data <- partition_and_normalize(features, KPI, better_smaller = FALSE)
preProcValues <- caret::preProcess(data$x.train, method = "YeoJohnson")
data$x.train <- predict(preProcValues, data$x.train)
data$x.test <- predict(preProcValues, data$x.test)
training <- AStrain(data, method = "nnet", parallel = TRUE)
pred <- ASpredict(training, newdata = data$x.test)
ranking(data, predictions = pred)


## ---- Using ASML for algorithm selection on ASlib scenarios
## -------------------------------------------------------------
set.seed(1234)
library(tidyverse)
library(rvest)
scen <- "CPMP-2015"
url <- paste0("https://coseal.github.io/aslib-r/scenario-pages/", scen, "/data_files")
page <- read_html(paste0(url, ".html"))
file_links <- page %>%  html_nodes("a") %>%  html_attr("href")

# Create directory for downloaded files
dir_data <- paste0(scen, "_data")
dir.create(dir_data, showWarnings = FALSE)

# Download files
for (link in file_links) {
  full_link <- ifelse(grepl("^http", link), link, paste0(url, "/", link))
  file_name <- basename(link)
  dest_file <- file.path(dir_data, file_name)
  if (!is.na(full_link)) {
    download.file(full_link, dest_file, mode = "wb", quiet = TRUE)
  }
}


## ---- Data preparation with aslib
library(aslib)
ASScen<-aslib::parseASScenario(dir_data)
llamaScen <- aslib::convertToLlama(ASScen)
folds<- llama::cvFolds(llamaScen)


## ---- key performance indicator
KPI <- folds$data[,folds$performance] 
features<-folds$data[,folds$features]
cutoff<-ASScen$desc$algorithm_cutoff_time
is.timeout <- ASScen$algo.runstatus[,-c(1,2)]!="ok"
KPI_pen <- KPI*ifelse(is.timeout,10,1) 
nins<-length(getInstanceNames(ASScen)) 
ID<-1:nins


## ---- Quantile random forest using ASML on instance-normalized KPI
data <- partition_and_normalize(x=features,y=KPI,x.test=features,y.test=KPI,better_smaller = TRUE)
train_control <- caret::trainControl(index=folds$train, savePredictions = 'final') 
training <- AStrain(data, method = "qrf", trControl = train_control)


## ----echo = TRUE, tidy=TRUE, eval=TRUE----------------------------------------
pred_list <- lapply(training, function(model) {
  model$pred %>%    arrange(rowIndex) %>%     pull(pred)             
})

pred <- do.call(cbind, pred_list)
alg_sel<-apply(pred,1,which.max)

succ = mean(!is.timeout[cbind(ID, alg_sel)])
par10 = mean(KPI_pen[cbind(ID, alg_sel)])
mcp = mean(KPI[cbind(ID, alg_sel)] - apply(KPI, 1, min))

results_table <- data.frame(Model="ASML qrf",succ=format(succ, nsmall = 3, digits = 3), par10=format(par10, nsmall = 3, digits = 3), mcp=format(mcp, nsmall = 3, digits = 3))

results_table

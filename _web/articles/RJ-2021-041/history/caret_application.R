library(bestNormalize)
library(caret)
library(recipes)
library(rsample)

if(!file.exists("caret_application_results.RData")) {
   
  set.seed(321)
  
  message("Fitting models using caret (may take some time)")
  
  #### First analysis ####
  
  df_split <- initial_split(autotrader, prop = .9)
  df_train <- training(df_split)
  df_test <- testing(df_split)
  
  rec <- recipe(price ~ Make + model +  mileage + Year, data = df_train) %>% 
    step_mutate(years_old = 2017 - Year) %>% 
    step_rm(Year) %>% 
    step_log(price) %>% 
    step_best_normalize(all_predictors(), -all_nominal()) %>% 
    step_other(all_nominal(), threshold = 10) %>% 
    step_dummy(all_nominal()) %>% 
    prep()
  
  trc <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
  
  fit1 <- train(price ~ ., bake(rec, NULL), method = 'glmnet', trControl = trc)
  fit2 <- train(price ~ ., bake(rec, NULL), method = 'earth', trControl = trc)
  fit3 <- train(price ~ ., bake(rec, NULL), method = 'rf', trControl = trc)

  r <- resamples(fits <- list(glmnet = fit1, earth = fit2, rf = fit3))
  summary(r)
  
  
  results <- lapply(fits, function(x) {
    p <- c(predict(x, newdata = bake(rec, df_test)))
    yardstick::metrics(data.frame(est = exp(p), truth = df_test$price), truth = truth, estimate = est)
  })
  
  save(rec, results, fits, r, file = "caret_application_results.RData")
} else
  load("caret_application_results.RData")


# ## Plots
# price.xx <- seq(min(autotrader$price), max(autotrader$price), length = 100)
# mileage.xx <- seq(min(autotrader$mileage), max(autotrader$mileage), length = 100)
# years.xx <- seq(min(autotrader$Year), max(autotrader$Year), length = 100)
# mods <- names(tail(sort(table(autotrader$model)), 6))
# maks <- sapply(strsplit(mods, " "), function(x) x[[1]])
# 
# # By years old
# newX = expand.grid(price = median(autotrader$price), Year = years.xx, mileage = median(autotrader$mileage), model = mods, stringsAsFactors = FALSE)
# newX$Make <- sapply(strsplit(newX$model, " "), function(x) x[[1]])
# 
# preds <- as.data.frame(exp(sapply(fits, predict, newdata= bake(rec, newX))))
# bind_cols(newX, preds) %>% 
#   pivot_longer(glmnet:rf, names_to = "Method", values_to = "Price") %>% 
#   ggplot(aes(y = Price, x = Year, color = Method)) +
#   geom_line()  + 
#   facet_wrap(vars(model), nrow = 3)
#   
# 
# 
# # By mileage
# newX = expand.grid(price = median(autotrader$price), Year = median(autotrader$Year), 
#                    mileage = mileage.xx, model = mods, status = "Used", stringsAsFactors = FALSE)
# newX$Make <- sapply(strsplit(newX$model, " "), function(x) x[[1]])
# 
# preds <- as.data.frame(exp(sapply(fits, predict, newdata= bake(rec, newX))))
# bind_cols(newX, preds) %>% 
#   pivot_longer(glmnet:rf, names_to = "Method", values_to = "Price") %>% 
#   ggplot(aes(y = Price, x = mileage, color = Method)) +
#   geom_line()  + 
#   facet_wrap(vars(model), nrow = 3)
# 
# 
# # Try interacting model with miles/year? 

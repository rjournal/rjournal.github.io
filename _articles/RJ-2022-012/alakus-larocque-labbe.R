## Before using the package for the first time, it should be installed.
## Install required packages
install.packages("RFpredInterval")
install.packages("AmesHousing")
install.packages("rockchalk")

## Load the packages
library("RFpredInterval")
library("AmesHousing")
library("rockchalk")

## R script of the manuscript examples
## Note: Running times may be long.

## Load AmesHousing data set
AmesHousing <- make_ordinal_ames()

## Data preprocessing
# remove observations with missing values
AmesHousing <- AmesHousing[complete.cases(AmesHousing), ]

# convert the response variable in thousands
AmesHousing$Sale_Price <- AmesHousing$Sale_Price/1000

# convert the ordered factors to numeric to preserve the ordering of the factors
ord_vars <- vapply(AmesHousing, is.ordered, logical(1))
nam_ord <- names(ord_vars)[ord_vars]
AmesHousing[, nam_ord] <- data.frame(lapply(AmesHousing[, nam_ord], as.numeric))

# group together levels with less than 30 observations
fac_vars <- vapply(AmesHousing, is.factor, logical(1))
AmesHousing[, fac_vars] <- data.frame(
  lapply(AmesHousing[, fac_vars],
         function(x, nmin) combineLevels(x, levs = names(table(x))[table(x)<nmin], 
                                         newLabel = c("combinedLevels")),
         nmin=30)
)

## Split the data into training and testing sets
set.seed(3456)
n <- nrow(AmesHousing)
trainindex <- sample(1:n, size = round(0.7*n), replace = FALSE)
traindata <- AmesHousing[trainindex, ]
testdata <- AmesHousing[-trainindex, ]

## Construct 95% PI with the PIBF method
out <- pibf(formula = Sale_Price ~ .,
            traindata = traindata, 
            testdata = testdata,
            alpha = 0.05,
            calibration = "cv", 
            numfolds = 5,
            coverage_range = c(0.945, 0.955),
            params_ranger = list(num.trees = 1000),
            oob = TRUE)

## Get the prediction intervals and bias-corrected point estimations for
## the observations in the testdata
out$pred_interval
out$test_pred
c(out$pred_interval$lower[10], out$test_pred[10], out$pred_interval$upper[10])

## Print the summary output
print(out)

## Construct 95% PI with the RFPI method with L1 splitting rule and 
## LM, Quant, and SPI PI methods
out2 <- rfpi(formula = Sale_Price ~ .,
             traindata = traindata, 
             testdata = testdata, 
             alpha = 0.05,
             calibration = TRUE, 
             split_rule = "l1", 
             pi_method = c("lm", "quant", "spi"),
             params_rfsrc = list(ntree = 1000),
             params_calib = list(range = c(0.945, 0.955)),
             oob = FALSE)

## Get the prediction intervals for the observations in the testdata
out2$lm_interval
out2$quant_interval
out2$spi_interval
c(out2$lm_interval$lower[10], out2$test_pred[10], out2$lm_interval$upper[10])

## Print the summary output
print(out2)

## Build 95% PIs with all 16 methods
out3 <- piall(formula = Sale_Price ~ .,
              traindata = traindata, 
              testdata = testdata, 
              alpha = 0.05,
              num.trees = 1000)

## Print the summary output
print(out3)

## Plot the constructed PIs for test_id = 15 (15th observation in the test set) 
## with all methods
plot(out3, test_id = 15)

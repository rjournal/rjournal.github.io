# Section 2
library(qCBA)

# Define cut points for quantization
temp_breaks <- seq(from = 15, to = 45, by = 5)
hum_breaks <- c(0, 40, 60, 80, 100)

data_discr <- arc::applyCuts(
  df = humtemp,
  cutp = list(temp_breaks, hum_breaks, NULL),
  infinite_bounds = TRUE,
  labels = TRUE
)
head(data_discr)

# Discover candidate class association rules (CARs)
txns <- as(data_discr, "transactions")
appearance <- list(rhs = c("Class=1", "Class=2", "Class=3", "Class=4"))
rules <- arules::apriori(
  data = txns,
  parameter = list(
    confidence = 0.5,
    support = 3 / nrow(data_discr),
    minlen = 1,
    maxlen = 3
  ),
  appearance = appearance
)
inspect(rules)

# Learn CBA classifier from the candidate CARs
classAtt <- "Class"
rmCBA <- cba_manual(
  datadf_raw = humtemp,
  rules = rules,
  txns = txns,
  rhs = appearance$rhs,
  classAtt = classAtt,
  cutp = list()
)
inspect(rmCBA@rules)

# Evaluate the CBA model
prediction_cba <- predict(rmCBA, data_discr)
acc_cba <- CBARuleModelAccuracy(
  prediction = prediction_cba,
  groundtruth = data_discr[[classAtt]]
)
prediction_cba

# Explain a prediction by finding the firing rule
ruleIDs <- predict(rmCBA, data_discr, outputFiringRuleIDs = TRUE)
inspect(rmCBA@rules[ruleIDs[1]])

# Post-process the model with QCBA
rmqCBA <- qcba(cbaRuleModel = rmCBA, datadf = humtemp)
rmqCBA@rules

# Evaluate the QCBA model
prediction <- predict(rmqCBA, humtemp)
acc <- CBARuleModelAccuracy(prediction, humtemp[[rmqCBA@classAtt]])
acc


# Section 4
library(arulesCBA) # version 1.2.7

# Prepare data: shuffle, split, and discretize
set.seed(12) # Chosen for demonstration purposes
allDataShuffled <- datasets::iris[sample(nrow(datasets::iris)), ]
trainFold <- allDataShuffled[1:100, ]
testFold <- allDataShuffled[101:nrow(datasets::iris), ]
classAtt <- "Species"

discrModel <- discrNumeric(df = trainFold, classAtt = classAtt)
train_disc <- as.data.frame(lapply(discrModel$Disc.data, as.factor))
cutPoints <- discrModel$cutp
test_disc <- applyCuts(
  testFold,
  cutPoints,
  infinite_bounds = TRUE,
  labels = TRUE
)
y_true <- testFold[[classAtt]]

# Learn a base CPAR model
rmBASE <- CPAR(train_disc, formula = as.formula(paste(classAtt, "~ .")))
predictionBASE <- predict(rmBASE, test_disc)
inspect(rmBASE$rules)
cat("Number of rules: ", length(rmBASE$rules))
cat("Total conditions: ", sum(rmBASE$rules@lhs@data))
cat("Accuracy on test data: ", mean(predictionBASE == y_true))

# Configure QCBA parameters, initially disabling optimizations
baseModel_arc <- arulesCBA2arcCBAModel(
  arulesCBAModel = rmBASE,
  cutPoints = cutPoints,
  rawDataset = trainFold,
  classAtt = classAtt
)
qcbaParams <- list(
  cbaRuleModel = baseModel_arc,
  datadf = trainFold,
  extend = "noExtend",
  attributePruning = FALSE,
  continuousPruning = FALSE,
  postpruning = "none",
  trim_literal_boundaries = FALSE,
  defaultRuleOverlapPruning = "noPruning",
  minImprovement = 0,
  minCondImprovement = 0
)

# Helper function to run QCBA and print a summary
qcba_with_summary <- function(params) {
  rmQCBA <- do.call(qcba, params)
  cat("Number of rules: ", nrow(rmQCBA@rules), " ")
  cat("Total conditions: ", sum(rmQCBA@rules$condition_count), " ")
  accuracy <- CBARuleModelAccuracy(predict(rmQCBA, testFold), testFold[[classAtt]])
  cat("Accuracy on test data: ", round(accuracy, 2))
  print(rmQCBA@rules)
}

# Run QCBA with only the mandatory refit step
qcba_with_summary(qcbaParams)

# Check for presence of specific values in the training data
any(trainFold$Petal.Length == 1.9)
any(trainFold$Petal.Length == 2.6)

# Enable boundary adjustments and attribute pruning
qcbaParams$attributePruning <- TRUE
qcbaParams$trim_literal_boundaries <- TRUE
qcbaParams$extend <- "numericOnly"
qcba_with_summary(qcbaParams)

# Enable post-pruning
qcbaParams$postpruning <- "cba"
qcba_with_summary(qcbaParams)

# Enable default rule pruning
qcbaParams$defaultRuleOverlapPruning <- "transactionBased"
qcba_with_summary(qcbaParams)


# Section 5
# With arc package
rmCBA <- cba(datadf = trainFold, classAtt = "Species")
rmqCBA <- qcba(cbaRuleModel = rmCBA, datadf = trainFold)

# With arulesCBA package
library(arulesCBA)
arulesCBAModel <- arulesCBA::CBA(Species ~ ., data = train_disc, supp = 0.1, conf = 0.9)
CBAmodel <- arulesCBA2arcCBAModel(arulesCBAModel, discrModel$cutp, iris, classAtt)
qCBAmodel <- qcba(cbaRuleModel = CBAmodel, datadf = iris)

# With rCBA package
# Note: This may produce a warning due to a compatibility issue between rCBA
# and newer versions of the arules package.
library(rCBA)
rCBAmodel <- rCBA::build(train_disc)
CBAmodel <- rcbaModel2CBARuleModel(rCBAmodel, discrModel$cutp, iris, "Species")
qCBAmodel <- qcba(CBAmodel, iris)

# Section 6

# Learn with default metaparameter values
stats <- benchmarkQCBA(train = trainFold, test = testFold, classAtt = classAtt)
print(stats)

# Calculate relative improvement
round((stats[, 6:10] / stats[, 1:5] - 1), 3)

# Run a more complex benchmark with custom parameters
output <- benchmarkQCBA(
  trainFold,
  testFold,
  classAtt,
  train_disc,
  test_disc,
  discrModel$cutp,
  CBA = list(support = 0.05, confidence = 0.5),
  algs = c("CPAR"),
  iterations = 10,
  return_models = TRUE,
  seed = 1
)

message("Evaluation statistics")
print(output$stats)
message("CPAR model")
inspect(output$CPAR[[1]])
message("QCBA model")
print(output$CPAR_QCBA[[1]])

# Plot figure 1 When run in R studio, the two plots will appear under the Plots tab.  THIS CODE IS INCLUDED FOR
# REPLICABILITY OF THE VISUALIZATION BUT WILL NOT APPEAR IN THE ARTICLE

attach(humtemp)
# custom discretization
data_raw <- humtemp
data_discr <- humtemp
temp_breaks <- seq(from = 15, to = 45, by = 5)
hum_breaks <- c(0, 40, 60, 80, 100)
temp_unique_vals <- setdiff(unique(Temperature), temp_breaks)
hum_unique_vals <- setdiff(unique(Humidity), hum_breaks)
data_discr[, 1] <- cut(Temperature, breaks = temp_breaks)
data_discr[, 2] <- cut(Humidity, breaks = hum_breaks)
# change interval syntax from (15,20] to (15;20], which is required by QCBA R package
data_discr[, 1] <- as.factor(unlist(lapply(data_discr[, 1], function(x) {
  gsub(",", ";", x)
})))
data_discr[, 2] <- as.factor(unlist(lapply(data_discr[, 2], function(x) {
  gsub(",", ";", x)
})))

head(data_discr)
plotGrid <- function(plotFineGrid = TRUE, plotDiscrGrid = TRUE) {
  if (plotDiscrGrid) {

    for (i in temp_breaks[-1]) {
      abline(h = i, lty = 2)
    }
    for (i in hum_breaks[-1]) {
      abline(v = i, lty = 2)
    }
  }
  if (plotFineGrid) {
    for (i in temp_unique_vals[-1]) {
      abline(h = i, lty = 3, col = "grey")
    }
    for (i in hum_unique_vals[-1]) {
      abline(v = i, lty = 3, col = "grey")
    }
  }
}

classAtt <- "Class"
appearance <- getAppearance(data_discr, classAtt)
txns <- as(data_discr, "transactions")
rules <- apriori(txns, parameter = list(confidence = 0.5, support = 3/nrow(data_discr), minlen = 1, maxlen = 3), appearance = appearance)
plot(Humidity, Temperature, pch = as.character(Class), main = "Discovered asociation rules", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
  cex.sub = 1.5)

plotGrid(FALSE)


plotHumTempRule <- function(rules, ruleIndex) {
  if (typeof(rules) == "S4") {
    # rules is a arules rule model
    r <- inspect(rules)[ruleIndex, ]
    rule <- paste(unlist(r$lhs[1]), collapse = "")
    rhs <- paste(unlist(r$rhs[1]), collapse = "")
  } else {
    # rules is a list of rules output by qCBA
    rule <- rules[ruleIndex, 1]
    rhs <- regmatches(rule, regexec("\\{Class=.*\\}", rule))
  }
  # get color
  if (rhs == "{Class=1}") {
    border = "red"
    col = rgb(1, 0.2, 0.2, alpha = 0.3)
  } else if (rhs == "{Class=2}") {
    border = "green"
    col = rgb(0, 1, 0, alpha = 0.3)
  } else if (rhs == "{Class=3}") {
    border = "black"
    col = rgb(0.4, 0.4, 0.4, alpha = 0.3)
  } else if (rhs == "{Class=4}") {
    border = "blue"
    col = rgb(0, 0, 1, alpha = 0.3)
  }


  temp_coordinates <- unlist(regmatches(rule, regexec("Temperature=.([0-9]+);([0-9]+).", rule)))
  if (length(temp_coordinates) == 0) {
    # if the temperature literal is missing in the rule, use the following coordinates
    temp_coordinates = c(0, 0, 50)
  }
  hum_coordinates <- unlist(regmatches(rule, regexec("Humidity=.([0-9]+);([0-9]+).", rule)))
  if (length(hum_coordinates) == 0) {
    # if the humidity literal is missing in the rule, use the following coordinates
    hum_coordinates = c(0, 0, 100)
  }
  m <- rect(hum_coordinates[2], temp_coordinates[2], hum_coordinates[3], temp_coordinates[3], border = border, col = col)
}

plotRules <- function(rules) {
  if (typeof(rules) == "S4") {
    # for arules/cba
    rule_count <- length(rules)
  } else {
    # for qcba
    rule_count <- nrow(rules)
  }
  for (i in 1:rule_count) {
    plotHumTempRule(rules, i)
  }
}

classAtt <- "Class"
appearance <- getAppearance(data_discr, classAtt)
# Note that we are calling `cba_manual()` instead of cba() because we want - for demonstration purposes - to construct the
# classifier from a externally-generated rule list.
rmCBA <- cba_manual(data_raw, rules, txns, appearance$rhs, classAtt, cutp = list(), pruning_options = list(default_rule_pruning = FALSE))

plot(Humidity, Temperature, pch = as.character(Class), main = "CBA model", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
plotGrid(FALSE)
plotRules(rmCBA@rules)

# To save the figure to a file uncomment the following two lines 
# dev.copy(png,filename='figures/figure1a.png'); 
# dev.off ();

rmqCBA <- qcba(cbaRuleModel = rmCBA, datadf = data_raw, extendType = "numericOnly", trim_literal_boundaries = TRUE, postpruning = "cba",
  attributePruning = TRUE, defaultRuleOverlapPruning = "transactionBased", createHistorySlot = TRUE, loglevel = "WARNING")

plot(Humidity, Temperature, pch = as.character(Class), main = "QCBA model", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
plotGrid(FALSE)
plotRules(rmqCBA@rules)

# To save the figure to a file uncomment the following two lines 
# dev.copy(png,filename='figures/figure1b.png'); 
# dev.off ();

message("To save the generated figures to a file uncomment the commented out code.")

message("Note that the code may produce a not logical or factor warning, which is caused by a compatibility issue between rCBA and newer versions of arules.")

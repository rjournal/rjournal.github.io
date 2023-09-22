library(MoTBFs)

## Load ecoli dataset and drop the first and last variables
  data("ecoli", package = "MoTBFs")
  data <- ecoli[,-c(1,9)]

## Split the dataset into train and test subsets
  set.seed(2)
  dataTT <- TrainingandTestData(data, percentage_test = 0.2)
  trainingData <- dataTT$Training
  testData <- dataTT$Test

## Learn the structure of the Bayesian network using the training data
  dag <- LearningHC(trainingData)
  dag
  
  plot(dag)
  

# Learning univariate MoTBFs from data -----------------------------------------------------
  ## Learn the density of variable mcg, using MTEs or MOP as basis functions
    f1 <- univMoTBF(trainingData[,1], POTENTIAL_TYPE = "MTE", nparam = 13)
    f2 <- univMoTBF(trainingData[,1], POTENTIAL_TYPE = "MOP", nparam = 11)
    
    print(f1)
    summary(f2)
  
  # Plot the densities f1 and f2 over the histogram of variable mcg
    hist(trainingData[,1], prob = TRUE , main = "", xlab = "X")
    plot(f1, xlim = range(trainingData[,1]), col = "red", add = TRUE)
    plot(f2, xlim = range(trainingData[,1]), col = "blue", add = TRUE)
    
  # Compute log-likelihood and BIC score of the fitted densities
    sum(log(as.function(f1)(testData[,1])))
    sum(log(as.function(f2)(testData[,1])))
    
    BICMoTBF(f1,testData[,1])
    BICMoTBF(f2,testData[,1])
  
  ## Simulate data from the estimated density f2
    set.seed(5)
    X <- rMoTBF(size = 400, fx = f2)
    
    # Test whether or not the simulated sample and the observed data come from the same distribution
    ks.test(trainingData[,1], X)
  
    # Compare the histogram and the CDF of both distributions
    hist(X, prob = TRUE, col = "deepskyblue3", main = "", ylim = c(0,2.2))
    hist(trainingData[,1], prob = TRUE, col = adjustcolor("gold",alpha.f = 0.5), add = T)

    plot(ecdf(trainingData[,1]), cex = 0, main = "")
    plot(integralMoTBF(f2), xlim = range(trainingData[,1]), col ="red", add = TRUE)
    
  ## Compute the derivative and integral of the fitted density
    coef(f1)
    integralMoTBF(f2)
    integralMoTBF(f2, min = min(trainingData[,1]), max = max(trainingData[,1]))
    derivMoTBF(f2)


# Learning joint MoTBFs from data --------------------------------------------
  ## Learn joint distributions
    parameters <- parametersJointMoTBF(X = trainingData[,c("mcg", "alm1")],
                                       dimensions = c(5,5))
    P <- jointMoTBF(parameters)
  
    attributes(P)
    summary(P)
    print(P)
    
   # Plot the joint distribution of 2 variables
    # Filled contour
    plot(P, data = trainingData[,c(1,6)]) 
    
    # Simple contour
    plot(P, data = trainingData[,c(1,6)], filled = FALSE) 
    
    # Perspective
    plot(P, type = "perspective", data = trainingData[,c(1,6)], orientation=c(60,20)) 
    
    
   # Compute the marginal distributions from the joint distribution (P)
    marginalJointMoTBF(P, var = 1)
    marginalJointMoTBF(P, var = 2)


# Learning conditional MoTBFs from data -----------------------------------

  ## Learn conditional distributions
    P <- conditionalMethod(trainingData, nameParents = "mcg", nameChild = "gvh",
                           numIntervals = 5, POTENTIAL_TYPE ="MOP")
    printConditional(P)
    
    plotConditional(P, data = trainingData, nameChild = "gvh", points = TRUE)
    

# Learning the parameters of a Bayesian network ---------------------------

  ## Learn the distributions of a Bayesian network
    bn <- MoTBFs_Learning(dag, data = trainingData, numIntervals = 4,
                           POTENTIAL_TYPE = "MTE")
    
    printBN(bn)

    bic <- BiC.MoTBFBN(bn, data = testData); bic

    
# Incorporating prior information -----------------------------------------

  # Obtain small training subset
  set.seed(4)
  dataTT <- TrainingandTestData(data, percentage_test = 0.99)

  trainingData <- dataTT$Training
  testData <- dataTT$Test
  nrow(trainingData)

  # Generate artificial prior dataset
  means <- sapply(data, mean)
  set.seed(4)
  priorData <- generateNormalPriorData(dag, data = trainingData, size = 5000,
                                          means = means)

  # Learn univariate distribution using prior information
  f <- learnMoTBFpriorInformation(priorData$aac, trainingData$aac,
                                  s = 5, POTENTIAL_TYPE = "MOP")

  print(f)  
   
  plot(f$posteriorFunction, xlim = f$domain, ylim = c(0,2.1))
  plot(f$dataFunction, xlim = f$domain, add = TRUE, col = 2)
  plot(f$priorFunction, xlim = f$domain, add = TRUE, col = 4)
  

  # Log-likelihood of the model that uses prior information
  sum(log(as.function(f$posteriorFunction)(testData$aac)))
  # Log-likelihood of the model that does not use prior information
  sum(log(as.function(f$dataFunction)(testData$aac)))
  
  
## Fit Bayesian network using prior information
  priorBN <- MoTBFs_Learning(dag, trainingData, numIntervals = 2,
                             POTENTIAL_TYPE = "MOP", s = 5, priorData = priorData)
  printBN(priorBN)  
  
  # Fit BN without using prior information
  BN <- MoTBFs_Learning(dag, trainingData, numIntervals = 2,
                             POTENTIAL_TYPE = "MOP")
  printBN(BN)  
  
  logLikelihood.MoTBFBN(priorBN, data = testData)
  logLikelihood.MoTBFBN(BN, data = testData)
  

# Inference ---------------------------------------------------------------
  # Learn a Bayesian network
  dag <- LearningHC(data)
  plot(dag)
  bn <- MoTBFs_Learning(dag, data = data, numIntervals = 4,
                           POTENTIAL_TYPE = "MTE")
  printBN(bn)
  
  # Specify the evidence set and target variable
  obs <- data.frame(lip = "0.48", alm1 = 0.55, gvh = 1, stringsAsFactors=FALSE)
  node <- "alm2"
  
  # Get the conditional distribution of 'node' and the generated sample
  set.seed(5)
  forward_sampling(bn, dag, target = node, evi = obs, size = 10, maxParam = 15)
  
  
  
    

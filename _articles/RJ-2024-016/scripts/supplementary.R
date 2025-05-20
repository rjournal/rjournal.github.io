# Generation of the initial sample

library(FuzzySimRes)
library(FuzzyNumbers)

# seed PRNG

set.seed(123456)

SimulateFuzzyNumber(originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                    incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                    suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                    suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                    type="trapezoidal")

# seed PRNG

set.seed(123456)

sample1 <- SimulateSample(n=10,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                          incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                          suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                          suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                          type="trapezoidal")


sample1$original[2]

sample1$value[2]

plot(sample1$value[[2]])

# Epistemic bootstrap

# seed PRNG

set.seed(123456)

epistemicOutput <- EpistemicBootstrap(sample1$value, cutsNumber = 3)

round(epistemicOutput,digits = 4)

# Antithetic epistemic bootstrap

# seed PRNG

set.seed(123456)

epistemicOutputAnt <- AntitheticBootstrap(sample1$value, cutsNumber = 3)

round(epistemicOutputAnt,digits = 4)

# Estimation of the statistical parameters

set.seed(56789)

EpistemicEstimator(sample1$value, estimator = "median",cutsNumber = 100)

set.seed(56789)

EpistemicCorrectedVariance(sample1$value, cutsNumber = 100)


# statistical tests

set.seed(56789)

sample2 <- SimulateSample(n=10,originalPD="rnorm",parOriginalPD=list(mean=0.5,sd=1),
                          incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                          suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                          suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                          type="trapezoidal")

EpistemicTest(sample1$value,sample2$value,algorithm = "ms",bootstrapMethod="anti",cutsNumber=100)

# the one-sample KS test for normality

set.seed(56789)

EpistemicTest(sample1$value,sample2=NULL,algorithm="avs",bootstrapMethod="std",cutsNumber=100,y="pnorm")


# Real-life dataset

controlChartData$X.1.2


# Estimation of statistical parameters

# Case 1


set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 10

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                              incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                              suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                              suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 1)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 1)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators

matrixOfEstimatorsN10 <- matrixOfEstimators

matrixOfEstimatorsN10




# Case 2

set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 100

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                              incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                              suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                              suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 1)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 1)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators


matrixOfEstimatorsN100 <- matrixOfEstimators

matrixOfEstimatorsN100


# Case 3

trueMean <- sqrt(pi)/2

trueMean

trueVar <- 1 - pi/4

trueVar

trueMed <- sqrt(log(2))

trueMed

set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 10

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rweibull",parOriginalPD=list(shape=2,scale=1),
                              incrCorePD="rexp",parIncrCorePD=list(rate=5),
                              suppLeftPD="rexp",parSuppLeftPD=list(rate=5),
                              suppRightPD="rexp", parSuppRightPD=list(rate=4),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueMean)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueMean)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueVar)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueVar)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueMean)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueMean)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators


matrixOfEstimatorsW10 <- matrixOfEstimators

matrixOfEstimatorsW10



# Case 4

set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 100

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rweibull",parOriginalPD=list(shape=2,scale=1),
                              incrCorePD="rexp",parIncrCorePD=list(rate=5),
                              suppLeftPD="rexp",parSuppLeftPD=list(rate=5),
                              suppRightPD="rexp", parSuppRightPD=list(rate=4),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueMean)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueMean)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueVar)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueVar)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = trueMed)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = trueMed)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators


matrixOfEstimatorsW100 <- matrixOfEstimators

matrixOfEstimatorsW100




# Case 5

set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 10

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rgamma",parOriginalPD=list(shape=2,rate=2),
                              incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                              suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.8),
                              suppRightPD="runif", parSuppRightPD=list(min=0,max=0.8),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = 1)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 1)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0.5)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0.5)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = NA)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = NA)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators


matrixOfEstimatorsGamma10 <- matrixOfEstimators

matrixOfEstimatorsGamma10





# Case 6

set.seed(12345)

iterationsEstimator <- 1000

sampleSize <- 100

cuts <- 100

pb <- txtProgressBar(min = 0,max = iterationsEstimator,style = 3,width = 50,char = "=") 

matrixOfEstimators <- matrix(0,nrow = 4,ncol = 6,dimnames = list(c("Value","SE","MSE","OAE"),rep(c("std", "anti"),3)))


for (i in 1:iterationsEstimator) {
  
  # generate sample
  
  newSample <- SimulateSample(n=sampleSize,originalPD="rgamma",parOriginalPD=list(shape=2,rate=2),
                              incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                              suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.8),
                              suppRightPD="runif", parSuppRightPD=list(min=0,max=0.8),
                              type="trapezoidal")
  # find estimators
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "std",trueValue = 1)
  
  matrixOfEstimators[1:3,1] <- matrixOfEstimators[1:3,1] + unlist(estimator)
  
  matrixOfEstimators[4,1] <- matrixOfEstimators[4,1] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicMean(newSample$value,cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 1)
  
  matrixOfEstimators[1:3,2] <- matrixOfEstimators[1:3,2] + unlist(estimator)
  
  matrixOfEstimators[4,2] <- matrixOfEstimators[4,2] + abs(mean(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "std",trueValue = 0.5)
  
  matrixOfEstimators[1:3,3] <- matrixOfEstimators[1:3,3] + unlist(estimator)
  
  matrixOfEstimators[4,3] <- matrixOfEstimators[4,3] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "var",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = 0.5)
  
  matrixOfEstimators[1:3,4] <- matrixOfEstimators[1:3,4] + unlist(estimator)
  
  matrixOfEstimators[4,4] <- matrixOfEstimators[4,4] + abs(var(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "std",trueValue = NA)
  
  matrixOfEstimators[1:3,5] <- matrixOfEstimators[1:3,5] + unlist(estimator)
  
  matrixOfEstimators[4,5] <- matrixOfEstimators[4,5] + abs(median(newSample$original) - estimator$value)
  
  estimator <- EpistemicEstimator(newSample$value,estimator = "median",cutsNumber = cuts, bootstrapMethod = "anti",trueValue = NA)
  
  matrixOfEstimators[1:3,6] <- matrixOfEstimators[1:3,6] + unlist(estimator)
  
  matrixOfEstimators[4,6] <- matrixOfEstimators[4,6] + abs(median(newSample$original) - estimator$value)
  
  setTxtProgressBar(pb, i)
  
}

matrixOfEstimators <- matrixOfEstimators/iterationsEstimator

matrixOfEstimators


matrixOfEstimatorsGamma100 <- matrixOfEstimators

matrixOfEstimatorsGamma100


# Analysis of the shift in the location

# Case 1

shiftVector <- seq(0,3,0.25)

methodNames <- c("avs-std","avs-anti","ms-std","ms-anti","res-std","res-anti","KS")



set.seed(56789)

iterationsTest <- 1000

sampleSize <- 100

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

for (j in 1:length(shiftVector)) {
  
  cat("\n", "Shift ", shiftVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    # add shift
    
    newSample2$original <- newSample2$original+shiftVector[j]
    
    fuzzyShift <- TrapezoidalFuzzyNumber(shiftVector[j],shiftVector[j],shiftVector[j],shiftVector[j])
    
    for (k in 1:length(newSample2$value)) {
      
      newSample2$value[[k]] <- newSample2$value[[k]]+fuzzyShift
      
    }
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesN100 <- matrixPValues

matrixRejectionsN100 <- matrixRejections

saveRDS(matrixPValuesN100,"matrixPValuesN100.rds")

saveRDS(matrixRejectionsN100,"matrixRejectionsN100.rds")



# Case 2


set.seed(56789)

iterationsTest <- 1000

sampleSize <- 10

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

for (j in 1:length(shiftVector)) {
  
  cat("\n", "Shift ", shiftVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    # add shift
    
    newSample2$original <- newSample2$original+shiftVector[j]
    
    fuzzyShift <- TrapezoidalFuzzyNumber(shiftVector[j],shiftVector[j],shiftVector[j],shiftVector[j])
    
    for (k in 1:length(newSample2$value)) {
      
      newSample2$value[[k]] <- newSample2$value[[k]]+fuzzyShift
      
    }
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesN10 <- matrixPValues

matrixRejectionsN10 <- matrixRejections

saveRDS(matrixPValuesN10,"matrixPValuesN10.rds")

saveRDS(matrixRejectionsN10,"matrixRejectionsN10.rds")


# Case 3


set.seed(56789)

iterationsTest <- 1000

sampleSize <- 100

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

for (j in 1:length(shiftVector)) {
  
  cat("\n", "Shift ", shiftVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rweibull",parOriginalPD=list(shape=2,scale=1),
                                 incrCorePD="rexp",parIncrCorePD=list(rate=5),
                                 suppLeftPD="rexp",parSuppLeftPD=list(rate=5),
                                 suppRightPD="rexp", parSuppRightPD=list(rate=4),
                                 type="trapezoidal")
    
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rweibull",parOriginalPD=list(shape=2,scale=1),
                                 incrCorePD="rexp",parIncrCorePD=list(rate=5),
                                 suppLeftPD="rexp",parSuppLeftPD=list(rate=5),
                                 suppRightPD="rexp", parSuppRightPD=list(rate=4),
                                 type="trapezoidal")
    
    # add shift
    
    newSample2$original <- newSample2$original+shiftVector[j]
    
    fuzzyShift <- TrapezoidalFuzzyNumber(shiftVector[j],shiftVector[j],shiftVector[j],shiftVector[j])
    
    for (k in 1:length(newSample2$value)) {
      
      newSample2$value[[k]] <- newSample2$value[[k]]+fuzzyShift
      
    }
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesW100 <- matrixPValues

matrixRejectionsW100 <- matrixRejections

saveRDS(matrixPValuesW100,"matrixPValuesW100.rds")

saveRDS(matrixRejectionsW100,"matrixRejectionsW100.rds")


# Case 4


set.seed(56789)

iterationsTest <- 1000

sampleSize <- 100

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(shiftVector),dimnames = list(shiftVector,methodNames))

for (j in 1:length(shiftVector)) {
  
  cat("\n", "Shift ", shiftVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rgamma",parOriginalPD=list(shape=2,rate=2),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.8),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=0.8),
                                 type="trapezoidal")
    
    
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rgamma",parOriginalPD=list(shape=2,rate=2),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=0.8),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=0.8),
                                 type="trapezoidal")
    
    
    # add shift
    
    newSample2$original <- newSample2$original+shiftVector[j]
    
    fuzzyShift <- TrapezoidalFuzzyNumber(shiftVector[j],shiftVector[j],shiftVector[j],shiftVector[j])
    
    for (k in 1:length(newSample2$value)) {
      
      newSample2$value[[k]] <- newSample2$value[[k]]+fuzzyShift
      
    }
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesGamma100 <- matrixPValues

matrixRejectionsGamma100 <- matrixRejections

saveRDS(matrixPValuesGamma100,"matrixPValuesGamma100.rds")

saveRDS(matrixRejectionsGamma100,"matrixRejectionsGamma100.rds")


# Figures for case 1

par(oma=c(0,0,0,0))

par(mar=c(4,4,0.2,0.2))

vectorColours <- c("darkgreen", "red", "violet", "orange", "blue","brown","black")

vectorPoints <- c(15:20,4)



matplot(shiftVector, matrixPValuesN100, type = c("b"), xlab = "Shift", ylab = "p-value",
        col = vectorColours,pch=vectorPoints)

legend("topright", legend = methodNames,col = vectorColours,pch=vectorPoints)


matplot(shiftVector, matrixPValuesN100[,-7]-matrixPValuesN100[,7], type = c("b"), xlab = "Shift", ylab = "Difference in p-value",
        col = vectorColours[-7],pch=vectorPoints[-7])

abline(h=0)

legend("bottomright", legend = methodNames[-7],col = vectorColours[-7],pch=vectorPoints[-7])



matplot(shiftVector, matrixRejectionsN100, type = c("b"), xlab = "Shift", ylab = "Power",
        col = vectorColours,pch=vectorPoints)

legend("bottomright", legend = methodNames,col = vectorColours,pch=vectorPoints)


matplot(shiftVector, matrixRejectionsN100[,-7]-matrixRejectionsN100[,7],
        type = c("b"), xlab = "Shift", ylab = "Difference in power",
        col = vectorColours[-7],pch=vectorPoints[-7])

abline(h=0)

legend("bottomright", legend = methodNames[-7],col = vectorColours[-7],pch=vectorPoints[-7])



# Analysis of the change of the standard deviation



sdVector <- seq(1,10,0.25)

methodNames <- c("avs-std","avs-anti","ms-std","ms-anti","res-std","res-anti","KS")

# Case 1

set.seed(56789)

iterationsTest <- 1000

sampleSize <- 100

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(sdVector),dimnames = list(sdVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(sdVector),dimnames = list(sdVector,methodNames))

for (j in 1:length(sdVector)) {
  
  cat("\n", "Shift ", sdVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=sdVector[j]),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesSdN100 <- matrixPValues

matrixRejectionsSdN100 <- matrixRejections

saveRDS(matrixPValuesSdN100,"matrixPValuesSdN100.rds")

saveRDS(matrixRejectionsSdN100,"matrixRejectionsSdN100.rds")


# Case 2


set.seed(56789)

iterationsTest <- 1000

sampleSize <- 10

cuts <- 100

resamplingNumber <- 100

significanceLevel <- 0.05



matrixPValues <- matrix(0,ncol = length(methodNames),nrow = length(sdVector),dimnames = list(sdVector,methodNames))

matrixRejections <- matrix(0,ncol = length(methodNames),nrow = length(sdVector),dimnames = list(sdVector,methodNames))

for (j in 1:length(sdVector)) {
  
  cat("\n", "Shift ", sdVector[j], "\n")
  
  pb <- txtProgressBar(min = 0,max = iterationsTest,style = 3,width = 50,char = "=")
  
  for (i in 1:iterationsTest) {
    
    # generate samples
    
    newSample1 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=1),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    newSample2 <- SimulateSample(n=sampleSize,originalPD="rnorm",parOriginalPD=list(mean=0,sd=sdVector[j]),
                                 incrCorePD="runif",parIncrCorePD=list(min=0,max=0.6),
                                 suppLeftPD="runif",parSuppLeftPD=list(min=0,max=1),
                                 suppRightPD="runif", parSuppRightPD=list(min=0,max=1),
                                 type="trapezoidal")
    
    
    pValueVector <- rep(0,length(methodNames))
    
    names(pValueVector) <- methodNames
    
    # "crisp" KS test
    
    pValueVector["KS"] <- ks.test(newSample1$original,newSample2$original)$p.value 
    
    # "avs-std" KS test
    
    pValueVector["avs-std"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                             bootstrapMethod = "std",
                                                             cutsNumber = cuts)
    
    # "avs-anti" KS test
    
    pValueVector["avs-anti"] <- AverageStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                              bootstrapMethod = "anti",
                                                              cutsNumber = cuts)
    
    # "ms-std" KS test
    
    pValueVector["ms-std"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                          bootstrapMethod = "std",
                                                          cutsNumber = cuts,combineMethod = "mean")
    
    # "ms-anti" KS test
    
    pValueVector["ms-anti"] <- MultiStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                           bootstrapMethod = "anti",
                                                           cutsNumber = cuts,combineMethod = "mean")
    
    
    # "res-std" KS test
    
    pValueVector["res-std"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                bootstrapMethod = "std",
                                                                cutsNumber = cuts,
                                                                K=resamplingNumber,
                                                                combineMethod = "mean")
    
    
    # "res-anti" KS test
    
    pValueVector["res-anti"] <- ResamplingStatisticEpistemicTest(newSample1$value,newSample2$value,
                                                                 bootstrapMethod = "anti",
                                                                 cutsNumber = cuts,
                                                                 K=resamplingNumber,
                                                                 combineMethod = "mean")
    
    
    
    
    matrixPValues[j,] <- matrixPValues[j,] + pValueVector
    
    # accept of reject?
    
    matrixRejections[j,] <- matrixRejections[j,] + (pValueVector < significanceLevel)
    
    
    
    setTxtProgressBar(pb, i)
  }
  
}

matrixPValues <- matrixPValues/iterationsTest

matrixRejections <- matrixRejections/iterationsTest

matrixPValuesSdN10 <- matrixPValues

matrixRejectionsSdN10 <- matrixRejections

saveRDS(matrixPValuesSdN10,"matrixPValuesSdN10.rds")

saveRDS(matrixRejectionsSdN10,"matrixRejectionsSdN10.rds")


# Figures for case 1

par(oma=c(0,0,0,0))

par(mar=c(4,4,0.2,0.2))

vectorColours <- c("darkgreen", "red", "violet", "orange", "blue","brown","black")

vectorPoints <- c(15:20,4)



matplot(sdVector, matrixPValuesSdN100, type = c("b"), xlab = "Standard deviation", ylab = "p-value",
        col = vectorColours,pch=vectorPoints)

legend("topright", legend = methodNames,col = vectorColours,pch=vectorPoints)


matplot(sdVector, matrixPValuesSdN100[,-7]-matrixPValuesSdN100[,7], type = c("b"), xlab = "Standard deviation", ylab = "Difference in p-value",
        col = vectorColours[-7],pch=vectorPoints[-7])

abline(h=0)

legend("bottomright", legend = methodNames[-7],col = vectorColours[-7],pch=vectorPoints[-7])


matplot(sdVector, matrixRejectionsSdN100, type = c("b"), xlab = "Standard deviation", ylab = "Power",
        col = vectorColours,pch=vectorPoints)

legend("bottomright", legend = methodNames,col = vectorColours,pch=vectorPoints)


matplot(sdVector, matrixRejectionsSdN100[,-7]-matrixRejectionsSdN100[,7],
        type = c("b"), xlab = "Standard deviation", ylab = "Difference in power",
        col = vectorColours[-7],pch=vectorPoints[-7])

abline(h=0)

legend("bottomright", legend = methodNames[-7],col = vectorColours[-7],pch=vectorPoints[-7])





# Goodness-of-fit test for control chart data

set.seed(5678)

randomSetsCCD <- sample(length(controlChartData),length(controlChartData)/2)

randomSetsCCD

EpistemicTest(controlChartData[randomSetsCCD],controlChartData[-randomSetsCCD],algorithm = "avs",cutsNumber=1000)

EpistemicTest(controlChartData[randomSetsCCD],controlChartData[-randomSetsCCD],algorithm = "ms",combineMethod="mean",
              cutsNumber=1000)

EpistemicTest(controlChartData[randomSetsCCD],controlChartData[-randomSetsCCD],algorithm = "res",combineMethod="mean",
              cutsNumber=1000,K=200)

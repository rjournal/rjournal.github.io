library(FuzzyResampling)

?resamplingMethods

# seed PRNG
set.seed(1234)

sample1 <- GeneratorNU(10, mu = 0, sigma = 1, a = 0.2, b = 0.6)

head(sample1,2)

set.seed(1234)

sampleEfron <- ClassicalBootstrap(sample1)

head(sampleEfron,2)

set.seed(1234)

sampleVA <- VAMethod(sample1, 20)

head(sampleVA,2)

BertoluzzaDistance(sample1[1,],sample1[2,])

set.seed(1234)

SEResamplingMean(sample1)

set.seed(1234)

SEResamplingMean(sample1,resamplingMethod = "VAMethod",trueMean = c(-0.4,-0.1,0.1,0.4))

set.seed(1234)

OneSampleCTest(sample1, mu_0 = c(-0.4,-0.1,0.1,0.4))

set.seed(1234)

OneSampleCTest(sample1, mu_0 = c(-0.4,-0.1,0.1,0.4),
               numberOfSamples = 1000, resamplingMethod = "VAMethod")

set.seed(1234)

sample2 <- GeneratorNU(10, mu = 0.2, sigma = 1, a = 0.2, b = 0.6)

TwoSampleCTest(sample1, sample2,numberOfSamples = 1000)




set.seed(1234567)

outputSEsample1 <- ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10,
                                    numberOfSamples = 10000,repetitions = 100,mu = 0,sigma = 1,a = 0.2,b = 0.6)

round(outputSEsample1, digits = 5)

set.seed(1234567)

outputSEsample2 <- ComparisonSEMean(generator = "GeneratorNU",sampleSize = 50,
                numberOfSamples = 10000,repetitions = 100,mu = 0,sigma = 1,a = 0.2,b = 0.6)

round(outputSEsample2, digits = 5)

set.seed(1234567)

outputSEsample3 <- ComparisonSEMean(generator = "GeneratorNExpUU",sampleSize = 10,
                   numberOfSamples = 10000,repetitions = 100,mu = 0,sigma = 4,lambda = 4,b = 0.4,c = 0.8)

round(outputSEsample3, digits = 5)


set.seed(1234567)

outputMSEsample1 <- ComparisonSEMean(generator = "GeneratorNU",sampleSize = 10, numberOfSamples = 10000,
                               repetitions = 100,trueMean=c(-0.4,-0.1,0.1,0.4),mu = 0, sigma = 1,a = 0.2, b = 0.6)

round(outputMSEsample1, digits = 5)

set.seed(1234567)

outputMSEsample2 <- ComparisonSEMean(generator = "GeneratorNExpUU",sampleSize = 10, numberOfSamples = 10000,
                                     repetitions = 100, trueMean = c(-0.45,-0.25,0.25,0.65), mu = 0, sigma = 4, lambda = 4, b = 0.4, c = 0.8)


round(outputMSEsample2, digits = 5)



set.seed(1234567)

outputCSizetest1 <- ComparisonOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
                                         sampleSize = 10,
                                         numberOfSamples = 100, initialSamples = 1000,
                                         mu = 0, sigma = 1, a = 0.2, b = 0.6)

round(outputCSizetest1, digits = 4)

round(outputCSizetest1-0.05, digits = 4)

set.seed(1234567)

outputCSizetest2 <- ComparisonOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
                                         sampleSize = 50,
                                         numberOfSamples = 100, initialSamples = 1000,
                                         mu = 0, sigma = 1, a = 0.2, b = 0.6)

round(outputCSizetest2, digits = 4)

set.seed(1234567)

outputCSizetest3 <- ComparisonOneSampleCTest("GeneratorNExpUU",mu_0 = c(-0.45,-0.25,0.25,0.65),
                                         sampleSize = 50,
                                         numberOfSamples = 100, initialSamples = 1000,
                                         mu = 0, sigma = 4, lambda = 4, b = 0.4, c = 0.8)

round(outputCSizetest3, digits = 4)

# special plot to compare the percentage of rejections for different
# significance levels

significanceVector <- seq(0.05,1,by = 0.05)



SignificancePlot <- function(numberOfSamples, initialSamples)
{
  # matrix prealocation

  outputMatrix <- matrix(NA, nrow = length(significanceVector), ncol = length(resamplingMethods),
                         dimnames = list(significanceVector,resamplingMethods))

  # let's make some simulations

  for (i in 1:length(significanceVector)) {

    outputMatrix[i,] <- ComparisonOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),
                                                 sampleSize = 10,
                                                 numberOfSamples = numberOfSamples, initialSamples = initialSamples,
                                                 significance = significanceVector[i],
                                                 mu = 0, sigma = 1, a = 0.2, b = 0.6)

  }

  return(outputMatrix)
}


outputCSizetestCPlotD <- SignificancePlot(numberOfSamples=1000, initialSamples=1000)

matplot(significanceVector, outputCSizetestCPlotD, type = c("b"), xlab = "Significance", ylab = "Power",
        col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),pch=c(4,15:20))

legend("topleft", legend = resamplingMethods,
       col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),pch=c(4,15:20))


matplot(significanceVector, outputCSizetestCPlotD-significanceVector, type = c("b"),
        xlab = "Significance", ylab = "Diff. in test size",
        col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),pch=c(4,15:20))

abline(h=0)


legend("topleft", legend = resamplingMethods,
       col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),pch=c(4,15:20))


# prepare vector of shifts

shifts <- seq(0.1,1,by = 0.05)

set.seed(1234567)

outputCPowerTest1 <- ComparePowerOneSampleCTest("GeneratorNU",mu_0 = c(-0.4,-0.1,0.1,0.4),shiftVector = shifts,
                                                sampleSize = 5,
                                                numberOfSamples = 100, initialSamples = 10000,
                                                mu = 0, sigma = 1, a = 0.2, b = 0.6)

head(round(outputCPowerTest1, digits = 4),4)

matplot(shifts, outputCPowerTest1, type = c("b"), xlab = "Shift", ylab = "Power",
        col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),pch=c(4,15:20))

legend("topleft", legend = resamplingMethods,col=c("gray44", "red", "violet", "orange", "blue","darkgreen","chocolate4"),
       pch=c(4,15:20))

matplot(shifts, outputCPowerTest1[,-1]-outputCPowerTest1[,1], type = c("b"), xlab = "Shift", ylab = "Power diff.",
        col=c("red", "violet", "orange", "blue","darkgreen","chocolate4"),
        pch=c(15:20))

legend("topleft", legend = resamplingMethods[-1],col=c("red", "violet", "orange", "blue","darkgreen","chocolate4"),
       pch=c(15:20))

abline(h=0)


expert1v <- c(65, 65, 35, 75, 66, 65, 70, 70, 65, 55, 45, 64, 60, 50, 65,
              55, 60, 60, 55, 64, 60, 50, 30, 50, 60, 65, 70, 50, 44, 40, 51, 70, 40, 44, 55, 34,
              80, 40, 80, 84, 75, 70, 37, 80, 70, 70, 74, 80, 70, 64, 50, 73, 66, 56, 65, 55, 65,
              70, 60, 71, 65, 50, 44, 54, 65, 75, 75, 55, 45, 44, 56, 76, 46, 50, 60, 40, 85, 45,
              84, 90, 85, 76, 44, 86, 75, 73, 80, 84, 75, 70, 57, 80, 70, 64, 70, 60, 75, 75, 66,
              80, 70, 55, 46, 60, 75, 80, 85, 60, 50, 50, 64, 85, 54, 53, 65, 46, 90, 51, 90, 95,
              85, 80, 50, 90, 80, 80, 84, 84, 80, 70, 65, 84, 75, 70, 76, 70, 80, 80, 70, 80, 74,
              65, 54, 65, 75, 86, 85, 66, 56, 50, 70, 85, 60, 60, 70, 46, 94, 60, 90, 95)

expert1M <- matrix(expert1v,ncol=4)


expert2v <- c(60, 53, 43, 70, 54, 76, 65, 77, 76, 70, 50, 43, 50, 65, 65, 50, 65, 74, 46, 50, 65,
              55, 65, 54, 73, 54, 50, 65, 40, 46, 55, 50, 40, 65, 55, 70, 60, 64, 40, 35, 35, 66,
              63, 58, 47, 76, 60, 80, 68, 80, 80, 76, 51, 47, 55, 67, 70, 55, 70, 80, 50, 57, 74,
              58, 73, 57, 80, 60, 55, 74, 47, 50, 60, 57, 47, 70, 60, 74, 66, 70, 44, 40, 44, 70,
              67, 63, 54, 83, 65, 83, 73, 86, 85, 80, 55, 51, 60, 73, 75, 60, 75, 85, 55, 64, 80,
              64, 80, 62, 85, 65, 60, 80, 53, 57, 65, 63, 53, 76, 65, 83, 74, 75, 51, 46, 50, 75,
              72, 68, 58, 86, 70, 86, 80, 90, 90, 85, 64, 58, 64, 80, 80, 65, 80, 90, 60, 70, 84,
              70, 85, 70, 90, 70, 64, 84, 60, 64, 74, 70, 60, 80, 70, 90, 81, 80, 56, 50, 55, 85)

expert2M <- matrix(expert2v,ncol=4)

# special function to compare outputs for two-sample C test applied to real-life case

CompareRealCaseTwoSample <- function(numberOfSamples, numberOfIterations)
{
  # print(as.list(match.call()))

  # vector prealocation

  outputVector <- rep(0, length(resamplingMethods))

  # progress bar

  pb <- txtProgressBar (1, numberOfIterations, style = 3)

  for (i in 1:numberOfIterations)
  {
    # find p-value for each method

    for (j in 1:length(resamplingMethods)) {

      outputVector[j] <- outputVector[j] + TwoSampleCTest(expert1M,expert2M,numberOfSamples,
                                                          resamplingMethod = resamplingMethods[j])

    }

    setTxtProgressBar(pb, i)

  }




  # print output

  cat("\n")

  names(outputVector) <- resamplingMethods

  outputVector <- outputVector / numberOfIterations

  outputVector

}

set.seed(1234567)

realCaseP <- CompareRealCaseTwoSample(numberOfSamples=100,numberOfIterations=10000)

round(realCaseP,5)


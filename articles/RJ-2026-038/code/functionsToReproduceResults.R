# This script is part of the submission of manuscript "hdtg: An R package for high-dimensional truncated normal simulation" to the R journal. It contains functions called by "hzz_rjournal.R" to reproduce the results in Table 2.

library(hdtg)

library(coda)
library(rbenchmark)
library(magrittr)
library(hash)
library(Matrix)

#' Benchmark the run-time of four samplers (Harmonic-HMC, Zigzag-HMC, Zigzag-NUTS,
#' and MET) on the same MTN target
#'
#' @param nHHMC number of MCMC iterations from Harmonic-HMC
#' @param nZHMC number of MCMC iterations from Zigzag-HMC
#' @param nZNUTS number of MCMC iterations from Zigzag-NUTS
#' @param nMET number of MCMC iterations from MET
#' @param repeatTimes number of test repeats when measuring the run time
#' @param forOneSampleFlg logical. `TRUE` for t1 and `FALSE` for t100
#' @param meanVec a d-dimensional mean vector
#' @param precMat a d-by-d precision matrix of the MTN
#' @param covMat a d-by-d covariance matrix of the MTN
#' @param pInitial a d-dimensional vector of the initial value that satisfy all constraints
#' @param matF the F matrix for Harmonic-HMC
#' @param vecg the a vector for Harmonic-HMC
#' @param lb lower bounds
#' @param ub upper bounds
#'
#' @return a list containing the timing results
benchMarkTMVN <-
  function(nHHMC,
           nZHMC,
           nZNUTS,
           nMET,
           repeatTimes,
           forOneSampleFlg,
           meanVec,
           precMat,
           covMat,
           pInitial,
           matF,
           vecg,
           lb,
           ub) {
    
    dimension <- length(pInitial)
    timingCols <- c("test",
                    "replications",
                    "elapsed",
                    "relative",
                    "user.self",
                    "sys.self")
    
    timeList <- list()
    samplesList <- list()
    numTest <- 1
    
    #Harmonic-HMC
    choleskyR <- cholesky(precMat)
    if (nHHMC > 0) {
      set.seed(1)
      timeList[[numTest]] <- benchmark(
        "HHMC" = {
          samplesHHMC <- harmonicHMC(
            nSample = nHHMC,
            burnin = 0,
            mean = meanVec,
            choleskyFactor = choleskyR,
            constrainDirec = matF,
            constrainBound = vecg,
            init = pInitial,
            time = c(pi / 8, pi / 2),
            precFlg = TRUE
          )
        },
        replications = repeatTimes,
        columns = timingCols,
        order = NULL
      )
      
      samplesList <-
        c(samplesList, samplesHHMC = list(samplesHHMC))
      numTest <- numTest + 1
    }
    
    #HZZ and NUTS (one e-sample)
    if (forOneSampleFlg) {
      if (nZHMC > 0) {
        set.seed(1)
        engine <- createEngine(
          dimension = dimension,
          lowerBounds = lb,
          upperBounds = ub,
          flags = 128,
          seed = 1,
          mean = meanVec,
          precision = precMat
        )
        
        samplesZHMC  <-  array(0, c(nZHMC, dimension))
        
        timeList[[numTest]] <- benchmark(
          "ZHMC" = {
            setMean(engine = engine, mean = meanVec)
            setPrecision(engine = engine, precision = precMat)
            
            HZZtime <-
              sqrt(2) / sqrt(min(mgcv::slanczos(
                A = precMat, k = 1, kl = 1
              )[['values']]))
            
            currentSample <- pInitial
            for (i in 1:nZHMC) {
              currentSample <- getZigzagSample(
                position = currentSample,
                nutsFlg = F,
                engine = engine,
                stepSize = HZZtime
              )
              samplesZHMC[i, ] <- currentSample
            }
          },
          replications = repeatTimes,
          columns = timingCols,
          order = NULL
        )
        samplesList <-
          c(samplesList, samplesZHMC = list(samplesZHMC))
        numTest <- numTest + 1
      }
      
      if (nZNUTS > 0) {
        set.seed(1)
        baseStep <-
          0.1 / sqrt(min(mgcv::slanczos(
            A = precMat, k = 1, kl = 1
          )[['values']]))
        engine <- createNutsEngine(
          dimension = dimension,
          lowerBounds = lb,
          upperBounds = ub,
          flags = 128,
          seed = 1,
          stepSize = baseStep,
          mean = meanVec,
          precision = precMat
        )
        
        samplesZNUTS  <-  array(0, c(nZNUTS, dimension))
        
        timeList[[numTest]] <- benchmark(
          "ZNUTS" = {
            baseStep <-
              0.1 / sqrt(min(mgcv::slanczos(
                A = precMat, k = 1, kl = 1
              )[['values']]))
            
            currentSample <- pInitial
            for (i in 1:nZNUTS) {
              currentSample <- getZigzagSample(position = currentSample,
                                               nutsFlg = T,
                                               engine = engine)
              samplesZNUTS[i, ] <- currentSample
            }
          },
          replications = repeatTimes,
          columns = timingCols,
          order = NULL
        )
        
        samplesList <-
          c(samplesList, samplesZNUTS = list(samplesZNUTS))
        numTest <- numTest + 1
      }
    } else {
      #HZZ and NUTS (multiple e-sample)
      if (nZHMC > 0) {
        set.seed(1)
        timeList[[numTest]] <- benchmark(
          "ZHMC" = {
            samplesZHMC <- zigzagHMC(
              nSample = nZHMC,
              burnin = 0,
              mean = meanVec,
              prec = precMat,
              lowerBounds = lb,
              upperBounds = ub,
              init = pInitial,
              nutsFlg = F
            )
          },
          replications = repeatTimes,
          columns = timingCols,
          order = NULL
        )
        
        samplesList <-
          c(samplesList, samplesZHMC = list(samplesZHMC))
        numTest <- numTest + 1
      }
      
      if (nZNUTS > 0) {
        set.seed(1)
        timeList[[numTest]] <- benchmark(
          "ZNUTS" = {
            samplesZNUTS <- zigzagHMC(
              nSample = nZNUTS,
              burnin = 0,
              mean = meanVec,
              prec = precMat,
              lowerBounds = lb,
              upperBounds = ub,
              init = pInitial,
              nutsFlg = T
            )
          },
          replications = repeatTimes,
          columns = timingCols,
          order = NULL
        )
        samplesList <-
          c(samplesList, samplesZNUTS = list(samplesZNUTS))
        numTest <- numTest + 1
      }
    }
    
    #MET
    if (nMET > 0) {
      set.seed(1)
      timeList[[numTest]] <- benchmark(
        "MET" = {
          samplesMET <- TruncatedNormal::rtmvnorm(
            n = nMET,
            mu = meanVec,
            sigma = covMat,
            lb = lb,
            ub = ub
          )
        },
        replications = repeatTimes,
        columns = timingCols,
        order = NULL
      )
      samplesList <- c(samplesList, samplesMET = list(samplesMET))
      numTest <- numTest + 1
    }
    return(timeList)
  }

#' Run MTN simulations by Harmonic-HMC/Zigzag-HMC/Zigzag-NUTS/MET.
#' with given MCMC chain length
#'
#' @param nHHMC number of MCMC iterations from Harmonic-HMC
#' @param nZHMC number of MCMC iterations from Zigzag-HMC
#' @param nZNUTS number of MCMC iterations from Zigzag-NUTS
#' @param nMET number of MCMC iterations from MET
#' @param meanVec a d-dimensional mean vector
#' @param precMat a d-by-d precision matrix of the MTN
#' @param covMat a d-by-d covariance matrix of the MTN
#' @param pInitial a d-dimensional vector of the initial value that satisfy all constraints
#' @param matF the F matrix for Harmonic-HMC
#' @param vecg the a vector for Harmonic-HMC
#' @param lb lower bounds
#' @param ub upper bounds
#'
#' @return a list containing the MCMC samples
sampleFromTMVN <-
  function(nHHMC,
           nZHMC,
           nZNUTS,
           nMET,
           meanVec,
           precMat,
           covMat,
           pInitial,
           matF,
           vecg,
           lb,
           ub) {
    samplesList <- list()
    numTest <- 1
    
    #Harmonic-HMC
    choleskyR <- cholesky(precMat)
    if (nHHMC > 0) {
      set.seed(1)
      
      samplesHHMC <- harmonicHMC(
        nSample = nHHMC,
        burnin = 0,
        mean = meanVec,
        choleskyFactor = choleskyR,
        constrainDirec = matF,
        constrainBound = vecg,
        init = pInitial,
        time = c(pi / 8, pi / 2),
        precFlg = TRUE
      )
      
      samplesList <-
        c(samplesList, samplesHHMC = list(samplesHHMC))
      numTest <- numTest + 1
    }
    
    #Zigzag-HMC
    if (nZHMC > 0) {
      set.seed(1)
      
      samplesZHMC <- zigzagHMC(
        nSample = nZHMC,
        burnin = 0,
        mean = meanVec,
        prec = precMat,
        lowerBounds = lb,
        upperBounds = ub,
        init = pInitial,
        nutsFlg = F
      )
      
      samplesList <- c(samplesList, samplesZHMC = list(samplesZHMC))
      numTest <- numTest + 1
    }
    
    #Zigzag-NUTS
    if (nZNUTS > 0) {
      set.seed(1)
      
      samplesZNUTS <- zigzagHMC(
        nSample = nZNUTS,
        burnin = 0,
        mean = meanVec,
        prec = precMat,
        lowerBounds = lb,
        upperBounds = ub,
        init = pInitial,
        nutsFlg = T
      )
      
      samplesList <-
        c(samplesList, samplesZNUTS = list(samplesZNUTS))
      numTest <- numTest + 1
    }
    
    #MET
    if (nMET > 0){
      set.seed(1)
        samplesMET <- TruncatedNormal::rtmvnorm(
          n = nMET,
          mu = meanVec,
          sigma = covMat,
          lb = lb,
          ub = ub
        )
      samplesList <-
        c(samplesList, samplesMET = list(samplesMET))
      numTest <- numTest + 1
    }
    #return the result list
    return(samplesList = samplesList)
  }

#' Get the minimal effective sample size (ESS)
#'
#' @param res an array containing MCMC samples (each row is one iteration).
#'
#' @return the minimal ESS across all dimensions.
#'
getMinESS <- function(res) {
  return(coda::mcmc(res, thin = 1) %>% coda::effectiveSize() %>% min())
}


#' Select the string in a string vector based on given patterns
#'
#' @param stringVec a vector of strings.
#' @param patterns keywords to be found in the target string, can be regular expression.
#'
#' @return the selected string
#'
getFileByName <- function(stringVec, patterns) {
  i <-
    sapply(stringVec, function(fn)
      all(sapply(patterns, grepl, fn)))
  stringVec[i]
}


#' Title
#'
#' @param mode
#' @param dimension
#' @param HIVdataFolder
#' @return
prepareMTN <- function(mode, dimension, HIVdataFolder = NULL) {
  meanV <- rep(0, dimension)
  lb <- rep(0, dimension)
  ub <- rep(Inf, dimension)
  direcHHMC <- diag(dimension)
  bdryHHMC <- rep(0, dimension)
  
  if (mode == 'HIV') {
    t <- readRDS(paste(
      HIVdataFolder, mode, dimension, '.Rds', sep = ''
    ))
    if (max(abs(t$covM - t(t$covM))) > 1e-6) warning("Covariance matrix asymmetry > 1e-6")
    t$covM <- as.matrix(forceSymmetric(t$covM))
    return(t)
  } else {
    if (mode == 'LKJ') {
      set.seed(1)
      covM <- trialr::rlkjcorr(n = 1, K = dimension, eta = 1)
      precM <- solve(covM)
    } else if (mode == 'CS') {
      covM <- matrix(0.9, nrow = dimension, ncol = dimension)
      diag(covM) <- rep(1, dimension)
      precM <- solve(covM)
    }
    p0 <-
      hdtg::getInitialPosition(mean = meanV,
                               lowerBounds = lb,
                               upperBounds = ub)
    return(
      list(
        meanV = meanV,
        precM = precM,
        covM = covM,
        p0 = p0,
        direcHHMC = direcHHMC,
        bdryHHMC = bdryHHMC,
        lb = lb,
        ub = ub
      )
    )
  }
}
# This script is part of the submission of manuscript "hdtg: An R package for high-dimensional truncated normal simulation" to the R journal. 
# It contains two parts: (1) the example code under the section "Using hdtg" and (2) code to reproduce the results in Table 1. 
# This script calls functions from the "functionsToReproduceResults.R" script. Besides hdtg there are a few other CRAN packages needs to be installed: coda, rbenchmark, magrittr, hash, dplyr, xtable, here

# example code in "Using hdtg" section ####
# first example
library(hdtg)
library(dplyr)
library(xtable)
library(this.path)

# code to reproduce results in Table 2 ####
# note that the actual timing results depend on the specific machine one used and
# it can take up to 20 hours to finish all the test steps. For a quicker check, simply
# skip the high dimensions by setting "dimensionTested" to 100.

# load necessary functions and set directory to save intermediate results

script_dir <- file.path(dirname(this.path()), "scripts")
source(file.path(script_dir, "functionsToReproduceResults.R"))
output_path <- file.path(script_dir, "outputs/")

if (dir.exists(output_path)) {
  # Find the next available folder name
  i <- 1
  while (dir.exists(file.path(script_dir, paste0("outputs_", i)))) {
    i <- i + 1
  }
  output_path <- file.path(script_dir, paste0("outputs_", i, "/"))
}
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# specify test cases and dimensions
mtnTypes <- c('LKJ', 'CS', 'HIV')
dimensionTested <- c(100) 
dimensionTested <- c(100, 400, 1600) #comment this line to run a quicker test

# set the path to HIV data that contains one .Rds file for each dimension
HIVdataFolder <- file.path(script_dir, "HIVdata/")
# Step1/3: estimate n1 for each test case by running a long chain ####

# set a sufficiently long chain length for each test case (minimal ESS > 100)
chainLengths <- list(
  LKJ = list(
    '100'  = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 100),
    '400'  = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 0),
    '1600' = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 0)
  ),
  CS = list(
    '100'  = c(HHMC = 1000, ZHMC = 500, ZNUTS = 5000, MET = 100),
    '400'  = c(HHMC = 1000, ZHMC = 500, ZNUTS = 25000, MET = 100),
    '1600' = c(HHMC = 1000, ZHMC = 500, ZNUTS = 5000, MET = 100)
  ),
  HIV = list(
    '100'  = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 100),
    '400'  = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 100),
    '1600' = c(HHMC = 1000, ZHMC = 1000, ZNUTS = 1000, MET = 100)
  )
)

# loop over each test case (LKJ, CS, and HIV)
for (mtn in mtnTypes) {
  for (dimension in dimensionTested) {
    dim_char <- as.character(dimension)
    sampler_lengths <- chainLengths[[mtn]][[dim_char]]

    nHHMC  <- sampler_lengths["HHMC"]
    nZHMC  <- sampler_lengths["ZHMC"]
    nZNUTS <- sampler_lengths["ZNUTS"]
    nMET    <- sampler_lengths["MET"]

    mtnParams <- prepareMTN(mtn, dimension, HIVdataFolder = HIVdataFolder)
    samplesList <-  sampleFromTMVN(
      nHHMC = nHHMC,
      nZHMC = nZHMC,
      nZNUTS = nZNUTS,
      nMET = nMET,
      meanVec = mtnParams$meanV,
      precMat = mtnParams$precM,
      covMat = mtnParams$covM,
      pInitial = mtnParams$p0,
      matF = mtnParams$direcHHMC,
      vecg = mtnParams$bdryHHMC,
      lb = mtnParams$lb,
      ub = mtnParams$ub
    )

    nSampleList <- list()
    for (i in 1:length(samplesList)) {
      sampler <- sub('.*samples', '', names(samplesList)[i])
      nName <- paste('n', sampler, sep = '')
      nTotal <- get(nName)

      samplesDF <- samplesList[[i]]
      # save the MCMC samples if needed
      saveRDS(
        samplesDF,
        file = paste(
          output_path,
          'samples',
          mtn,
          '_d',
          dimension,
          '_',
          sampler,
          '.rds',
          sep = ''
        )
      )
      if (sampler == "MET"){
        minESS <- 100
      } else {
        minESS <- getMinESS(samplesDF)
      }
      # give a message if the minimal ESS is less than 100
      if (minESS < 100) {
        message("\n", paste(rep("=", 50), collapse = ""))
        message("WARNING: Sampler ", sampler, " at d = ", dimension, "type = ", mtn, " has min ESS < 100!")
        message("minESS = ", minESS, ", nTotal = ", nTotal)
        message(paste(rep("=", 50), collapse = ""), "\n")
      }
      nSampleList[[i]] <-
        data.frame(
          sampler = sampler,
          minESS = minESS,
          nTotal = nTotal,
          n1 = ceiling(nTotal / minESS),
          n100 = ceiling(nTotal / minESS * 100),
          n1000 = ceiling(nTotal / minESS * 1000))
    }
    # save minimal ESS information and estimated n1 values to files
    essDF <- dplyr::bind_rows(nSampleList)
    saveRDS(essDF,
            file = paste(output_path, 'ess', mtn, '_d', dimension, '.rds', sep = ''))
  }
}
# Step2/3: estimate the run-time for obtaining n1 and n100 samples ####

# read in n1 values from saved files
for (mtn in mtnTypes) {
  for (dimension in dimensionTested) {
    df <-
      readRDS(paste(output_path, 'ess', mtn, '_d', dimension, '.rds', sep = ''))
    n1HHMC <- ceiling(df[df$sampler == 'HHMC', 'n1'])
    n1ZHMC <- ceiling(df[df$sampler == 'ZHMC', 'n1'])
    n1ZNUTS <- ceiling(df[df$sampler == 'ZNUTS', 'n1'])

    n100HHMC <- ceiling(df[df$sampler == 'HHMC', 'n100'])
    n100ZHMC <- ceiling(df[df$sampler == 'ZHMC', 'n100'])
    n100ZNUTS <- ceiling(df[df$sampler == 'ZNUTS', 'n100'])

    n1000HHMC <- ceiling(df[df$sampler == 'HHMC', 'n1000'])
    n1000ZHMC <- ceiling(df[df$sampler == 'ZHMC', 'n1000'])
    n1000ZNUTS <- ceiling(df[df$sampler == 'ZNUTS', 'n1000'])

    if ((mtn == 'LKJ' && dimension > 100) || (mtn == 'HIV' && dimension > 400)) {
      n1MET <- 0
      n100MET <- 0
      n1000MET <- 0
    } else {
      n1MET <- 1
      n100MET <- 100
      n1000MET <- 1000
    }

    mtnParams <- prepareMTN(mtn, dimension, HIVdataFolder = HIVdataFolder)

    resT1 <- benchMarkTMVN(
      n1HHMC,
      n1ZHMC,
      n1ZNUTS,
      n1MET,
      repeatTimes = 3,
      forOneSampleFlg = T,
      meanVec = mtnParams$meanV,
      precMat = mtnParams$precM,
      covMat = mtnParams$covM,
      pInitial = mtnParams$p0,
      matF = mtnParams$direcHHMC,
      vecg = mtnParams$bdryHHMC,
      lb = mtnParams$lb,
      ub = mtnParams$ub
    )

    resT100 <- benchMarkTMVN(
      n100HHMC,
      n100ZHMC,
      n100ZNUTS,
      n100MET,
      repeatTimes = 3,
      forOneSampleFlg = F,
      meanVec = mtnParams$meanV,
      precMat = mtnParams$precM,
      covMat = mtnParams$covM,
      pInitial = mtnParams$p0,
      matF = mtnParams$direcHHMC,
      vecg = mtnParams$bdryHHMC,
      lb = mtnParams$lb,
      ub = mtnParams$ub
    )

    resT1000 <- benchMarkTMVN(
      n1000HHMC,
      n1000ZHMC,
      n1000ZNUTS,
      n1000MET,
      repeatTimes = 3,
      forOneSampleFlg = F,
      meanVec = mtnParams$meanV,
      precMat = mtnParams$precM,
      covMat = mtnParams$covM,
      pInitial = mtnParams$p0,
      matF = mtnParams$direcHHMC,
      vecg = mtnParams$bdryHHMC,
      lb = mtnParams$lb,
      ub = mtnParams$ub
    )

    timingT1 <- dplyr::bind_rows(resT1)
    timingT100 <- dplyr::bind_rows(resT100)
    timingT1000 <- dplyr::bind_rows(resT1000)

    saveRDS(timingT1,
            file = paste(output_path, 'timing', mtn, 'one_d', dimension, '.rds', sep = ''))
    saveRDS(timingT100,
            file = paste(output_path, 'timing', mtn, 'h_d', dimension, '.rds', sep = ''))
    saveRDS(timingT1000, file = paste(output_path, 'timing', mtn, 'k_d', dimension, '.rds', sep = ''))
  }
}

# Step3/3: organize the final results ####
rdsFiles <-
  list.files(output_path,
             pattern = paste('^timing.*\\.rds$', sep = ''),
             full.names = T)

for (mtn in mtnTypes) {
  resList <- list()
  for (i in 1:length(dimensionTested)) {
    dimension <- dimensionTested[i]
    
    # Function to safely read RDS files, returning NA dataframe if file doesn't exist
    safeReadRDS <- function(file_pattern) {
      file_path <- getFileByName(rdsFiles, c(as.character(dimension), file_pattern))
      if (length(file_path) == 0) {
        # Return empty dataframe if file doesn't exist
        return(data.frame(sampler = character(), elapsed = numeric()))
      }
      return(readRDS(file_path))
    }
    
    # Try to read each file, use empty dataframe if missing
    t1 <- safeReadRDS(paste(mtn, 'one_', sep = ''))
    t100 <- safeReadRDS(paste(mtn, 'h_', sep = ''))
    t1000 <- safeReadRDS(paste(mtn, 'k_', sep = ''))
    
    # Create dataframes even if empty
    if (nrow(t1) > 0) {
      t1 <- t1[, c('test', 'elapsed'), drop = FALSE]
      colnames(t1) <- c('sampler', 't')
    } else {
      t1 <- data.frame(sampler = character(), t = numeric())
    }
    
    if (nrow(t100) > 0) {
      t100 <- t100[, c('test', 'elapsed'), drop = FALSE]
      colnames(t100) <- c('sampler', 't')
    } else {
      t100 <- data.frame(sampler = character(), t = numeric())
    }
    
    if (nrow(t1000) > 0) {
      t1000 <- t1000[, c('test', 'elapsed'), drop = FALSE]
      colnames(t1000) <- c('sampler', 't')
    } else {
      t1000 <- data.frame(sampler = character(), t = numeric())
    }
    
    # Start with t1, merge with t100, then t1000
    if (nrow(t1) > 0) {
      subDF <- t1
      if (nrow(t100) > 0) {
        subDF <- merge(subDF, t100, by = c('sampler'), 
                       suffixes = paste(c('1_d', '100_d'), dimension, sep = ''),
                       all = TRUE, sort = FALSE)
      } else {
        # Add t100 column with NA
        subDF[[paste('t100_d', dimension, sep = '')]] <- NA
      }
    } else if (nrow(t100) > 0) {
      # If no t1 but have t100, start with t100
      subDF <- t100
      colnames(subDF)[2] <- paste('t100_d', dimension, sep = '')
      # Add t1 column with NA
      subDF[[paste('t1_d', dimension, sep = '')]] <- NA
      # Reorder columns
      subDF <- subDF[, c('sampler', paste('t1_d', dimension, sep = ''), 
                         paste('t100_d', dimension, sep = ''))]
    } else {
      # No data at all for this dimension
      subDF <- data.frame(sampler = character())
      subDF[[paste('t1_d', dimension, sep = '')]] <- numeric()
      subDF[[paste('t100_d', dimension, sep = '')]] <- numeric()
    }
    
    # Merge with t1000 if it exists
    if (nrow(t1000) > 0) {
      subDF <- merge(subDF, t1000, by = c('sampler'), all.x = TRUE, sort = FALSE)
    } else {
      # Add t1000 column with NA
      subDF[[paste('t1000_d', dimension, sep = '')]] <- NA
    }
    
    # Rename t1000 column
    if (paste('t1000_d', dimension, sep = '') %in% colnames(subDF)) {
      # Already named correctly
    } else if ('t' %in% colnames(subDF)) {
      colnames(subDF)[colnames(subDF) == 't'] <- paste('t1000_d', dimension, sep = '')
    }
    
    subDF[, 2:ncol(subDF)] <- lapply(subDF[, 2:ncol(subDF)], function(x) {
      if(is.numeric(x)) round(x, 3) else x
    })
    
    resList[[i]] <- subDF
  }
  
  # Combine results for all dimensions
  if (length(resList) > 0) {
    resForMTN <- resList[[1]]
    if (length(resList) > 1) {
      for (j in 2:length(resList)) {
        resForMTN <- merge(resForMTN, resList[[j]], by = "sampler", all = TRUE)
      }
    }
    
    # Ensure all 4 samplers are present
    all_samplers <- c("HHMC", "ZHMC", "ZNUTS", "MET")
    for (samp in all_samplers) {
      if (!samp %in% resForMTN$sampler) {
        na_row <- data.frame(sampler = samp)
        for (col in colnames(resForMTN)[-1]) {
          na_row[[col]] <- NA
        }
        resForMTN <- rbind(resForMTN, na_row)
      }
    }
    
    resForMTN$sampler <- factor(resForMTN$sampler, levels = all_samplers)
    resForMTN <- resForMTN[order(resForMTN$sampler), ]
    
    print(mtn)
    print(resForMTN)
    print(xtable(resForMTN, digits = 3)) # print latex table
  }
}



################## install/load required packages

# un-comment to install these packages, if necessary

# devtools::install_github("CSAFE-ISU/cmcR")
# devtools::install_github("heike/x3ptools")
# install.packages("tidyverse")

# these packages are for parallelization
# install.packages("future")
# install.packages("furrr")

library(tidyverse)
library(cmcR)
library(x3ptools)

################## Preprocess masked scans

# un-comment to parallelize code and speed up computation

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 5))


# furrr::future_walk(
walk(
  list.files("../wellerMasked",pattern = "x3p",full.names = TRUE,recursive = TRUE),
  function(file){

    ret <- str_split(file,"/")[[1]]

    ret <- str_remove(ret[length(ret)],"\\.x3p")

    dat <- x3p_read(file)

    dat$surface.matrix[t(as.matrix(dat$mask)) == "#CD7F32FF"] <- NA

    dat <- dat %>%
      cmcR::preProcess_removeTrend(statistic = "quantile",
                                   tau = .5,
                                   method = "fn") %>%
      cmcR::preProcess_gaussFilter() %>%
      cmcR::preProcess_removeTrend(statistic = "quantile",
                                   tau = .5,
                                   method = "fn") %>%
      cmcR::preProcess_gaussFilter() %>%
      x3p_sample() %>%
      preProcess_erode(region = "interior",morphRadius = round(50/4)) %>%
      preProcess_erode(region = "exterior",morphRadius = round(50/4))

    processedScan <- dat %>%
      cmcR:::preProcess_cropWS(croppingProp = .5)

    processedScan$mask <- NULL

    x3ptools::x3p_write(processedScan,file = paste0("../wellerProcessed/",ret,".x3p"))

  })

################## read processed scans into environment

scanNames <- list.files("../wellerProcessed/") %>%
  str_remove("\\.x3p")

wellerProcessed <- map(list.files("../wellerProcessed/",full.names = TRUE),
                       x3p_read)

################## perform all pairwise comparisons

if(!dir.exists("wellerComparisons")){
  dir.create("wellerComparisons")
}

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 6))
#
# furrr::future_walk(
walk(
  1:length(wellerProcessed),
  function(refInd){
    purrr::walk(refInd:length(wellerProcessed),
                function(targetInd){

                  refName <- scanNames[refInd]
                  targetName <- scanNames[targetInd]

                  reference <- wellerProcessed[[refInd]]
                  target <- wellerProcessed[[targetInd]]

                  print(paste0(refName," vs. ",targetName))

                  dat <- map_dfr(seq(-30,30,by = 3),
                                 function(theta){

                                   reference %>%
                                     cmcR::comparison_cellDivision(numCells = 64) %>%
                                     mutate(refMissingProp = cmcR::comparison_calcPropMissing(cellHeightValues),
                                            refMissingCount = map_dbl(cellHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
                                     filter(refMissingProp <= .85) %>%
                                     mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues,
                                                                                             target = target,
                                                                                             theta = theta,
                                                                                             regionSizeMultiplier = 9)) %>%
                                     mutate(targMissingProp = cmcR::comparison_calcPropMissing(regionHeightValues),
                                            targMissingCount = map_dbl(regionHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
                                     filter(targMissingProp <= .85) %>%
                                     dplyr::mutate(cellHeightValues = comparison_standardizeHeights(.data$cellHeightValues),
                                                   regionHeightValues = comparison_standardizeHeights(.data$regionHeightValues)) %>%
                                     dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(.data$cellHeightValues),
                                                   regionHeightValues_replaced = comparison_replaceMissing(.data$regionHeightValues)) %>%
                                     dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = .data$cellHeightValues_replaced,
                                                                                   regionHeightValues = .data$regionHeightValues_replaced)) %>%
                                     dplyr::mutate(alignedTargetCell = comparison_alignedTargetCell(cellHeightValues,regionHeightValues,fft_ccf_df)) %>%
                                     mutate(jointlyMissing = map2_dbl(cellHeightValues,alignedTargetCell,~ sum(is.na(.x$surface.matrix) & is.na(.y$surface.matrix))),
                                            pairwiseCompCor = map2_dbl(cellHeightValues,alignedTargetCell,
                                                                       ~ cor(c(.x$surface.matrix),c(.y$surface.matrix),
                                                                             use = "pairwise.complete.obs"))) %>%
                                     tidyr::unnest(.data$fft_ccf_df) %>%
                                     select(cellIndex,
                                            refMissingProp,refMissingCount,
                                            targMissingProp,targMissingCount,
                                            fft_ccf,x,y,pairwiseCompCor,
                                            jointlyMissing) %>%
                                     mutate(comparisonName = paste0(refName," vs. ",targetName),
                                            theta = theta,
                                            direction = "refToTarget")

                                 })

                  dat1 <-
                    map_dfr(seq(-30,30,by = 3),
                            function(theta){

                              target %>%
                                cmcR::comparison_cellDivision(numCells = 64) %>%
                                mutate(refMissingProp = cmcR::comparison_calcPropMissing(cellHeightValues),
                                       refMissingCount = map_dbl(cellHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
                                filter(refMissingProp <= .85) %>%
                                mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues,
                                                                                        target = reference,
                                                                                        theta = theta,
                                                                                        regionSizeMultiplier = 9)) %>%
                                mutate(targMissingProp = cmcR::comparison_calcPropMissing(regionHeightValues),
                                       targMissingCount = map_dbl(regionHeightValues,~ sum(is.na(.$surface.matrix)))) %>%
                                filter(targMissingProp <= .85) %>%
                                dplyr::mutate(cellHeightValues = comparison_standardizeHeights(.data$cellHeightValues),
                                              regionHeightValues = comparison_standardizeHeights(.data$regionHeightValues)) %>%
                                dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(.data$cellHeightValues),
                                              regionHeightValues_replaced = comparison_replaceMissing(.data$regionHeightValues)) %>%
                                dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = .data$cellHeightValues_replaced,
                                                                              regionHeightValues = .data$regionHeightValues_replaced)) %>%
                                dplyr::mutate(alignedTargetCell = comparison_alignedTargetCell(cellHeightValues,regionHeightValues,fft_ccf_df)) %>%
                                mutate(jointlyMissing = map2_dbl(cellHeightValues,alignedTargetCell,~ sum(is.na(.x$surface.matrix) & is.na(.y$surface.matrix))),
                                       pairwiseCompCor = map2_dbl(cellHeightValues,alignedTargetCell,
                                                                  ~ cor(c(.x$surface.matrix),c(.y$surface.matrix),
                                                                        use = "pairwise.complete.obs"))) %>%
                                tidyr::unnest(.data$fft_ccf_df) %>%
                                select(cellIndex,
                                       refMissingProp,refMissingCount,
                                       targMissingProp,targMissingCount,
                                       fft_ccf,x,y,pairwiseCompCor,
                                       jointlyMissing) %>%
                                mutate(comparisonName = paste0(refName," vs. ",targetName),
                                       theta = theta,
                                       direction = "targetToRef")

                            })

                  compData <- bind_rows(dat,dat1)

                  save(compData,file = paste0("wellerComparisons/",refName,"_vs_",targetName,".RData"))

                })
  })

################## calculate the Congruent Matching Cells across a variety of
################## processing conditions

if(!dir.exists("wellerCMCs")){
  dir.create("wellerCMCs")
}

source("supplementaryFunctions.R")

cmcCalced <- list.files("wellerCMCs/")

cmcNeedCalc <- list.files("wellerComparisons/")

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 6))

# furrr::future_walk(
walk(
  cmcNeedCalc[!(cmcNeedCalc %in% cmcCalced)],
  function(fileName){

    print(fileName)

    load(paste0("wellerComparisons/",fileName))

    CMCs <- calcCMC_grid(comparisonData = compData)

    compName <- str_remove(fileName,".RData")

    save(CMCs,file = paste0("wellerCMCs/",compName,".RData"))

  })

################## calculate the CMC count similarity scores

if(!dir.exists("wellerCombinedCMCs")){
  dir.create("wellerCombinedCMCs")
}

cmcCalced <- list.files("wellerCombinedCMCs/")

cmcNeedCalc <- list.files("wellerCMCs/")

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 6))

# furrr::future_walk(
walk(
  paste0("wellerCMCs/",cmcNeedCalc[!(cmcNeedCalc %in% cmcCalced)]),
  ~ {

    load(.)

    combinedCMCs <- CMCs %>%
      group_by(comparisonName,xThresh,corrThresh,thetaThresh) %>%
      group_split() %>%
      map_dfr(function(dat){

        dat1 <- cmcR::decision_combineDirections(reference_v_target_CMCs = dat %>%
                                                   filter(direction == "refToTarget"),
                                                 target_v_reference_CMCs = dat %>%
                                                   filter(direction == "targetToRef"),
                                                 compareThetas = FALSE)

        ret <- dat %>%
          select(c(comparisonName,xThresh,corrThresh,thetaThresh)) %>%
          distinct() %>%
          tidyr::separate(remove = FALSE,col = comparisonName,
                          sep = " vs. ",into = c("reference","target")) %>%
          mutate(originalMethod_refToTarget = nrow(dat1$originalMethodCMCs[[1]]),
                 originalMethod_targetToRef = nrow(dat1$originalMethodCMCs[[2]]),
                 highCMC = dat1$highCMCs %>%
                   select(-direction) %>%
                   distinct() %>%
                   nrow())

        return(ret)
      })

    save(combinedCMCs,
         file = paste0("wellerCombinedCMCs/",
                       unique(combinedCMCs$comparisonName),".RData"))

  })

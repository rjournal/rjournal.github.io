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
  list.files("../fadulMasked/",pattern = "*.x3p",
             full.names = TRUE,recursive = TRUE),
  function(file){

    dat <- x3p_read(file)

    scanName <- file %>%
      str_remove("../fadulMasked/") %>%
      str_remove("\\.x3p")

    processedScan <- dat %>%
      cmcR::preProcess_crop(region = "exterior",
                            offset  = -30,
                            method = "legacy") %>%
      cmcR::preProcess_crop(region = "interior",
                            offset = 200) %>%
      cmcR::preProcess_removeTrend(statistic = "quantile",
                                   tau = .5,
                                   method = "fn") %>%
      cmcR::preProcess_gaussFilter() %>%
      x3ptools::x3p_sample(m=2)

    processedScan$mask <- NULL

    x3p_write(processedScan,file = paste0("../fadulProcessed/",scanName,".x3p"))

  })

################## read processed scans into environment

fadulProcessed <- map(list.files("../fadulProcessed/",full.names = TRUE),
                      x3p_read)

matchDictionary <-  data.frame(scanName = c('Fadul 1-1','Fadul 1-2','Fadul F','Fadul 10-1','Fadul 10-2','Fadul E','Fadul L','Fadul 2-1','Fadul 2-2','Fadul C','Fadul M','Fadul 3-1','Fadul 3-2','Fadul A','Fadul T','Fadul Z','Fadul 4-1','Fadul 4-2','Fadul B','Fadul 5-1','Fadul 5-2','Fadul H','Fadul N','Fadul 6-1','Fadul 6-2','Fadul I','Fadul O','Fadul 7-1','Fadul 7-2','Fadul R','Fadul S','Fadul Y','Fadul 8-1','Fadul 8-2','Fadul P','Fadul Q','Fadul X','Fadul 9-1','Fadul 9-2','Fadul K'),
                               barrelNum = c(1,1,1,10,10,10,10,2,2,2,2,3,3,3,3,3,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,7,8,8,8,8,8,9,9,9))
scanNames <- matchDictionary %>%
  arrange(scanName) %>%
  pull(scanName)

################## perform all pairwise comparisons

if(!dir.exists("comparisonResults")){
  dir.create("comparisonResults")
}

# un-comment to parallelize code and speed up computation

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 5))

# furrr::future_walk(
walk(
  1:(length(fadulProcessed) - 1),
  function(refInd){
    map((refInd + 1):length(fadulProcessed),
        function(targInd){

          ref <- fadulProcessed[[refInd]]
          targ <- fadulProcessed[[targInd]]

          CMCs <- purrr::map_dfr(seq(-30,30,by = 3),
                                 function(theta){

                                   comparison_cellDivision(ref,c(8,8)) %>%
                                     dplyr::mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues = .data$cellHeightValues,
                                                                                                    target = targ,
                                                                                                    theta = theta,
                                                                                                    method = "legacy")) %>%
                                     dplyr::mutate(cellPropMissing = comparison_calcPropMissing(.data$cellHeightValues),
                                                   regionPropMissing = comparison_calcPropMissing(.data$regionHeightValues)) %>%
                                     dplyr::filter(.data$cellPropMissing <= .9 & .data$regionPropMissing <= .9) %>%
                                     dplyr::mutate(cellHeightValues = comparison_standardizeHeights(.data$cellHeightValues),
                                                   regionHeightValues = comparison_standardizeHeights(.data$regionHeightValues)) %>%
                                     dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(.data$cellHeightValues),
                                                   regionHeightValues_replaced = comparison_replaceMissing(.data$regionHeightValues)) %>%
                                     dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = .data$cellHeightValues_replaced,
                                                                                   regionHeightValues = .data$regionHeightValues_replaced)) %>%
                                     dplyr::mutate(pairwiseCompCor = comparison_cor(.data$cellHeightValues,.data$regionHeightValues,.data$fft_ccf_df)) %>%
                                     tidyr::unnest(.data$fft_ccf_df) %>%
                                     dplyr::select(.data$cellIndex,.data$x,.data$y,.data$fft_ccf,.data$pairwiseCompCor) %>%
                                     dplyr::mutate(theta = theta)

                                 })  %>%
            mutate(comparisonName = paste0(scanNames[refInd]," vs. ",scanNames[targInd]),
                   direction = "reference_vs_target")

          CMCs1 <- purrr::map_dfr(seq(-30,30,by = 3),
                                  function(theta){

                                    comparison_cellDivision(targ,c(8,8)) %>%
                                      dplyr::mutate(regionHeightValues = comparison_getTargetRegions(cellHeightValues = .data$cellHeightValues,
                                                                                                     target = ref,
                                                                                                     theta = theta,
                                                                                                     method = "legacy")) %>%
                                      dplyr::mutate(cellPropMissing = comparison_calcPropMissing(.data$cellHeightValues),
                                                    regionPropMissing = comparison_calcPropMissing(.data$regionHeightValues)) %>%
                                      dplyr::filter(.data$cellPropMissing <= .9 & .data$regionPropMissing <= .9) %>%
                                      dplyr::mutate(cellHeightValues = comparison_standardizeHeights(.data$cellHeightValues),
                                                    regionHeightValues = comparison_standardizeHeights(.data$regionHeightValues)) %>%
                                      dplyr::mutate(cellHeightValues_replaced = comparison_replaceMissing(.data$cellHeightValues),
                                                    regionHeightValues_replaced = comparison_replaceMissing(.data$regionHeightValues)) %>%
                                      dplyr::mutate(fft_ccf_df = comparison_fft_ccf(cellHeightValues = .data$cellHeightValues_replaced,
                                                                                    regionHeightValues = .data$regionHeightValues_replaced)) %>%
                                      dplyr::mutate(pairwiseCompCor = comparison_cor(.data$cellHeightValues,.data$regionHeightValues,.data$fft_ccf_df)) %>%
                                      tidyr::unnest(.data$fft_ccf_df) %>%
                                      dplyr::select(.data$cellIndex,.data$x,.data$y,.data$fft_ccf,.data$pairwiseCompCor) %>%
                                      dplyr::mutate(theta = theta)

                                  })  %>%
            mutate(comparisonName = paste0(scanNames[refInd]," vs. ",scanNames[targInd]),
                   direction = "target_vs_reference")

          compData <- bind_rows(CMCs,CMCs1)

          save(compData,file = paste0("comparisonResults/",scanNames[refInd]," vs. ",scanNames[targInd],".RData"))

        })
  })


################## calculate the Congruent Matching Cells across a variety of
################## processing conditions

if(!dir.exists("cmcResults")){
  dir.create("cmcResults")
}

# un-comment to parallelize code and speed up computation

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 5))


# furrr::future_walk(
walk(
  1:(length(fadulProcessed) - 1),
  function(refInd){
    walk((refInd + 1):length(fadulProcessed),
         function(targInd){

           load(paste0("comparisonResults/",scanNames[refInd]," vs. ",scanNames[targInd],".RData"))

           cmcResults <- map_dfr(
             cross(list("x" = seq(5,30,by = 5),"theta" = c(3,6),"corr" = seq(.35,.6,by = .05))),
             function(params){

               compData %>%
                 group_by(direction) %>%
                 mutate(originalMethod = decision_CMC(cellIndex = cellIndex,x=x,y=y,theta=theta,corr=pairwiseCompCor,
                                                      xThresh = params$x,thetaThresh = params$theta,corrThresh = params$corr),
                        highCMC = decision_CMC(cellIndex = cellIndex,x=x,y=y,theta=theta,corr=pairwiseCompCor,
                                               xThresh = params$x,thetaThresh = params$theta,corrThresh = params$corr,tau = 1)) %>%
                 mutate(xThresh = params$x,thetaThresh = params$theta,corrThresh = params$corr)

             })

           save(cmcResults,file = paste0("cmcResults/",scanNames[refInd]," vs. ",scanNames[targInd],".RData"))

         })
  })

################## calculate the CMC count similarity scores

if(!dir.exists("combinedCMC")){
  dir.create("combinedCMC")
}


# un-comment to parallelize code and speed up computation

# future:::ClusterRegistry("stop")
#
# future::plan(future::multisession(workers = future::availableCores() - 5))

# furrr::future_walk(
walk(
  1:(length(fadulProcessed) - 1),
  function(refInd){
    walk((refInd + 1):length(fadulProcessed),
         function(targInd){

           load(paste0("cmcResults/",scanNames[refInd]," vs. ",scanNames[targInd],".RData"))

           combinedCMC <- cmcResults %>%
             group_by(comparisonName,xThresh,thetaThresh,corrThresh) %>%
             group_split() %>%
             map_dfr(function(dat){

               ret <- cmcR::decision_combineDirections(reference_v_target_CMCs = dat %>%
                                                         filter(direction == "reference_vs_target") %>%
                                                         rename(originalMethodClassif = originalMethod,
                                                                highCMCClassif = highCMC),
                                                       target_v_reference_CMCs = dat %>%
                                                         filter(direction == "target_vs_reference") %>%
                                                         rename(originalMethodClassif = originalMethod,
                                                                highCMCClassif = highCMC),
                                                       missingThetaDecision = "fail")


               combinedCMC <- data.frame(comparisonName = unique(dat$comparisonName),
                                         xThresh = unique(dat$xThresh),
                                         thetaThresh = unique(dat$thetaThresh),
                                         corrThresh = unique(dat$corrThresh),
                                         original_rToT = nrow(ret[[1]][[1]]),
                                         original_tToR = nrow(ret[[1]][[2]]),
                                         highCMC = nrow(ret[[2]]))

               return(combinedCMC)

             })

           save(combinedCMC,file = paste0("combinedCMC/",scanNames[refInd]," vs. ",scanNames[targInd],".RData"))

         })
  })

################## visualize CMC counts as a histogram

cmcCounts <- map_dfr(list.files("combinedCMC/",full.names = TRUE),
                     function(fileName){

                       load(fileName)

                       return(combinedCMC)

                     })

cmcCounts %>%
  filter(xThresh == 20 & thetaThresh == 6 & corrThresh == .5) %>%
  tidyr::separate(col = "comparisonName",into = c("reference","target"),sep = " vs. ",remove = FALSE) %>%
  left_join(matchDictionary,by = c("reference" = "scanName")) %>%
  rename(refBarrel = barrelNum) %>%
  left_join(matchDictionary,by = c("target" = "scanName")) %>%
  rename(targBarrel = barrelNum) %>%
  mutate(type = ifelse(refBarrel == targBarrel,"match","non-match")) %>%
  ggplot(aes(x=highCMC,y=..density..,fill = type)) +
  geom_histogram(binwidth = 1)

cmcCounts %>%
  filter(xThresh == 20 & thetaThresh == 6 & corrThresh == .5) %>%
  group_by(comparisonName) %>%
  mutate(originalMethod = min(c(original_rToT,original_tToR))) %>%
  tidyr::separate(col = "comparisonName",into = c("reference","target"),sep = " vs. ",remove = FALSE) %>%
  left_join(matchDictionary,by = c("reference" = "scanName")) %>%
  rename(refBarrel = barrelNum) %>%
  left_join(matchDictionary,by = c("target" = "scanName")) %>%
  rename(targBarrel = barrelNum) %>%
  mutate(type = ifelse(refBarrel == targBarrel,"match","non-match")) %>%
  ggplot(aes(x=originalMethod,y=..density..,fill = type)) +
  geom_histogram(binwidth = 1)

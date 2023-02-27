calcCMC_grid <- function(comparisonData,translationThresh = seq(5,25,by = 5),
                         corrThresh = seq(.3,.6,by = .05),thetaThresh = 6,tau = 1,
                         translationThresh_y = translationThresh){

  if(!any(is.na(comparisonData$cellIndex))){

    if(all(translationThresh == translationThresh_y)){

      CMCs <-
        purrr::map_dfr(cross(.l = list("trans" = translationThresh,
                                       "corr" = corrThresh,
                                       "theta" = thetaThresh,
                                       "tau" = tau)),
                       function(thresholds){

                         comparisonData %>%
                           group_by(comparisonName,
                                    direction) %>%
                           mutate(originalMethodClassif = decision_CMC(cellIndex = cellIndex,
                                                                       x = x,
                                                                       y = y,
                                                                       theta = theta,
                                                                       corr = pairwiseCompCor,
                                                                       xThresh = thresholds$trans,
                                                                       yThresh = thresholds$trans,
                                                                       corrThresh = thresholds$corr,
                                                                       thetaThresh = thresholds$theta),
                                  highCMCClassif = decision_CMC(cellIndex = cellIndex,
                                                                x = x,
                                                                y = y,
                                                                theta = theta,
                                                                corr = pairwiseCompCor,
                                                                xThresh = thresholds$trans,
                                                                yThresh = thresholds$trans,
                                                                corrThresh = thresholds$corr,
                                                                thetaThresh = thresholds$theta,
                                                                tau = thresholds$tau)) %>%
                           ungroup() %>%
                           mutate(xThresh = thresholds$trans,
                                  yThresh = thresholds$trans,
                                  corrThresh = thresholds$corr,
                                  thetaThresh = thresholds$theta,
                                  highCMCThresh = thresholds$tau)

                       })

    }

    else{

      CMCs <-
        purrr::map_dfr(cross(.l = list("trans_x" = translationThresh,
                                       "trans_y" = translationThresh_y,
                                       "corr" = corrThresh,
                                       "theta" = thetaThresh,
                                       "tau" = tau)),
                       function(thresholds){

                         comparisonData %>%
                           group_by(comparisonName,
                                    direction) %>%
                           group_split() %>%
                           map_dfr(~ {

                             mutate(.,originalMethodClassif = decision_CMC(cellIndex = cellIndex,
                                                                           x = x,
                                                                           y = y,
                                                                           theta = theta,
                                                                           corr = pairwiseCompCor,
                                                                           xThresh = thresholds$trans_x,
                                                                           yThresh = thresholds$trans_y,
                                                                           corrThresh = thresholds$corr,
                                                                           thetaThresh = thresholds$theta),
                                    highCMCClassif = decision_CMC(cellIndex = cellIndex,
                                                                  x = x,
                                                                  y = y,
                                                                  theta = theta,
                                                                  corr = pairwiseCompCor,
                                                                  xThresh = thresholds$trans_x,
                                                                  yThresh = thresholds$trans_y,
                                                                  corrThresh = thresholds$corr,
                                                                  thetaThresh = thresholds$theta,
                                                                  tau = thresholds$tau)) %>%
                               ungroup() %>%
                               mutate(xThresh = thresholds$trans_x,
                                      yThresh = thresholds$trans_y,
                                      corrThresh = thresholds$corr,
                                      thetaThresh = thresholds$theta,
                                      highCMCThresh = thresholds$tau)

                           })

                       })

    }
  }
  else{
    CMCs <- comparisonData %>%
      mutate(originalMethodCount = NA,
             highCMCCount = NA,
             type = "match",
             xThresh = NA,
             yThresh = NA,
             corrThresh = NA,
             thetaThresh = NA)
  }

  return(CMCs)

}

calcVarianceRatio <- function(cmcData,similarityCol = "cmcCount"){
  grand_similarityColAverage <- mean(unlist(cmcData[,similarityCol]))

  withinGroup_similarityCol <- cmcData %>%
    group_by(type) %>%
    summarise(similarityColAverage = mean(!!as.name(similarityCol)),
              similarityColVar = var(!!as.name(similarityCol)),
              .groups = "drop")

  betweenGroupVariability <- withinGroup_similarityCol %>%
    mutate(similarityColSS = (similarityColAverage - grand_similarityColAverage)^2) %>%
    pull(similarityColSS) %>%
    sum()

  withinGroupVariability <- withinGroup_similarityCol %>%
    pull(similarityColVar) %>%
    sum()

  cmcData <- cmcData %>%
    mutate(varRatio = betweenGroupVariability/withinGroupVariability)

  return(cmcData)
}

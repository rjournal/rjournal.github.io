# Takes in the changepoint technique to apply, and the dataset to apply it to.
#   The dataset should be for a single site/phenomena, as the function treats
#   it like a single timeseries.  The output is a dataframe with columns for 
#   the differing changepoint output values of mean, variance etc.

# known issues: doesn't deal with ranges of pentalty values (e.g. CROPS)
createChangePointDataset = function(currTechnique, 
                                    currMethod, 
                                    currTestStatistic,
                                    currPenaltyType,
                                    currPenaltyVal,
                                    currMinSegLen,
                                    currData){
  tryCatch({
    # Perform the changepoint analysis
    if (currTechnique == 'cpt.mean') {
      if (currPenaltyType %in% c('Asymptotic',
                                 'CROPS')) {
        cptOutput = cpt.mean(currData$value, 
                             method = currMethod,
                             test.stat = currTestStatistic,
                             penalty = currPenaltyType,
                             pen.value = currPenaltyVal,
                             minseglen = currMinSegLen)
      }else {
        cptOutput = cpt.mean(currData$value, 
                             method = currMethod,
                             test.stat = currTestStatistic,
                             penalty = currPenaltyType,
                             minseglen = currMinSegLen)      
      }
    }else if (currTechnique == 'cpt.var') {
      if (currPenaltyType %in% c('Asymptotic',
                                 'CROPS')) {
        cptOutput = cpt.var(currData$value, 
                            method = currMethod,
                            test.stat = currTestStatistic,
                            penalty = currPenaltyType,
                            pen.value = currPenaltyVal,
                            minseglen = currMinSegLen)
      }else {
        cptOutput = cpt.var(currData$value, 
                            method = currMethod,
                            test.stat = currTestStatistic,
                            penalty = currPenaltyType,
                            minseglen = currMinSegLen)      
      }
    }else if (currTechnique == 'cpt.meanvar') {
      if (currPenaltyType %in% c('Asymptotic',
                                 'CROPS')) {
        cptOutput = cpt.meanvar(currData$value, 
                                method = currMethod,
                                test.stat = currTestStatistic,
                                penalty = currPenaltyType,
                                pen.value = currPenaltyVal,
                                minseglen = currMinSegLen)
      }else {
        cptOutput = cpt.meanvar(currData$value, 
                                method = currMethod,
                                test.stat = currTestStatistic,
                                penalty = currPenaltyType,
                                minseglen = currMinSegLen)      
      }
    }
    
    # Create the changepoint columns that will be updated based on the technique
    #  applied
    currData$mean = 0
    currData$meanGroup = 0
    currData$var = 0
    currData$varGroup = 0
    
    # handle CROPS cpt ranges, temporoary 20200817
    # if (nrow(cptOutput@cpts.full) >1) {
    #   cptOutput@cpts = cptOutput@cpts.full[1,]
    # }
    
    # For every identified changepoint
    if (length(cptOutput@cpts) > 1) {
      for (cpt in 1:length(cptOutput@cpts)) {
        # If it is the first changepoint, it provides the position in the timeseries where
        #  the change occurs, relative to the start of the dataframe.
        if (cpt == 1) {
          if (currTechnique == 'cpt.mean' | 
              currTechnique == 'cpt.meanvar') {
            currData$mean[1:cptOutput@cpts[cpt]] = cptOutput@param.est$mean[cpt]
            currData$meanGroup[1:cptOutput@cpts[cpt]] = cpt                  
          }
          if (currTechnique == 'cpt.var' |
              currTechnique == 'cpt.meanvar') {
            currData$var[1:cptOutput@cpts[cpt]] = cptOutput@param.est$variance[cpt]
            currData$varGroup[1:cptOutput@cpts[cpt]] = cpt                  
          }     
        }
        # If it is the last changepoint, it provides the position in the timeseries where the
        #  change occurs, relative to the end of the dataframe
        else if (cpt == length(cptOutput@cpts)) {
          if (currTechnique == 'cpt.mean' | 
              currTechnique == 'cpt.meanvar') {
            currData$mean[cptOutput@cpts[cpt - 1]:dim(currData)[1]] = cptOutput@param.est$mean[cpt]
            currData$meanGroup[cptOutput@cpts[cpt - 1]:dim(currData)[1]] = cpt                  
          }
          
          if (currTechnique == 'cpt.var' |
              currTechnique == 'cpt.meanvar') {
            currData$var[cptOutput@cpts[cpt - 1]:dim(currData)[1]] = cptOutput@param.est$variance[cpt]
            currData$varGroup[cptOutput@cpts[cpt - 1]:dim(currData)[1]] = cpt                  
          }     
        }else{
          # If it is neither the first nor last changepoint, it provides the position in the timeseries
          #  where the change occurs, relative to the preceeding and succeeding changepoint positions
          if (currTechnique == 'cpt.mean' | 
              currTechnique == 'cpt.meanvar') {
            currData$mean[cptOutput@cpts[cpt - 1]:cptOutput@cpts[cpt]] = cptOutput@param.est$mean[cpt]
            currData$meanGroup[cptOutput@cpts[cpt - 1]:cptOutput@cpts[cpt]] = cpt                  
          }
          
          if (currTechnique == 'cpt.var' |
              currTechnique == 'cpt.meanvar') {
            currData$var[cptOutput@cpts[cpt - 1]:cptOutput@cpts[cpt]] = cptOutput@param.est$variance[cpt]
            currData$varGroup[cptOutput@cpts[cpt - 1]:cptOutput@cpts[cpt]] = cpt                  
          }     
        }
      }
    }
    # If there is only one change point, this provides only one mean, and or variance entry, which should
    #  be applied to the whole series.  It cannot be used to split a dataset into two parts, if there is
    #  only one set of mean, variance values.
    else{
      if (currTechnique == 'cpt.mean' | 
          currTechnique == 'cpt.meanvar') {
        currData$mean = cptOutput@param.est$mean[1]
        currData$meanGroup = 1              
      }
      
      if (currTechnique == 'cpt.var' |
          currTechnique == 'cpt.meanvar') {
        currData$var = cptOutput@param.est$variance[1]
        currData$varGroup = 1 
      }
    }
    
    currData$mean = round(currData$mean,2)
    currData$meanGroup = factor(currData$meanGroup)
    currData$var = factor(round(currData$var,2))
    currData$varGroup = factor(currData$varGroup)
    
    return(currData)
  },
  error = function(e){
    currData$mean = 0
    currData$meanGroup = 0
    currData$var = 0
    currData$varGroup = 0
    return(currData)
  })
  
}

# This uses the data returned by createChangePointDataset above to create a plot output
#  dependent on the technique that's been applied
createChangePointPlot = function(currTechnique, currData){
  
  print(head(currData))
  if (currTechnique == "cpt.mean") {
    p = ggplot(currData) +
      geom_line(aes(x = temporalField, y = value, colour = 'red')) +
      scale_x_date() +
      geom_line(aes(x = temporalField, y = mean, group = meanGroup)) +
      ggtitle(str_c("Changepoints of ", currData$featureLabel[1], " using ", currTechnique)) +
      ylab("Value") + xlab('Date') +
      theme(legend.position = 'none',
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold"))
    
  }else if (currTechnique == 'cpt.var') {
    # https://stackoverflow.com/questions/47754080/add-vertical-line-to-ggplotly-plot
    #a = (cptData %>% group_by(varGroup) %>% slice(1) %>% ungroup() %>% select(temporalField) %>% slice(-1))
    p = ggplot(currData) +
      geom_line(aes(x = temporalField, y = value, colour = var, group = varGroup)) +
      #geom_vline(data = a, aes(xintercept =  as.Date(temporalField) ) ) +
      scale_x_date() +
      ggtitle(str_c("Changepoints of ", currData$featureLabel[1], " using ", currTechnique)) +
      ylab("Value") + xlab('Date') +
      guides(fill = FALSE) +
      theme(legend.position = 'none',
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold"))
    
  }else if (currTechnique == 'cpt.meanvar') { 
    p = ggplot(currData) +
      geom_line(aes(x = temporalField, y = value, colour = var, group = varGroup)) +
      scale_x_date() +
      geom_line(aes(x = temporalField, y = mean, group = meanGroup)) +
      ggtitle(str_c("Changepoints of ", currData$featureLabel[1], " using ", currTechnique)) +
      ylab("Value") + xlab('Date') +
      guides(fill = FALSE) +
      theme(legend.position = 'none',
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold"))
  }
  return(p)
}


cpts_confint <- function(x_series,x_cpts,N_reps){
  
  #Create some arrays to hold the changepoint confidence intervals.
  cpts_lwr <- rep(0,length(x_cpts))
  cpts_upr <- rep(0,length(x_cpts))
  
  #Define the number of changepoints.
  n_cpts <- length(x_cpts)
  
  #Define a function that bootstraps the cpts for each segment.
  cpt_boot <- function(seg_1,seg_2){
    #Gernerate new samples for each segment with replacement.
    seg_1_boot <- sample(seg_1, length(seg_1), replace=TRUE)
    seg_2_boot <- sample(seg_2, length(seg_2), replace=TRUE)
    #Now estimate the new changepoint (fix at 1 using AMOC).
    cpt_samp <- cpt.meanvar(c(seg_1_boot,seg_2_boot), method='AMOC', penalty='MBIC')
    return(cpts(cpt_samp))
  }
  
  #Now loop over the changepoints and estimate the confidence intervals based on bootstrap sample.
  for (ll in 1:n_cpts){
    
    #Get the segments for the current sample.
    if(ll == 1){
      x1_l <- 1
      x1_r <- x_cpts[ll]
      x2_l <- x_cpts[ll]+1
      x2_r <- x_cpts[ll+1]
    } else if (ll == n_cpts){
      x1_l <- x_cpts[ll-1]+1
      x1_r <- x_cpts[ll]
      x2_l <- x_cpts[ll]+1
      x2_r <- length(x_series)
    } else {
      x1_l <- x_cpts[ll-1]+1
      x1_r <- x_cpts[ll]
      x2_l <- x_cpts[ll]+1
      x2_r <- x_cpts[ll+1]
    }
    
    #Extract the segments from the time series.
    seg_1_proc <- x_series[x1_l:x1_r]
    seg_2_proc <- x_series[x2_l:x2_r]
    
    #Create an array to hold the boot strap sample.
    summary_cpts <- numeric()
    #Boot strap the new changepoint locations.
    for (mm in 1:N_reps){
      summary_cpts <- append(summary_cpts,cpt_boot(seg_1_proc,seg_2_proc))
    }
    
    #Get the quantiles of the samples (based on chosen level). 
    #As we are working with subsamples will need to make sure we add the baseline index of rht 1st segment
    #here to correctly locate the bootstrap changes points.
    cpts_CI <- as.integer(quantile(summary_cpts,c(0.025,0.975)))+(x1_l-1)
    
    #Add to the main arrays.	
    cpts_lwr[ll] <- cpts_CI[1]
    cpts_upr[ll] <- cpts_CI[2]
    
  }
  
  #Combine the results into a data.frame to output.
  cpts_CI_all        <- data.frame(x_cpts,cpts_lwr,cpts_upr)
  names(cpts_CI_all) <- c('cpt','ci.left','ci.right')
  return(cpts_CI_all)
  
}





  
# Compares density estimates to their known distributions, using the
# chi-squared metric in the philentropy package, with an optional plot.

compareEstimates = function(distribution, sampleSize, 
                             types = c("pdfe", "kde", "kernsmooth", "np"),
                             colorTypes = c("cyan4", "navy", "orangered",  "deeppink4"),
                             trials = 1, methodName = "squared_chi",
                             plotDistribution = FALSE, ylim = NULL) {
  
  exact = berdev(distribution)                              #generate random sample 
  left = min(exact$support)                                 #set boundaries if they
  right = max(exact$support)                                # exist (can be infinite)
  
  clockTime = vector(length = length(types))
  accuracy = vector(length = length(types))
  
  if (plotDistribution) {
    trials = 1
  }
  
  for (iTrial in 1:trials) {                                #averaged of all trials
    sample = rberdev(sampleSize, dnum = distribution)
    binLength = NULL
    
    for (iType in 1:length(types)) {
      type = types[iType]
      if (type == "np") {
        if (sampleSize > 1000) {
          next
        }
      }
      
      estimate = getEstimate(type, sample, 
                             binLength = binLength, 
                             left = left, right = right)
      if (iType == 1) {                                     #set bin length to the  
        binLength = length(estimate$x)                      # length returned by pdfe
      } 
    
      
      pdfExact = dberdev(estimate$x, dnum = distribution)   #remove infinite values
      finite = which(!is.infinite(pdfExact))
      pdfExact = pdfExact[finite]
      estimate$x = estimate$x[finite]
      estimate$y = estimate$y[finite]
      
      
      idx = vector()                                        #remove discontinuities
      for (cBreak in 1:length(exact$breaks)) {              # from comparison 
        if (!is.null(exact$breaks[cBreak])) {
          for(j in 1:length(estimate$x)) {
            if(abs(exact$breaks[cBreak] - estimate$x[j]) < 0.00001) {
              idx = c(idx, j)
            }
          }
        }
      }
      idx = unique(idx)
      idxAll = 1:length(estimate$x)
      idxGood = idxAll[!idxAll %in% idx]
      estimate$x = estimate$x[idxGood]
      estimate$y = estimate$y[idxGood]
      pdfExact = pdfExact[idxGood]
      
      if (methodName != "") {
        pdfCompare = rbind(pdfExact/sum(pdfExact),      #perform comparison
                         estimate$y/sum(estimate$y))
        measure = distance(pdfCompare, method = methodName, 
                         test.na = FALSE, p = 1)
        clockTime[iType] = clockTime[iType] + estimate$time[3]
        accuracy[iType] = accuracy[iType] + measure
      }
      
      if (plotDistribution) {
        if (iType == 1) {
          if (!is.null(ylim)) {
            plot(x = estimate$x, y = pdfExact, type = "l", lwd = 2, ylim = ylim,
                 col = "darkgray", xlab = "sample range", ylab = "PDF")
          } else {
            plot(x = estimate$x, y = pdfExact, type = "l", lwd = 2, 
                 col = "darkgray", xlab = "sample range", ylab = "PDF")
          }
        }
        lines(estimate$x, estimate$y, lwd = 2, col = colorTypes[iType], 
              xlab = "", ylab = "")
      }
    }
  }
  return (paste(accuracy / trials, clockTime / trials))
}
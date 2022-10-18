# Estimates the distribution for sample data according to a given estimation 
# function type and returns a list of pertinant results.  Currently supports 
# PDFEstimator package, and three kernel density methods.  Other estimators
# can be added in the future.


getEstimate <- function(type, sample, binLength = NULL, left, right) {
  
  finiteLeft = TRUE                                         
  finiteRight = TRUE
  if ((left == -Inf) || (left == Inf)) {
    finiteLeft = FALSE  
  }
  if ((right == -Inf) ||(right == Inf)) {
    finiteRight = FALSE  
  }
  
  estimateData = list(x=NULL, y=NULL, time=NULL)
  estTime = proc.time()
  
  switch(type,
         "pdfe" = {
            debugOn = FALSE
            if (finiteLeft && finiteRight) {
              pdfe = estimatePDF(sample, lowerBound = left, 
                                 upperBound = right, outlierCutoff = 0, 
                                 debug = debugOn)
            } else {
              if (finiteRight) {
                pdfe = estimatePDF(sample, upperBound = right, 
                                   debug = debugOn)
              } else {
                if (finiteLeft) {
                  pdfe = estimatePDF(sample, lowerBound = left, 
                                     debug = debugOn) 
                } else {
                  pdfe = estimatePDF(sample, debug = debugOn)
                }
              }
            }
            estimateData$x = pdfe$x
            estimateData$y = pdfe$pdf
         },
         
         
         "kde" = {
           if (finiteLeft && finiteRight) {
             kde = density(sample, from = left, to = right, n = binLength)
           } else {
             if (finiteRight) {
               kde = density(sample, to = right, n = binLength) 
             } else {
               if (finiteLeft) {
                 kde = density(sample, from = left, n = binLength) 
               } else {
                 kde = density(sample, n = binLength)
               }
             }
           }
           estimateData$x = kde$x
           estimateData$y = kde$y
         },
         
         
         "np" = {
           dx = (max(sample) - min(sample)) / (binLength - 1)
           xPoints = seq(min(sample), max(sample), by = dx)
           np = npudens(sample, edat = xPoints)
           estimateData$x = xPoints
           estimateData$y = np$dens
         },
         
         
         "kernsmooth" = {
            lower = min(sample)
            upper = max(sample)
           
            if (finiteRight) {
              upper = right 
            } 
            if (finiteLeft) {
              lower = left 
            } 
           
           kern = bkde(sample, gridsize = binLength, range.x = c(lower, upper))
           
           estimateData$x = kern$x
           estimateData$y = kern$y
         },
         )
  
  estTime = proc.time() - estTime
  estimateData$time = estTime
  return(estimateData)
}
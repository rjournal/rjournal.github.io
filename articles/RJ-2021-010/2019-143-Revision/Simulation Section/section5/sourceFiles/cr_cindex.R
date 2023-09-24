cr_cidx <- function(ftime, fstatus, eta) {
  ftime   <- ifelse(fstatus > 1, Inf, ftime) #Treat competing risks as immortal time
  fstatus <- ifelse(fstatus > 1, 0, fstatus)
  #Similar to cindex from dynpred package.
  n       <- length(ftime)
  ord     <- order(ftime, -fstatus)
  ftime   <- ftime[ord]
  fstatus <- fstatus[ord]
  eta     <- eta[ord]
  d1      <- which(fstatus == 1)
  total   <- concordant <- 0
  for (i in d1) {
    for (j in ((i + 1):n)) {
      if (ftime[j] > ftime[i]) {
        total <- total + 1
        if (eta[j] < eta[i]) 
          concordant <- concordant + 1
        if (eta[j] == eta[i]) 
          concordant <- concordant + 0.5
      }
    }
  }
  return(list(concordant = concordant, 
              total = total,
              c = round(concordant / total, 4)))
}


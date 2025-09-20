
################
#              #
# MATA_2FACTOR #
#              #
################


# This function creates an array containing the loadings 
# and acquiescence that will be used in the simulation


#########################
#
# IN:
#       n : number of content items
#
#     npf : number of items per factor
#
#     pos : number of positive items in each factor. 
#           (The number of positive items will be the same
#           for both factors.)
#
#      ld : mean of loading value
#
#     acq : mean of acquiescence value
#
# pattern : ACQ pattern, which can be 0, 1 or 2. 
#           The difference in the three types is the value 
#           of the standard deviation.
#
#########################


#########################
#
# OUT: 
# loadT : a matrix with simulated loadings (with acq) for each
#           factor. Size: "total number of items" X "number of
#           content factors"
#
#########################



matA_2factor <- function(n,npf, pos, ld, acq, pattern){
  lambdas0a  <- rnorm(npf, mean=ld, sd=0.1)
  lambdas0b  <- rnorm(npf, mean=ld, sd=0.1)
  zeros <- rep(0, npf)
  
  matone <- c(rep(1,pos), rep(-1, (npf-pos)))
  loada <- c(lambdas0a*matone, zeros)
  loadb <- c(zeros, lambdas0b*matone)
  
  
  if(pattern==0){
    ac <- rnorm(n, acq, 0.001)
  }
  if(pattern==1){
    ac <- rnorm(n, acq, 0.01)
  }
  if(pattern==2){
    ac <- rnorm(n, acq, 0.1)
  }
  
  loadT <- cbind(loada, loadb, ac)
  
  return(loadT)
}


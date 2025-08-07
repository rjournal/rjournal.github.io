
##################
#                #
# DATAGENERATOR2 #
#                #
##################


# This function creates a raw data matrix that simulate subject puntuation on a
# continuous test. It also creates an array containing the loadings 
# and acquiescence that will be used in the simulation



#########################
#
# IN:
# subject : number of subject 
#       
#       n : number of content items
#
#     npf : number of items per factor
#
#     pos : number of positive items in each factor. 
#           (The number of positive items will be the same
#           for both factors.)
#
#      ld : value of loadings
#
#     acq : value of acquiescence
#
# 
#
#########################


#########################
#
# OUT: 
# Z : a raw data matrix whose size is "number of subjects" x 
#          "total number of items".
#
# A : a matrix with simulated loadings (with acq) for each
#           factor. Size: "total number of items" X "number of
#           content factors"
#########################

# You need: 

source("matA_2factor.R")
library(EFA.MRFA)


datagenerator2 <- function(subject, n,npf, pos, ld, acq, pattern){
  
  
  PH <- matrix(c(1, 0, 0,0, 1, 0, 0, 0, 1), ncol=3, byrow=T)
  
  
  A <- matA_2factor(n, npf, pos, ld, acq, pattern)
  a1<- A[,1];a2 <- A[,2]; ac <- A[,3]
  
  
  th1 <- rnorm(subject, mean=0,sd=1)
  th2 <- rnorm(subject, mean=0,sd=1)
  th3 <- rnorm(subject, mean=0,sd=.03)
  TH0 <- cbind(th1, th2, th3)
  c1 <- var(TH0)
  chol1 <- solve(chol(c1))
  TH1 <-  TH0 %*% chol1
  eigen(PH)
  chol2 <- chol(PH)
  TH <- TH1 %*% chol2 * sd(th1) + mean(th1)
  
  
  U <- diag(as.vector(sqrt(1-(a1^2)- (a2^2)-(ac^2))))
  
  e <- rnorm(subject*n, mean=0, sd=1)
  E <- matrix(e, ncol=n, nrow=subject)
  
  Zpre <- (A%*%EFA.MRFA::transpose(TH))+(U%*%EFA.MRFA::transpose(E))
  Z <- t(Zpre)
  
  Z
  
  
  c1 <- "V"
  c2 <- 1:ncol(Z)
  
  columnas <- as.vector(paste(c1, c2, sep=""))
  colnames(Z) <- columnas
  
  
  return(list(Z=Z, A=A))
}
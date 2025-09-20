
##############
#            #
# KATEGORIZE #
#            #
##############


# A function that categorizes a continuous data matrix 



#########################
#
# IN:
#    Z : a raw data matrix whose size is "number of subjects" x 
#          "total number of items".
# thre : a raw data matrix whose size is "number of subjects" x 
#          "total number of items".
#
#########################


#########################
#
# OUT: 
# X : categorized data matrix that will have the same size as Z.
#
#########################


#You need: 
source("matthre4.R")



Kategorize <- function(Z, thre){
  K <- dim(thre)[2]
  N <- dim(Z)[1]
  m <- dim(Z)[2]
  
  
  (X <- (matrix(NA, ncol=m, nrow=N)))
  
  
  labels <- as.character(seq(1:(K+1)))
  
  for(i in 1:m){
    X[,i] <- cut(Z[,i], breaks=c(-1000000000, thre[i,], 
                                 100000000000), labels=labels, right=FALSE)
  }
  
  return(X)
}

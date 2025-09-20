
############
#          #
# MATTHRE4 #
#          #
############


# A function that generates an matrix of cut-off points 
# with which continuous variables are categorized into 5 options.


#########################
#
# IN:
# X : a raw data matrix whose size is "number of subjects" x 
#          "total number of items".  
#
#########################


#########################
#
# OUT: 
# thre : a raw data matrix whose size is "number of subjects" x 
#          "total number of items".
#
#########################



matthre4 <- function(X){
  quantile(X[,1], c(0.13,.37,.50,.87))
  thre <- matrix(0, ncol=4, nrow=ncol(X))
  for(i in 1:ncol(X)){
    thre[i,] <-quantile(X[,i], c(0.13,.37,.50,.87))
  }
  return(thre)
}
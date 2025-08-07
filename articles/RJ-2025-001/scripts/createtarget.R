
################
#              #
# CREATETARGET #
#              #
################


# This function creates a target matrix. 

#The target matrix is used as referece for assesing which items have significant
#loadings on which factor, and exact value is not used. 



#########################
#
# IN:
# pos : number of positive items in each factor. 
#       (The number of positive items will be the same
#        for both factors.)
#
# pattern : ACQ pattern, which can be 0, 1 or 2. 
#           The difference in the three types is the value 
#           of the standard deviation.
#
#########################


#########################
#
# OUT: 
# target : a matrix  composed of nines and zeros, 
#          whose size is "number of factors" x 
#          "total number of items".
#

#########################


createtarget <- function(pos, npf) {
  targeta <- matrix(c(rep(9, pos), rep(-9, (npf - pos)), rep(0, npf)), ncol=1)
  targetb <- matrix(c(rep(0, npf), rep(9, pos), rep(-9, (npf - pos))), ncol=1)
  target <- cbind(targeta, targetb)
  
  return(target)
}
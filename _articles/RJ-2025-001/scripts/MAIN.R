
#############
#           #
# main code #
#           #
#############


# MAIN sIt is a function that exemplifies the simulation study 
# that has been carried out. You only need to modify the 
# parameters by the levels of the dependent variables.

###################################################



#########################
#
# IN:
#     pos : number of positive items in each factor. 
#           (The number of positive items will be the same
#           for both factors.)
#
# pattern : ACQ pattern, which can be 0, 1 or 2. 
#           The difference in the three types is the value 
#           of the standard deviation.
#
#     type: Can be 0 (continuous) or 1 (ordinal). 
#           In the case of ordinal variables "corr = "Polychoric""
#           is used in the acquihybrid function. 
#           For continuous variables "corr= Pearson".
#
#########################


#########################
#
# OUT: 
# list : including:
#          
#        - blcf = bias factor 1
#        - blcf2 = bias factor 2
#        - bac = bias acq factor
#        - fit = goodnes of fit
#
#########################


###################################################

# You need:

library(EFA.MRFA)
library(siren)
source("datagenerator2.R")
source("kategorize.R")
source("createtarget.R")
source("matthre4.R")

###################################################



MAIN<-function(pattern, pos, type){
  # The input values are
    # pattern : acq pattern (SD), potentially 0, 1 or 2
    # pos : number of positive items 5, 6 or 7
    
  
  # The fixed variables are:
  
  rep <- 10 # number of replicate, 
  
  sub <- 300 # number of subjects
  n <- 20    # number of items
  npf <- 10  # number of items per factor
  ld <- 0.6  # mean of loading value
  acq <- 0.2 # mean of acq value
  
  
  
  
  ###################################################
  
  
  # The target matrix is used as referece for assesing which 
  #    items have significant loadings on which factor, 
  #    and exact value is not used.
  
  target <- createtarget(pos, npf)
  
  
  
  # Empty arrays and vectors are created to save dependent 
  #    variables in replicates.
  
  #goodness of fit
  gfi <- srmr <- rmsea <- cfi <- c() 
  
  #bias on content factor
  sesloa1 <-sesloa2 <- matrix(0, nrow=npf, ncol=rep) 
  
  #bias on acquiescence factor
  sesacq <- matrix(0, nrow=n, ncol=rep) 
  
  
  
  for(w in 1:rep){
    
    
    dat <- datagenerator2(sub, n, npf, pos, ld, acq, pattern)
    
    X <- dat$Z
    
    
    if(type==1){
      thre <- matthre4(X)
      X <- Kategorize(X, thre)
    }
    
    
    if(type==0){
      execute <- acquihybrid(x=X,content_factors=2,target, 
                             corr = "Pearson", raw_data=T , 
                             method="fixed", display = TRUE)
    } else{execute <- acquihybrid(x=X,content_factors=2,target, 
                                  corr = "Polychoric", raw_data=T , 
                                  method="fixed", display = TRUE)}
  
    
    
    
    # bias
    sesloa1[,w] <-(execute$loadings - dat$A)[1:npf,1] 
    sesloa2[,w] <-(execute$loadings - dat$A)[(npf+1):n,2]
    sesacq[,w]  <-(execute$loadings - dat$A)[,3] 
    
    # fit
    gfi[w]   <- as.matrix(execute$fit_indices)[1,1]
    srmr[w]  <- as.matrix(execute$fit_indices)[2,1]
    rmsea[w] <- as.matrix(execute$fit_indices)[3,1]
    cfi[w]   <- as.matrix(execute$fit_indices)[4,1]
    
  }
  
  ###################################################
  
    # Output results
  
  blcf <- sesloa1  # factor 1 bias
  blcf2 <- sesloa2 # factor 2 bias
  bac <- sesacq    # AC factor bias
  
  fitc_70 <- cbind(cfi, gfi, rmsea, srmr) # fit
  
  out <- list("blcf"=blcf, "blcf2"=blcf2, "bac"=bac, "fit"=fitc_70)
  return(out)

}


###################################################

# example: 

#MAIN(0,5,0)
#MAIN(1,5,0)
MAIN(2,6,1)
#...

#From these results, a summary table was prepared with all the biases. 


#Finally, a variance analysis was performed with the results of GFI and RMSE.





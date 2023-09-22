version <- R.version()
if(version$major>3){
  RNGkind(sample.kind="Rounding")
}else{
  if(version$minor>5.3){
    RNGkind(sample.kind="Rounding")
  }
}
#### Classification #####

 set.seed(1)
 library(ordinalClust)
 data("dataqol")
 data("dataqol.classif")
 nbSEM <- 150
 nbSEMburn <- 100
 nbindmini <- 1
 init <- "randomBurnin"
 percentRandomB <- c(50, 50)
 x <- as.matrix(dataqol.classif[,2:29])
 v <- as.vector(dataqol.classif$death)
 nb.sample <- ceiling(nrow(x)*7/10)
 sample.train <- sample(1:nrow(x), nb.sample, replace=FALSE)
 
 x.train <- x[sample.train,]
 x.validation <- x[-sample.train,]
 
 v.train <- v[sample.train]
 v.validation <- v[-sample.train]
 # classes
 kr <- 2
 # levels
 m <- 4
 kcol <- c(0, 1, 2, 3, 4)
 preds <- matrix(0, nrow = length(kcol),ncol = nrow(x.validation))
 
 for( kc in 1:length(kcol) ){
     classif <- bosclassif(x = x.train, y = v.train, kr = kr, kc = kcol[kc],
                           m = m, nbSEM = nbSEM, nbSEMburn = nbSEMburn, 
                           nbindmini = nbindmini, init = init,
                           percentRandomB = percentRandomB)
     new.prediction <- predict(classif, x.validation)
     if(!is.character(new.prediction)){
       preds[kc,] <- new.prediction@zr_topredict
     }
 }



preds
v.validation

library(caret)

actual <- v.validation - 1

specificities <- rep(0,length(kcol))
sensitivities <- rep(0,length(kcol))

for(i in 1:length(kcol)){
  prediction <- unlist(as.vector(preds[i,])) - 1
  u <- union(prediction, actual)
  conf_matrix <- table(factor(prediction, u),factor(actual, u))
  sensitivities[i] <- recall(conf_matrix)
  specificities[i] <- specificity(conf_matrix)
}

sensitivities
specificities




 #### Clustering #####
set.seed(1)
x <- as.matrix(dataqol[,2:29])

clust <- bosclust(x = x, kr = 3, m = 4, 
            nbSEM = nbSEM, nbSEMburn = nbSEMburn, 
            nbindmini = nbindmini, init = init)

plot(clust)

clust@params



#### Co-clustering ####

set.seed(1)
coclust <- boscoclust(x = x, kr = 3, kc = 3, m = 4,
                nbSEM = nbSEM, nbSEMburn = nbSEMburn,
                nbindmini = nbindmini, init = init)

plot(coclust)

coclust@params


# Setting the SEMburn and nbSEMburn arguments.

par(mfrow=c(3,3))

for(kr in 1:3){
    for(kc in 1:3){
        toplot <- rep(0, nbSEM)
        for(i in 1:nbSEM){
            toadd <- coclust@paramschain[[1]]$pis[kr,kc,i]
            toplot[i] <- toadd
        }
        plot.default(toplot, type = "l",ylim = c(0,1), 
                 col = "hotpink3", main = "pi", 
                 ylab = paste0("pi_", kr, kc, "values"), 
                 xlab = "SEM-Gibbs iterations")
    }
}

# Missing Values

missing <- which(is.na(x))
missing

values.imputed.clust <- clust@xhat[[1]][missing]
values.imputed.clust

values.imputed.coclust <- coclust@xhat[[1]][missing]
values.imputed.coclust


# Comparing coclust and clust row partitions:
mclust::adjustedRandIndex(coclust@zr, clust@zr)



#### Model Selection ####

## Clustering

set.seed(1)

library(ordinalClust)
data("dataqol")
M <- as.matrix(dataqol[,2:29])

nbSEM <- 150
nbSEMburn <- 100
nbindmini <- 2
init <- "randomBurnin"
percentRandomB <- c(30)
icl <- rep(0,3)

for(kr in 2:4){
    object <- bosclust(x = M, kr = kr, m = 4, nbSEM = nbSEM,
                nbSEMburn = nbSEMburn, nbindmini = nbindmini, 
                percentRandomB = percentRandomB, init = init)

    if(length(object@icl))
    {
    	icl[kr-1] <- object@icl 
    }  
}
icl

## Co-clustering

set.seed(1)
library(ordinalClust)
data("dataqol")
M <- as.matrix(dataqol[,2:29])

nbSEM <- 150
nbSEMburn <- 100
nbindmini <- 2
init <-  "randomBurnin"
percentRandomB <- c(50, 50)
icl <- matrix(0, nrow = 3, ncol = 3)

for(kr in 2:4){
    for(kc in 2:4){
        object <- boscoclust(x = M,kr = kr, kc = kc, m = 4, nbSEM = nbSEM,  
                        nbSEMburn = nbSEMburn, nbindmini = nbindmini,
                        percentRandomB = percentRandomB, init = init)
        if(length(object@zr))
	      {
	     	 icl[kr-1, kc-1] <- object@icl 
	      } 
    }

}
icl








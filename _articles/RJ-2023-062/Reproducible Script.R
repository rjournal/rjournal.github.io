### Script to reproduce results of manuscript: C443: An r-package to see a forest for the trees
## Aniek Sies & Iven Van Mechelen

################## session info #####################
#R version 4.2.2 (2022-10-31 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19044)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
#[3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
#[5] LC_TIME=English_United Kingdom.utf8    

#attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] randomForest_4.7-1.1 partykit_1.2-16      mvtnorm_1.1-3        libcoin_1.0-9       
#[5] rpart_4.1.19         C443_3.3.1           devtools_2.4.5       usethis_2.1.6       

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.9         lattice_0.20-45    prettyunits_1.1.1  ps_1.7.5          
#[5] rprojroot_2.0.3    digest_0.6.31      utf8_1.2.2         mime_0.12         
#[9] ranger_0.15.1      plyr_1.8.8         R6_2.5.1           ggplot2_3.4.2     
#[13] pillar_1.9.0       rlang_1.1.1        curl_5.0.0         rstudioapi_0.14   
#[17] miniUI_0.1.1.1     callr_3.7.3        urlchecker_1.0.1   Matrix_1.5-3      
#[21] labeling_0.4.2     desc_1.4.2         splines_4.2.2      stringr_1.5.0     
#[25] htmlwidgets_1.6.2  igraph_1.3.5       munsell_0.5.0      shiny_1.7.4       
#[29] compiler_4.2.2     httpuv_1.6.11      pkgconfig_2.0.3    pkgbuild_1.4.0    
#[33] htmltools_0.5.5    tidyselect_1.2.0   gridExtra_2.3      tibble_3.1.8      
#[37] fansi_1.0.3        crayon_1.5.2       dplyr_1.0.10       withr_2.5.0       
#[41] later_1.3.1        MASS_7.3-58.1      DBI_1.1.3          xtable_1.8-4      
#[45] gtable_0.3.3       lifecycle_1.0.3    magrittr_2.0.3     scales_1.2.1      
#[49] cli_3.4.1          stringi_1.7.8      cachem_1.0.8       farver_2.1.1      
#[53] fs_1.6.2           promises_1.2.0.1   remotes_2.4.2      ellipsis_0.3.2    
#[57] vctrs_0.5.1        generics_0.1.3     Formula_1.2-5      RColorBrewer_1.1-3
#[61] tools_4.2.2        glue_1.6.2         purrr_1.0.1        parallel_4.2.2    
#[65] processx_3.8.1     pkgload_1.3.2      fastmap_1.1.1      survival_3.4-0    
#[69] colorspace_2.0-3   cluster_2.1.4      sessioninfo_1.2.2  memoise_2.0.1     
#[73] inum_1.0-5         profvis_0.3.8 


########################################################################################################
# REAL DATA EXAMPLE1
########################################################################################################

######### Load the package #######
library (C443)
EcstData = drugs [, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", "Agree",
                      "Consc", "Impul", "Sensat", "Ecst")]
head(EcstData, 3)

######## Grow a single tree ###########
library(rpart)
library(partykit)

set.seed(123)
#Grow the tree
EcstTree <- rpart(Ecst ~ ., data = EcstData, control = rpart.control(
  minsplit = 100, maxdepth = 3, maxsurrogate=0, maxcompete=0))
#Prune the tree
cp <- EcstTree$cptable[order(EcstTree$cptable[1:which.min(EcstTree$cptable[, "xerror"]), "xerror"]), ]
maxcp <- cp[1, "xerror"] + 1.96 * cp[1, "xstd"]
cp <- cp[as.numeric(which.max(1 / (maxcp - cp[, "xerror"]))), "CP"]

PrunedEcstTree <- prune.rpart(EcstTree, cp = cp)
plot(as.party(PrunedEcstTree))



##### Create a forest: Draw bootstrap samples and grow trees ######
#Function to draw bootstrap samples
DrawBoots <- function(dataset, i){
    set.seed(1234 + i)
    Boot <- dataset[sample(1:nrow(dataset), size = nrow(dataset), 
                           replace = TRUE), ]
    return(Boot)
  }



#Function to grow trees
GrowTree <- function(x,y,BootsSample){
  controlrpart <- rpart.control(minsplit = 100, minbucket = 50,
                                maxdepth = 3, maxsurrogate=0, maxcompete=0)
  tree <- rpart(as.formula(paste(noquote(paste(y, "~")), noquote(paste(x,
                                                                       collapse = "+")))), data = BootsSample, control = controlrpart)
  
  cp <- tree$cptable[order(tree$cptable[1:which.min(tree$cptable[,    
                                                                 "xerror"]), "xerror"]), ]
  maxcp <- cp[1, "xerror"] + 1.96 * cp[1, "xstd"]
  cp <- cp[as.numeric(which.max(1 / (maxcp - cp[, "xerror"]))), "CP"]
  
  PrunedTree <- prune.rpart(tree, cp = cp) 
  return(PrunedTree)
}



# Draw the samples & Grow the trees
set.seed(12345)
Boots <- lapply(1:100, function(k) DrawBoots(EcstData, k))
Trees <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",
                                                   "Neuro", "Extr", "Open", "Agree", "Consc", "Impul", "Sensat"),
                                             y = "Ecst", Boots[[i]]))
  

############## Cluster the trees in the forest ############################
set.seed(1)
ClusterForest<- clusterforest(observeddata=EcstData,treedata=Boots,
                                trees=Trees, m=6, fromclus=1, toclus=8)


########### Post processing #################################
#plot ASW and within cluster similiarty per# clusters
par(mfrow = c(2, 2))
plot(ClusterForest)
#plot 1 cluster solution 
plot(ClusterForest, solution = 1)

# plot 2, 3, 4 cluster solutions
plot(ClusterForest, solution = 2)
plot(ClusterForest, solution = 3)
plot(ClusterForest, solution = 4)

#cluster sizes
table(clusters(ClusterForest,solution=2))
table(clusters(ClusterForest,solution=3))
table(clusters(ClusterForest,solution=4))



#########################################################################
################# Real Data Example 2 ###################################
#########################################################################
library(randomForest)
EcstData$Ecst <- as.factor (EcstData$Ecst)

set.seed(123)
train.id <- sample(nrow(EcstData), 3/4 * nrow(EcstData))
EcstData.train <- EcstData[train.id, ]
EcstData.test <- EcstData[-train.id, ]


set.seed(1111)
DrugsRF <- randomForest(Ecst ~ ., data = EcstData.train, importance=TRUE,
                         proximity=TRUE, keep.inbag=T, maxnodes=6,ntree=500)


preds_test=predict(DrugsRF,EcstData.test )
preds_train=predict(DrugsRF,EcstData.train )

mean(preds_test==EcstData.test$Ecst)
mean(preds_train==EcstData.train$Ecst)


############## Cluster the trees in the forest ############################
set.seed(1)
ClusterForest<- clusterforest(observeddata=EcstData.train,trees=DrugsRF, 
                              m=6, fromclus=1, toclus=20)

########### Post processing #################################
#plot ASW and within cluster similiarty per# clusters
plot(ClusterForest, predictive_plots=TRUE )

#plot 1 cluster solution 
plot(ClusterForest, solution = 3)


ClusterForest$medoids[[3]]

tree310=C443:::randomForest2party(EcstData.train, DrugsRF, ClusterForest$medoids[[3]][1])
tree329=C443:::randomForest2party(EcstData.train, DrugsRF, ClusterForest$medoids[[3]][2])
tree453=C443:::randomForest2party(EcstData.train, DrugsRF, ClusterForest$medoids[[3]][3])

preds_test310=predict(tree310,EcstData.test )
preds_test329=predict(tree329,EcstData.test )
preds_test453= predict(tree453,EcstData.test )
preds_total = round(((as.numeric(preds_test310)-1)*table(clusters(ClusterForest,solution=3))[1]+
 (as.numeric(preds_test329)-1)*table(clusters(ClusterForest,solution=3))[2]+ (as.numeric(preds_test453)-1)*table(clusters(ClusterForest,solution=3))[3])/500 , 0)

mean(preds_total==preds_test)
mean(preds_total==EcstData.test$Ecst)

mean(preds_test310==preds_test329)
mean(preds_test310==preds_test453)
mean(preds_test453==preds_test329)

########################################################################################################
# REAL DATA EXAMPLE 3
########################################################################################################

#################Create dataset for each drug ###########
AmphetData <- drugs[, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", 
                        "Agree", "Consc", "Impul", "Sensat", "Amphet")]
BenzoData <- drugs[, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", 
                       "Agree","Consc", "Impul", "Sensat", "Benzos")]
EcstData <- drugs[, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", 
                      "Agree", "Consc", "Impul", "Sensat", "Ecst")]
LSDData <- drugs[, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", 
                     "Agree", "Consc", "Impul", "Sensat", "LSD")]
MushData <- drugs[, c("Age", "Gender", "Edu", "Neuro", "Extr", "Open", 
                      "Agree", "Consc", "Impul", "Sensat", "Mush")]

###########Draw bootstrap samples and grow trees for each drug ################
BootsAmphet <- lapply(1:100, function(k) DrawBoots(AmphetData, k))
set.seed(123456)
TreesAmphet <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",   
                                                        "Neuro", "Extr", "Open", "Agree", "Consc", "Impul", "Sensat"), y="Amphet",
                                                  BootsAmphet[[i]]))

BootsBenzo <- lapply(1:100, function(k) DrawBoots(BenzoData, k))
set.seed(123456)
TreesBenzo <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",
                                                       "Neuro", "Extr", "Open", "Agree", "Consc", "Impul", "Sensat"), y = "Benzos",
                                                 BootsBenzo[[i]]))

BootsEcst <- lapply(1:100, function(k) DrawBoots(EcstData, k))
set.seed(123456)
TreesEcst <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",
                                                      "Neuro", "Extr",  "Open", "Agree", "Consc", "Impul", "Sensat"), y="Ecst",
                                                BootsEcst[[i]]))

BootsLSD <- lapply(1:100, function(k) DrawBoots(LSDData, k))
set.seed(123456)
TreesLSD <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",
                                                     "Neuro", "Extr", "Open", "Agree", "Consc", "Impul", "Sensat"), y = "LSD",
                                               BootsLSD[[i]]))

BootsMush <- lapply(1:100, function(k) DrawBoots(MushData, k))
set.seed(123456)
TreesMush <- lapply(1:100, function (i) GrowTree(x = c("Age", "Gender", "Edu",
                                                      "Neuro", "Extr", "Open", "Agree", "Consc", "Impul", "Sensat"), y = "Mush",
                                                BootsMush[[i]]))
# add everything together
Boots <- c(BootsAmphet, BootsBenzo, BootsEcst, BootsLSD, BootsMush)
Trees <- c(TreesAmphet, TreesBenzo, TreesEcst, TreesLSD, TreesMush)

############### Cluster the forest ######################
set.seed(1)
ClusterForestMultiple <- clusterforest(observeddata=drugs,treedata=Boots, trees=Trees, m=6, fromclus=1, toclus=8, treecov=rep(c("Amphet", "Benzo", "Ecst", "LSD", "Mush"), each = 100))
####### Post Processing ######
# Plot ASW and within cluster similarity per # clusters                           
par(mfrow = c(2, 2))
plot(ClusterForestMultiple) 
summary(ClusterForestMultiple)

#Plot 1 cluster solution
plot(ClusterForestMultiple, solution = 1)

#link heterogeneity to response  
treesource(ClusterForestMultiple, solution= 3)

#Plot 3 cluster solution
plot(ClusterForestMultiple, solution = 3)

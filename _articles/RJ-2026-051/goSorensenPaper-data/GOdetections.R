## The following script implements the 5-step procedure to obtain the biological
## functions (GO terms) associated with the functional similarities detected
## from the irrelevance-threshold matrix of dissimilarities for the gene lists
## contained in allOncoGeneLists, in the BP ontology and at the GO level 4. 
## The results of this script are depicted in the Table 1 of the paper.
library(goSorensen)
library(GO.db)
library(org.Hs.eg.db)
data("allOncoGeneLists")
humanEntrezIDs <- keys(org.Hs.eg.db, keytype = "ENTREZID")

cont_all_BP4 <- buildEnrichTable(allOncoGeneLists,
                                 geneUniverse = humanEntrezIDs, 
                                 orgPackg = "org.Hs.eg.db", 
                                 onto = "BP", 
                                 GOLevel = 4)

dissMatrx_BP4 <- sorenThreshold(cont_all_BP4, trace = FALSE)
dissMatrx_BP4


# Auxiliary functions:
sdm <- function(x, ...){
  nlists <- length(x)
  ifelse(nlists > 1, sd(x, ...), 0)
}

# get_go_description returns the description of a GO term.
get_go_description <- function(go_id) {
  go_term <- Term(GOTERM[[go_id]])
  return(go_term)
}

## splitdim divides a dimension of the MDS biplot based on a user-defined proportion. 
splitdim <- function(dm, prp){
  dm <- as.vector(dm)
  prp <- c(prp, 1 - (2*prp), prp)
  rg <- range(dm)
  cutpoints <- cumsum(prp) * diff(rg) + rg[1]
  cutpoints[-length(cutpoints)]
}

# gotermsid implements a five-step process to identify the GO terms that 
# characterize the dimensions of an MDS biplot.
gotermsid <- function(list, dm, prp, dimen, contabs){
  sorted <- as.data.frame(dm[order(dm[, dimen]), ])
  cpdim1 <- splitdim(dm[, dimen], prp)
  lleft <- rownames(sorted[sorted[, dimen] < cpdim1[1], ]) # Identify lists to the left
  lright <- rownames(sorted[sorted[, dimen] > cpdim1[2], ])
  
  # STEP 2)  1-0 Table for extremes left and right
  #tableleft <- data.frame(enrichedIn(list[lleft], ...))
  enrichmat <- attr(contabs, "enriched")
  tableleft <- data.frame(enrichmat[, lleft])
  colnames(tableleft) <- lleft
  #tableright <- data.frame(enrichedIn(list[lright], ...))
  tableright <- data.frame(enrichmat[, lright])
  colnames(tableright) <- lright
  
  # STEP 3) Mean and sd 
  lmnsd <- apply(tableleft, 1, 
                 function(x){c("meanLeft" = mean(x), "sdLeft" = sdm(x))})
  rmnsd <- apply(tableright, 1, 
                 function(x){c("meanRight" = mean(x), "sdRight" = sdm(x))})
  
  # STEP 4) Pseudo t
  nl <- ncol(tableleft) ;   nr <- ncol(tableright) 
  Pseudo_t <- abs(lmnsd[1, ] - rmnsd[1, ]) / sqrt((((lmnsd[2, ] / nl) + (rmnsd[2, ] / nr))) + 0.0000001)
  
  
  # STEP 5) Summary table
  sum <- as.data.frame(t(rbind(lmnsd, rmnsd, Pseudo_t)))
  sortSum <- sum[order(sum[, "Pseudo_t"]), ]
  prevRes <- sortSum[sortSum$Pseudo_t == max(sortSum$Pseudo_t), ]
  
  GOIDs <- rownames(prevRes)
  desc <- data.frame(Description = sapply(rownames(prevRes), get_go_description, 
                                          USE.NAMES = F))
  
  cbind(GOIDs, desc)
}

# Applying gotermsid on the irrelevance-threshold matrix of dissimilarities 
# for the gene lists contained in allOncoGeneLists, in the BP ontology and at 
# the GO level 4
prmds <- cmdscale(dissMatrx_BP4, k = 2, eig = TRUE)
prmds$points <- as.data.frame(prmds$points)
colnames(prmds$points) <- paste0("Dimension", 1:2)

labels <- attr(dissMatrx_BP4, "Labels")
dfbiplot <- cbind(prmds$points, label = labels)


dim1 <- gotermsid(
  list = allOncoGeneLists, 
  dm = cmdscale(dissMatrx_BP4, k = 2, eig = T)$points, 
  prp = 0.2, 
  dimen = 1,
  contabs = cont_all_BP4
)

dim2 <- gotermsid(
  list = allOncoGeneLists, 
  dm = cmdscale(dissMatrx_BP4, k = 2, eig = T)$points, 
  prp = 0.2, 
  dimen = 2,
  contabs = cont_all_BP4
)

save(dim1, dim2, file = "GOdetections.rda")


########################################################################
############## Code to replicate results in the manuscript ##############
########################################################################
### To install the netgwas package, please follow the below steps.
### Step 1: If it is needed, first install core Bioconductor package using the below code, then run line 11 to install RBGL from Bioconductor
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install()

#Step 2: Install RBGL package from Bioconductor 
#BiocManager::install("RBGL")

#Install netgwas package (please check steps 1 and 2)
install.packages( "netgwas" )
library( "netgwas" )


############## Code to replicate figure 2 ##############
set.seed(2)
sim <- simgeno(p = 90, n = 200, k = 3, g = 5, adjacent = 3, alpha = 0.1, 
               beta = 0.02, con.dist = "Mnorm", d = NULL, vis = TRUE)

############## Code to replicate Module 1. Data simulation, simRIL function (page 6) ##############
set.seed(2)
ril <- simRIL(g = 5, d = 25, n = 200, cM = 100, selfing = 2)
ril$data[1:3, ]
ril$map

############## Code to replicate figure 3 ##############
data(tetraPotato)
# Shuffle the order of markers
dat <- tetraPotato[ , sample(ncol(tetraPotato))]
potato.map <- netmap(dat, cross = "outbred") # takes around 3 min
potato.map.ordered <- buildMap(potato.map, opt.index = 3); potato.map.ordered

plot(potato.map.ordered, vis = "unordered markers") # figure 3a
plot(potato.map.ordered, vis = "ordered markers") # figure 3b
map <- potato.map.ordered$map

############## Code to replicate figure 4 ##############
data(CviCol)
set.seed(1)
cvicol <- CviCol[ ,sample(ncol(CviCol))]
out <- netmap(cvicol, cross= "inbred", ncores= 1)
out$opt.index
bm.thaliana <- buildMap(out, opt.index= 4); bm.thaliana
thalianaMap <- bm.thaliana$map
plot(bm.thaliana, vis= "summary")

############## Code to replicate figure 5 #############
data(CviCol)
set.seed(2)
out <- netsnp(CviCol)
sel <- selectnet(out)
# Steps to visualize the selected network
cl <- c(rep("palegoldenrod", 24), rep("white",14), rep("tan1",17), 
        rep("gray",16), rep("lightblue2",19))
plot(sel, vis= "parcor.network", sign.edg = TRUE, layout = NULL, vertex.color = cl) #figure 5a
plot(sel, vis= "image.parcorMatrix", xlab="markers", ylab="markers")                #figure 5b

############## Code to replicate figure 6 #############
data(thaliana)
head(thaliana, n = 3)

set.seed(12)
out <- netphenogeno(thaliana)
sel <- selectnet(out)

# Steps to visualize the network
cl <- c(rep("red", 8), rep("white",56), rep("yellow2",31),
        rep("gray",33), rep("lightblue2",31), rep("salmon2",30))

id <- c("DTF_LD","CLN_LD","RLN_LD","TLN_LD","DTF_SD","CLN_SD", 
        "RLN_SD", "TLN_SD","snp16", "snp49","snp50", "snp60","snp83", 
        "snp86", "snp113","snp150", "snp155","snp159","snp156",
        "snp161","snp158", "snp160","snp162", "snp181")

plot(sel, vis= "interactive", n.mem= c(8,56,31,33,31,30),
     vertex.color= cl, label.vertex= "some", sel.nod.label= id,
     edge.color= "gray", w.btw= 200, w.within= 20, tk.width = 900,
     tk.height = 900)

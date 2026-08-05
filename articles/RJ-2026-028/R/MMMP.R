################################################################################
### R E P L I C A T I O N    S C R I P T  --  The R Journal submission       ###
###                                                                          ###
### Paper title: ProfileLadder: Functional-based reserving                   ###
### Autors: Maciak, Matúš, Mizera, and Pešta (2025)                          ###
### Date:   Submission:   August, 2025                                       ###
###         Resubmission: October, 2025                                      ###
###         Acceptance:   January, 2026                                      ###
###                                                                          ### 
### ProfileLadder package: Version 0.2.2 (available on CRAN/GitHub)          ###
################################################################################

rm(list = ls())

### installation of the ProfileLadder package (CRAN or direct link)
install.packages("ProfileLadder", dependences = TRUE, type = "source" ) # v0.2.2
install.packages("https://cran.r-project.org/src/contrib/ProfileLadder_0.2.2.tar.gz")


### required libraries
library("ChainLadder")
library("ProfileLadder")


################################################################################
### M a n u s c r i p t   --   S E C T I O N   2
################################################################################

### Figure 1 -- portfolio data
data(CameronMutual, package = "ProfileLadder")

### run-off triangle
CameronMutual

### development profiles
plot(as.profileLadder(observed(CameronMutual)))

  
################################################################################
### M a n u s c r i p t   --   S E C T I O N   3.1           (PARALLAX/REACT)
################################################################################

### Functional profile completion by PARALLAX
parallax <- parallelReserve(CameronMutual, method = "parallax")

### Functional profile completion by REACT
react <- parallelReserve(CameronMutual, method = "react", residuals = TRUE)

### Output of parallelReserve() for PARALLAX
parallax

### Fancy/standard output print 
print(as.profileLadder(observed(CameronMutual)), fancy.print = TRUE)
print(as.profileLadder(observed(CameronMutual)), fancy.print = FALSE)

### Summary output from parallelReserve() for REACT
summary(react)

### Applying standard bechmark ODP model from the ChainLadder package
summary(glmReserve(observed(CameronMutual)))

### Back-fitted residuals
summary(parallelReserve(observed(CameronMutual), method = "react", residuals = TRUE))

### Figure 2a and Figure 2b
plot(as.profileLadder(CameronMutual), ylim = c(3500, 16000))
plot(parallelReserve(CameronMutual), ylim = c(3500, 16000), ylab = "")


################################################################################
### M a n u s c r i p t   --   S E C T I O N   3.2                  (MACRAME)
################################################################################

### Functional profile completion by MACRAME (default setting)
macrame <- mcReserve(CameronMutual)

### MC breaks from output5
mcBreaks(macrame)

### MC states from output5
mcStates(macrame)

### Transition matrix
mcTrans(macrame)

### Data-driven set of breaks and states for CameronMutual dataset
print(runoff.exploratory <- incrExplor(CameronMutual, out = 1))

### summary of runoff.exporatory
summary(runoff.exploratory)

### Figure 3 
plot(runoff.exploratory)

### All increments considered (out = 0)
print(runoff.exploratory.all <- incrExplor(CameronMutual, out = 0))

### Figure 4
plot(runoff.exploratory.all)

### MC breaks for the triangle with all increments (out = 0)
mcBreaks(runoff.exploratory.all)

### MC states for the triangle with all increments (out = 0)
mcStates(runoff.exploratory.all)


### Comparing the DEFAULT performance of the MACRAME algorithm with the 
### user-based modifications but using default breaks and states in incrExplor()
### (thus,  both outputs must be identical)

mcReserve(CameronMutual) ## default

user.states <- mcStates(incrExplor(CameronMutual))
user.breaks <- mcBreaks(incrExplor(CameronMutual))

### user-modified MACRAME with the default breaks and states - ideantical output
### as from 'mcReserve(CameronMutual)'
mcReserve(CameronMutual, states = user.states, breaks = user.breaks)


### Example 1
plot(incrExplor(CameronMutual, states = 5)) ## Figure 5a (Panel (II))
plot(mcReserve(CameronMutual, states = 5)) ## Figure 5b 


### Example 2
user.states <- c(500, 1000, 1500, 2000, 2500)
plot(incrExplor(CameronMutual, states = user.states)) ## Figure 6a (Panel (II))
plot(mcReserve(CameronMutual, states = user.states)) ## Figure 6b


### Example 3
user.breaks <- c(500, 1000, 1500, 2000)
plot(incrExplor(CameronMutual, breaks = user.breaks)) ## Figure 7a (Panel (II))
plot(mcReserve(CameronMutual, breaks = user.breaks)) ## Figure 7b


### Example 4
user.breaks <- c(500, 1000, 1500, 2000)
user.method <- incrExplor(CameronMutual, breaks = user.breaks, method = "min")

final.states <- mcStates(user.method)
mcReserve(CameronMutual, breaks = user.breaks, states = final.states)
     

################################################################################
### M a n u s c r i p t   --   S E C T I O N   3.3    (PERMUTATION BOOTSTRAP)
################################################################################

### overal reserve distribution by MACRAME
set.seed(1234)
print(distribution.macrame <- permuteReserve(mcReserve(CameronMutual)))

### summary
summary(distribution.macrame)

### Figure 8
plot(distribution.macrame)

### overall reserve by GLM-based reserving (from ChainLadder pkg)
set.seed(1234)
(distribution.glm <- permuteReserve(glmReserve(observed(CameronMutual))))


################################################################################
### M a n u s c r i p t   --   S E C T I O N   3.5          (OTHER FEATURES) ###
################################################################################


### Figure 9
### S3 class'triangle' from the ChainLadder pkg
plot(as.triangle(CameronMutual))
### S3 class'profileLadder' from the ProfileLadder pkg
plot(as.profileLadder(CameronMutual))


### Benchmark ODP model with the glmReserve() function from the 'ChainLadder'  
### package with fully observed data -- ERROR MESSAGE
try(glmReserve(CameronMutual)) ### ILLUSTRATION OF A WRONG COMMAND WITH ERROR

### Run-off layout provided by the observed() function works correctly
glmReserve(observed(CameronMutual))


### Illustration of the predict method -- predicting new running diagonal
predict(parallelReserve(CameronMutual))


################################################################################
### M a n u s c r i p t   --   S E C T I O N   4        (CONCLUDING REMARKS) ###
################################################################################

### predict method applied to Covid19 data
diagonal <- predict(mcReserve(covid19CZ[,3:6]))
print(diagonal)


### Figure 10 
### 1-step-ahead prediction of the new running diagonal
plot(predict(mcReserve(covid19CZ[,3:6])), trueProfiles = covid19CZ[,1:7])


################################################################################
### A d d i t i o n a l    m a t e r i a l   (not explicitely in the paper)  ###
################################################################################

### The following R code is not included in the manuscript. However, the 
### illustrative dataset CameronMutual (which is used in the R package 
### 'ProfileLadder')is also implicitly given in another R package 'raw' 
### (in different structure however). The extraction of the data from 'raw' 
### and its restructuring in terms of the run-off triangle (as available in 
### the R package 'ProfileLadder') is showed here

library("raw") ### data files from Meyers and Shi (2011)

### CameronMutual extraction from the package 'raw' 
### (R data object 'ppauto', company group code: 5320)
dataObject <- ppauto
groupCode <- 5320

ins.line.data <- function(g.code){
  b=subset(dataObject, dataObject$GroupCode==g.code)
  name=b$Company
  grpcode=b$GroupCode
  ay=b$AccidentYear
  dev=b$Lag
  
  cum_incloss=b[,6]
  cum_pdloss=b[,7]
  bulk_loss=b[,8]
  dir_premium=b[,9]
  ced_premium=b[,10]
  net_premium=b[,11]
  single=b[,12]
  posted_reserve97=b[,13]
  
  data.out=data.frame(name,grpcode,ay,dev,net_premium,dir_premium,ced_premium,
                      cum_pdloss,cum_incloss,bulk_loss,single,posted_reserve97)
  return(data.out)
} ### end of ins.line.data() 

### group code for the Cameron Mutual insurance company is 5320
comauto <- ins.line.data(groupCode)
### upper triangle extraction 
com.insample <- subset(comauto,ay+dev <= 1998)
### lower triangle extraction
com.outsample <- subset(comauto,ay+dev > 1998)

### converting the data.frame into a Chain-Ladder triangle
ChainLadder <- matrix(rep(NA, 100), nrow = 10) 
start <- 1 
for (i in 10:1){### upper triangle 
  ChainLadder[10 - i + 1, 1:i] <- com.insample[start:(start + i - 1),8]
  start <- start + i 
} 
start <- 1 
for (i in 2:10){### lower triangle
  ChainLadder[i, (10 - i + 2):10] <- com.outsample[start:(start + (i - 2)),8]
  start <- start + (i - 1) 
}

### CameronMutual data set and the ChainLadder extracted from the 'raw' package
### are identical 
all(CameronMutual == ChainLadder)

### The same also applies for another two datasets taken from the R package 'raw'
### and used in the R package 'ProfileLadder'. These datasets are NevadaGeneral 
### (with the group code 10007) and MidwestMutual (using the group code 23574)






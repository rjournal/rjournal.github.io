# RKHSMetaMod through examples
################################################################################################
rm(list=ls())
install.packages("RKHSMetaMod")
library("RKHSMetaMod")
library(lhs)
################################################################################################
# Example 1
################################################################################################
# Generate the learning and testing datasets 
# example with n=200 and kernel="matern":
d <- 5
n <- 200
sigma <- 0.2
a <- c(0.2,0.6,0.8,100,100,100,100,100,100,100)[1:d]
Nsim <- 50
Nb.sim <- 0
lX<-list()
lY<-list()
lXT<-list()
lYT<-list()
for (s in 1:Nsim) {
  Nb.sim <- Nb.sim+1  
  # generate the learning dataset X, Y:
  X<- maximinLHS(n, d)
  g=1;for (i in 1:d) g=g*(abs(4*X[,i]-2)+a[i])/(1+a[i])
  epsilon <- rnorm(n,0,1)
  Y <- g + sigma*epsilon
  # generate the testing dataset XT, YT: 
  XT <- maximinLHS(n, d)
  gT=1;for (i in 1:d) gT=gT*(abs(4*XT[,i]-2)+a[i])/(1+a[i])
  epsilonT <- rnorm(n,0,1)
  YT <- gT + sigma*epsilonT
  # save the testing and learning datasets: 
  lX[[s]] <- X
  lY[[s]] <- Y
  lXT[[s]] <- XT
  lYT[[s]] <- YT
  save(lX,lY,lXT,lYT,file="RJ.Ex2.n200.ExperimentalDesign.RData")
}

# to load the datasets:
# setwd("/Users/hkamari/Dropbox/Thesis_me/Report_ThesisHalaleh/RJtemplate/RExamples")
# load('RJ.Ex2.n200.ExperimentalDesign.RData')

################################## START CODE ########################################
Dmax <- 3
# in this example three kernels are considered: "matern", "brownian", "gaussian"
kernel <- "matern" 
# kernel <- "brownian" 
# kernel <- "gaussian" 
Sobol.emp <- NULL
TotRes <- list(NULL)
Nsim <- 50
Nb.sim <- 0
for (s in 1:Nsim) {
  print(paste("Simulation", s))
  Nb.sim <- Nb.sim+1  
  # call the learning dataset X, Y: 
  X <- lX[[s]]
  Y <- lY[[s]]
  
  # grid of values of the tuning parameters:
  gamma<-c(0.2,0.1,0.01,0.005,0)
  frc <- 1/(0.5^(2:6)) #defines the values of mu
  
  # generate the squence of the RKHS meta models:
  res <- RKHSMetMod(Y,X,kernel,Dmax,gamma,frc,FALSE)
  
  # call testing dataset:
  XT<- lXT[[s]]
  YT <- lYT[[s]]
  
  # calculate the prediction error:
  mu <- vector(length=length(frc))
  l <- length(gamma)
  for(i in 1:length(frc)){mu[i]=res[[(i-1)*l+1]]$mu}
  ErrPred <- PredErr(X, XT,YT,mu,gamma,res,kernel, Dmax)
  
  # choose (mu, gamma) that minimizes the prediction error: 
  minCritTest <-  which(ErrPred == min(ErrPred, na.rm=TRUE),arr.ind = TRUE)
  
  # calculate the empirical Sobol indices:
  Sobol.emp <- cbind(Sobol.emp,SI_emp(res,ErrPred))
  
  # result:
  TotRes[[s]] <-list(ErrPred=ErrPred,minCritTest=minCritTest,Sobol.emp=Sobol.emp,res=res)
  print(paste("Number of simulations=",Nb.sim))
  print("Mean of empirical Sobol indices")
  mean.estim <- apply(Sobol.emp,1,mean)
  print(data.frame(SI.wght.1= mean.estim))
  # save the result:
  save(list=ls(),file="RJ.Ex2.n200.matern.result.RData")
}
################################## CALL THE RESULTS #######################################
# call results when n=200:
load('RJ.Ex2.n200.matern.result.RData') # kernel="matern"
# load('RJ.Ex2.n200.brownian.result.RData') # kernel="brownian"
# load('RJ.Ex2.n200.gaussian.result.RData') # kernel="gaussian"

# call results when n=100:
# load('RJ.Ex2.n100.matern.result.RData') # kernel="matern"
# load('RJ.Ex2.n100.brownian.result.RData') # kernel="brownian"
# load('RJ.Ex2.n100.gaussian.result.RData') # kernel="gaussian"

# call results when n=50:
# load('RJ.Ex2.n50.matern.result.RData') # kernel="matern"
# load('RJ.Ex2.n50.brownian.result.RData') # kernel="brownian"
# load('RJ.Ex2.n50.gaussian.result.RData') # kernel="gaussian"

# the estimated empirical Sobol indices ×100 greater than 0.01 displayed in 3-th row of Table 6:
SI <- (mean.estim[mean.estim>10^{-2}])*100
print(SI)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 46.105485 26.328975 20.623767  2.996705  2.219894  1.127800 

# calculate GPE:
d <- 5
N <- 1000
library(lhs)
a <- c(0.2,0.6,0.8,100,100,100,100,100,100,100,100,100,100)[1:d]
XN <- maximinLHS(N, d)
g=1;for (i in 1:d) g=g*(abs(4*XN[,i]-2)+a[i])/(1+a[i])

# call the function Kv_new(X,XT,kernel,Dmax) 
# to calculate the Gram matrices using new experimental design XN
# the function Kv_new() is a part of the function PredErr()
library(Rcpp)
sourceCpp('Kv_new.cpp')
# number of repetitions is 50:
vMax <- sum(choose(d,1:Dmax))
Nr <- 50
errN <- vector()
for(j in 1:Nr){ 
  # call the experimetal design at each repetition:
  X<- lX[[j]]
  # calculate vMax Gram matrices using new experimental design XN:
  kvnew <- Kv_new(X,XN,kernel,Dmax)
  # call the estimated parameters of the "best" RKHS meta-model at each repetition:
  dx <- TotRes[[j]]$minCritTest
  indx <- dx[1]+((dx[2]-1)*5)
  tt <- TotRes[[j]]$res[[indx]]$`Meta-Model`$teta
  teta <- t(tt)
  fvi <- vector()
  fvnew <- NULL
  for(ii in 1:vMax){
    kvi <- kvnew[[ii]] #ii in 1:vMax
    fvi <-kvi%*%teta[,ii]
    fvnew <- cbind(fvnew,fvi)
  }
  c <- rep(TotRes[[j]]$res[[indx]]$`Meta-Model`$intercept,N)
  fvhat <- cbind(c,fvnew)
  # calculate the "best" RKHS meta-model using new experimental design XN:
  # it is the prediction at each repetition
  fhatnew <- apply(fvhat,1,sum)
  
  errN[j] <- (1/N)*(sum((fhatnew-g)^2)) 
}
# calculate GPE:
GPE <- sum(errN)/Nr
# the value of GPE displayed in the first row and 3-th column of Table 4:
print(GPE)
# [1] 0.03425556

# calculate MSE:

# true SI: 
sv <- c(43.3,24.3,19.2,5.63,4.45,2.50,0.57)

# vector of the estimated empirical SI:
# the components of vector mean.estim with the same index as the vector of true SI sv:
# groups: v1., v2., v3., v1.2., v1.3., v2.3., v1.2.3.
# index: 1,2,3,6,7,10,16 in vector mean.estim
sb <- c(mean.estim[1],mean.estim[2],mean.estim[3]
        ,mean.estim[6],mean.estim[7],mean.estim[10],mean.estim[16])
  
sb <- sb*100

# bias:
b <- sum((sb-sv)^2)

# variance:
sd <- apply(Sobol.emp,1,sd)
w <- c(sd[1],sd[2],sd[3],sd[6],sd[7],sd[10],sd[16])

# MSE value
MSE <- b+sum(w)
# the value of MSE displayed in the first row and 3-th column of Table 5:
print(MSE)
# [1] 28.22342
##################################### END CODE #############################################
# for example with n=200 and kernel="brownian" or kernel="gaussian" do the same procedure as for kernel="matern"
# by setting kernel <- "brownian" and kernel <- "gaussian"
# the experimental design is the same as for kernel "matern":
# load('RJ.Ex2.n200.ExperimentalDesign.RData')
# the results of example with n=200 and kernel="brownian" are stored in the file "RJ.Ex2.n200.brownian.result.RData"
# the results of example with n=200 and kernel="gaussian" are stored in the file "RJ.Ex2.n200.gaussian.result.RData"
# to reproduce the results of SI, GPE and MSE run the codes above from "# CALL THE RESULTS #" to "# END CODE #"
# summary of results with n=200 and kernel="brownian":
# print(SI)
#       v1.       v2.       v3.     v1.2.     v1.3. 
# 46.745541 26.676920 20.916912  2.467472  1.739927 
# print(GPE)
# [1] 0.04972734
# print(MSE)
# [1] 41.05749

# summary of results with n=200 and kernel="gaussian":
# print(SI)
#       v1.       v2.       v3.     v1.2. 
# 47.955002 27.042053 21.023422  1.400599 
# print(GPE)
# [1] 0.07300661
# print(MSE)
# [1] 67.01946
####################################### n=100 #################################################
# example with n=100 and kernel="matern": 
# load the datasets:
# load('RJ.Ex2.n100.ExperimentalDesign.RData')
# to reproduce all the results run the codes above from "# START CODE #" to "# END CODE #"
# to reproduce the results of SI, GPE and MSE run the codes above from "# CALL THE RESULTS #" to "# END CODE #"
# summary of results:
# print(SI)
#       v1.       v2.       v3.     v1.2.     v1.3. 
# 47.558878 25.926671 20.566050  2.073540  1.544644
# print(GPE)
# [1] 0.07559562
# print(MSE)
# [1] 46.72291

# for kernel="brownian" and kernel="gaussian" do the same procedure as for kernel="matern"
# by setting kernel <- "brownian" and kernel <- "gaussian"

# the experimental design is the same as for kernel "matern":
# load('RJ.Ex2.n100.ExperimentalDesign.RData')
# the results of example with n=100 and kernel="brownian" are stored in the file "RJ.Ex2.n100.brownian.result.RData"
# the results of example with n=100 and kernel="gaussian" are stored in the file "RJ.Ex2.n100.gaussian.result.RData"
# to reproduce the results of SI, GPE and MSE run the codes above from "# CALL THE RESULTS #" to "# END CODE #"
# summary of results with n=100 and kernel="brownian":
# print(SI)
#      v1.      v2.      v3. 
# 48.76371 26.92958 21.32919
# print(GPE)
# [1] 0.09657686
# print(MSE)
# [1] 84.99325

# summary of results with n=100 and kernel="gaussian":
# print(SI)
#      v1.      v2.      v3. 
# 49.56589 26.89075 20.86036
# print(GPE)
# [1] 0.1040278
# print(MSE)
# [1] 94.67396
########################################### n=50 #############################################
# example with n=50 and kernel="matern":
# load the datasets:
# load('RJ.Ex2.n50.ExperimentalDesign.RData')
# to reproduce all the results run the codes above from "# START CODE #" to "# END CODE #"
# to reproduce the results of SI, GPE and MSE run the codes above from "# CALL THE RESULTS #" to "# END CODE #"
# summary of results:
# print(SI)
#       v1.       v2.       v3.     v1.2. 
# 49.161318 25.927833 20.302154  1.266329  
# print(GPE)
# [1] 0.1335819
# print(MSE)
# [1] 75.12521

# for kernel="brownian" and kernel="gaussian" do the same procedure as for kernel="matern"
# by setting kernel <- "brownian" and kernel <- "gaussian"

# the experimental design is the same as for kernel "matern":
# load('RJ.Ex2.n50.ExperimentalDesign.RData')
# the results of example with n=50 and kernel="brownian" are stored in the file "RJ.Ex2.n50.brownian.result.RData"
# the results of example with n=50 and kernel="gaussian" are stored in the file "RJ.Ex2.n50.gaussian.result.RData"
# to reproduce the results of SI, GPE and MSE run the codes above from "# CALL THE RESULTS #" to "# END CODE #"
# summary of results with n=50 and kernel="brownian":
# print(SI)
#      v1.      v2.      v3. 
# 49.64665 27.87992 20.79316  
# print(GPE)
# [1] 0.1385501
# print(MSE)
# [1] 110.7119
# summary of results with n=50 and kernel="gaussian":
# print(SI)
#      v1.      v2.      v3. 
# 48.72976 26.37106 20.22625 
# print(GPE)
# [1] 0.1497919
# print(MSE)
# [1] 78.21762
################################################################################################
# Example 2: 
################################################################################################
# generate the learning dataset: 
d <- 10
n <- 1000
library(lhs)
X<- maximinLHS(n, d)
a <- c(0.2,0.6,0.8,100,100,100,100,100,100,100)[1:d]
g=1;for (i in 1:d) g=g*(abs(4*X[,i]-2)+a[i])/(1+a[i])
epsilon <- rnorm(n,0,1)
sigma <- 0.2
Y <- g + sigma*epsilon
# calculate the eigenvalues and eigenvectors of the Gram matrices K_v for all v in 1,...,vMax
# where vMax=sum(choose(d,1:Dmax))
kernel <- "matern"
Dmax <- 3
Kv <- calc_Kv(X,kernel,Dmax,TRUE,TRUE)
# calculate mu_max:
mumax <- mu_max(Y,Kv$kv)
# Step 1:
# first set gamma=0 and define a grid of values od mu:
gamma <- c(0)
mu_g <- c(mumax*0.5^(2:10))
# calculate the RKHS meta-models and gather them in the list res_g: 
resg <- list() ; res_g <- list()
for(i in 1:length(mu_g)){
  resg[[i]] <- RKHSgrplasso(Y,Kv, mu_g[i] , 1000, FALSE)
  res_g[[i]] <- list("mu_g"=mu_g,"gamma"=0,"MetaModel"=resg[[i]])
}

# generate the testing dataset: 
XT <- maximinLHS(n, d)
gT=1;for (i in 1:d) gT=gT*(abs(4*XT[,i]-2)+a[i])/(1+a[i])
epsilonT <- rnorm(n,0,1)
YT <- gT + sigma*epsilonT
# calculate the prediction error of the RKHS meta-models in res_g:
Err_g <- PredErr(X,XT,YT,mu_g,gamma,res_g,kernel,Dmax)

# find the value of mu_i which is the value of mu with the smallest error of prediction:
minCritTest <-  which(Err_g == min(Err_g, na.rm=TRUE),arr.ind = TRUE)
ind <- minCritTest[1,2]
# Step 2:
# set the new grid of values of mu:
index <- vector()
mu <- mu_g/sqrt(n)
ll <- length(mu)
if(ind==1){
   mu <- c(mu[ind],mu[ind+1],mu[ind+2])
   index <- c(ind,ind+1,ind+2)
   }
if(ind==ll){
   mu <- c(mu[ind-2],mu[ind-1],mu[ind])
   index <- c(ind-2,ind-1,ind)
   }
if(ind!=1&&ind!=ll){
   mu <- c(mu[ind-1],mu[ind],mu[ind+1])
   index <- c(ind-1,ind,ind+1)
   }
# set the grid of values of gamma>0:
gamma <- c(0.2, 0.1, 0.01, 0.005)
# calculate the sequence of the RKHS meta-models associated with new mu and gamma:
resgnew <- list()
for(i in 1:3){
   resgnew[[i]] <- resg[[index[i]]]
}
res <- pen_MetMod(Y,Kv,gamma,mu,resgnew,0,0)
# calculate the prediction error:
Err <- PredErr(X,XT,YT,mu,gamma,res,kernel,Dmax)

# the results of this example are stored in the file "RJ.Ex4.result.RData"
# call the results: 
load('RJ.Ex4.result.RData')
# print the results:
print(mumax)
# [1] 5.217221
print(mu_g)
# [1] 1.304305195 0.652152598 0.326076299 0.163038149 0.081519075 0.040759537 0.020379769 0.010189884 0.005094942
length(res_g)
# [1] 9
print(Err_g)
#           mu = 1.304305 mu = 0.652153 mu = 0.326076 mu = 0.163038 mu = 0.081519 mu = 0.04076
# gamma = 0     0.1969247     0.1563421      0.144809      0.097349    0.06343475   0.05513544
#           mu = 0.02038 mu = 0.01019 mu = 0.005095
# gamma = 0    0.0562901   0.06333747    0.07334751
print(ind)
# [1] 6
print(mu)
# [1] 0.0025778595 0.0012889297 0.0006444649
print(Err)
#               mu = 0.002578 mu = 0.001289 mu = 0.000644
# gamma = 0.2      0.15297228    0.13070157    0.11936140
# gamma = 0.1      0.09793322    0.07933739    0.07188460
# gamma = 0.01     0.06510466    0.05426732    0.05279821
# gamma = 0.005    0.06410582    0.05441954    0.05394214

# calculate the empirical sobol indices for the "best" RKHS meta-model:
SI <- (SI.minErr[SI.minErr>10^{-2}])*100
print(SI)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 42.911328 25.505785 20.812168  4.396788  3.844243  2.130742 
# sum of the SI:
sumSI <- sum(SI)
print(sumSI)
# [1] 99.60105
# calculate RE:
#  vector of true SI: 
sv <- c(43.3,24.3,19.2,5.63,4.45,2.50,0.57)
# vector of the estimated empirical SI:
# the components of vector SI.minErr with the same index as the vector of true SI sv:
# groups: v1., v2., v3., v1.2., v1.3., v2.3., v1.2.3.
# index: 1,2,3,11,12,20,56 in vector SI.minErr:
sb <- c(SI.minErr[1],SI.minErr[2],SI.minErr[3]
        ,SI.minErr[11],SI.minErr[12],SI.minErr[20],SI.minErr[56])

sb <- sb*100
# RE value:
RE <- sum(abs((sb-sv))/sv)
# the value of RE displayed in the last column of Table 9:
print(RE)
# [1] 1.644009
################################################################################################
# Example 3:
################################################################################################
# example with n=2000:
# generate the learning dataset: 
d <- 10
n <- 2000 # n <- 5000
library(lhs)
X<- maximinLHS(n, d)
a <- c(0.2,0.6,0.8,100,100,100,100,100,100,100)[1:d]
g=1;for (i in 1:d) g=g*(abs(4*X[,i]-2)+a[i])/(1+a[i])
epsilon <- rnorm(n,0,1)
sigma <- 0.2
Y <- g + sigma*epsilon
# calculate the sequence of the RKHS meta-models:
kernel <- "matern"
Dmax <- 3
# grid of values of the tuning parameters:
gamma <- c(0.01)
frc <- 1/(0.5^(7:8))

res <- RKHSMetMod(Y,X,kernel,Dmax,gamma,frc,FALSE) 

# generate the testing dataset: 
XT <- maximinLHS(n, d)
gT=1;for (i in 1:d) gT=gT*(abs(4*XT[,i]-2)+a[i])/(1+a[i])
epsilonT <- rnorm(n,0,1)
YT <- gT + sigma*epsilonT
# calculate the prediction error:
mu <- vector(); for(i in 1:length(frc)){mu[i] <- res[[i]]$mu} 
Err <- PredErr(X,XT, YT,mu,gamma, res, kernel,Dmax)
print(Err)
# calculate the empirical Sobol indices
SI <- SI_emp(res, NULL)
# the estimated empirical Sobol indices displayed in the first row of Table 12:
SI_mu1 <- SI[[1]]
si_mu1 <- (SI_mu1[SI_mu1>10^{-2}])*100
print(si_mu1)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 45.539248 24.776221 21.014232  3.962293  3.032355  1.652182 
# sum of the estimated empirical Sobol indices
print(sum(si_mu1))
# [1] 99.97653
# the estimated empirical Sobol indices displayed in the second row of Table 12:
SI_mu2 <- SI[[2]]
si_mu2 <- (SI_mu2[SI_mu2>10^{-2}])*100
print(si_mu2)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 45.382212 25.070399 19.694360  4.355889  3.663572  1.790697
# sum of the estimated empirical Sobol indices
print(sum(si_mu2))
# [1] 99.95713

# calculate RE:
#  vector of true SI: 
sv <- c(43.3,24.3,19.2,5.63,4.45,2.50,0.57)
# vector of the estimated empirical SI:
# the components of vector SI_mu1 with the same index as the vector of true SI sv:
# groups: v1., v2., v3., v1.2., v1.3., v2.3., v1.2.3.
# index: 1,2,3,11,12,20,56 in vector SI.minErr:
sb <- c(SI_mu1[1],SI_mu1[2],SI_mu1[3],SI_mu1[11],SI_mu1[12],SI_mu1[20],SI_mu1[56])
sb <- sb*100
# RE value:
RE <- sum(abs((sb-sv))/sv)
# the value of RE displayed in the first row and last column of Table 12:
print(RE)
# [1] 2.119721
# the value of RE displayed in the second row and last column of Table 12:
sb <- c(SI_mu2[1],SI_mu2[2],SI_mu2[3],SI_mu2[11],SI_mu2[12],SI_mu2[20],SI_mu2[56])
sb <- sb*100
RE <- sum(abs((sb-sv))/sv)
print(RE)
# [1] 1.792294

# results with n=5000 and mu_1:
print(si_mu1)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 44.772791 25.392829 20.052468  4.486283  3.376972  1.900450 
sum(si_mu1)
# [1] 99.98179
print(RE)
# [1] 1.807482
# results with n=5000 and mu_2:
print(si_mu2)
#       v1.       v2.       v3.     v1.2.     v1.3.     v2.3. 
# 43.782300 24.995284 19.558852  5.432377  3.896154  2.316176 
sum(si_mu2)
# [1] 99.98114
print(RE)
# [1] 1.291532
# load('RJ.Ex5.n5000.mu2.result.RData')
#SI <- SI_emp(res, NULL)
#SI_mu2 <- SI[[1]]
#si_mu2 <- (SI_mu2[SI_mu2>10^{-2}])*100
#print(si_mu2)
#fitted <- res[[1]]$`Meta-Model`$fitted
#plot(Y,fitted,xlab="g-function",ylab="RKHS meta-model")
#abline(0,1,col="red")
#plot(SI_mu2,ylab="Estimated SI", xlab="Groups")
#text(SI_mu2, names(SI_mu2), cex=0.6, pos=4, col="red")
################################################################################################
# Timing plot:
n <- c(100,500,1000,2000,5000)
#calc_Kv
tkv <- c(0.09,33,197,1498,34282)
#mu_max
tmu <- c(0.01,9,53,420,6684)
#RKHSgrplasso
tgrmu1 <- c(1,247,959,3984,38957)
tgrmu2 <- c(2,599,2757,12951,99221)
#pen_MetMod
tpenmu1 <- c(2,333,1336,4664,49987)
tpenmu2 <- c(3,816,4345,22385,111376)
#sum
tsummu1 <- tkv+tmu+tgrmu1+tpenmu1
tsummu2 <- tkv+tmu+tgrmu2+tpenmu2
par(mfrow=c(1,1))
plot(log(n),log(tkv),type='b',ylab="log(t(s))",xlab='log(n)',ylim=c(-5,12),col=3)
lines(log(n), log(tmu),type='b', col=7)
lines(log(n), log(tgrmu1),type='b', col=1)
lines(log(n), log(tgrmu2),type='b',lty=2, col=1)
lines(log(n), log(tpenmu1),type='b', col=2)
lines(log(n), log(tpenmu2),type='b',lty=2, col=2)
#lines(log(n), log(tsummu1),type='b', col=5)
#lines(log(n), log(tsummu2),type='b',lty=2, col=5)
legend(x="bottomright",legend=c("calc_Kv","mu_max","RKHSgrplasso(mu_1)","RKHSgrplasso(mu_2)","pen_MetMod(mu_1)","pen_MetMod(mu_2)"),
       bty = "n",
       lty=c(1,1,1,2,1,2),
       col=c(3,7,1,1,2,2),
       lwd=2,
       cex=0.8)
################################################################################################
# Example 4:
################################################################################################
# generate the learning dataset: 
# n <- 12; d <- 1 
sinewave <- function(x){3*sin(5*pi*x)*x+cos(7*pi*x)}
X <- as.matrix(seq(0,1,1/11))
Y <- sinewave(X)

predict_X <- as.matrix(seq(0,1,1/99))

library(RobustGaSP)
res.rgasp <- rgasp(design=X, response=Y, kernel_type="matern_3_2")
rgasp.predict <- predict(res.rgasp, predict_X)

library(DiceKriging)
res.km <- km(design=X, response=Y, covtype="matern3_2")
km.predict <- predict(res.km, predict_X, type='UK')

library(RKHSMetaMod)
kernel <- "matern"; Dmax <- 1 
# grid of values of the tuning parameters:
gamma <- c(0.2, 0.1, 0.01, 0.005,0); frc <- 1/(0.5^(2:10))
res <- RKHSMetMod(Y, X, kernel, Dmax, gamma, frc, FALSE)

mu <- vector(length=length(frc))
l <- length(gamma)
for(i in 1:length(frc)){mu[i]=res[[(i-1)*l+1]]$mu}

XT <- as.matrix(seq(0,1,1/11)); YT <- sinewave(XT)
Err <- PredErr(X, XT,YT,mu,gamma,res,kernel, Dmax)

#Download the functions 'Kv_new.cpp' and 'prediction.R' from: https://github.com/halalehkamari/RKHSMetaMod
Rcpp::sourceCpp('./Kv_new.cpp')
prediction <- function(X, Xnew, kernel, Dmax, res, Err){
  Kv.new <- Kv_new(X, Xnew, kernel, Dmax)[[1]]
  minCritTest <-  which(Err == min(Err, na.rm=TRUE),arr.ind = TRUE)
  l <- dim(Err)[1]
  nbr <- l*(minCritTest[2]-1)+minCritTest[1]
  tetavs <- res[[nbr]]$`Meta-Model`$teta 
  fvnew <- Kv.new%*%t(tetavs) 
  fhatnew <- apply(fvnew,1,sum)
  return(fhatnew)
}
res.predict <- prediction(X, predict_X, kernel, Dmax, res, Err) 

predict_Y <- sinewave(predict_X)
#Plots:
par(mfrow=c(1,1))
plot(predict_Y,res.predict, xlab='m(X)',ylab='prediction')
points(predict_Y,km.predict$mean,col=4)
points(predict_Y,rgasp.predict$mean,col=3)
abline(0,1,col=2)

par(mfrow=c(1,1))
plot(predict_X, predict_Y, type='l', xlab='input',ylab='output', col=2)
lines(predict_X, res.predict, type='l')
lines(predict_X, rgasp.predict$mean, type='l', col=3)
lines(predict_X, km.predict$mean, type='l', col=4)
legend(x="bottomleft",legend=c("m(X)","RKHS meta-model","GP meta-model (RobustGaSP)","GP meta-model (DiceKriging)"),
       bty = "n",
       lty=c(1,1,1,1),
       col=c(2,1,3,4),
       lwd=2,
       cex=0.8)
################################################################################################
# Example 5:
################################################################################################
# generate the learning dataset: 
n <- 80; d <- 8
library(lhs); X <- optimumLHS(n, d); XT <- optimumLHS(n, d)
library(sensitivity); Y <- sobol.fun(X); YT <- sobol.fun(XT)
# calculate the sequence of the RKHS meta-models:
kernel <- "matern"; Dmax <- 3 
# grid of values of the tuning parameters:
gamma <- c(0.2, 0.1, 0.01, 0.005,0); frc <- 1/(0.5^(2:10))
start.time <- proc.time()
res <- RKHSMetMod(Y, X, kernel, Dmax, gamma, frc, FALSE)
print(proc.time() - start.time)
#utilisateur     système      écoulé 
#    392.593       7.474     401.925 
# calculate the prediction error:
l <- length(gamma); mu <- vector(); for(i in 1:length(frc)){mu[i] <- res[[(i-1)*l+1]]$mu}
Err <- PredErr(X, XT, YT, mu, gamma, res, kernel, Dmax)
minCritTest <-  which(Err == min(Err, na.rm=TRUE),arr.ind = TRUE)
print(minCritTest)
#row col
#gamma = 0.01   3  14
# calculate the empirical Sobol indices
start.time <- proc.time()
SI <- SI_emp(res,Err)
print(proc.time() - start.time)
#utilisateur     système      écoulé 
#      0.012       0.002       0.017  

# Calculate kriging meta-model:
library(DiceKriging)
start.time <- proc.time()
res.km <- km(design = X, response = Y, covtype = "matern3_2")
print(proc.time() - start.time)
#utilisateur     système      écoulé 
#      0.144       0.028       0.178
# Calculate Sobol indices for kriging meta-model:
kriging.mean <- function(Xnew, m){
  predict.km(m, Xnew, "UK", se.compute = FALSE, checkNames = FALSE)$mean
}
start.time <- proc.time()
SI.km <- fast99(model = kriging.mean, factors = d, n = 1000,
                       q = "qunif", q.arg = list(min = 0, max = 1), m = res.km)
print(proc.time() - start.time)
#utilisateur     système      écoulé 
#      0.182       0.010       0.201
print(SI.km)
# Calculate mse:
nbr <- l*(minCritTest[2]-1)+minCritTest[1]
rkhsmetamod <- res[[nbr]]$`Meta-Model`$fitted
fitted.km <- plot(res.km)$mean

MSE.rkhsmetamod <- sum((Y-rkhsmetamod)^2)/n
#[1] 0.0007110671
MSE.km <- sum((Y-fitted.km)^2)/n
#[1] 0.03957234

# Plots:
par(mfrow=c(1,1))
plot(Y,rkhsmetamod, xlab='m(X)',ylab='meta-model')
points(Y,fitted.km,col=4)
abline(0,1,col='red')
## Sobol function:
sobol.fun <- function(X) {
  a <- c(0, 1, 4.5, 9, 99, 99, 99, 99)
  y <- 1
  for (j in 1:8) {
    y <- y * (abs(4 * X[, j] - 2) + a[j]) / (1 + a[j])
  }
  y
}
# True Sobol indices:
a <- c(0, 1, 4.5, 9, 99, 99, 99, 99)
# a <- c(0.2,0.6,0.8,100,100,100,100,100,100,100)[1:d]
Das <- vector()  
for (i in 1:d) Das[i]=(1/(3*(1+a[i])^2))
D=1;for (i in 1:d) D=D*(Das[i]+1)
D <- D-1
# First order Sobol indices
Si <- Das/D 
Si <- (Si[Si*100>10^{-2}])*100
# Second order Sobol indices
Sij <- matrix(nrow=d,ncol=d)
for(i in 1:d) for(j in 1:d) if(i<j) Sij[i,j] <- (Das[i]*Das[j]/D) else Sij[i,j] <- 0
Sij <- Sij*100
#Third order Sobol indices: Sijk
((Das[1]*Das[2]*Das[3])/D)*100>10^{-2}
((Das[1]*Das[2]*Das[4])/D)*100>10^{-2}
((Das[2]*Das[3]*Das[4])/D)*100<10^{-2}
## 
Sv <- c(71.62,17.90,2.37,0.72,5.97,0.79,0.24,0.20,0.06,0.07,0.02)
#Calculate RE:
SI <- (SI[SI*100>10^{-2}])*100
#SI <- c(75.78,17.42,1.71,0.47,4.00,0.05,0.07,0.28,0.09,0,0)
SI.km <- SI.km$D1/SI.km$V
SI.km <- (SI.km[SI.km*100>10^{-2}])*100
#SI.km <- c(71.18,15.16,1.42,0.44,0,0,0,0,0,0,0)

RESI <- sum(abs((SI-Sv))/Sv)
#[1] 5.585629
RESI.km <- sum(abs((SI.km-Sv))/Sv)
#[1] 7.948949
# save(list=ls(),file="Comparison_DiceKriging_Sensitivity_example.RData")
# load("Comparison_DiceKriging_Sensitivity_example.RData")



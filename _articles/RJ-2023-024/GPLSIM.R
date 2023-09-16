# This script includes all codes used to produce real data analysis and simulation study
# in the R journal manuscript, titled
# GPLSIM: An R Package for Penalized Spline Estimation for Generalized Partially Linear Single-index Models.
#
# Authors: Tianhai Zu and Yan Yu.
# Original submitted: October 15 2020
# Modified: Aug 15 2022
# The GPLSIM package proposed in the manuscript is available at Cran
# install.packages("gplsim")

# supporting function to make truncated power spline
smooth.construct.tr.smooth.spec <- gplsim::smooth.construct.tr.smooth.spec
##### end of supporting functions;

## Air Pollution Data
library(gplsim) #load the proposed package
data(air) #load the built-in data of the package
y=air$ozone               # response
X=as.matrix(air[,c(3,4,2)])    # single index term ;
colnames(X)=colnames(air[,c(3,4,2)])
Z=NULL

#fit single-index model
air.fit <- gplsim(y,X,Z=Z,family = gaussian,bs="ps")
summary(air.fit)

#plot the estimated unknown univariate single index function
plot_si(air.fit,yscale=c(1,6),plot_data=TRUE)

#NLS algorithm
air.fit <- gplsim(y,X,Z=Z,family = gaussian,profile = FALSE,bs="ps")
summary(air.fit)


## simulation
set.seed(2020)
# Gaussian case
# parameter settings
n=1000
M=200
true.theta = c(1, 1, 1)/sqrt(3)
# This function generate a plain sin bump model with gaussian response.
data <- generate_data(n,true.theta=true.theta,family="gaussian",ncopy=M)
y=(data$Y)[[1]]       # continuous response
X=data$X       # single index term ;
Z=data$Z       # partially linear term ;

#fit generalized single-index partial linear model
result <- gplsim(y,X,Z,user.init=NULL,family = gaussian)
summary(result)

#plot the estimated single index function curve from simulation
sort_index = order(X%*%true.theta)
plot((X%*%true.theta)[sort_index],data$single_index_values[sort_index],lty=1,xaxt="n", yaxt="n",col="red",type="l",xlab="single index",ylab="mean")
add_sim_bound(data,family = gaussian(),M=M,n=n,true.theta=true.theta)
legend("topright",legend=c("Mean Fit Curve", "True Curve"),lty=c(1,1),col = c("black","red"))



## Table 1
#table 1

library(doParallel)
library(foreach)
cl <- makeCluster(12)
registerDoParallel(cl)

# Gaussian case
# parameter settings
M=200
gaussian_coefs <- foreach(i=1:M, .combine=rbind,.packages = c("gplsim","splines","mgcv")) %dopar%{
        n=1000
        true.theta = c(1, 1, 1)/sqrt(3)
        # This function generate a plain sin bump model with gaussian response.
        data <- generate_data(n,true.theta=true.theta,family="gaussian")
        y=data$Y       # continous response
        X=data$X       # single index term ;
        Z=data$Z       # partially linear term ;
        result <- gplsim(y,X,Z,user.init=NULL,family = gaussian)
        c(result$theta,result$gamma)
}

apply(gaussian_coefs,2,mean)
apply(gaussian_coefs,2,sd)

# Binomial case
# parameter settings
M=200
binomial_coefs <- foreach(i=1:M, .combine=rbind,.packages = c("gplsim","splines","mgcv")) %dopar%{
        n=1000
        true.theta = c(1, 1, 1)/sqrt(3)
        # This function generate a plain sin bump model with Binomial response.
        data <- generate_data(n,true.theta=true.theta,family="binomial")
        y=data$Y       # Binomial response
        X=data$X       # single index term ;
        Z=data$Z       # partially linear term ;
        result <- gplsim(y,X,Z,user.init=true.theta,family = binomial)
        c(result$theta,result$gamma)
}

apply(binomial_coefs,2,mean)
apply(binomial_coefs,2,sd)

# poisson case
# parameter settings
M=200
poisson_coefs<- foreach(i=1:M, .combine=rbind,.packages = c("splines","mgcv","gplsim")) %dopar%{
        n=1000
        true.theta = c(1, 1, 1)/sqrt(3)
        # This function generate a plain sin bump model with gaussian response.
        data <- generate_data(n,true.theta=true.theta,family="poisson")
        y=data$Y       # poisson response
        X=data$X       # single index term ;
        Z=data$Z       # partially linear term ;
        result <- gplsim(y,X,Z,user.init=NULL,family = poisson(link = "log"),k=13)
        c(result$theta,result$gamma)
}

apply(poisson_coefs,2,mean)
apply(poisson_coefs,2,sd)
stopCluster(cl)

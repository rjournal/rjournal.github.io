library(RobustCalibration)


##Example 1: Bayarri et al. 2007, model calibration with replicates
##10 calibration inputs, each containing 3 replicates
input=c(.110, .432, .754, 1.077, 1.399, 1.721, 2.043, 2.366, 2.688,3.010)
n=length(input)
k=3
output=t(matrix(c(4.730,4.720,4.234,3.177,2.966,3.653,1.970,2.267,2.084,2.079,2.409,2.371,1.908,1.665,1.685,
                  1.773,1.603,1.922,1.370,1.661,1.757,1.868,1.505,1.638,1.390,1.275,1.679,1.461,1.157,1.530),k,n))
#computer model
Bayarri_07<-function(x,theta){
  5*exp(-x*theta) 
}
#range of calibration parameters
theta_range=matrix(c(0,50),1,2) 

##fitting No-discrepancy, comparing  MLE with posterior samples
set.seed(1)
X=matrix(1,n,1)
m_no_discrepancy=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
                              X =X, have_trend = T,discrepancy_type = 'no-discrepancy')
m_no_discrepancy

m_no_discrepancy_small_sd=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
                              sd_proposal = c(rep(0.025,dim(theta_range)[1]),
                                              rep(0.25,dim(as.matrix(input) )[2]),0.25),
                              X =X, have_trend = T,discrepancy_type = 'no-discrepancy')
m_no_discrepancy_small_sd

testing_input=seq(0,5,5/199)
X_testing=matrix(1,length(testing_input),1)
m_no_discrepancy_pred=predict(m_no_discrepancy,testing_input,math_model=Bayarri_07,
                              interval_est=c(0.025, 0.975),X_testing=X_testing) 
# or one can use this one
# m_no_discrepancy_pred=predict.rcalibration(m_no_discrepancy,testing_input,math_model=Bayarri_07,
#                               interval_est=c(0.025, 0.975),X_testing=X_testing) 

# prediction with mle 
m_no_discrepancy_mle=rcalibration(input,output,math_model=Bayarri_07,X =X, have_trend = T,
                                  theta_range=theta_range,discrepancy_type = 'no-discrepancy',method='mle')
m_no_discrepancy_mle
m_no_discrepancy_mle_pred=predict(m_no_discrepancy_mle,testing_input,math_model=Bayarri_07,
                                  interval_est=c(0.025, 0.975),X_testing=X_testing)

#fitting GaSP calibration, comparing MLE with posterior samples 
#one may adjust the number of MCMC samples to make it more robust
m_gasp=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
                    X =X, have_trend = T,discrepancy_type = 'GaSP')
m_gasp

m_gasp_pred=predict(m_gasp,testing_input,math_model=Bayarri_07,
                    interval_est=c(0.025, 0.975),X_testing=X_testing) 

# mle for gasp calibration
# m_gasp_mle=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
#                     X =X, have_trend = T,discrepancy_type = 'GaSP',method='mle')
# m_gasp_mle
# m_gasp_mle_pred=predict(m_gasp_mle,testing_input,math_model=Bayarri_07,
#                         interval_est=c(0.025, 0.975),X_testing=X_testing)

##fitting S-GaSP, comparing MLE with posterior samples


m_sgasp=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
                     X =X, have_trend = T,discrepancy_type = 'S-GaSP')
m_sgasp

m_sgasp_pred=predict(m_sgasp,testing_input,math_model=Bayarri_07,
                     interval_est=c(0.025, 0.975),X_testing=X_testing) 

# mle for sgasp calibration

# m_sgasp_mle=rcalibration(input, output,math_model = Bayarri_07,theta_range = theta_range,
#                     X =X, have_trend = T,discrepancy_type = 'S-GaSP',method='mle')
# m_sgasp_mle
# m_sgasp_mle_pred=predict(m_sgasp_mle,testing_input,math_model=Bayarri_07,
#                          interval_est=c(0.025, 0.975),X_testing=X_testing)



##the unknown reality
reality<-function(x){
  3.5*exp(-1.7*x)+1.5
}

truth=reality(testing_input)

##get this for plots
input_replicates=as.vector(t(matrix(input,n,k)))
output_replicates=as.vector(t(output))
X_replicates=matrix(1,n*k,1)



##math model only
pdf('Bayarri_2007_math_model.pdf',height=5,width=3)
plot(testing_input, m_sgasp_pred@math_model_mean,type='l',  col='blue',lty=2,
     xlab='x', ylab='y', ylim=c(min(m_sgasp_pred@interval[,1]),max(m_sgasp_pred@interval[,2])),
     main='CM+trend',mgp=c(2.5,1,0))
polygon( c(testing_input,rev(testing_input)), c(m_no_discrepancy_pred@interval[,1],
                                                rev(m_no_discrepancy_pred@interval[,2])), col = "grey80", border = F)
lines(testing_input, truth)
lines(testing_input,m_gasp_pred@math_model_mean, type='l', col='cyan',lty=2)
lines(testing_input,m_sgasp_pred@math_model_mean, type='l', col='blue',lty=2)
lines(testing_input,m_no_discrepancy_pred@math_model_mean, type='l', col='red',lty=2)
lines(input_replicates,output_replicates,type='p',pch=20)
legend("topright", c('reality', 'GaSP','S-GaSP','no-discrepancy'),
       lty=c(1,2,2,2),col=c('black','cyan','blue','red'),cex=.8)
dev.off()
##math model and discrepancy
pdf('Bayarri_2007_math_model_discrepancy.pdf',height=5,width=3)
plot(testing_input, m_sgasp_pred@mean,type='l', col='blue',
     xlab='x', ylab='y', 
     ylim=c(min(m_gasp_pred@interval[,1]),max(m_gasp_pred@interval[,2])),
     main='CM+trend+discrepancy',mgp=c(2.5,1,0))
polygon( c(testing_input,rev(testing_input)), c(m_gasp_pred@interval[,1],
                                                rev(m_gasp_pred@interval[,2])), col = "grey80", border = F)
lines(testing_input, truth)
lines(testing_input,m_gasp_pred@mean, type='l', col='cyan')
lines(testing_input,m_sgasp_pred@mean, type='l', col='blue')
#lines(testing_input,m_no_discrepancy_pred@mean, type='l', col='darkgreen')
legend("topright", c('reality', 'GaSP','S-GaSP'),
       lty=c(1,1,1),col=c('black','cyan','blue'),cex=1)
input_replicates=as.vector(t(matrix(input,n,k)))
lines(input_replicates,output_replicates,type='p',pch=20)
dev.off()

##posterior samples
pdf('Bayarri_2007_post_samples.pdf',height=5,width=3)
plot(m_gasp@post_sample[,1],m_gasp@post_sample[,5],pch=17,col='cyan',cex=.5,type='p',
     xlab=expression(theta),ylab=expression(theta[m]),mgp=c(2.5,1,0),main='posterior samples')
lines(m_sgasp@post_sample[,1],m_sgasp@post_sample[,5],pch=20,col='blue',cex=.5,type='p')
lines(m_no_discrepancy@post_sample[,1],m_no_discrepancy@post_sample[,3],pch=15,col='red',cex=.5,type='p')
legend("bottomright", c('GaSP','S-GaSP','no-discrepancy'),
       pch=c(17,20,15),col=c('cyan','blue','red'),cex=.8)
dev.off()

###RMSE in prediction by math model and trend
sqrt(mean((m_gasp_pred@math_model_mean-truth)^2))
sqrt(mean((m_sgasp_pred@math_model_mean-truth)^2))
sqrt(mean((m_no_discrepancy_pred@math_model_mean-truth)^2))




###RMSE in prediction by math model, trend and discrepancy
sqrt(mean((m_gasp_pred@mean-truth)^2))
sqrt(mean((m_sgasp_pred@mean-truth)^2))



##coverage 
n_testing=length(testing_input)
length(which( m_gasp_pred@interval[,2]>truth & m_gasp_pred@interval[,1]<truth))/n_testing
length(which( m_sgasp_pred@interval[,2]>truth & m_sgasp_pred@interval[,1]<truth))/n_testing
length(which( m_no_discrepancy_pred@interval[,2]>truth & m_no_discrepancy_pred@interval[,1]<truth))/n_testing

##average length

mean(m_gasp_pred@interval[,2]-m_gasp_pred@interval[,1])
mean(m_sgasp_pred@interval[,2]-m_sgasp_pred@interval[,1])
mean(m_no_discrepancy_pred@interval[,2]-m_no_discrepancy_pred@interval[,1])







###Example 2: Lorenz 96, a difficult example for chaotic system
###this is directly using the solver, another example to use emulator will be discussed later
Lorenz_96<-function(param,x){
  dx <- rep(0, n1)
  dx[1] = (x[2] - x[n1-1])*x[n1] - x[1] + param
  dx[2] = (x[3] - x[n1])*x[1] - x[2] + param
  dx[n1] = (x[1] - x[n1-2])*x[n1-1] - x[n1]+param
  for(i in 3:(n1-1))
    dx[i] = (x[i+1] - x[i-2])*x[i-1] - x[i]+param
  return(dx)
}

##implementation of Runge-Kutta of order 4

Lorenz_96_Solved<-function(input,param ){
  x_record <- matrix(NA, n1, n2)
  x_temp <- x0
  
  for(i in 1:(n2))
  {
    k1 <- Lorenz_96(param=param, x_temp)
    k2 <- Lorenz_96(param=param,  x_temp + h*k1/2)
    k3 <- Lorenz_96(param=param,  x_temp + h*k2/2)
    k4 <- Lorenz_96(param=param, x_temp + h*k3)
    x_temp <- x_temp + (1/6)*h*(k1+2*k2+2*k3+k4)
    x_record[,i] <- x_temp
    t <- t + h
  }
  return(x_record[input])
}

n1=40   # 40 state components
n2=40   # L = 40 time steps
N=n2*n1
set.seed(1)
p0 = (rWishart(1, df=n1, Sigma=diag(n1)))[,,1]
L = t(chol(p0))
x0 <- as.vector(L%*%matrix(rnorm(n1),n1,1))
t = 0 
h =0.05 # step-size
input1 <- x0


reality_all=Lorenz_96_Solved(1:(n1*n2),param=8)

sigma_0 = 1
obs_all = reality_all + sigma_0*rnorm(N)

obs_mat <- matrix(obs_all,n1,n2)
reality_mat <- matrix(reality_all,n1,n2)

# #plot of the reality
library(plot3D)
image2D(t(reality_mat),  xlab="Time step", ylab="State components", x=0.05*seq(1,n2), y=seq(1,n1),
        main = "Latent states")

image2D(t(obs_mat), xlab="Time step", ylab="State components",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "Observations")

sqrt(mean( (obs_mat-reality_mat)^2))
sd(reality_mat)

##test scenario 1: no-discrepancy in reality, but it contains a large noise
##4 obs for each state
prop_obs=0.05
observations=rep(NA,n1*n2*prop_obs)
index_obs=NULL
design=matrix(NA,n1*n2*prop_obs,2)
for(i in 1:n2){
  index_sample=sample(1:n1,n1*prop_obs)
  
  count=(i-1)*n1*prop_obs+(1:(n1*prop_obs))
  observations[count]=obs_mat[index_sample,i]
  index_obs[count]=(i-1)*n1+index_sample
  design[count,]=cbind(index_sample,rep(i,n1*prop_obs))
}

##specify theta range
theta_range=matrix(c(-20,20),1,2)

##calibration
m_no_discrepancy=rcalibration(design=design,observations=observations,theta_range=theta_range,
             math_model=Lorenz_96_Solved,discrepancy_type = 'no-discrepancy')

m_no_discrepancy_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                              math_model=Lorenz_96_Solved,discrepancy_type = 'no-discrepancy',
                              method='mle',num_initial_values = 5)


m_gasp=rcalibration(design=design,observations=observations,theta_range=theta_range,
                    math_model=Lorenz_96_Solved,discrepancy_type = 'GaSP')

m_gasp_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                    math_model=Lorenz_96_Solved,discrepancy_type = 'GaSP',
                    method='mle',num_initial_values = 5)


m_sgasp=rcalibration(design=design,observations=observations,theta_range=theta_range,
                    math_model=Lorenz_96_Solved,discrepancy_type = 'S-GaSP')

m_sgasp_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                     math_model=Lorenz_96_Solved,discrepancy_type = 'S-GaSP',
                     method='mle',num_initial_values = 5)


testing_input=matrix(NA,n1*n2,2)
  
  for(i in 1:n2){
    count=(i-1)*n1+1:n1
    testing_input[count,]=cbind(1:n1,rep(i,n1))

  }


pred_m_no_discrepancy=predict(m_no_discrepancy,testing_input,math_model=Lorenz_96_Solved)
pred_m_no_discrepancy_mle=predict(m_no_discrepancy_mle,testing_input,math_model=Lorenz_96_Solved)

pred_m_gasp=predict(m_gasp,testing_input,math_model=Lorenz_96_Solved)
pred_m_gasp_mle=predict(m_gasp_mle,testing_input,math_model=Lorenz_96_Solved)

pred_m_sgasp=predict(m_sgasp,testing_input,math_model=Lorenz_96_Solved)
pred_m_sgasp_mle=predict(m_sgasp_mle,testing_input,math_model=Lorenz_96_Solved)

## the results for posterior distribution is more stable than MLE 
sqrt(mean( (pred_m_no_discrepancy@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_no_discrepancy_mle@math_model_mean-as.vector(reality_mat))^2))

sqrt(mean( (pred_m_gasp@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp_mle@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp@mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp_mle@mean-as.vector(reality_mat))^2))


sqrt(mean( (pred_m_sgasp@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp_mle@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp@mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp_mle@mean-as.vector(reality_mat))^2))



library(plot3D)
pdf('Lorenz_96_reality.pdf',height=5,width=3)
image2D(t(reality_mat),  xlab="Time", ylab="State", x=0.05*seq(1,n2), y=seq(1,n1), 
        main = "Latent states",zlim=c(-13,18))
dev.off()
pdf('Lorenz_96_obs.pdf',height=5,width=3)
image2D(t(obs_mat), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "Observations",zlim=c(-13,18))
points(0.05*design[,2],design[,1],cex=.5,lwd=1,pch=1)
dev.off()

pdf('sample_theta_Lorenz_96.pdf',height=5,width=3)
xlim=c(7.8,8.2)
hist(m_no_discrepancy@post_sample[,1], breaks=60,
     col='red',xlim=xlim,ylim=c(0,1000),xlab=expression(theta),ylab='counts',main='posterior samples')
hist(m_gasp@post_sample[,1],col='cyan',breaks=60, add=T)
hist(m_sgasp@post_sample[,1],col='blue', breaks=60,add=T)
legend('topright',legend=c("No-discrepancy","GaSP","S-GaSP"),lwd=2,cex=.8,
       col=c('red','cyan','blue'))
dev.off() 

pdf('no_discrepancy_Lorenz_96_obs.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_no_discrepancy@math_model_mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, no-discrepancy",zlim=c(-0.25,0.25))
dev.off()
pdf('gasp_Lorenz_96_obs.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_gasp@math_model_mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, GaSP",zlim=c(-0.25,0.25))
dev.off()

pdf('sgasp_discrepancy_Lorenz_96_obs.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_sgasp@math_model_mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, S-GaSP",zlim=c(-0.25,0.25))
dev.off()


##test case two, contain discrepancy and large noise
reality_all=Lorenz_96_Solved(param=8)+2*testing_input[,2]*0.05*(sin(2*pi*testing_input[,1]/40))

sigma_0 = 1
obs_all = reality_all + sigma_0*rnorm(N)

obs_mat <- matrix(obs_all,n1,n2)
reality_mat <- matrix(reality_all,n1,n2)


##get obs using previous design
prop_obs=0.05
observations=rep(NA,n1*n2*prop_obs)
for(i in 1:n2){
  count=(i-1)*n1*prop_obs+(1:(n1*prop_obs))
  index_sample=design[count,1]
  observations[count]=obs_mat[index_sample,i]
}


obs_mat[design[,1],design[,2]]

theta_range=matrix(c(-20,20),1,2)

###no discrepancy, gasp and sgasp
m_no_discrepancy=rcalibration(design=design,observations=observations,theta_range=theta_range,
                              math_model=Lorenz_96_Solved,discrepancy_type = 'no-discrepancy')

m_no_discrepancy_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                                  math_model=Lorenz_96_Solved,discrepancy_type = 'no-discrepancy',
                                  method='mle',num_initial_values = 5)


m_gasp=rcalibration(design=design,observations=observations,theta_range=theta_range,
                    math_model=Lorenz_96_Solved,discrepancy_type = 'GaSP')

m_gasp_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                        math_model=Lorenz_96_Solved,discrepancy_type = 'GaSP',
                        method='mle',num_initial_values = 5)


m_sgasp=rcalibration(design=design,observations=observations,theta_range=theta_range,
                     math_model=Lorenz_96_Solved,discrepancy_type = 'S-GaSP')

m_sgasp_mle=rcalibration(design=design,observations=observations,theta_range=theta_range,
                         math_model=Lorenz_96_Solved,discrepancy_type = 'S-GaSP',method='mle',num_initial_values = 5)


testing_input=matrix(NA,n1*n2,2)

for(i in 1:n2){
  count=(i-1)*n1+1:n1
  testing_input[count,]=cbind(1:n1,rep(i,n1))

}


pred_m_no_discrepancy=predict(m_no_discrepancy,testing_input,math_model=Lorenz_96_Solved)
pred_m_no_discrepancy_mle=predict(m_no_discrepancy_mle,testing_input,math_model=Lorenz_96_Solved)

pred_m_gasp=predict(m_gasp,testing_input,math_model=Lorenz_96_Solved)
pred_m_gasp_mle=predict(m_gasp_mle,testing_input,math_model=Lorenz_96_Solved)

pred_m_sgasp=predict(m_sgasp,testing_input,math_model=Lorenz_96_Solved)
pred_m_sgasp_mle=predict(m_sgasp_mle,testing_input,math_model=Lorenz_96_Solved)


sqrt(mean( (pred_m_no_discrepancy@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_no_discrepancy_mle@math_model_mean-as.vector(reality_mat))^2))

sqrt(mean( (pred_m_gasp@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp_mle@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp@mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_gasp_mle@mean-as.vector(reality_mat))^2))



sqrt(mean( (pred_m_sgasp@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp_mle@math_model_mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp@mean-as.vector(reality_mat))^2))
sqrt(mean( (pred_m_sgasp_mle@mean-as.vector(reality_mat))^2))


pdf('Lorenz_96_reality_with_discrepancy.pdf',height=5,width=3)
image2D(t(reality_mat),  xlab="Time", ylab="State", x=0.05*seq(1,n2), y=seq(1,n1), 
        main = "Latent states",zlim=c(-13,18))
dev.off()

pdf('Lorenz_96_obs_with_discrepancy.pdf',height=5,width=3)
image2D(t(obs_mat), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "Observations",zlim=c(-13,18))
points(0.05*design[,2],design[,1],cex=.5,lwd=1,pch=1)
dev.off()

pdf('sample_theta_Lorenz_96_with_discrepancy.pdf',height=5,width=3)
#xlim=c(min(m_no_discrepancy@post_sample[,1],m_gasp@post_sample[,1],m_sgasp@post_sample[,1]),
#       max(m_no_discrepancy@post_sample[,1],m_gasp@post_sample[,1],m_sgasp@post_sample[,1]))
xlim=c(7.7,8.3)
hist(m_no_discrepancy@post_sample[,1], breaks=60,
     col='red',xlim=xlim,ylim=c(0,1000),xlab=expression(theta),ylab='counts',main='posterior samples')
hist(m_gasp@post_sample[,1],col='cyan',breaks=60, add=T)
hist(m_sgasp@post_sample[,1],col='blue', breaks=60,add=T)
legend('topright',legend=c("No-discrepancy","GaSP","S-GaSP"),lwd=2,cex=.8,
       col=c('red','cyan','blue'))
dev.off() 

pdf('no_discrepancy_Lorenz_96_obs_with_discrepancy.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_no_discrepancy@math_model_mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, no-discrepancy",zlim=c(-5,5))
dev.off()
pdf('gasp_Lorenz_96_obs_with_discrepancy.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_gasp@mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, GaSP",zlim=c(-5,5))
dev.off()

pdf('sgasp_discrepancy_Lorenz_96_obs_with_discrepancy.pdf',height=5,width=3)
image2D(t(reality_mat-matrix(pred_m_sgasp@mean,n1,n2) ), xlab="Time", ylab="State",  x=0.05*seq(1,n2), y=seq(1,n1),
        main = "diff, S-GaSP",zlim=c(-5,5))
dev.off()



##Example 3: Box (1956)
library(deSolve)
Box_model <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dM1=-10^{parameters[1]-3}*M1
    dM2=10^{parameters[1]-3}*M1-10^{parameters[2]-3}*M2
    list(c(dM1, dM2))
  })
}

Box_model_solved<-function(input, theta){
  init <- c(M1 = 100 , M2 = 0)
  out <- ode(y = init, times = c(0,input), func = Box_model, parms = theta)
  return(out[-1,3])
}

##12 data from Box
n=6
output=t(matrix(c(19.2,14,14.4,24,42.3,30.8,42.1,40.5,40.7,46.4,27.1,22.3),2,n))
input=c(10,20,40,80,160,320)
theta_range=matrix(c(0.5,0.5,1.5,1.5),2,2)
###the testing input for emulator should contain the observed inputs
testing_input=as.matrix(seq(1,350,1)) 
#if observed inputs are not included, then add it
set_diff_obs=setdiff(input,testing_input) 
testing_input=sort(c(testing_input,set_diff_obs)) 

##GaSP prediction, directly call computer model
set.seed(1)
m_sgasp_time=system.time({
  m_sgasp=rcalibration(input,output,math_model=Box_model_solved,
                       sd_proposal=c(0.25,0.25,1,1),
                       theta_range=theta_range)
  m_sgasp_pred=predict(m_sgasp,testing_input,math_model=Box_model_solved,interval_est=c(0.025,0.975))
}
)
# m_gasp_mle=rcalibration(input,output,p_theta=2,math_model=Box_model_solved,
#                     theta_range=theta_range,discrepancy_type = 'GaSP',method='mle')
# m_gasp_mle
# m_gasp_mle_pred=predict(m_gasp_mle,testing_input,math_model=Box_model_solved,interval_est=c(0.025,0.975))

###emulator ###need to explore a bit more
library(lhs)
m_sgasp_emulator_time=system.time({
  ##constructing simulation data
  D=50
  p=2
  lhs_sample=maximinLHS(n=D,k=p)
  input_simul=matrix(NA,D,p)
  input_simul[,1]=lhs_sample[,1]+0.5 ## 0.5 to 1.5
  input_simul[,2]=lhs_sample[,2]+0.5 ## 0.5 to 1.5
  k_simul=length(testing_input)
  
  output_simul=matrix(NA,D,k_simul)
  for(i_D in 1:D){
    output_simul[i_D,]=Box_model_solved(c(testing_input), input_simul[i_D,])
  }
  ## create loc_index_emulator as a subset of the simulation output 
  loc_index_emulator=rep(NA,n)
  for(i in 1:n){
    loc_index_emulator[i]=which(testing_input==input[i])
  }
  
  ##emulator
  m_sgasp_with_emulator=rcalibration(input,output,simul_type=0,
                                     input_simul=input_simul, output_simul=output_simul,simul_nug=T,
                                     loc_index_emulator=loc_index_emulator,
                                     sd_proposal=c(0.25,0.25,1,1),
                                     theta_range=theta_range)
  m_sgasp_with_emulator_pred=predict(m_sgasp_with_emulator,testing_input)
})

m_sgasp_time[3]/m_sgasp_emulator_time[3]



##make plots
output_replicates=as.vector(t(output))
input_replicates=as.vector(t(matrix(input,n,2)))

pdf('Box_sgasp.pdf',height=4.8,width=4.8)
plot(testing_input,m_sgasp_pred@mean,col='blue',type='l',
     ylim=c(min(m_sgasp_pred@interval[,1]),max(m_sgasp_pred@interval[,2])),mgp=c(2.5,1,0),
     xlab='t',ylab='y',main='prediction')
polygon( c(testing_input,rev(testing_input)), c(m_sgasp_pred@interval[,1],
                                                rev(m_sgasp_pred@interval[,2])), col = "grey80", border = F)
lines(input_replicates,output_replicates,type='p',pch=20)
lines(testing_input,m_sgasp_pred@mean,col='blue',type='l')
lines(testing_input,m_sgasp_pred@math_model_mean,col='blue',type='l',lty=2)

lines(testing_input,m_sgasp_with_emulator_pred@mean,col='green',type='l')
lines(testing_input,m_sgasp_with_emulator_pred@math_model_mean,col='green',type='l',lty=2)

legend("bottomright", c('predictive mean','calibrated computer model',
                        'predictive mean with emulator', 'calibrated emulator'),
       lty=c(1,2,1,2),col=c('blue','blue','green','green'),cex=.8)
dev.off()


pdf('Box_post_samples.pdf',height=4.8,width=4.8)
plot(m_sgasp@post_sample[,1],m_sgasp@post_sample[,2],pch=20,col='blue',cex=.5,type='p',
     xlab=expression(theta[1]),ylab=expression(theta[2]),mgp=c(2.5,1,0),main='posterior samples',
     xlim=c(0.5,1.5),ylim=c(0.5,1.5))
lines(m_sgasp_with_emulator@post_sample[,1],m_sgasp_with_emulator@post_sample[,2],pch=20,col='green',cex=.5,type='p')

legend("topleft", c('without emulator','with emulator'),
       pch=c(20,20),col=c('blue','green','red'),cex=.8)
dev.off()


##Example 4:  calibration with multiple source of data
#here I include RobustGaSP to use kernel functions
library(RobustGaSP)
## multicalibration with  measurement bias 
set.seed(1)
k=5
beta_measurement=rep(10,k)
beta_model=30

n=100
input_measurement=list()
input_model=matrix(0,n,1)

R0_measurement=list()
R_measurement=list()
L_measurement=list()
delta_measurement=list()

input_real=seq(0,1,1/(n-1))
var_measurement_real=seq(0.5,1,(1-0.5)/(k-1))

var_model_real=.2^2
delta_measurement_vec=matrix(0,k,n)


input_model=input_real
R0_model=abs(outer(input_model,input_model,'-'))

R_model=matern_5_2_funct(R0_model,beta_model)
L_model=t(chol(var_model_real*R_model))

for(i in 1:k){
  input_measurement[[i]]=input_real
  R0_measurement[[i]]=abs(outer(input_measurement[[i]],input_measurement[[i]],'-'))
  R_measurement[[i]]=matern_5_2_funct(R0_measurement[[i]],beta_measurement[i])
  L_measurement[[i]]=t(chol(var_measurement_real[i]*R_measurement[[i]]))
  
}

math_model_sin<-function(x,theta){
  sin(theta[1]*x)
}

output=list()
output_no_measurement_bias=list()
error_sd=rep(0.05,k)
#error_sd=seq(0.1,0.5,0.4/(k-1))

theta_real=c(pi)

for(i in 1:k){
  delta_measurement[[i]]= L_measurement[[i]]%*%rnorm(n,0,1)
}
delta_model=L_model%*%rnorm(n,0,1)


##here the reality is defined to be the one that contain bias
##I will define truth later
reality=list()
truth=math_model_sin(input_real,theta_real)+delta_model

for(i in 1:k){
  reality[[i]]=truth+delta_measurement[[i]]
  noise=rnorm(n,0,sd=error_sd[i])
  output[[i]]=reality[[i]]+noise
}



math_model=list()
for(i in 1:length(input_measurement)){
  math_model[[i]]=math_model_sin
}


##here the true discrepancy (delta) is from GaSP so the GaSP is expected to perform better. Here S-GaSP is misspecified but it also performs well

##4.  with measurement bias
system.time(
  for(i in 1:1){
    model_sgasp=rcalibration_MS(design=input_measurement,observations=output,p_theta=1,math_model=math_model,
                                simul_type=rep(1, length(input_measurement)),
                                S=5000,S_0=2000,thinning=1,measurement_bias=T,shared_design=input_model,
                                have_measurement_bias_recorded=T,
                                discrepancy_type=c(rep('GaSP',length(input_measurement)),'S-GaSP'),
                                theta_range=matrix(c(-2*pi,2*pi),1,2),sd_proposal_theta=rep(0.02,2))
  }
)
# 

system.time(
  for(i in 1:1){
    model_gasp=rcalibration_MS(design=input_measurement,observations=output,p_theta=1,math_model=math_model,
                               simul_type=rep(1, length(input_measurement)),
                               S=5000,S_0=2000,thinning=1,measurement_bias=T,shared_design=input_model,
                               have_measurement_bias_recorded=T,
                               discrepancy_type=c(rep('GaSP',length(input_measurement)),'GaSP'),
                               theta_range=matrix(c(-2*pi,2*pi),1,2),sd_proposal_theta=rep(0.02,2))
  }
)


##predict reality
M=dim(model_gasp@post_individual_par[[1]])[1] ###number of samples saved 
pred_reality_gasp_invididual=pred_reality_sgasp_invididual=0

for(i_M in 1 :M ){
  pred_reality_gasp_invididual=pred_reality_gasp_invididual+math_model_sin(input_real,model_gasp@post_theta[i_M,])+model_gasp@post_delta[i_M,]
  pred_reality_sgasp_invididual=pred_reality_sgasp_invididual+math_model_sin(input_real,model_sgasp@post_theta[i_M,])+model_sgasp@post_delta[i_M,]
}
pred_reality_gasp_invididual=pred_reality_gasp_invididual/M
pred_reality_sgasp_invididual=pred_reality_sgasp_invididual/M



##create stack (averaged) data
output_stack=0

for(i in 1:k){
  output_stack=output_stack+ output[[i]]
  
}
output_stack=output_stack/k

#stack image

model_sgasp_stack=rcalibration(design=as.matrix(input_real),observations=as.matrix(output_stack),p_theta=1,math_model=math_model_sin,
                               simul_type=1,
                               S=5000,S_0=2000,thinning=1,
                               discrepancy_type=c('S-GaSP'),
                               theta_range=matrix(c(-2*pi,2*pi),1,2),sd_proposal=c(rep(0.02,2),rep(0.25,1),0.25))

Pred_sgasp_stack=predict(model_sgasp_stack,as.matrix(input_real),math_model=math_model_sin)


model_gasp_stack=rcalibration(design=as.matrix(input_real),observations=as.matrix(output_stack),p_theta=1,math_model=math_model_sin,
                              simul_type=1,
                              S=5000,S_0=2000,thinning=1,
                              discrepancy_type=c('GaSP'),
                              theta_range=matrix(c(-2*pi,2*pi),1,2),sd_proposal=c(rep(0.02,2),rep(0.25,1),0.25))

Pred_gasp_stack=predict(model_gasp_stack,as.matrix(input_real),math_model=math_model_sin)




###compute MSE
##all show the first two are better 
RMSE_record_truth=rep(NA,5)
RMSE_record_delta=rep(NA,4)
RMSE_record_measurement_bias=matrix(NA,k,4)

RMSE_record_truth[1]=sqrt(mean( (pred_reality_gasp_invididual-truth)^2))
RMSE_record_truth[2]=sqrt(mean( (pred_reality_sgasp_invididual-truth)^2))
RMSE_record_truth[3]=sqrt(mean( (Pred_gasp_stack@mean-truth)^2))
RMSE_record_truth[4]=sqrt(mean( (Pred_sgasp_stack@mean-truth)^2))
RMSE_record_truth[5]=sqrt(mean( (output_stack-truth)^2))

RMSE_record_delta[1]=sqrt(mean( (colMeans(model_gasp@post_delta)-delta_model)^2))
RMSE_record_delta[2]=sqrt(mean( (colMeans(model_sgasp@post_delta)-delta_model)^2))
RMSE_record_delta[3]=sqrt(mean( (Pred_gasp_stack@mean-Pred_gasp_stack@math_model_mean-delta_model)^2))
RMSE_record_delta[4]=sqrt(mean( (Pred_sgasp_stack@mean-Pred_sgasp_stack@math_model_mean-delta_model)^2))

for(i_k in 1:k){
  RMSE_record_measurement_bias[i_k,1]=sqrt(mean( (colMeans(model_gasp@post_measurement_bias[[i_k]])-delta_measurement[[i_k]])^2))
  RMSE_record_measurement_bias[i_k,2]=sqrt(mean( (colMeans(model_sgasp@post_measurement_bias[[i_k]])-delta_measurement[[i_k]])^2))
  RMSE_record_measurement_bias[i_k,3]=sqrt(mean( (output[[i_k]]-Pred_gasp_stack@mean-delta_measurement[[i_k]])^2))
  RMSE_record_measurement_bias[i_k,4]=sqrt(mean( (output[[i_k]]-Pred_sgasp_stack@mean-delta_measurement[[i_k]])^2))
}

colMeans(RMSE_record_measurement_bias)

i=1 ##plot the first one source

pdf("measurement_bias_1_k_5.pdf",height=4.9,width=6)
plot(seq(0,1,1/(n-1)),delta_measurement[[i]],xlab='x',ylab=expression(delta[1](x)),ylim=c(-2,2),type='l',
     pch=18,main='Measurement Bias',mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5,lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_gasp@post_measurement_bias[[i]]),xlab='x',ylim=c(-2,2),type='l',col='red',lty=2,lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_sgasp@post_measurement_bias[[i]]),xlab='x',ylim=c(-2,2),type='l',col='blue',lty=3,lwd=2)
lines(seq(0,1,1/(n-1)),output[[i]]-Pred_gasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='pink',lty=4,lwd=2)
lines(seq(0,1,1/(n-1)),output[[i]]-Pred_sgasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='cyan',lty=5,lwd=2)

leg <- c("Truth",  "GaSP","S-GaSP","GaSP Stack","S-GaSP Stack")
legend("bottomright", legend = leg, col = c("black", "red", "blue","pink","cyan"),
       lty = c(1,2,3,4,5),lwd=rep(2,5),cex=.8)
dev.off()


i=2
pdf("measurement_bias_2_k_5.pdf",height=4.9,width=6)
plot(seq(0,1,1/(n-1)),delta_measurement[[i]],xlab='x',ylab=expression(delta[1](x)),ylim=c(-2,2),type='l',
     pch=18,main='Measurement Bias',mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5,lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_gasp@post_measurement_bias[[i]]),xlab='x',ylim=c(-2,2),type='l',col='red',lty=2,lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_sgasp@post_measurement_bias[[i]]),xlab='x',ylim=c(-2,2),type='l',col='blue',lty=3,lwd=2)
lines(seq(0,1,1/(n-1)),output[[i]]-Pred_gasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='pink',lty=4,lwd=2)
lines(seq(0,1,1/(n-1)),output[[i]]-Pred_sgasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='cyan',lty=5,lwd=2)

leg <- c("Truth",  "GaSP","S-GaSP","GaSP Stack","S-GaSP Stack")
legend("bottomright", legend = leg, col = c("black", "red", "blue","pink","cyan"),
       lty = c(1,2,3,4,5),lwd=rep(2,5),cex=.8)
dev.off()



pdf("model_discrepancy_k_5.pdf",height=4.9,width=6)
plot(seq(0,1,1/(n-1)),delta_model,xlab='x',ylab=expression(delta(x)),ylim=c(-2,2),pch=18,main='Model Discrepancy', cex=.8,
     mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5,type='l',lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_gasp@post_delta),xlab='x',ylim=c(-2,2),col='red',type='l',lty=2,lwd=2)
lines(seq(0,1,1/(n-1)),colMeans(model_sgasp@post_delta),xlab='x',ylim=c(-2,2),col='blue',type='l',lty=3,lwd=2)
lines(seq(0,1,1/(n-1)),Pred_gasp_stack@mean-Pred_gasp_stack@math_model_mean,xlab='x',ylim=c(-2,2),type='l',col='pink',lty=4,lwd=2)
lines(seq(0,1,1/(n-1)),Pred_sgasp_stack@mean-Pred_sgasp_stack@math_model_mean,xlab='x',ylim=c(-2,2),type='l',col='cyan',lty=5,lwd=2)
leg <- c("Truth",  "GaSP","S-GaSP","GaSP Stack","S-GaSP Stack")
legend("bottomright", legend = leg, col = c("black", "red", "blue","pink","cyan"),
       lty = c(1,2,3,4,5),lwd=rep(2,5),cex=.8)
dev.off()
pdf("reality_k_5.pdf",height=4.9,width=6)
plot(seq(0,1,1/(n-1)),delta_model+math_model_sin(input_real,theta_real),
     xlab='x',ylab=expression({y^R}(x)),mgp=c(2.5,1,0),ylim=c(-2.3,2.3),pch=18,main='Reality', 
     mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5,type='l',lty=1,lwd=2)
lines(seq(0,1,1/(n-1)),output[[1]],xlab='x',ylim=c(-2,2),type='p',col='black',pch=17,cex=.5)
lines(seq(0,1,1/(n-1)),output[[2]],xlab='x',ylim=c(-2,2),type='p',col='black',pch=19,cex=.5)
lines(seq(0,1,1/(n-1)),pred_reality_gasp_invididual,xlab='x',ylim=c(-2,2),type='l',col='red',lty=2,lwd=2)
lines(seq(0,1,1/(n-1)),pred_reality_sgasp_invididual,xlab='x',ylim=c(-2,2),type='l',col='blue',lty=3,lwd=2)
lines(seq(0,1,1/(n-1)),Pred_gasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='pink',lty=4,lwd=2)
lines(seq(0,1,1/(n-1)),Pred_gasp_stack@mean,xlab='x',ylim=c(-2,2),type='l',col='cyan',lty=5,lwd=2)
#lines(seq(0,1,1/(n-1)),output_stack,xlab='x',ylim=c(-2,2),type='l',col='black',lty=6,lwd=2)

leg <- c("Truth","Obs Source 1","Obs Source 2",  "GaSP","S-GaSP","GaSP Stack","S-GaSP Stack")
legend("bottomright", legend = leg, col = c("black","black","black", "red", "blue","pink","cyan"),
       pch = c(NA,17,19,NA,NA,NA,NA), lty = c(1,NA,NA,2,3,4,5), cex=.8)
dev.off()


###they are similar
pi
mean(model_gasp@post_theta)
mean(model_sgasp@post_theta)
mean(model_gasp_stack@post_sample[,1])
mean(model_sgasp_stack@post_sample[,1])
xlim=c(0,6)
hist(model_gasp@post_theta, breaks=30,
     col='red',xlim=xlim,ylim=c(0,600),xlab=expression(theta),ylab='counts',main='posterior samples')
hist(model_sgasp@post_theta,col='blue',breaks=30, add=T)
hist(model_gasp_stack@post_sample[,1],col='pink', breaks=30,add=T)
hist(model_sgasp_stack@post_sample[,1],col='cyan', breaks=30,add=T)

legend('topright',legend=c("GaSP","S-GaSP","GaSP stack","S-GaSP stack"),lwd=2,cex=.8,
       col=c('red','blue','pink','cyan'))





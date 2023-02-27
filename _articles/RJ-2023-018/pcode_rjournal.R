#load dependencies
library(deSolve)
library(fda)
library(MASS)
library(pracma)
library(ggplot2)
library(gridExtra)
set.seed(123)

#install.packages('pCODE')
#devtools::install_github('alex-haixuw/PCODE')
# The newest version is 0.9.4
#packageVersion('pCODE') == '0.9.4'

#install.packages('CollocInfer')
library(pCODE)
library(CollocInfer)


#---------------------------------------------------------------------------
#Example 4.1: A simple ODE model
#define model parameters
model.par   <- c(theta = c(0.1))
#define state initial value
state       <- c(X     = 0.1)


#Define model for function 'ode' to numerically solve the system
ode.model <- function(t, state,parameters){
  with(as.list(c(state,parameters)),
       {
         dX <- theta*X*(1-X/10)
         return(list(dX))
       })
}


#Observation time points
times <- seq(0,100,length.out=101)
#Solve the ode model
desolve.mod <- ode(y=state,times=times,func=ode.model,parms = model.par)

#Plot solution
plot(desolve.mod,main=names(state),ylab='') #curve
points(times,desolve.mod[,2],col='red') #Points

#Prepare for doing parameter cascading method

#Generate basis object for interpolation and as argument of pcode
#21 konts equally spaced within [0,100]
knots <- seq(0,100,length.out=21)
#order of basis functions
norder <- 4
#number of basis funtions
nbasis <- length(knots) + norder - 2
#creating Bspline basis
basis  <- create.bspline.basis(c(0,100),nbasis,norder,breaks = knots)


#Add random noise to ode solution for simulating data
nobs  <- length(times)
scale <- 0.5
noise <- scale*rnorm(n = nobs, mean = 0 , sd = 1)
observation <- desolve.mod[,2] + noise

#plot simulated data against generating model
plot(desolve.mod,main=names(state),ylab='') #curve
points(times, observation,pch='*',col='blue')    #observation

#parameter estimation
pcode.result <- pcode(data = observation, time = times, ode.model = ode.model,
                      par.initial = 0.3, par.names = 'theta',state.names = 'X',
                      basis.list = basis, lambda = 1e2,controls = list(verbal = 0))  #Added verbal options.
pcode.result$structural.par
# > pcode.result$structural.par
# theta
# 0.09995229
#Bootstrap variance estimate of structural parameters
bootsvar.res <- bootsvar(data = observation, time = times, ode.model = ode.model,
	                     par.initial = 0.3, par.names = 'theta',state.names = 'X',
                         basis.list = basis, lambda = 1e2,bootsrep = 20,controls = list(verbal = 0))
bootsvar.res
#
#> bootsvar.res
# theta
# 1.042763e-05
#
#Delta variance estimate of structural parameters
numeric.var <- deltavar(data = observation, time = times, ode.model = ode.model,
                        par.initial = 0.3, par.names = 'theta',state.names = 'X',
                        basis.list = basis, lambda = 1e2,stepsize = 1e-5,y_stepsize = 1e-5,controls = list(verbal = 0))
numeric.var
# > numeric.var
# [1] 9.923318e-06



#100 repeats
#Change repeats = True to run replications
repeats = FALSE
if (repeats){

  #21 konts equally spaced within [0,100]
  knots <- seq(0,100,length.out=21)
  #order of basis functions
  norder <- 5
  #number of basis funtions
  nbasis <- length(knots) + norder - 2
  #creating Bspline basis
  basis  <- create.bspline.basis(c(0,100),nbasis,norder,breaks = knots)

  sim.data     <- matrix(NA, nrow = 100, ncol = 101)
  comp.time    <- rep(NA, nrow = 100)
  par.result   <- rep(NA, 100)
  nui.result   <- matrix(NA, ncol = 24, nrow = 100)
  bootsvar.res <- rep(NA, 100)
  numeric.var   <- rep(NA,100)
  #Estimate structural parameters over simulated data sets
  for (jj in 1:100){
    print(jj)
    #Simulate data
    nobs  <- length(times)
    scale <- 0.5
    noise <- scale*rnorm(n = nobs, mean = 0 , sd = 1)

    observ <- desolve.mod[,2] + noise
    sim.data[jj,] <- observ


    #start_time <- Sys.time()
    pcode.result <- pcode(data = observ, time = times, ode.model = ode.model, par.initial = 0.3, par.names = 'theta',state.names = 'X',
                          basis.list = basis, lambda = 1e2)
    #end_time   <- Sys.time()

    #comp.time[jj] <- round(end_time - start_time,digits = 3)
    #print(paste('Number ',jj,' simulation ends after ', comp.time[jj],' seconds',sep=''))
    par.result[jj]  <- pcode.result$structural.par
    nui.result[jj,] <- pcode.result$nuisance.par

  }


  true.solution <- desolve.mod[,2]
  true.df    <- data.frame(cbind(times = times,solution = true.solution))
  one.observ <- data.frame(cbind(times = times, observ = observ))
  ggplot() + geom_line(aes(x = times, y = solution), data = true.df) +
    geom_point(aes(x = times, y = observ), col='blue',data = one.observ) +
    theme_classic() +labs(y='X(t)',x='Time') +
    theme(axis.text = element_text(size=12,face='bold')) +
    theme(axis.title = element_text(size=15,face='bold'))


  #check estimation of structural parameters in boxplot
  boxplot(par.result)
  abline(h = 0.1,col='red',lwd=2)
  df = data.frame(par.result)
  names(df) = 'theta'
  ggplot(df, aes(x=0, y=theta)) +  theme_classic() +
    labs(x='',y='') + theme(axis.text.y = element_text(size=15,face='bold')) +
    stat_boxplot(geom = "errorbar", width = 0.3)  + geom_boxplot() +
    geom_hline(aes(yintercept = 0.1,colour = 'red'),size=2) + theme(legend.position = "none") +
    theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())


  #solve ODE by structural parameters and initial value
  ini.val <- rep(NA, 100)
  plot(times, desolve.mod[,2],ylab='X(t)',main=' ',type='l',lwd=2, col='red')
  est.curve <- matrix(NA, nrow =length(times), ncol = 100)
  #There might be one case where ode solver does not work,
  #please skip such jj, and modify jj in 1:100 to jj in suchjj:100 for continuing

  for (jj in 1:100){
    ini.val[jj] <-  (eval.basis(times, basis) %*% nui.result[jj,])[1]
    est.state   <- c(X = ini.val[jj])
    est.par     <- c(theta = par.result[jj])
    tryCatch({
      desolve.mod <- ode(y=est.state,times=times,func=ode.model,parms = est.par)
      est.curve[,jj]   <- desolve.mod[,2]
    }, error=function(e){cat("ERROR at iteration ",jj, "\n")})
  }



  #See the estimated solutions against the true solution
  solve.plot <- ggplot()
  for (ii in 1:100){
    df <- data.frame(cbind(times = times, est.curve = est.curve[,ii]))
    solve.plot <- solve.plot + geom_line(aes(x = times, y = est.curve),col='grey',alpha=0.9,data=df)
  }
  df <- data.frame(cbind(times = times,solution = true.solution))
  with.true.plot <- solve.plot + geom_line(aes(x = times, y= solution),col='black',size=1,data=df)

  final.plot <- with.true.plot  +theme_classic() +labs(y='X(t)',x='Time') +
    theme(axis.text = element_text(size=12,face='bold')) +
    theme(axis.title = element_text(size=15,face='bold'))
  final.plot

}




#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#Example 4.2: Fitz-Hugh Nagumo model
#---------------------------------------------------------------------------
#Define model parameters
model.par   <- c(a=0.2,b=0.2,c=3)
#Define initial value of state variables
state       <- c(V=-1,R=1)
#Define ode model for obtaining numeric solution
ode.model <- function(t, state,parameters){
  with(as.list(c(state,parameters)),
       {
         dV <- c*(V - (V^3)/3 + R)
         dR <- -(1/c) * (V - a + b*R)
         return(list(c(dV,dR)))
       })}


#Define observation points
times <- seq(0,20,length.out=401)
#Obtain ode solution
desolve.mod <- ode(y=state,times=times,func=ode.model,parms = model.par)
#Plot of the ode solution
par(mfrow=c(2,1))
par(mar = c(4,4,2,2) + 0.1)
plot(desolve.mod[,1],desolve.mod[,2],type='l',main='FitzHugh-Nagumo model',ylab='V(t)',xlab='',lwd=2)
plot(desolve.mod[,1],desolve.mod[,3],type='l',main='',ylab='R(t)',xlab='t',lwd=2)


plot_list = list()
for (ii in 1:2){
  plotdf = data.frame(x = times, y = desolve.mod[,ii+1])
  plot_list[[ii]] = ggplot(data = plotdf, aes(x = x, y = y))+geom_line()+ylab(colnames(desolve.mod)[ii+1])+xlab('t')
}
do.call("grid.arrange", c(plot_list, ncol=floor(sqrt(2))))


#Generate measurement noises
nobs  <- length(times)
scale <- 0.1
noise_v <- scale*rnorm(n = nobs, mean = 0 , sd = 1)
noise_r <- scale*rnorm(n = nobs, mean = 0 , sd = 1)
#Store observations
observ <- matrix(NA, nrow = length(times),ncol =3)
observ[,1] <- times
observ[,2] <- desolve.mod[,2] + noise_v
observ[,3] <- desolve.mod[,3] + noise_r
#Plot observations
plot(desolve.mod[,1],desolve.mod[,2],type='l',main='V(t)',ylab='V(t)',xlab='t')
points(desolve.mod[,1],observ[,2],col='blue')
plot(desolve.mod[,1],desolve.mod[,3],type='l',main='R(t)',ylab='R(t)',xlab='t')
points(desolve.mod[,1],observ[,3],col='blue')

#Define basis for interpolating observation of both state variables
#can use same basis for both dimensions
knots <- seq(0,20,length.out=101)
#order of basis functions
norder <- 4
#number of basis funtions
nbasis <- length(knots) + norder - 2
#creating basis
basis  <- create.bspline.basis(c(0,20),nbasis,norder,breaks = knots)
#Make a list of basis object for interpolation
basis.list <- list(basis, basis)

data <- observ[,2:3]
colnames(data) <- c('V','R')

#parameter estimation
pcode.result <- pcode(data = observ[,2:3], time= observ[,1], ode.model = ode.model,
                      par.names = c('a','b','c'),state.names = c('V','R'), par.initial = c(0.1,0.3,4),
                      lambda = c(1e2,1e2),basis.list = basis.list,
                      controls = list(smooth.lambda = 10,verbal = 1,maxeval = 50))
pcode.result$structural.par

# > pcode.result$structural.par
# [1] 0.1864545 0.2126701 2.9515862





# Running CollocInfer
FhNvarnames <- c("V", "R")
FhNparnames <- c("a", "b", "c")
x0 <- c(-1, 1)
names(x0) <- FhNvarnames
FhNpars <- c(0.2, 0.2, 3)
names(FhNpars) <- FhNparnames
fhn.ode <- function(times, x, p) {
  dx <- x
  dimnames(dx) <- dimnames(x)
  dx["V"] <- p["c"] * (x["V"] - x["V"]^3/3 + x["R"])
  dx["R"] <- -(x["V"] - p["a"] + p["b"] * x["R"])/p["c"]

  return(list(dx))
}
FhNplottimes <- seq(0, 20, 0.05)
out <- lsoda(x0, times = FhNplottimes, fhn.ode, FhNpars)


FhNn <- length(times)
out <- lsoda(x0, times = times, fhn.ode, FhNpars)
FhNdata <- out[, 2:3] + 0.1 * matrix(rnorm(2 * FhNn), FhNn, 2)


FhNrange <- c(0, 20)
breaks <- seq(0, 20, 0.5)
norder <- 4
FhNbasis <- create.bspline.basis(range = FhNrange, norder = norder, breaks = breaks)
FhNfdPar <- fdPar(FhNbasis, int2Lfd(2), 1)
DEfd0 <- smooth.basis(times, FhNdata, FhNfdPar)$fd
coefs0 <- DEfd0$coef
colnames(coefs0) <- FhNvarnames
fhn.fun <- function(times, x, p, more) {
  dx <- x
  dx[, "V"] <- p["c"] * (x[, "V"] - x[, "V"]^3/3 + x[, "R"])
  dx[, "R"] <- -(x[, "V"] - p["a"] + p["b"] * x[, "R"])/p["c"]
  return(dx)
}
lambda <- 1000

profile.obj <- LS.setup(pars = FhNpars, fn = fhn.fun, lambda = lambda, times = times,
                        coefs = coefs0, basisvals = FhNbasis)

proc <- profile.obj$proc
lik <- profile.obj$lik
Pres0 <- ParsMatchOpt(FhNpars, coefs0, proc)
#Initial parameter estimates of CollocInfer
pars1 <- Pres0$pars


Ires1 <- inneropt(FhNdata, times = times, pars1, coefs0, lik, proc)
coefs1 <- Ires1$coefs
Ores2 <- outeropt(FhNdata, times, pars1, coefs1, lik, proc)

#Parameter estimates of CollocInfer
Ores2$pars




#100 repeats
#Change repeats = True to run replications
repeats = FALSE
pcode_result  = matrix(NA, nrow= 100, ncol = 3)
colloc_result = matrix(NA, nrow= 100, ncol = 3)
if (repeats){
  for (jj in 1:100){
    print(jj)
    nobs  <- length(times)
    scale <- 0.1
    noise_v <- scale*rnorm(n = nobs, mean = 0 , sd = 1)
    noise_r <- scale*rnorm(n = nobs, mean = 0 , sd = 1)
    #Store observations
    observ <- matrix(NA, nrow = length(times),ncol =3)
    observ[,1] <- times
    observ[,2] <- desolve.mod[,2] + noise_v
    observ[,3] <- desolve.mod[,3] + noise_r

    data <- observ[,2:3]
    colnames(data) <- c('V','R')
    pcode.result <- pcode(data = observ[,2:3], time= observ[,1], ode.model = ode.model,
                          par.names = c('a','b','c'),state.names = c('V','R'), par.initial = c(0.1,0.3,4),
                          lambda = c(1e2,1e2),basis.list = basis.list,
                          controls = list(smooth.lambda = 10,verbal = 0,maxeval = 150))
    pcode_result[jj,] = pcode.result$structural.par

    profile.obj <- LS.setup(pars = FhNpars, fn = fhn.fun, lambda = lambda, times = times,
                            coefs = coefs0, basisvals = FhNbasis)

    proc <- profile.obj$proc
    lik <- profile.obj$lik
    Pres0 <- ParsMatchOpt(FhNpars, coefs0, proc)
    #Initial parameter estimates of CollocInfer
    pars1 <- Pres0$pars


    Ires1 <- inneropt(data, times = times, pars1, coefs0, lik, proc)
    coefs1 <- Ires1$coefs
    Ores2 <- outeropt(data, times, pars1, coefs1, lik, proc)

    #Parameter estimates of CollocInfer
    colloc_result[jj,] = Ores2$pars

  }
}

# Comparisons

#Create plot dataframes
a.df     = data.frame('a'=c(pcode_result[,1],colloc_result[,1]),'pkg'=c(rep('pCODE',100),rep('CollocInfer',100)))
a.df$a   = as.numeric(a.df$a)
a.df$pkg = as.factor(a.df$pkg)

b.df     = data.frame('b'=c(pcode_result[,2],colloc_result[,2]),'pkg'=c(rep('pCODE',100),rep('CollocInfer',100)))
b.df$b   = as.numeric(b.df$b)
b.df$pkg = as.factor(b.df$pkg)


c.df     = data.frame('c'=c(pcode_result[,3],colloc_result[,3]),'pkg'=c(rep('pCODE',100),rep('CollocInfer',100)))
c.df$c   = as.numeric(c.df$c)
c.df$pkg = as.factor(c.df$pkg)

#Plotting dependencies
library(cowplot)

aplot = ggplot(a.df, aes(x = pkg,y=a)) + geom_violin(aes(fill=pkg),size=1.1)+theme_classic() +labs(y='a',x='Package') +
  theme(axis.text = element_text(size=12,face='bold')) +
  theme(axis.title = element_text(size=18,face='bold',angle=0)) +
  theme(axis.title.x = element_blank()) + geom_boxplot(width=0.2) +
  geom_hline(aes(yintercept = model.par[1]),alpha=0.5,size=2) +
  theme(legend.position = 'none')


bplot = ggplot(b.df, aes(x = pkg,y=b)) + geom_violin(aes(fill=pkg),size=1.1)+theme_classic() +labs(y='b',x='Package') +
  theme(axis.text = element_text(size=12,face='bold')) +
  theme(axis.title = element_text(size=18,face='bold')) +
  theme(axis.title.x = element_blank()) +geom_boxplot(width=0.2) +
  geom_hline(aes(yintercept = model.par[2]),alpha=0.5,size=2) +
  theme(legend.position = 'none')


cplot = ggplot(c.df, aes(x = pkg,y=c)) + geom_violin(aes(fill=pkg),size=1.1)+theme_classic() +labs(y='c',x='Package') +
  theme(axis.text = element_text(size=12,face='bold')) +
  theme(axis.title = element_text(size=18,face='bold')) +
  theme(axis.title.x = element_blank()) +geom_boxplot(width=0.2) +
  geom_hline(aes(yintercept = model.par[3]),alpha=0.5,size=2) +
  theme(legend.position = 'none')

plot_grid(aplot,bplot,cplot,nrow=1,ncol=3)


#---------------------------------------------------------------------------
#Example 4.3: Non-normal error model
#---------------------------------------------------------------------------
#clear workspace

#load dependencies
library(deSolve)
library(fda)
library(MASS)
library(pracma)
set.seed(123)
#install.packages('pCODE')
library(pCODE)


#Define model parameters
model.par   <- c(theta = c(0.1))
#Define initial value of state variable
state       <- c(X     = 0.1)
#Define ODE model

ode.model <- function(t, state,parameters){
  with(as.list(c(state,parameters)),
       {
         dX <- theta*X*(1-X/10)
         return(list(dX))
       })
}
#Observation points
times <- seq(0,100,length.out=101)
#Obain numeric solution of the ode model
desolve.mod <- ode(y=state,times=times,func=ode.model,parms = model.par)
#Generate noisay observations
nobs  <- length(times)
noise <- rnorm(n = nobs, mean = 0 , sd = 0.1)
observ <- exp(log(desolve.mod[,2]) + noise)
#plot simulated data against generating model
plot(desolve.mod,ylab='X',main='',ylim=c(0,15)) #curve
points(times,desolve.mod[,2],col='red') #Points
points(times, observ,pch='*',col='blue')

#parameter estimation
#Define basis for doing interpolation
#knots location
knots <- seq(0,100,length.out=21)
#order of basis functions
norder <- 4
#number of basis funtions
nbasis <- length(knots) + norder - 2
#creating basis
basis  <- create.bspline.basis(c(0,100),nbasis,norder,breaks = knots)


#Define the likelihood function for evaluting the fitness of the model
likfun <- function(y,x){
  residuals <- log(y)-log(x)
  res <- lapply(residuals,function(t){ dnorm(t, mean= 0, sd= 0.1,log=TRUE)})
  return(sum(unlist(res)))
}

lkh.result <- pcode(data = observ,time = times, likelihood.fun = likfun, par.initial = 0.3,
                   ode.model = ode.model, par.names = 'theta',state.names ='X',basis.list = basis, lambda = 1e2)
lkh.result$structural.par
# 0.105



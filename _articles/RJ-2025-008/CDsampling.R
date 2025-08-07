#this R codes run the examples codes in CDsampling manuscript by Huang, Tong and Yang
library(CDsampling)
library(Rglpk)
library(lpSolve)


# ------------------------------------------Section 3.1 GLM trial_data example--------------------------------------------

## -----------------Section3.1 Figure 4 of trial data------------------------
library(tidyverse)
trial_data_agecombined = trial_data %>% mutate(age = ifelse(age_1==1,"26-64",ifelse(age_2==1,">=65","18-25")), gender_c = ifelse(gender==0,"F","M"))
trial_data_agecombined$age = factor(trial_data_agecombined$age, levels=c("18-25", "26-64", ">=65"))
#table(trial_data_agecombined$gender_c, trial_data_agecombined$age)
trial_data_agecombined %>% ggplot(aes(x=factor(Y)))+geom_bar(stat='count', fill='grey')+theme_bw()+xlab("Response")+facet_grid(age~gender_c, space="free_x")

## ---------------------Section3.1 Sampling Codes----------------------------
beta = c(0, 3, 3, 3)
X=matrix(data=c(1,0,0,0,1,0,1,0,1,0,0,1,1,1,0,0,1,1,1,0,1,1,0,1), ncol=4, byrow=TRUE)
W=CDsampling::W_func_GLM(X=X, b=beta, link="logit")
rc = c(50, 40, 10, 200, 150, 50)/200 #available volunteers/sample size
m = 6
g.con = matrix(0,nrow=(2*m+1), ncol=m)
g.con[1,] = rep(1, m)
g.con[2:(m+1),] = diag(m)
g.con[(m+2):(2*m+1), ] = diag(m)
g.dir = c("==", rep("<=", m), rep(">=", m))
g.rhs = c(1, rc, rep(0, m))

lower.bound=function(i, w){
  rc = c(50, 40, 10, 200, 150, 50)/200
  m=length(w) #num of categories
  temp = rep(0,m)
  temp[w>0]=1-pmin(1,rc[w>0])*(1-w[i])/w[w>0];
  temp[i]=0;
  max(0,temp);
  }

upper.bound=function(i, w){
  rc = c(50, 40, 10, 200, 150, 50)/200
  min(1,rc[i]);
  }

label = c("F, 18-25", "F, 26-64", "F, >=65", "M, 18-25", "M, 26-64", "M, >=65")

###---------------Section 3.1 find constrained local D-optimal sampling----------------------
set.seed(092)
approximate_design = liftone_constrained_GLM(X=X, W=W, g.con=g.con, g.dir=g.dir, g.rhs=g.rhs, lower.bound=lower.bound, upper.bound=upper.bound, reltol=1e-10, maxit=100, random=TRUE,nram=4, w00=NULL, epsilon=1e-8, label=label)
print(approximate_design)
# Optimal Sampling Results:
# ================================================================================
# Optimal approximate allocation:
#   F, 18-25 F, 26-64 F, >=65 M, 18-25 M, 26-64 M, >=65
# w  0.25     0.2      0.05    0.5      0.0      0.0
# w0 0.25     0.0      0.05    0.7      0.0      0.0
# --------------------------------------------------------------------------------
#   maximum :
#   2.8813e-08
# --------------------------------------------------------------------------------
#   convergence :
#   TRUE
# --------------------------------------------------------------------------------
#   itmax :
#   9.0
# --------------------------------------------------------------------------------
#   deriv.ans :
#   0.0, 3.6017e-08, 4.8528e-07, -1.1525e-07, -1.0310e-07, -7.9507e-08
# --------------------------------------------------------------------------------
#   gmax :
#   0.0
# --------------------------------------------------------------------------------
#   reason :
#   "gmax <= 0"
# --------------------------------------------------------------------------------

exact_design = CDsampling::approxtoexact_constrained_func(n=200, w=approximate_design$w, m=6, beta=beta, link='logit', X=X, Fdet_func=Fdet_func_GLM,
                                              iset_func=iset_func_trial, label=label)
print(exact_design)
# Optimal Sampling Results:
# ================================================================================
#   allocation :
#   50.0, 40.0, 10.0, 100.0, 0.0, 0.0
# --------------------------------------------------------------------------------
#   allocation.real :
#   0.25, 0.2, 0.05, 0.5, 0.0, 0.0
# --------------------------------------------------------------------------------
#   det.maximum :
#   46.1012
# --------------------------------------------------------------------------------


###-----------------------Section3.1 constrained EW D-optimal sampling---------------------------------------
#e.g. use uniform distribution as prior
library(cubature)
unif.prior <- rbind(c(-2, -1, -1, -1), c(2,  5,  5, 5))
#expectation
W.EW.unif = matrix(rep(0,6))
for (i in 1:6){
  x = X[i,]
  W.EW.unif[i] = hcubature(function(beta) dunif(beta[1], min=unif.prior[1,1], max=unif.prior[2,1])*dunif(beta[2], min=unif.prior[1,2], max=unif.prior[2,2])*dunif(beta[3], min=unif.prior[1,3], max=unif.prior[2,3])*dunif(beta[4], min=unif.prior[1,4], max=unif.prior[2,4])*(exp(x[1]*beta[1]+x[2]*beta[2]+x[3]*beta[3]+x[4]*beta[4])/(1+exp(x[1]*beta[1]+x[2]*beta[2]+x[3]*beta[3]+x[4]*beta[4]))^2), lowerLimit = unif.prior[1,], upperLimit  = unif.prior[2,])$integral
  }

set.seed(123)
approximate_design_EW = liftone_constrained_GLM(X=X, W=W.EW.unif,  g.con=g.con, g.dir=g.dir, g.rhs=g.rhs, lower.bound=lower.bound, upper.bound=upper.bound, reltol=1e-12, maxit=100, random=TRUE, nram=4, w00=NULL, epsilon=1e-10, label=label)
exact_design_EW = approxtoexact_constrained_func(n=200, w=approximate_design_EW$w, m=6, beta=beta, link='logit', X=X, Fdet_func=Fdet_func_GLM, iset_func=iset_func_trial, label=label)
print(exact_design_EW)
# Optimal Sampling Results:
# ================================================================================
# Optimal exact allocation:
#                F, 18-25 F, 26-64 F, >=65 M, 18-25 M, 26-64 M, >=65
# allocation      48.0     40.0     10.0    43.0     19.0     40.0
# allocation.real 0.2406   0.2      0.05    0.2102   0.0991   0.2001
# --------------------------------------------------------------------------------
#   det.maximum :
#   25.59
# --------------------------------------------------------------------------------



###----------------------------Section3.1 Constrained uniform design----------------------------------
#use bounded_uniform()
bounded_uniform(Ni=c(50, 40, 10, 200, 150, 50), nsample=200, label=label)
# Optimal Sampling Results:
# ================================================================================
# Optimal exact allocation:
#           F, 18-25 F, 26-64 F, >=65 M, 18-25 M, 26-64 M, >=65
# Allocation 38.0     38.0     10.0    38.0     38.0     38.0
# --------------------------------------------------------------------------------

#use approxtoexact_constrained_func()
w00 = rep(1/200, 6)
unif_design = CDsampling::approxtoexact_constrained_func(n=200, w=w00, m=6, beta=NULL, link=NULL, X=NULL, Fdet_func=Fdet_func_unif, iset_func=iset_func_trial, label=label)
print(unif_design)
# Optimal Sampling Results:
# ================================================================================
# Optimal exact allocation:
#               F, 18-25 F, 26-64 F, >=65 M, 18-25 M, 26-64 M, >=65
# allocation      38.0     38.0     10.0    38.0     38.0     38.0
# allocation.real 0.005    0.005    0.005   0.005    0.005    0.005
# --------------------------------------------------------------------------------
#   det.maximum :
#   792351680.0
# --------------------------------------------------------------------------------

#Figure 5 relevant codes provided below in Supplementary Section S3


#-------------------------------------Section 3.2 MLM trauma_data example----------------------------------------
##-------------------------Section 3.2 Figure 6 trauma data --------------------------------
trauma_data$Outcome = factor(trauma_data$Outcome, levels=c("Death", "Vegetative State", "Major Disability", "Minor Disability", "Good Recovery"))
trauma_data$Dose = factor(trauma_data$Dose, levels=c("Placebo", "Low", "Medium", "High"))
trauma_data %>% ggplot(aes(x=Outcome))+geom_bar(stat='count', fill='grey')+theme_bw()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+facet_grid(Severity~Dose, space="free_x")

##-------------------------Section 3.2 Sampling Codes---------------------------------------
J=5 #response levels
p=12 #num of parameters
m=8 #sampling groups
beta = c(-4.047, -0.131, 4.214, -2.225, -0.376, 3.519, -0.302, -0.237,  2.420, 1.386,  -0.120,  1.284)
Xi=rep(0,J*p*m)
dim(Xi)=c(J,p,m)

Xi[,,1] = rbind(c( 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,2] = rbind(c( 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,3] = rbind(c( 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,4] = rbind(c( 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 4, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,5] = rbind(c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,6] = rbind(c( 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 2, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,7] = rbind(c( 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,8] = rbind(c( 1, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 4, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

nsample=600
constraint = c(392, 410)  #mild:severe
lower.bound <- function(i, w0){
  n = 600
  constraint = c(392,410)
  if(i <= 4){
    a.lower <- (sum(w0[5:8])-(constraint[2]/n)*(1-w0[i]))/(sum(w0[5:8]))
    }
  else{
    a.lower <- (sum(w0[1:4])-(constraint[1]/n)*(1-w0[i]))/(sum(w0[1:4]))}
  a.lower
  }

upper.bound <- function(i, w0){
  n = 600
  constraint = c(392,410)
  if(i <= 4){
    b.upper <- ((constraint[1]/n)*(1-w0[i]) - (sum(w0[1:4])-w0[i]))/(1-sum(w0[1:4]))
    }
  else{
    b.upper <- ((constraint[2]/n)*(1-w0[i]) - (sum(w0[5:8])-w0[i]))/(1-sum(w0[5:8]))
    }
  b.upper
  }

g.con = matrix(0,nrow=length(constraint)+1+m, ncol=m)
g.con[2:3,] = matrix(data=c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), ncol = m, byrow=TRUE)
g.con[1,] = rep(1, m)
g.con[4:(length(constraint)+1+m), ] = diag(1, nrow=m)
g.dir = c("==", "<=","<=", rep(">=",m))
g.rhs = c(1, ifelse((constraint/nsample<1),constraint/nsample,1), rep(0, m))

label=label = c("Placebo-Mild", "Low-Mild", "Medium-Mild", "High-Mild", "Placebo-Severe", "Low-Severe", "Medium-Severe", "High-Severe")

###-------------------------Section 3.2 constrained D-optimal sampling---------------------------------------
set.seed(123)
approx_design = liftone_constrained_MLM(m=m, p=p, Xi=Xi, J=J, beta=beta,
                                                    lower.bound=lower.bound, upper.bound=upper.bound,
                                                    g.con=g.con, g.dir=g.dir, g.rhs=g.rhs, w00=NULL,
                                                    link='cumulative', Fi.func = Fi_func_MLM,
                                                    reltol=1e-5, maxit=500, delta = 1e-6,
                                                    epsilon=1e-8, random=TRUE, nram=3, label=label)
#approximate to exact
exact_design = approxtoexact_constrained_func(n=600, w=approx_design$w, m=8, beta=beta, link='cumulative', X=Xi, Fdet_func=Fdet_func_MLM,
                                              iset_func=iset_func_trauma, label=label)
print(exact_design)
# Optimal Sampling Results:
# ================================================================================
# Optimal exact allocation:
#               Placebo-Mild Low-Mild Medium-Mild High-Mild Placebo-Severe
# allocation      155.0        0.0      0.0         100.0     168.0
# allocation.real 0.2593       0.0      0.0         0.1667    0.2796
#               Low-Severe Medium-Severe High-Severe
# allocation      0.0        0.0           177.0
# allocation.real 0.0        0.0           0.2944
# --------------------------------------------------------------------------------
#   det.maximum :
#   1.63163827059162e+23
# --------------------------------------------------------------------------------



###-------------------------------Section 3.2 constrained uniform design-------------------------------
unif_design = approxtoexact_constrained_func(n=600, w=rep(1/600,8), m=8, beta=NULL, link=NULL, X=NULL, Fdet_func=Fdet_func_unif,
                                             iset_func=iset_func_trauma, label=label)
print(unif_design)
# Optimal Sampling Results:
# ================================================================================
# Optimal exact allocation:
#               Placebo-Mild Low-Mild Medium-Mild High-Mild Placebo-Severe Low-Severe
# allocation      75.0         75.0     75.0        75.0      75.0           75.0
# allocation.real 0.0017       0.0017   0.0017      0.0017    0.0017         0.0017
#               Medium-Severe High-Severe
# allocation      75.0          75.0
# allocation.real 0.0017        0.0017
# --------------------------------------------------------------------------------
# det.maximum :
# 1001129150390625
# --------------------------------------------------------------------------------


#----------------------------------Supplementary S1 Example S1.1 GLM Fisher information matrix-----------------------------------------
beta=c(0.5, 0.5, 0.5)
X = matrix(data=c(1,-1,-1,1,-1,1,1,1,-1), byrow=TRUE, nrow=3)
w = c(1/3,1/3,1/3)
CDsampling::F_func_GLM(w=w, beta=beta, X=X, link='logit')
# Dimensions: 3 x 3
# Matrix:
# ------------------------------
#   [,1]        [,2]        [,3]
# [1,]  0.23500371 -0.07833457 -0.07833457
# [2,] -0.07833457  0.23500371 -0.07833457
# [3,] -0.07833457 -0.07833457  0.23500371
# ------------------------------

#---------------------------------Supplementary S1 Example S1.2 MLM Fisher information matrix---------------------------------------------
J = 5
p=12
m=8
beta = c(-4.047, -0.131, 4.214, -2.225, -0.376, 3.519, -0.302, -0.237,  2.420, 1.386,  -0.120,  1.284)
Xi=rep(0,J*p*m)
dim(Xi)=c(J,p,m)

Xi[,,1] = rbind(c( 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,2] = rbind(c( 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,3] = rbind(c( 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 3, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,4] = rbind(c( 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 4, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,5] = rbind(c( 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,6] = rbind(c( 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 2, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,7] = rbind(c( 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

Xi[,,8] = rbind(c( 1, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 1, 4, 1, 0, 0, 0, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 0),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 1),
                c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

alloc = rep(1/8,m)
F_func_MLM(w=alloc, beta=beta, X=Xi, link='cumulative')
# Dimensions: 12 x 12
# Matrix:
# ------------------------------------------------------------------------------------------------------------------------
#       [,1]        [,2]        [,3]        [,4]        [,5]        [,6]
# [1,]  0.44505694  1.37915564  0.43609135 -0.37247296 -1.21252053 -0.36379349
# [2,]  1.37915564  4.78410934  1.35694145 -1.21252053 -4.31436344 -1.19085831
# [3,]  0.43609135  1.35694145  0.43609135 -0.36379349 -1.19085831 -0.36379349
# [4,] -0.37247296 -1.21252053 -0.36379349  0.51192600  1.55177413  0.48018678
# [5,] -1.21252053 -4.31436344 -1.19085831  1.55177413  5.31193908  1.48241981
# [6,] -0.36379349 -1.19085831 -0.36379349  0.48018678  1.48241981  0.48018678
# [7,]  0.00000000  0.00000000  0.00000000 -0.09154268 -0.22027625 -0.07471168
# [8,]  0.00000000  0.00000000  0.00000000 -0.22027625 -0.64323991 -0.18445802
# [9,]  0.00000000  0.00000000  0.00000000 -0.07471168 -0.18445802 -0.07471168
# [10,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [11,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [12,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
#       [,7]        [,8]        [,9]        [,10]       [,11]       [,12]
# [1,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [2,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [3,]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
# [4,] -0.09154268 -0.22027625 -0.07471168  0.00000000  0.00000000  0.00000000
# [5,] -0.22027625 -0.64323991 -0.18445802  0.00000000  0.00000000  0.00000000
# [6,] -0.07471168 -0.18445802 -0.07471168  0.00000000  0.00000000  0.00000000
# [7,]  0.29320484  0.71978889  0.16205254 -0.10435894 -0.25399393 -0.06194131
# [8,]  0.71978889  2.13312603  0.41294396 -0.25399393 -0.74842743 -0.15305771
# [9,]  0.16205254  0.41294396  0.16205254 -0.06194131 -0.15305771 -0.06194131
# [10,] -0.10435894 -0.25399393 -0.06194131  0.17861575  0.45287619  0.06925715
# [11,] -0.25399393 -0.74842743 -0.15305771  0.45287619  1.37180187  0.17395849
# [12,] -0.06194131 -0.15305771 -0.06194131  0.06925715  0.17395849  0.06925715
# ------------------------------------------------------------------------------------------------------------------------

#------------------------------------Supplementary S2 Lift-one and constrained lift-one comparison--------------------------------
beta = c(0.5, 0.5, 0.5)
X = matrix(data=c(1,-1,-1,1,-1,1,1,1,-1), byrow=TRUE, nrow=3)
W_matrix = W_func_GLM(X=X, b=beta)
w00 = c(1/6, 1/6, 2/3) #an arbitrary starting allocation

#Example S2.1 unconstrained lift-one
CDsampling::liftone_GLM(X=X, W=W_matrix, reltol=1e-10, maxit=100, random=FALSE, nram=3, w00=w00)
# Optimal Sampling Results:
# ================================================================================
# Optimal approximate allocation:
#   1      2      3
# w  0.3333 0.3333 0.3333
# w0 0.1667 0.1667 0.6667
# --------------------------------------------------------------------------------
# Maximum :
# 0.0077
# --------------------------------------------------------------------------------
# itmax :
# 7.0
# --------------------------------------------------------------------------------
# convergence :
# TRUE
# --------------------------------------------------------------------------------

#Example S2.2 constrained lift-one
#define constrained conditions and upper/lower bound
g.con = matrix(,nrow=10, ncol=3)
g.con[1,]=c(1,1,1)
g.con[2:7, ]=rbind(diag(3),diag(3))
g.con[8,]=c(1,0,0)
g.con[9,]=c(0,0,1)
g.con[10,]=c(4,0,-1)
g.dir = c("==", rep(">=",3), rep("<=",3),"<=", ">=", ">=")
g.rhs = c(1, rep(0,3), rep(1,3), 1/6, 8/15, 0)

lower.bound = function(i, w){
  if(i == 1){
    return(max(1-(1-w[i])/w[2], 1-(1-w[i])/w[3], (w[3])/(4-4*w[i]+w[3])))
  }
  if(i == 2){
    return(max(0, 1-((1-w[i])/(6*w[1])), 1-(1-w[i])/w[3]))
  }
  if(i == 3){
    return(max(8/15, (1-(1-w[i])/(6*w[1])), 1-(1-w[i])/w[2]))}
}

upper.bound = function(i, w){
  if(i == 1){
    return(min(1/6, 1-(8*(1-w[i])/(15*w[3]))))
  }
  if(i == 2){
    return(1-(8*(1-w[i])/(15*w[3])))
  }
  if(i == 3){
    return((4*w[1])/(1+4*w[1]-w[i]))}
}


set.seed(123)
liftone_constrained_GLM(X=X, W=W_matrix, g.con=g.con, g.dir=g.dir, g.rhs=g.rhs,
                        lower.bound=lower.bound, upper.bound=upper.bound,reltol=1e-10,
                        maxit=100, random=FALSE, nram=3, w00=w00, epsilon = 1e-8)
# Optimal Sampling Results:
# ================================================================================
# Optimal approximate allocation:
#    1      2      3
# w  0.1667 0.3    0.5333
# w0 0.1667 0.1667 0.6667
# --------------------------------------------------------------------------------
# maximum :
# 0.0055
# --------------------------------------------------------------------------------
# convergence :
# TRUE
# --------------------------------------------------------------------------------
# itmax :
# 1.0
# --------------------------------------------------------------------------------
# deriv.ans :
# 0.0199, 0.0026, -0.0133
# --------------------------------------------------------------------------------
# gmax :
# 0.0
# --------------------------------------------------------------------------------
# reason :
# "gmax <= 0"
# --------------------------------------------------------------------------------




#----------------------------------------Supplementary S3 Simulation Codes for Figure 5------------------------------------#
# Simulation to compare Constrained local D-optimal with SRSWOR, Constrained Uniform, and EW with uniform prior designs
beta = c(0, 3, 3, 3)
X=matrix(data=c(1,0,0,0,1,0,1,0,1,0,0,1,1,1,0,0,1,1,1,0,1,1,0,1), ncol=4, byrow=TRUE)
W=CDsampling::W_func_GLM(X=X, b=beta, link="logit")
rc = c(50, 40, 10, 200, 150, 50)/200 #available volunteers/sample size
m = 6
g.con = matrix(0,nrow=(2*m+1), ncol=m)
g.con[1,] = rep(1, m)
g.con[2:(m+1),] = diag(m)
g.con[(m+2):(2*m+1), ] = diag(m)
g.dir = c("==", rep("<=", m), rep(">=", m))
g.rhs = c(1, rc, rep(0, m))

lower.bound=function(i, w){
  nsample = 200
  rc = c(50, 40, 10, 200, 150, 50)/nsample
  m=length(w) #num of categories
  temp = rep(0,m)
  temp[w>0]=1-pmin(1,rc[w>0])*(1-w[i])/w[w>0];
  temp[i]=0;
  max(0,temp);
}

upper.bound=function(i, w){
  nsample = 200
  rc = c(50, 40, 10, 200, 150, 50)/nsample
  m=length(w) #num of categories
  rc[i];
  min(1,rc[i]);
}

#find constrained local D-optimal
set.seed(123)
approximate_design = CDsampling::liftone_constrained_GLM(X=X, W=W, g.con=g.con, g.dir=g.dir, g.rhs=g.rhs, lower.bound=lower.bound,
                                                         upper.bound=upper.bound, reltol=1e-10, maxit=100, random=TRUE,nram=4, w00=NULL, epsilon=1e-8)

exact_design = CDsampling::approxtoexact_constrained_func(n=200, w=approximate_design$w, m=6, beta=beta, link='logit', X=X, Fdet_func=Fdet_func_GLM,
                                                          iset_func=iset_func_trial)

#constrained EW D-optimal design
#e.g. use uniform distribution as prior
library(cubature)
unif.prior <- rbind(c(-2, -1, -1, -1), c(2,  5,  5, 5))
#expectation
W.EW.unif = matrix(rep(0,6))
for (i in 1:6){
  x = matrix((cbind(1, unique(X)))[i,])
  W.EW.unif[i] = hcubature(function(beta) dunif(beta[1], min=unif.prior[1,1], max=unif.prior[2,1])*dunif(beta[2], min=unif.prior[1,2], max=unif.prior[2,2])*dunif(beta[3], min=unif.prior[1,3], max=unif.prior[2,3])*dunif(beta[4], min=unif.prior[1,4], max=unif.prior[2,4])*(exp(x[1]*beta[1]+x[2]*beta[2]+x[3]*beta[3]+x[4]*beta[4])/(1+exp(x[1]*beta[1]+x[2]*beta[2]+x[3]*beta[3]+x[4]*beta[4]))^2), lowerLimit = unif.prior[1,], upperLimit  = unif.prior[2,])$integral
}

set.seed(602)
approximate_design_EW = liftone_constrained_GLM(X=X, W=W.EW.unif,  g.con=g.con, g.dir=g.dir, g.rhs=g.rhs, lower.bound=lower.bound,
                                                upper.bound=upper.bound, reltol=1e-12, maxit=100, random=TRUE, nram=12, w00=NULL, epsilon=1e-12)
exact_design_EW = approxtoexact_constrained_func(n=200, w=approximate_design_EW$w, m=6, beta=beta, link='logit', X=X,
                                                 Fdet_func=Fdet_func_GLM, iset_func=iset_func_trial)

#Constrained uniform design
w00 = rep(1/200, 6)
unif_design = CDsampling::approxtoexact_constrained_func(n=200, w=w00, m=6, beta=NULL, link=NULL, X=NULL, Fdet_func=Fdet_func_unif, iset_func=iset_func_trial)

#simulation of generated data
nsimu=100
p=3 # 3 covariates (excluding intercept)
set.seed(666)
seeds <- sample(1:10000, nsimu)   #** random seeds changed

## matrix for recording beta estimates
beta.estimate <- rep(NA, nsimu*(p+1)*(5)) #nsimu run's , p+1 parameters to estimate(includes intercept), 5 methods (full_data, SRSWOR, Constrained_Unif, EW_Unif, local_Dopt)
dim(beta.estimate) = c(nsimu, p+1, 5) # [1] 100   4   8
dimnames(beta.estimate)[[2]] = paste0("beta",0:p,seq="")
dimnames(beta.estimate)[[3]] <- c('full', "SRSWOR", "Unif", "local_Dopt", "EW_Unif")

#generate X for original 500 patients
gender = c(rep(0,50),rep(0,40),rep(0,10),rep(1,200),rep(1,150),rep(1,50)) #(0 for female and 1 for male)
age1 = c(rep(0,50), rep(1,40), rep(0,10), rep(0,200), rep(1,150), rep(0,50)) #age group (0 for 18~25, 1 for 26~64, and 2 for 65 or above)
age2 = c(rep(0,50), rep(0,40), rep(1,10), rep(0,200), rep(0,150), rep(1,50))
X.original = cbind(gender, age1, age2)
prob = exp(beta %*% t(cbind(1,X.original))) / (1+exp(beta %*% t(cbind(1,X.original)))) #probability of Y = 1 given X using coefficient beta under logistic model

label = rep(0, length(X.original[,1])) #create label to store category of the population data
#In the example unique(X) gives the 6 categories (gender, age1, age2) = (0,0,0), (0,1,0),(0,0,1),(1,0,0),(1,1,0),(1,0,1)
for(k in 1:length(X.original[,1])){
  for (m in 1:m){
    if((X.original[k,]==unique(X.original)[m,])[1] & (X.original[k,]==unique(X.original)[m,])[2] & (X.original[k,]==unique(X.original)[m,])[3]){
      label[k] = m
    }
  }
}
length(label)  # 500
n = tabulate(label) #number of subjects available in each category
sampling_func = function(label, n.sample, X, s, n, m){
  #set constants used in sampling
  J = 0                                   #num of total samples collected
  j = rep(0, m)                   #num of samples collected for ith category in the loop
  c = rep(0, m)                   #num of data in ith category going through the loop already
  isample = vector()                     #empty vector to store sample label being selected
  while(J < n.sample){
    for(k in 1:length(X[,1])){
      i = label[k]
      c[i] = c[i] + 1
      if(j[i]<s[i]){
        if(runif(1) < (s[i] - j[i])/(n[i] - c[i] + 1)){
          j[i]=j[i]+1
          J = J + 1
          isample = c(isample, k)
        }
      }
    }
  }
  return(isample)
}

s_unif = unif_design$allocation #num to sample in constrained uniform from each category
s_localD = exact_design$allocation #num to sample in local D-optimal from each category
s_EWD = exact_design_EW$allocation #num to sample in EW D-optimal from each category

for(isimu in 1:nsimu){
  set.seed(seeds[isimu])
  ##Generate Y response under logistic model
  Y = rbinom(n=length(X.original[,1]), size=1, prob = prob) #using binomial to generate random logistic model response Y with prob calculated before

  #Estimate beta with full data
  beta.estimate[isimu,,"full"] <- glm(Y~X.original,family = "binomial")$coefficients

  #SRSWOR sample
  isample1 <- sample(x=1:500, size=200); #simple sample without replacement, output selected label
  X1=X.original[isample1,];
  Y1=Y[isample1];

  #Estimate beta with SRSWOR sampling
  beta.estimate[isimu,,"SRSWOR"] <- glm(Y1~X1,family = "binomial")$coefficients

  #Uniform sample
  #stratified function returns s[k] (num of samples plan to collect for ith category)
  isample2 = sampling_func(label=label, n.sample=200, X=X.original, s=s_unif, n=n, m=m)
  X2=X.original[isample2,];
  Y2=Y[isample2];

  #Estimate beta with Unif sampling
  beta.estimate[isimu,,"Unif"] <- glm(Y2~X2,family = "binomial")$coefficients

  #Local D-opt Sample
  isample3 = sampling_func(label=label, n.sample=200, X=X.original, s=s_localD, n=n,m=m)
  X3=X.original[isample3,];
  Y3=Y[isample3];

  #Estimate beta with local D-optimal sampling
  beta.estimate[isimu,,"local_Dopt"] <- glm(Y3~X3,family = "binomial")$coefficients

  #EW D-opt Sample
  isample4 = sampling_func(label=label, n.sample=200, X=X.original, s=s_EWD, n=n, m=m)
  X4=X.original[isample4,];
  Y4=Y[isample4];
  #Estimate beta with EW D-optimal sampling
  beta.estimate[isimu,,"EW_Unif"] <- glm(Y4~X4,family = "binomial")$coefficients

}

## RMSE from simulation
beta.noint = beta[2:(p+1)]
## Estimating beta0 (intercept) compared to true
btemp0 = abs(beta.estimate[,1,]) #abs(beta0_est - beta0), beta0 = 0
mse.b0.mean=apply(btemp0,02,mean, na.rm = TRUE) #apply mean to columns, each method's beta_0 abs error mean (when beta=0, it's mean square error as well)
mse.b0.sd=apply(btemp0,2,sd) #sd of beta0 mse

##Estimating beta1-beta4 compared to true
btemp=1/3*(beta.estimate[,2:(p+1),]-beta.noint)^2 #square of beta's error
btemp=sqrt(apply(btemp,c(1,3),sum, na.rm = TRUE)) #firstly sum square error over all the 3 parameters for each method and simulation run, then take sqrt
mse.b5all.mean=apply(btemp,2,mean, na.rm = TRUE) #apply mean over column
mse.b5all.sd=apply(btemp,2,sd, na.rm = TRUE)


## separated mse sd for each beta parameter
btemp1 = sqrt((beta.estimate[,2,] - beta[2])^2)
mse.b1.mean=apply(btemp1,2,mean, na.rm = TRUE) #apply mean to columns, each method's beta_1 mse
mse.b1.sd=apply(btemp1,2,sd, na.rm = TRUE) #sd of beta1 mse

btemp2 = sqrt((beta.estimate[,3,] - beta[3])^2)
mse.b2.mean=apply(btemp2,2,mean, na.rm = TRUE) #apply mean to columns, each method's beta_1 mse
mse.b2.sd=apply(btemp2,2,sd, na.rm = TRUE) #sd of beta1 mse

btemp3 = sqrt((beta.estimate[,4,] - beta[4])^2)
mse.b3.mean=apply(btemp3,2,mean, na.rm = TRUE) #apply mean to columns, each method's beta_1 mse
mse.b3.sd=apply(btemp3,2,sd, na.rm = TRUE) #sd of beta1 mse

output_simulation_rmse = as.data.frame(rbind(cbind(btemp0, category="beta0"),
                                             cbind(btemp, category="all_coef"),
                                             cbind(btemp1, category="beta1"),
                                             cbind(btemp2, category="beta21"),
                                             cbind(btemp3, category="beta22")))


library(data.table)
output_simulation_rmse.long=melt(setDT(output_simulation_rmse),
                                 id.vars=c("category"),
                                 variable.name='Method') #change to long format for plotting boxplot
colnames(output_simulation_rmse.long)[3]='RMSE'
colnames(output_simulation_rmse.long)[1]='Coef'

#making boxplot
library(ggplot2)
library(dplyr)

output_simulation_rmse.long %>% ggplot(aes(x=Coef, y=as.numeric(RMSE), fill=factor(Method))) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE)+
  scale_fill_grey(start = 0, end = 1, name="Design Method") +xlab("Coefficient")+ylab("RMSE")+
  theme_bw()+facet_wrap(~Coef, scales='free')




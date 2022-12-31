library(data.table)
library(ggplot2)
library(libstableR)
#library(mvgb)
library(mvpd)
library(stable, lib.loc="~/RLIBS") ## nolan, not lindsey
library(bench )
library(broman)
library(stabledist)
library(cubature)
library(mvtnorm)
library(matrixcalc)
library(Matrix)

set.seed(10) 

######################################
### BEGIN  dim d; alpha ##############
######################################


## dimension d is > 1 integer
d<-4

## exchangeable or arbitrary shape matrix?
## Pick one:
shape_exch_or_arbi<-"exch"
#shape_exch_or_arbi<-"toep"
#shape_exch_or_arbi<-"arbi"

## integration limits the same or different?
## Pick one:
int_lim_same_or_diff <- "same"
#int_lim_same_or_diff <- "diff"

## 0<alpha<2 `alp.in`
alp.in<-df.in <- 1.70

covariation<-function(qij, qjj, a) qij*qjj^(a/2 - 1)
qjj <-V <- 1
rho <- 0.90
qij <- rho*V
covariation(qij=qij,qjj = qjj,a=alp.in)

######################################
### END dim d; alpha   ###############
######################################

######################################
### BEGIN shape matrix ###############
######################################


##########################################################
# source: https://stat.ethz.ch/pipermail/r-help/2008-February/153708
# Generating a random positive-definite matrix with user-specified positive eigenvalues
# If eigenvalues are not specified, they are generated from a uniform distribution
## Posdef <- function (n, ev = runif(n, 0, 10))
pos_def_mat <- function (n, ev = runif(n, 0, 10))
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  ZnearPD <- matrix(Matrix::nearPD(Z)$mat,n,n) ## bruce adds line to Ravi's fcn
  return(ZnearPD)
}

S_exch <- suppressWarnings(matrix(c(V, rep(V*rho, d)), d,d))
matrixcalc::is.positive.definite(S_exch, tol=1e-8)
S_toep <- toeplitz(c(V, seq(from=0.90*V,by=-0.1*V, length.out=(d-1))))
matrixcalc::is.positive.definite(S_toep, tol=1e-8)
S_arbi <- pos_def_mat(n=d, ev= eigen(S_toep)$val)
eigen(S_arbi)$val
matrixcalc::is.positive.definite(S_arbi, tol=1e-8)


ifelse(shape_exch_or_arbi=="exch", S <- S_exch, ifelse(shape_exch_or_arbi=="arbi", S <- S_arbi, S <- S_toep))


######################################
### END shape matrix #################
######################################


######################################
### BEGIN limits of integration ######
######################################

ifelse(int_lim_same_or_diff=="same",
       ## ab_same:
       {bb.upp <- rep(2,d);
       bb.low <- -bb.upp
       },
       ## ab_diff:
       {bb.upp <- rep(c(1,2,3,4),length.out=d);
       bb.low <- -rev(bb.upp)
       }
)
######################################
### END limits of integration ########
######################################


######################################
### BEGIN bench::mark error   ########
######################################

bm.abs.err <- 1e-4

######################################
### END bench::mark error     ########
######################################

print(paste0("Benchmark Testing for d=", d))
print(paste0("alpha: ", alp.in))
print("Shape matrix, S:")
print(S)
print(paste0("With integration bounds:"))
print(cbind(bb.low, bb.upp))
print(paste0("At an accuracy level of abs(W-Y) < ", bm.abs.err))


pdest<-
mvpd::pmvss(lower=bb.low,
                            upper=bb.upp,
                                Q=S,
                            alpha=alp.in)$val
pdest

# gbest <-
# mvgb::pmvss(lower=bb.low,
#                             upper=bb.upp,
#                                 Q=S,
#                             alpha=alp.in,
#                             maxpts=25000*350)
# gbest[c("value","inform","error")]

# use cubature to test new method of computing prob. of a hyperrectangle
# for a sub-Gaussian distribution
test1 <- function( lower, upper, alpha, R, tol ) {

    fn1 <- function( x, alpha, R ) {
          dmvstable.elliptical(matrix(x,ncol=1), alpha, R )
          }

    a <- hcubature( fn1, lower, upper, alpha=alpha, R=R, tol=tol )
    return(a)
}
###############################################################
truth_tol <- 1e-5
begin.truth <- Sys.time()
nsest3 <- test1(lower=bb.low, upper=bb.upp, alpha=alp.in, R=S, tol=truth_tol )
end.truth <- Sys.time()
nsest3 # tol<- 1e-05: 0.7075104
       # tol<- 1e-07:
print(paste0("The hcubature TRUTH integral took:" ))
time_for_truth <- end.truth - begin.truth
print(time_for_truth)
print(paste0("The hcubature TRUTH integral value:" ))
print(nsest3)

# > print(paste0("The hcubature TRUTH integral took:" ))
# [1] "The hcubature TRUTH integral took:"
# > time_for_truth <- end.truth - begin.truth
# > print(time_for_truth)
# Time difference of 59.68943 mins
# > print(paste0("The hcubature TRUTH integral value:" ))
# [1] "The hcubature TRUTH integral value:"
# > print(nsest3)
# $integral
# [1] 0.7075104
# 
# $error
# [1] 7.075068e-06
# 
# $functionEvaluations
# [1] 1870341
# 
# $returnCode
# [1] 0



## 40 instances of PD
## 40 instances of GB
## doing 1e-5 and 25000*10 is too much....runs for days.  Need better reporting for the finished > absep for GB
  
pmv_sim_val <- 8.0e5  # 8.0e7
TRUTH <- 0.7075104

## set this and re-run
(rel.tol.si.in <- 1e-3) # 1e-4

(abseps.pmvnorm.in = rel.tol.si.in*1e-2 )
(maxpts.pmvnorm.in = 25e3 * min(c((1/rel.tol.si.in)/10,85000)))

(abseps.in = rel.tol.si.in)
(maxpts.in = 25e3 * min(c((1/rel.tol.si.in)/1000,85000)))

sb <-
  bench::mark(
    qTRUTHq = TRUTH,
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    RAMC   = pmvstable.MC(a=bb.low,
                          b=bb.upp,
                          dist=mvstable.elliptical(R=S,alpha=alp.in),
                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PDMC = mvpd::pmvss_mc(bb.low,
                                          bb.upp,
                                          Q=S,
                                          alpha=alp.in,
                                          n=pmv_sim_val),
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,
    PD   = mvpd::pmvss(bb.low,
                                       bb.upp,
                                       Q=S,
                                       alpha=alp.in,
                                       rel.tol.si = rel.tol.si.in, 
                                       subdivisions.si = 5000L, 
                                       abseps.pmvnorm=abseps.pmvnorm.in, 
                                       maxpts.pmvnorm=maxpts.pmvnorm.in)$val,    
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    # GB   = mvgb::pmvss(bb.low,
    #                                    bb.upp,
    #                                    Q=S,
    #                                    alpha=alp.in,
    #                                    maxpts=maxpts.in,
    #                                    abseps = abseps.in,
    #                                    rerun.nan=TRUE)[1],
    check=function(W,Y) abs(as.numeric(W[1])-as.numeric(Y[1])) < bm.abs.err*1e4
  )

sb$result

approach <- attr(summary(sb)$expression, "description")

print(summary(sb,relative=FALSE))
print(summary(sb,relative=TRUE))
print(sb$result)

results <- sb
results$approach <- attr(summary(sb)$expression, "description")
results$alpha <- alp.in
results$dimension <- d
results$Q <- shape_exch_or_arbi
results$integRAMCtion_limits <- int_lim_same_or_diff
results$hcubature_true_value <- TRUTH             ## on mac we reuse results from biowulf
results$hcubature_true_tol   <- NA #truth_tol     ## on mac we reuse results from biowulf
results$hcubature_true_time  <- NA #time_for_truth## on mac we reuse results from biowulf
results$check_error <- bm.abs.err
results$pmv_sim_val <- pmv_sim_val

# print(results)
# results
# setDT(results)
# results

setDF(results)
# results$estimate <- just_estimate$value
results$outcome <- as.numeric(results$result)

wonty <-  
  ggplot(results[-1,]) +
  # geom_hline(yintercept=TRUTH-1e-5, color="orange") +
  # geom_hline(yintercept=TRUTH+1e-5, color="orange") +
  geom_hline(yintercept=TRUTH-1e-4, lty=2) +
  geom_hline(yintercept=TRUTH+1e-4, lty=2) +
  geom_hline(yintercept=TRUTH-1e-3, lty=3) +
  geom_hline(yintercept=TRUTH+1e-3, lty=3) +
  geom_hline(yintercept=TRUTH) +
  geom_point(aes(color=approach, y=outcome, x=median)) +
  scale_y_continuous(name=NULL,
                     labels=c(#TRUTH-1e-5,
                       #TRUTH+1e-5,
                       expression(0.707*underline(4)*104),#TRUTH-1e-4, ##0.7075104
                       expression(0.707*underline(6)*104),#TRUTH+1e-4,
                       expression(underline(0.706)*5104),#TRUTH-1e-3, ##0.7075104
                       expression(underline(0.708)*5104),#TRUTH+1e-3,
                         TRUTH
                     ),
                     breaks=c(#TRUTH-1e-5,
                       #TRUTH+1e-5,
                       TRUTH-1e-4,
                       TRUTH+1e-4,
                       TRUTH-1e-3,
                       TRUTH+1e-3,
                       TRUTH)#,
                     #limits=c(TRUTH-1e-3, TRUTH+1e-3)
  ) +
  xlab("Median Benchmark Time")

wonty 
saveRDS(wonty,
        paste0("gg_rel.tol.si_",rel.tol.si.in,"_pmv_sim_val_",pmv_sim_val,".RDS")) 


if(d < 9){dprint <- paste0("00",d)}
if(d > 9 & d<99){dprint <- paste0("0",d)}
if(d > 99){dprint <-paste0(d)}

file_prefix <-
  paste("d"    , dprint,
        "alpha", broman::myround(alp.in,2),
        "Q"    , shape_exch_or_arbi,
        "IL"   , int_lim_same_or_diff,
        "TV"   , broman::myround(TRUTH,7),
        "Error", broman::myround(bm.abs.err,7),
        "pmvSimval", pmv_sim_val,
        
        sep="_")
print(file_prefix)
saveRDS(results,
        paste0(file_prefix,".RDS"))

require(LCCR)

#---- Analysis about victims of trafficking ----
UKdat5 = data.frame(LA = c(1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1), 
                    NG = c(0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1), 
                    PFNCA = c(0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1), 
                    GO = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1), 
                    GP = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0), 
                    count = c(54, 463, 995, 695, 316, 15, 19, 3, 62, 19, 
                              1, 76, 11, 8, 1, 1, 4, 1))

# prepare data
Y = freq_data(as.matrix(UKdat5[,1:5]),UKdat5[,6])
Ya = aggr_data(Y)$Ya

# estimate model with one latent class
est1 = estLCCR(Ya,H=1,se_out=TRUE)
summary(est1)

# estimate model with two latent classes
est2 = estLCCR(Ya,H=2,se_out=TRUE)

#---- Analysis of meningitis data ----
# load data
load("meningits_data.rda")
Y = freq_data(D[,1:4])

#---- Analysis without covariates ----
# estimate with model with 1 to 3 latent classes
est1 = estLCCR(Y,H=1,se_out=TRUE)
est2 = estLCCR(Y,H=2,se_out=TRUE)
est3 = estLCCR(Y,H=3,se_out=TRUE)

# table of results
Tab = rbind(c(k=1,Loglik=est1$lk,np=est1$np,BIC=est1$BIC,N=est1$N),
            c(2,est2$lk,est2$np,est2$BIC,est2$N),
            c(3,est3$lk,est3$np,est3$BIC,est3$N))

# check starting values
est2r = est2
for(it in 1:5){
  tmp = estLCCR(Y,H=2,init_rand=TRUE)
  if(est2r$lk>est2$lk) est2r = tmp
}

# confidence interval
CI2 = confint(est2)

# summary results
summary(est2)
summary(CI2)
plot(CI2)

#---- Analysis with covariates ----
# build covariate matrices
Age = D[,5]
Aez = D[,6]
Year = D[,7]
W = cbind(Age,Aez)
X = Year-2003
agg = aggr_data(Y,W=W,X=X)
Wa = agg$Wa; Xa = cbind(agg$Xa); Ya = agg$Ya
colnames(Xa) = "Year"

# estimate model with covariates
biv = rbind(c(1,2),c(2,4))
est2cov = estLCCR(Ya,model ="loglin",H=2,W=Wa,X=Xa,biv=biv,free_cov="both",
                  free_biv="int",se_out=TRUE)
CI2cov = confint(est2cov,parm=list(step=0.5))

# estimate model without covariates and aggregated data
est2aggr = estLCCR(Ya,H=2)

# summary results 
summary(est2cov)
summary(CI2cov)
plot(CI2cov)

library("generalRSS")

################
## Section 3.1
################

## Example 1
data("iris")
id = 1:nrow(iris)
set.seed(12)
rss.data = rss.sampling(ID=id, X=iris$Petal.Length, H=3, nsamp=c(2,2,2))
head(rss.data)


## Example 2
set.seed(12)
rss.data = rss.sampling(ID=id, Y=iris$Sepal.Length, X=iris$Petal.Length, H=3, nsamp=c(2,2,2))
head(rss.data)

## Example 3
set.seed(123)
data(iris)
id = 1:nrow(iris)
X = ifelse(iris$Sepal.Length < 5.8,0,1)
head(X)
rss.prop.data = rss.prop.sampling(ID=id, X=X, H=3, nsamp=c(2,2,2))
rss.prop.data

## Example 4
set.seed(123)
rss.prop.data = rss.prop.simulation(H=3, nsamp=c(2,2,2), p=0.6)
rss.prop.data

## Example 5
set.seed(123)
rss.data = rss.simulation(H=3, nsamp=c(3,10,5), dist="t", rho=1,delta=0)
rss.design(rss.data)

## Example 6
set.seed(123)
rss.prop.data= rss.prop.simulation(H=3, nsamp=c(10,15,20), p=0.5)
rss.design(rss.prop.data, prop=TRUE)

################
## Section 3.2
################

## Example 1
set.seed(123)
rss.data1 = rss.simulation(H=3,nsamp=c(6,8,10),dist="normal", rho=0.8,delta=0)

set.seed(123)
rss.data2 = rss.simulation(H=3,nsamp=c(6,6,6),dist="normal", rho=0.8,delta=0.5)
rss.t.test(data1=rss.data1, data2=rss.data2, alpha=0.05,
           alternative="two.sided", mu0=0, method="naive")

## Example 2
rss.ELR.test(data=rss.data1, alpha=0.05, mu0=0)

## Example 3
rss.sign.test(data=rss.data1, alpha=0.05, alternative="two.sided", median0=0)

## Example 4
set.seed(123)
rss.prop.data=rss.prop.simulation(H=3,nsamp=c(12,9,6),p=0.6)
rss.prop.test(data=rss.prop.data, alpha=0.05, alternative="two.sided", p0=0.2)

## Example 5
rss.AUC.test(data1=rss.data1, data2=rss.data2, alpha=0.05, delta0=0.5)

################
## Section 4.1
################
## One-sample problem

library("dplyr")
library("NHANES")
library("ggplot2")

# preprocess the data and assume it as a true population
dat = NHANES |> 
	distinct(ID, .keep_all = TRUE) |> 
	select(ID, BMI, Weight)


mu0 = mean(dat$BMI,na.rm=T)
cor(na.omit(cbind(dat$BMI, dat$Weight)))

# Histogram: BMI distribution in the NHANES dataset
ggplot(dat, aes(x = BMI)) +
	geom_histogram(aes(y = after_stat(density)), alpha = 0.4, position = "identity", bins = 30) +
	labs(x = "BMI", y = "Density") +
	theme_minimal()

# BRSS sampling
set.seed(12)
org.nsamp = c(10,10,10)
rss.dat = rss.sampling(ID=dat$ID, X=dat$Weight, H=3, nsamp=org.nsamp)
ind<- match(rss.dat$ID,dat$ID)
rss.dat$y = dat$BMI[ind]
head(rss.dat)

# remove the subject having NA from BRSS
rss.dat = na.omit(rss.dat)
org.nsamp = table(rss.dat$rank)
print(org.nsamp) # resulting URSS

# RSS t-test using original URSS data
org.t = rss.t.test(data1=rss.dat,mu0=mu0, method="sample")
print(org.t)
diff(org.t$CI)

# Find the efficient sample allocation for URSS
alloc = rss.design(data=rss.dat[,c("rank","y")])
alloc
add.samp = alloc$Adj.Neyman - org.nsamp
add.samp

set.seed(12)
add.dat = rss.sampling(ID=dat$ID, X=dat$Weight, H=3, nsamp=add.samp)
add.ind = match(add.dat$ID, dat$ID)
add.dat$y = dat$BMI[add.ind]

update.dat = rbind(rss.dat, add.dat)
table(update.dat$rank) # Efficient URSS allocation

update.t = rss.t.test(data1=update.dat, mu0=mu0, method="sample")
print(update.t)
diff(update.t$CI)


# simulations in Table 4

# subfunction for resampling to update RSS until no missing
repeat_sampling = function(after.dat, H, add.samp) {
  repeat {
    # Sampling
    add.dat = rss.sampling(ID = after.dat$ID, X = after.dat$Weight, H = H, nsamp = add.samp)
    add.ind = match(add.dat$ID, after.dat$ID)
    add.dat$y = after.dat$BMI[add.ind]

    # Check for missing values
    if (all(!is.na(add.dat$y))) {
      return(add.dat)
    }
  }
}

# preprocess the data and assume it as a true population
dat = NHANES |> 
	distinct(ID, .keep_all = TRUE) |>
	select(ID, BMI, Weight)

# repeat 500 times
res = matrix(NA,500,4)
idx = NULL
update.alloc = matrix(NA,500,3)

# assume about 10% missing
for(ite in 1:500){
	set.seed(ite*1)

	org.nsamp = c(10,10,10)
	rss.dat = rss.sampling(ID=dat$ID, X=dat$Weight, H=3, nsamp=org.nsamp)
	ind = match(rss.dat$ID,dat$ID)
	rss.dat$y = dat$BMI[ind]

	n_miss = sum(org.nsamp)*0.1
	miss_idx = sample(1:nrow(rss.dat), size = n_miss)
	rss.dat$y[miss_idx] = NA

	# remove the subject having NA
	if(sum(is.na(rss.dat$y)) > 0){
		rss.dat = na.omit(rss.dat)
		org.nsamp = table(rss.dat$rank)
		idx[ite] = 30-sum(org.nsamp)

		org.t = rss.t.test(data1=rss.dat, mu0=mu0, method="sample")
		res[ite,1] = diff(org.t$CI)
		res[ite,3] = as.numeric(org.t$CI[1] <= mu0 & mu0 <= org.t$CI[2])
		alloc = rss.design(data=rss.dat[,c("rank","y")])
		add.samp1 = alloc$Adj.Neyman - org.nsamp
		add.samp2 = alloc$LRC.allocation - org.nsamp
		add.samp = if(sum(add.samp1) < sum(add.samp2)) add.samp1 else add.samp2

		if(sum(add.samp)!=0){
			set.seed(ite*12)
			add.dat = repeat_sampling(after.dat = dat, H = 3, add.samp = add.samp)

			update.dat = rbind(rss.dat, add.dat)
		}else{
			update.dat=rss.dat
		}

		update.alloc[ite,] = table(update.dat$rank)
		update.t = rss.t.test(data1=update.dat, mu0=mu0, method="sample")
		res[ite,2] = diff(update.t$CI)
		res[ite,4] = as.numeric(update.t$CI[1] <= mu0 & mu0 <= update.t$CI[2])

	}else{
		idx[ite] = 0
		update.t = org.t = rss.t.test(data1=rss.dat, mu0=mu0, method="sample")
		res[ite,1:2] = diff(org.t$CI)
		res[ite,3:4] = as.numeric(org.t$CI[1] <= mu0 & mu0 <= org.t$CI[2])
		update.alloc[ite,] = table(rss.dat$rank)
	}
}

# averaged length of CI and coverage probabilities
# orginal URSS
apply(res,2,mean)[c(1,3)]
# updated RSS
apply(res,2,mean)[c(2,4)]

# averaged sample size of original URSS
mean(30-idx)

# SRS with same sample size (updated sample size)
srs.size = apply(update.alloc, 1, sum)

# Remove missings
dat = dat %>% filter(!is.na(dat$BMI))

# repeat 500 times
srs.res = matrix(NA,500,2)
for(ite in 1:500){
	set.seed(ite*123)
	srs.n = sample(1:nrow(dat), srs.size[ite],replace=T)
	srs.dat = dat[srs.n,]
	srs.t = t.test(srs.dat$BMI, mu=mu0)
	srs.res[ite,1] = diff(srs.t$conf.int)
	srs.res[ite,2] = as.numeric(srs.t$conf.int[1] <= mu0 & mu0 <= srs.t$conf.in[2])
}

# averaged length of CI and coverage probabilities
apply(srs.res,2,mean)

# averaged sample size
mean(srs.size)



################
## Section 4.2
################

## Two-sample problem
library(tidyverse)
library(haven)

load("diabetesAUC.RData")

grp1 = data |> filter(DIQ010 == 0) # control
grp2 = data |> filter(DIQ010 == 1) # diabetes

mean(grp1$LBXGLU)
mean(grp2$LBXGLU)

cor(data$LBXGH, data$LBXGLU)

library(pROC)
roc(data$DIQ010, data$LBXGH, na.rm=T, ci=T)
delta0 = roc(data$DIQ010, data$LBXGLU, direction=c("<"), levels=c(0,1))$auc
delta0

# histograms of two groups
combined_data = data.frame(
	FPG = c(grp1$LBXGLU, grp2$LBXGLU),
	Group = c(rep("No Diabetes", nrow(grp1)), rep("Diabetes", nrow(grp2)))
)

ggplot(combined_data, aes(x = FPG, fill = Group)) +
	geom_histogram(aes(y = after_stat(density)), alpha = 0.4, position = "identity", bins = 30) +
	labs(
		x = "FPG",
		y = "Density",
		fill = "Group",
		color = "Group"
	) +
	theme_minimal()


# Application example
## brss
set.seed(12)
org.nsamp1 = c(10,10,10)
org.nsamp2 = c(10,10,10)
H = length(org.nsamp1)
brss.grp1 = rss.sampling(ID=grp1$SEQN, X=grp1$LBXGH, Y=grp1$LBXGLU,H=H, nsamp=org.nsamp1)
brss.grp2 = rss.sampling(ID=grp2$SEQN, X=grp2$LBXGH, Y=grp2$LBXGLU,H=H, nsamp=org.nsamp2)
brss.auc = rss.AUC.test(data1=brss.grp1, data2=brss.grp2, alpha=0.05, delta0=delta0)
brss.auc

## urss
set.seed(12)
org.nsamp1 = c(5,10,15)
org.nsamp2 = c(15,10,5)
H = length(org.nsamp1)
urss.grp1 = rss.sampling(ID=grp1$SEQN, X=grp1$LBXGH, Y=grp1$LBXGLU,H=H, nsamp=org.nsamp1)
urss.grp2 = rss.sampling(ID=grp2$SEQN, X=grp2$LBXGH, Y=grp2$LBXGLU,H=H, nsamp=org.nsamp2)
urss.auc = rss.AUC.test(data1=urss.grp1, data2=urss.grp2, alpha=0.05, delta0=delta0)
urss.auc

## comparisons
diff(brss.auc$CI)
diff(urss.auc$CI)

# Simulation for Table 5
nsim=500

## RSS Empirical LR test for AUC
rss.sim = function(nsim,grp1,grp2,theta0,org.nsamp1,org.nsamp2,H){
	rss.grp1 = rss.sampling(ID=grp1$SEQN, X=grp1$LBXGH, Y=grp1$LBXGLU,
                          H=H, nsamp=org.nsamp1)

	rss.grp2 = rss.sampling(ID=grp2$SEQN, X=grp2$LBXGH, Y=grp2$LBXGLU,
                        H=H, nsamp=org.nsamp2)

	# AUC inference using rss.AUC.test function
	rss.auc = rss.AUC.test(data1=rss.grp1, data2=rss.grp2, alpha=0.05, delta0=delta0)
}

## Assume BRSS allocation and sampling
set.seed(123) 
org.nsamp1 = c(10,10,10)
org.nsamp2 = c(10,10,10)
H = length(org.nsamp1)
brss.result = matrix(NA,nsim,2)
for (i in 1:nsim){
	repeat {
		rss.auc = try(rss.sim(nsim,grp1,grp2,theta0,org.nsamp1,org.nsamp2,H))
		if (!(inherits(rss.auc,"try-error")))
			break
		}
	brss.result[i,1] = (delta0<rss.auc$CI[2])*(delta0>rss.auc$CI[1])
	brss.result[i,2] = diff(rss.auc$CI)
}
colMeans(brss.result)

## Assume URSS allocation and sampling
set.seed(123) 
org.nsamp1 = c(5,10,15)
org.nsamp2 = c(15,10,5)

urss.result = matrix(NA,nsim,2)
for (i in 1:nsim){
	repeat {
		rss.auc = try(rss.sim(nsim,grp1, grp2, theta0, org.nsamp1, org.nsamp2, H))
		if (!(inherits(rss.auc, "try-error")))
			break
	}
	urss.result[i,1] = (delta0 < rss.auc$CI[2])*(delta0 > rss.auc$CI[1])
	urss.result[i,2] = diff(rss.auc$CI)
}
colMeans(urss.result)

# SRS Empirical LR test for AUC
## functions for SRS EL test
srs.AUC.test <- function(data1, data2, alpha=0.05, delta0=0.5){
	if( (alpha > 0) & (alpha < 1)){
		nx = length(data1)
		ny = length(data2)
		srs.el.res = srs.el(srsx=data1, srsy=data2, nx, ny)
		result <- list(CI = c(srs.el.res$Low, srs.el.res$Up))
		return(result)
	}else stop("alpha is out of bound", call. = F)
}

## adjusted EL
el.auc <- function(mu,x,r.adj) {
	elres = emplik::el.test(x,mu)
	# adjust log EL
	elres$"-2LLR" = elres$"-2LLR"*r.adj
	return(elres)
}

## SRS + EL
srsv10 = function(xij,y,ny) {
	return(1/(ny)*sum(xij<=y))
}
srsv01 = function(yrs,x,nx) {
	return(1/(nx)*sum(yrs>=x))
}

srs.el = function(srsx,srsy,nx,ny){
	## One minus U
	srsOU = rep(NA,length(srsy))
	for (j in 1:length(srsy)) {
		srsOU[j] = (sum(srsy[j]>=srsx)/(length(srsx)))
	}

	## delta hat
	srs.delhat = 0
	for (i in 1:length(srsx)){
		for (j in 1:length(srsy)) {
			srs.delhat = srs.delhat+(srsy[j]>=srsx[i])
		}
	}
	srs.delhat = srs.delhat/(nx*ny)

	## sum(1-Uhat-delhat)^2
	srsOUDsq = 0
	for (j in 1:length(srsy)) {
		srsOUDsq = srsOUDsq + (sum(srsy[j]>=srsx)/(length(srsx))-srs.delhat)^2
	}

	## S^2
	srsS10sq = 1/(nx-1)*( sum((purrr::map_dbl(srsx,srsv10,srsy,nx)-srs.delhat)^2) )
	srsS01sq = 1/(ny-1)*( sum((purrr::map_dbl(srsy,srsv01,srsx,ny)-srs.delhat)^2) )
	srsSsq = (nx*srsS01sq+ny*srsS10sq)/(nx+ny)

	## adjustment
	srs.r.adj = (nx)/(nx+ny)*srsOUDsq/(ny*srsSsq)
	srs.el.res = emplik::findUL2(step=0.005, fun=el.auc, MLE=srs.delhat, x=srsOU, r.adj=srs.r.adj)
}


nsample1 = sum(org.nsamp1)
nsample2 = sum(org.nsamp2)

srs.sim = function(nsim,grp1,grp2,theta0,nsample1,nsample2){
	srs.n1 = sample(1:nrow(grp1), nsample1, replace=T)
	srs.grp1 = grp1[srs.n1,]
	data1 = srs.grp1$LBXGLU

	srs.n2 = sample(1:nrow(grp2), nsample2, replace=T)
	srs.grp2 = grp2[srs.n2,]
	data2 = srs.grp2$LBXGLU

	srs.CI = srs.AUC.test(data1, data2, alpha=0.05, delta0=delta0)
	return(srs.CI)
}

set.seed(123)
srs.result = matrix(NA,nsim,2)
for (i in 1:nsim){
	repeat {
		srs.CI = try(srs.sim(nsim,grp1,grp2,theta0,nsample1,nsample2))
		if (!(inherits(srs.CI,"try-error")))
			break
	}
	srs.result[i,1] = (delta0<srs.CI$CI[2])*(delta0>srs.CI$CI[1])
	srs.result[i,2] = srs.CI$CI[2] - srs.CI$CI[1]
}

# results
colMeans(urss.result)
colMeans(brss.result)
colMeans(srs.result)

sessionInfo()



########### BayesPPDSurv Replication Code ####################


####### create E1684 data ###############
# library(dplyr)
# df <- read.table("mina-e1684-e1690.txt",header = TRUE)
# hist <- df %>% filter(stage==4, nodes!=".")
# hist$stratum <- ifelse(hist$nodes <= 2, 1, 2)
# melanoma <- hist %>% select(study,failtime, rfscens, trt, stratum)
# save(melanoma, file="melanoma.Rdata")


########## 1. Summary Table 1 ##############
library(BayesPPDSurv)
library(dplyr)
data("melanoma")
tab_sum <- melanoma %>% group_by(study,trt, stratum) %>% summarise(ss=n(),event=sum(rfscens), risktime=sum(failtime))


######### 2. Analysis of E1690 ###########
set.seed(1)
hist <- melanoma[melanoma$study=="1684",]
current <- melanoma[melanoma$study=="1690",]
n.intervals <- c(4,3)
nMC <- 10000
nBI <- 200
historical <- list(list(time=hist$failtime, event=hist$rfscens, 
                        X=as.matrix(hist[,"trt"]), S=hist$stratum))
result <- phm.fixed.a0(time=current$failtime, event=current$rfscens, 
                       X=as.matrix(current[,"trt"]), S=current$stratum, 
                       historical=historical, a0=0.5, n.intervals=n.intervals, 
                       nMC=nMC, nBI=nBI)
quantile(result$beta_samples)
colMeans(result$lambda_samples[[1]])
colMeans(result$lambda_samples[[2]])
colMeans(result$lambda0_samples[[1]])
colMeans(result$lambda0_samples[[2]])

a <- result$lambda_samples[[2]][,3]
mean(a)
sd(a)
quantile(a,0.025)
quantile(a,0.975)

############ 3. Sampling Prior Generation ################
nMC <- 10000
nBI <- 200
set.seed(1)
samples <- phm.fixed.a0(historical=historical, a0=1, n.intervals=n.intervals, 
                       current.data=FALSE, nMC=nMC, nBI=nBI)
beta_priors <- samples$beta_samples
DN_beta_samp_prior <- as.matrix(beta_priors[beta_priors[,1] > 0, ])
DA_beta_samp_prior <- as.matrix(beta_priors[beta_priors[,1] < 0, ])
lambda_samp_prior <- samples$lambda_samples
  

############ 3. Compute Power / Type I Error with Fixed a0 ################
set.seed(1)
a0 <- 0.6
n.events <- 350
n.subjects <- n.events * 3
nMC <- 10000
nBI <- 200
N <- 5 # N should be much larger in practice
result <- power.phm.fixed.a0(historical=historical, a0=a0, n.subjects=n.subjects, 
            n.events=n.events, n.intervals=n.intervals, 
            samp.prior.beta=DA_beta_samp_prior, samp.prior.lambda=lambda_samp_prior, 
            dist.enroll="Uniform", param.enroll=4, nMC=nMC, nBI=nBI, 
            delta=0, nullspace.ineq=">", N=N)

########## 4. Compute Power / Type I Error with Random a0 ##########

set.seed(1)
nMC <- 10000 
nBI <- 200
prior.beta <- approximate.prior.beta(historical, n.intervals, 
                   prior.a0.shape1=1, prior.a0.shape2=1, 
                   nMC=nMC, nBI=nBI)



library(mixtools)
mix <- normalmixEM(prior.beta)  
list_mixture <- list(list(mix$mu[1], as.matrix(mix$sigma[1]), mix$lambda[1]),
                     list(mix$mu[2], as.matrix(mix$sigma[2]), mix$lambda[2]))

pdf("mix_normal.pdf",width=5,height=5)
par(mfrow=c(1,1))
curve(dnorm(x,mean=mean(prior.beta[,1]), sd=sd(prior.beta[,1])),
      xlim=c(-2,2),ylim=c(0,2), xlab=expression(beta),ylab="Density",col="blue")
curve(mix$lambda[1]*dnorm(x,mean=mix$mu[1], 
    sd=mix$sigma[1])+mix$lambda[2]*dnorm(x,mean=mix$mu[2], sd=mix$sigma[2]),
    col="red",add=TRUE)
lines(density(prior.beta))
legend("topright", c("Density of discrete beta samples from the NPP",
                     "Approximation with a weighted mixture of two normal distributions",
                     "Approximation with a single normal distribution"),
       lty=c(1,1,1),col=c(1,"red","blue"),cex=0.6)
dev.off()

result <- power.phm.random.a0(historical=historical, n.subjects=n.subjects, 
            n.events=n.events, n.intervals=n.intervals, 
            prior.beta.mvn=list_mixture,
            samp.prior.beta=DA_beta_samp_prior, samp.prior.lambda=lambda_samp_prior,  
            dist.enroll="Uniform", param.enroll=4, nMC=nMC, nBI=nBI, 
            delta=0, nullspace.ineq=">", N=N)

##' Install BCClong package (version 1.0.3) from CRAN 

##' Example 1: epileptic data
# install.packages("BCClong")
library("BCClong")
library("ggplot2")
library("joineRML")
library("cowplot")

##' Fitting Bayesian Consensus Clustering for epileptic data
head(epileptic.qol[,c(1,5,6,7,8)])
epileptic.qol$time_month <- epileptic.qol$time/30.25 		# convert days to months
epileptic.qol <- epileptic.qol[order(epileptic.qol$id,epileptic.qol$time_month),]  # Sort by ID and time

##' Make Spaghetti Plots to Visualize the Patterns of the Features 
p1 <- ggplot(data = epileptic.qol, aes(x = time_month, y = anxiety, group = id))+
	 	 geom_point() + geom_line() + 
		 geom_smooth(method = "loess", linewidth = 1.5,group = 1,se = FALSE, span = 2) + 
		 theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text=element_text(size = 20),
			axis.title=element_text(size = 20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face = "bold")) + 
		 xlab("Time (months)") + ylab("anxiety") 
p2 <- ggplot(data = epileptic.qol, aes(x = time_month, y = depress, group = id))+
	 	 geom_point() +
		 geom_line() + 
		 geom_smooth(method = "loess", linewidth = 1.5,group = 1,se = FALSE, span = 2) + 
		 theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text = element_text(size = 20),
			axis.title = element_text(size = 20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face = "bold")) + 
		xlab("Time (months)") + ylab("depress") 
p3 <- ggplot(data = epileptic.qol, aes(x = time_month, y = aep, group = id))+
	  	 geom_point() +
		 geom_line() + 
		 geom_smooth(method = "loess", linewidth = 1.5,group = 1,se = FALSE, span = 2) + 
		 theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text = element_text(size = 20),
			axis.title = element_text(size = 20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face = "bold")) + 
		xlab("Time (months)") + ylab("aep") 
 
# dev.new(width=180, height=90)
plot_grid(p1, NULL, p2, NULL,p3, NULL, 
		labels = c("(A)","", "(B)","","(C)",""), nrow = 1,  
		align = "v", rel_widths = c(1,0.1,1,0.1,1,0.1))

epileptic.qol$anxiety_scale <- scale(epileptic.qol$anxiety)
epileptic.qol$depress_scale <- scale(epileptic.qol$depress)
epileptic.qol$aep_scale <- scale(epileptic.qol$aep)
dat <- epileptic.qol

##' Computed the mean adjusted adherence to determine the number of clusters
##' Here we compute the model under a k from 2 to 5
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#

set.seed(20220929)
alpha.adjust <- NULL
for (k in 2:5){ 
 fit.BCC <- BCC.multi (
    	mydat = list(dat$anxiety_scale, dat$depress_scale, dat$aep_scale),
    	dist = c("gaussian"),
    	id = list(dat$id),
    	time = list(dat$time),
    	formula = list(y ~ time +  (1 |id)),
    	num.cluster = k,
    	initials = NULL,			  
    	burn.in = 1000, 			  
    	thin = 1, 				    
    	per = 100, 				      
    	max.iter = 2000) 	
    	alpha.adjust <- c(alpha.adjust, fit.BCC$alpha.adjust)
	}

##' Plot the mean adherenced parameter for model with k from 2 to 5
num.cluster <- 2:5
plot(num.cluster, alpha.adjust, type = "o", 
	cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, lwd = 2,
	xlab = "Number of Clusters",
	ylab = "mean adjusted adherence", main="mean adjusted adherence")

##' Fitting the final model with 2 clusters
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#
set.seed(89)
fit.BCC2 <-  BCC.multi (
    	mydat = list(dat$anxiety_scale, dat$depress_scale, dat$aep_scale),
    	dist = c("gaussian"),
    	id = list(dat$id),
    	time = list(dat$time),
  	formula = list(y ~ time + (1|id)),
  	num.cluster = 2,
  	burn.in = 1000, 				# number of samples discarded
  	thin = 1, 					# thinning
  	per = 100, 				# output information every "per" iteration 
  	max.iter = 2000) 			# maximum number of iteration 


##' Printing Result Summary (e.g., number of individuals, features, global and local clusterings)
print(fit.BCC2)
summary(fit.BCC2)

##' Printing Summary Statistics for all model parameters
print(fit.BCC2$summary.stat)

##' Printing Summary Statistics for key model parameters
print(fit.BCC2$summary.stat$PPI)
print(fit.BCC2$summary.stat$ALPHA)

##' Traceplot for PPI and ALPHA
traceplot(fit=fit.BCC2, parameter="PPI", ylab="pi", xlab="MCMC samples")
traceplot(fit=fit.BCC2, parameter="ALPHA", ylab="alpha", xlab="MCMC samples")

##' Trace-plot for key model parameters
traceplot(fit=fit.BCC2, cluster.indx = 1, feature.indx=1, parameter="GA", ylab="GA", xlab="MCMC samples")
traceplot(fit=fit.BCC2, cluster.indx = 1, feature.indx=2, parameter="GA", ylab="GA", xlab="MCMC samples")
traceplot(fit=fit.BCC2, cluster.indx = 1, feature.indx=3, parameter="GA", ylab="GA", xlab="MCMC samples")
traceplot(fit=fit.BCC2, cluster.indx = 2, feature.indx=1, parameter="GA", ylab="GA", xlab="MCMC samples")
traceplot(fit=fit.BCC2, cluster.indx = 2, feature.indx=2, parameter="GA", ylab="GA", xlab="MCMC samples")
traceplot(fit=fit.BCC2, cluster.indx = 2, feature.indx=3, parameter="GA", ylab="GA", xlab="MCMC samples")

table(fit.BCC2$cluster.local[[1]])
table(fit.BCC2$cluster.local[[2]])
table(fit.BCC2$cluster.local[[3]])
table(fit.BCC2$cluster.global)

##' Trajectory plot for features
gp1 <- trajplot(fit = fit.BCC2, feature.ind = 1,
			which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[1] ==.(round(fit.BCC2$alpha[1],2)),")")),
			xlab = "time (months)", ylab = "anxiety", color = c("#00BA38", "#619CFF"))
gp2 <- trajplot(fit = fit.BCC2, feature.ind = 2,
			which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[2] ==.(round(fit.BCC2$alpha[2],2)),")")),
			xlab = "time (months)", ylab = "depress", color = c("#00BA38", "#619CFF"))
gp3 <- trajplot(fit = fit.BCC2, feature.ind = 3,
			which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[3] ==.(round(fit.BCC2$alpha[3],2)),")")),
			xlab = "time (months)", ylab = "aep",color = c("#00BA38", "#619CFF"))
gp4 <- trajplot(fit = fit.BCC2,feature.ind = 1,
			which.cluster = "global.cluster",
			title = "Global Clustering", xlab="time (months)", ylab = "anxiety", 
			color = c("#00BA38", "#619CFF"))
gp5 <- trajplot(fit = fit.BCC2, feature.ind = 2,
			which.cluster = "global.cluster",
			title = "Global Clustering", xlab = "time (months)", ylab = "depress", 
			color = c("#00BA38", "#619CFF"))
gp6 <- trajplot(fit = fit.BCC2, feature.ind = 3,
			which.cluster = "global.cluster",
			title = "Global Clustering",
			xlab = "time (months)", ylab = "aep", color = c("#00BA38", "#619CFF"))
plot_grid(gp1, NULL, gp2, NULL, gp3, NULL,
	    gp4, NULL, gp5, NULL, gp6, NULL,
		labels = c("(A)","", "(B)","","(C)","","(D)","","(E)","","(F)",""), nrow = 2,  
		align = "v", rel_widths = c(1,0.1,1,0.1,1,0.1))

##' Posterior Check 
set.seed(20239)
res <- BayesT(fit = fit.BCC2)
plot(log(res$T.obs), log(res$T.rep), xlim = c(8.45,8.7), cex = 1.5,
	ylim = c(8.45,8.7), xlab = "Observed T statistics (in log scale)", 
	ylab = "Predicted T statistics (in log scale)")
abline(0,1,lwd = 2,col = 2)

##' Calculate Bayesian p value
p.value <- sum(res$T.rep > res$T.obs)/length(res$T.rep)
p.value 

fit.BCC2$cluster.global <- factor(fit.BCC2$cluster.global, labels = c("Cluster 1", "Cluster 2"))
boxplot(fit.BCC2$postprob ~ fit.BCC2$cluster.global, ylim = c(0.5,1),
		xlab = "", ylab = "Posterior Cluster Probability")


##' Using a different set of initial values for local and global cluster membership 
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#
set.seed(89)
fit.BCC2a <-  BCC.multi (
   	mydat = list(dat$anxiety_scale, dat$depress_scale, dat$aep_scale),
   	dist = c("gaussian"),
   	id = list(dat$id),
   	time = list(dat$time),
   	formula = list(y ~ time + (1|id)),
 	initial.cluster.membership = "input",
   	input.initial.local.cluster.membership = list(fit.BCC2$cluster.local[[1]],
          							fit.BCC2$cluster.local[[2]], 
								fit.BCC2$cluster.local[[3]]),
   	input.initial.global.cluster.membership = fit.BCC2$cluster.global,
   	num.cluster = 2,
   	burn.in = 1000, 
   	thin = 1, 
   	per = 100,  
   	max.iter = 2000)  

##' Compare the class membership with previous model
table(fit.BCC2$cluster.global, fit.BCC2a$cluster.global)
table(fit.BCC2$cluster.local[[1]], fit.BCC2a$cluster.local[[1]])
table(fit.BCC2$cluster.local[[2]], fit.BCC2a$cluster.local[[2]])
table(fit.BCC2$cluster.local[[3]], fit.BCC2a$cluster.local[[3]])

##' For sensitivity analysis: fitting a more complicated model 
##' with both random intercept and slope
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#
set.seed(20220929)
fit.BCC2b <-  BCC.multi (
    	mydat = list(dat$anxiety_scale, dat$depress_scale, dat$aep_scale),
    	dist = c("gaussian"),
    	id = list(dat$id),
    	time = list(dat$time),
  	formula = list(y ~ time + (1 + time|id)),
  	num.cluster = 2,
  	burn.in = 1000, 			 
  	thin = 1, 				 
  	per = 100, 				  
  	max.iter = 2000) 			 

##' For sensitivity analysis: fitting a more complicate model 
##' with a quadratic term, and with both random intercept and slope
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#
set.seed(20220929)
fit.BCC2c <-  BCC.multi (
    	mydat = list(dat$anxiety_scale, dat$depress_scale, dat$aep_scale),
    	dist = c("gaussian"),
    	id = list(dat$id),
    	time = list(dat$time),
  	formula = list(y ~ time + time2 + (1 + time|id)),
  	num.cluster = 2,
  	burn.in = 1000, 			 
  	thin = 1, 				 
  	per = 100, 				 
  	max.iter = 2000) 			 

##' Cross-tabulation to compare different models
##' based on global clustering
fit.BCC2b$cluster.global <- factor(fit.BCC2b$cluster.global,
	labels = c("Cluster 1", "Cluster 2"))
table(fit.BCC2$cluster.global, fit.BCC2b$cluster.global)

fit.BCC2c$cluster.global <- factor(fit.BCC2c$cluster.global,
	labels = c("Cluster 1", "Cluster 2"))
table(fit.BCC2$cluster.global, fit.BCC2c$cluster.global)


##' Example 2: PBC Data
library("mixAK")
data(PBC910)
# ------------------------------------------------------------------------------------------------#
##' Note to users: the following part of codes (using BCC.multi ()) is computationally intensive 
##' and will take a long time to run, if users just want to get familiar with the codes and outputs,
##' a smaller number of iterations and burn-in can be used to reduce the computational time,
##' for example, by setting burn.in = 10, per = 1, and max.iter = 20 
# ------------------------------------------------------------------------------------------------#
set.seed(89)
fit.BCC2 <- BCC.multi(
    	mydat = list(PBC910$lbili, PBC910$platelet, PBC910$spiders),
    	dist = c("gaussian", "poisson", "binomial"),
    	id = list(PBC910$id),
    	time = list(PBC910$month),
    	formula = list(y ~ time + (1|id),y ~ time + (1|id), y ~ time + (1|id)),
    	num.cluster = 2,
    	burn.in = 10000, 			  
    	thin = 10, 				    
    	per = 100, 				      
    	max.iter = 20000)

##' Printing Summary Statistics for key model parameters
print(fit.BCC2$summary.stat$PPI)
print(fit.BCC2$summary.stat$ALPHA)
print(fit.BCC2$cluster.global)
print(fit.BCC2$cluster.local[[1]])
print(fit.BCC2$cluster.local[[2]])
print(fit.BCC2$cluster.local[[3]])

##' Trajectory plot for features
gp1 <- trajplot(fit = fit.BCC2, feature.ind = 1, which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[1] ==.(round(fit.BCC2$alpha[1],2)),")")),
			xlab = "months", ylab="lbili", color=c("#00BA38", "#619CFF"))
gp2 <- trajplot(fit = fit.BCC2, feature.ind = 2, which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[2] ==.(round(fit.BCC2$alpha[2],2)),")")),
			xlab = "months", ylab="platelet", color = c("#00BA38", "#619CFF"))
gp3 <- trajplot(fit = fit.BCC2, feature.ind=3, which.cluster = "local.cluster",
			title = bquote(paste("Local Clustering (",hat(alpha)[3] ==.(round(fit.BCC2$alpha[3],2)),")")),
			xlab = "months", ylab="spiders", color = c("#00BA38", "#619CFF"))
gp4 <- trajplot(fit = fit.BCC2, feature.ind = 1, which.cluster = "global.cluster",
			title = "Global Clustering",
			xlab = "months", ylab="lbili", color = c("#00BA38", "#619CFF"))
gp5 <- trajplot(fit = fit.BCC2, feature.ind = 2, which.cluster = "global.cluster",
			title = "Global Clustering",
			xlab = "months", ylab="platelet", color = c("#00BA38", "#619CFF"))
gp6 <- trajplot(fit = fit.BCC2, feature.ind = 3, which.cluster = "global.cluster",
			title = "Global Clustering",
			xlab = "months", ylab = "spiders", color = c("#00BA38", "#619CFF"))
plot_grid(gp1, gp2, gp3, gp4, gp5, gp6, 
          		labels = c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)"), 
			ncol = 3, 
			align = "v" )  

set.seed(20042007)
mod <- GLMM_MCMC(y = PBC910[, c("lbili", "platelet", "spiders")],
		dist = c("gaussian", "poisson(log)", "binomial(logit)"),
		id = PBC910[, "id"], 
		x = list(lbili = "empty", platelet = "empty", spiders = PBC910[, "month"]), 
		z = list(lbili = PBC910[, "month"],
		platelet = PBC910[, "month"], spiders = "empty"),
		random.intercept = rep(TRUE, 3), 
		prior.b = list(Kmax = 2),
		nMCMC = c(burn = 100, keep = 1000, thin = 10, info = 100),
		parallel = FALSE)
mod <- NMixRelabel(mod, type = "stephens", keep.comp.prob = TRUE)
cluster.mixAK <- apply(mod[[1]]$poster.comp.prob, 1, which.max)

##' Cross tabulation between mixAK and BCClong methods
table(mixAK = cluster.mixAK, BCClong = fit.BCC2$cluster.global)


 





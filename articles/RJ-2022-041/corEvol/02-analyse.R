## This source code is licensed under the FreeBSD license (see "LICENSE" file)
## (c) 2013/2017 Felix Schönbrodt

## ======================================================================
## This source code takes the simulated data sets from 01-simdata.R,
## computes the point of stability and returns the table
## Note: The original simulations from Schönbrodt & Perugini (2013)
## unfortunately did not use a seed; hence the reported results are not
## fully reproducible and the script here will produce slightly different
## results.
## ======================================================================


library(dplyr)
library(reshape2)

r2Z <- function(r) {
	return(0.5 * log((1 + r)/(1 - r)))
}

# Helper: REcode  Fisher's to correlation
Z2r <- function(Z) {
	return((exp(2*Z)-1)/(exp(2*Z)+1))
}

# function that looks for the point of stability in each trajectory
#' @param metric Is the width of the corridor of stability defined in the r metric ("r") or in the Z metric ("Z")?
getPOS <- function(simData, w, metric="r") {

	if (metric == "r") {
		r1 <- simData %>% mutate(breach = r < (rho-w) | r > (rho+w))
	} else if (metric == "Z") {
		r1 <- simData %>% mutate(breach = r < (Z2r(r2Z(rho)-w)) | r > (Z2r(r2Z(rho)+w)))
	}

	r1 <- r1	%>%
		group_by(unique) %>%
		arrange(unique, -n) %>%
		mutate(breaks=cumsum(breach)) %>%
		filter(breaks==0) %>%
		arrange(unique, n) %>%
		slice(1) %>%
		ungroup()

	return(r1)
}

getQuantiles <- function(POS, probs=c(.80, .90, .95)) {
	res <- data.frame(
		prob=probs,
		qu=round(quantile(POS$n, prob=probs))
	)

	return(res)
}


## ======================================================================
## Analysis in the r metric
## ======================================================================

# ---------------------------------------------------------------------
# Compute POS distributions

# this takes some time ...
for (rho in seq(.1, .1, by=.1)) {
	#print(paste0("Analyzing rho = ", rho))
	load(file=paste0("simData/sim", rho*10, ".RData"))
	sim <- data.frame(sim)

	# compute and save the points of stability
	POS.10 <- getPOS(sim, w=.10)
  #POS.15 <- getPOS(sim, w=.15)
  #POS.20 <- getPOS(sim, w=.20)

	#save(POS.10, file=paste0("POS/POS.", rho*10, ".10.RData"))
	#save(POS.15, file=paste0("POS/POS.", rho*10, ".15.RData"))
	#save(POS.20, file=paste0("POS/POS.", rho*10, ".20.RData"))
}



# If you use the precomputed POS files from /POS, start the script here

Q <- data.frame()
for (rho in seq(.1, .1, by=.1)) {

	#load(file=paste0("POS/POS.", rho*10, ".10.RData"))
	#load(file=paste0("POS/POS.", rho*10, ".15.RData"))
	#load(file=paste0("POS/POS.", rho*10, ".20.RData"))

	# save the quantiles of the POS distributions
	Q <- rbind(Q,
		data.frame(rho=rho, w=.10, getQuantiles(POS.10))#,
		#data.frame(rho=rho, w=.15, getQuantiles(POS.15)),
		#data.frame(rho=rho, w=.20, getQuantiles(POS.20))
	)
}

# reshape to the table format of the publication
#print(dcast(Q, rho ~ prob + w, value.var="qu"))



## ======================================================================
## Analysis in the Z metric
## ======================================================================

# ---------------------------------------------------------------------
# Compute POS distributions
# 
# for (rho in seq(.1, .1, by=.1)) {
# 	print(paste0("Analyzing rho = ", rho))
# 	load(file=paste0("simData/sim", rho*10, ".RData"))
# 	sim <- data.frame(sim)
# 
# 	# compute and save the points of stability
#   tic <- Sys.time()
# 	POS.10 <- getPOS(sim, w=.10, metric="Z")
#   POS.15 <- getPOS(sim, w=.15, metric="Z")
#   POS.20 <- getPOS(sim, w=.20, metric="Z")
# 
# 	save(POS.10, file=paste0("POS_Z/POS.", rho*10, ".10.RData"))
# 	save(POS.15, file=paste0("POS_Z/POS.", rho*10, ".15.RData"))
# 	save(POS.20, file=paste0("POS_Z/POS.", rho*10, ".20.RData"))
# }
# 
# 
# 
# If you use the precomputed POS files from /POS, start the script here
# 
# Q <- data.frame()
# for (rho in seq(.1, .7, by=.1)) {
# 
# 	load(file=paste0("POS_Z/POS.", rho*10, ".10.RData"))
# 	load(file=paste0("POS_Z/POS.", rho*10, ".15.RData"))
# 	load(file=paste0("POS_Z/POS.", rho*10, ".20.RData"))
# 
# 	# save the quantiles of the POS distributions
# 	Q <- rbind(Q,
# 		data.frame(rho=rho, w=.10, getQuantiles(POS.10)),
# 		data.frame(rho=rho, w=.15, getQuantiles(POS.15)),
# 		data.frame(rho=rho, w=.20, getQuantiles(POS.20))
# 	)
# }
# 
# # reshape to the table format of the publication
# dcast(Q, rho ~ prob + w, value.var="qu")

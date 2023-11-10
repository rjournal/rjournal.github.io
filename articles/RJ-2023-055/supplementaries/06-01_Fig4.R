

# Created 10.12.2021 


# Fig 3. PC prior plots


#---------------------------------------------------------------------------------

library(INLA)
library(ggplot2)
library(patchwork)
library(sf)
library(spdep)


# prior for standard deviation

xvals = seq(0.1, 10, length.out=1000)
lambda=-log(0.01)/1

ggplot() + geom_line(aes(x=xvals, y=lambda*exp(-lambda*xvals))) + 
  theme_light() + ylab(expression("Implied prior for "~sigma[b])) + 
  xlab(expression(sigma[b])) + ggtitle("A.") + 
  theme(text = element_text(size = 6)) -> p1



# prior for mixing parameter

shp = read_sf("data/ProvCM01012020_g_WGS84.shp")
W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 


# Create the precision matrix
pol <- spdep::poly2nb(shp)
W <- spdep::nb2mat(pol, style = "B", zero.policy = TRUE)
W <- -W
diag(W) <- abs(apply(W, 1, sum))
n <- nrow(W)
QQ <- W


log.prior = INLA:::inla.pc.bym.phi(Q = QQ, rankdef = 1, u = 0.5, alpha = 0.5)
phis = 1/(1+exp(-seq(-7, 7, len=10000)))

ggplot() + geom_line(aes(x=phis, y=exp(log.prior(phis)))) + 
  theme_light() + ylab(expression("PC prior for "~phi)) + 
  xlab(expression(phi)) + ggtitle("B.") + 
  theme(text = element_text(size = 6)) -> p2



png("Output/PCpriors.png", width = 14, height = 5, units = "cm", res = 300)
(p1|p2) 
dev.off()



######################################################################################
######################################################################################
######################################################################################
######################################################################################



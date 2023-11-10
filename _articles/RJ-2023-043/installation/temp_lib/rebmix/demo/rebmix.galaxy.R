##############################################
## R sources for reproducing the results in ##
##   Marko Nagode:                          ##
##   Finite Mixture Modeling via REBMIX     ##
##############################################

options(prompt = "> ", continue = "+ ", width = 70,
  useFancyQuotes = FALSE, digits = 3)

###################
## Preliminaries ##
###################

# Load package.

library(rebmix)

####################
## Galaxy dataset ##
####################

# Load galaxy dataset.

data(galaxy)

galaxyest <- list(normal = NULL, lognormal = NULL, Weibull = NULL, gamma = NULL)

# Estimate number of components, component weights and component parameters.

pdf <- c("normal", "lognormal", "Weibull", "gamma")

for (i in 1:4) {
  galaxyest[[i]] <- REBMIX(Dataset = list(galaxy = galaxy),
    Preprocessing = "histogram",
    cmax = 10,
    Criterion = "AIC",
    pdf = pdf[i],
    K = 7:20)
}

summary(galaxyest$normal)
summary(galaxyest$lognormal)
summary(galaxyest$Weibull)
summary(galaxyest$gamma)

#library(tikzDevice) # Uncomment to use tikzDevice package.
#tikz("galaxy.tex", width = 4.5, height = 2.25) # Uncomment to use tikzDevice package.
plot(galaxyest$lognormal, pos = 1, what = c("pdf", "marginal pdf"), ncol = 2, npts = 1000)
#dev.off() # Uncomment to use tikzDevice package.

a.theta1.all(galaxyest$lognormal, pos = 1)

a.theta2.all(galaxyest$lognormal, pos = 1)

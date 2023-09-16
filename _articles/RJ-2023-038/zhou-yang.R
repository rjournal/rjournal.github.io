## Code for replication for paper submission: "Estimating Causal Effects using Bayesian Methods with the R Package BayesCACE"
#
# Jincheng Zhou, Gilead (jeni.zhou9@gilead.com)
# Jinhui Yang, University of Minnesota Twin Cities (yang7004@umn.edu)
# James S. Hodges, University of Minnesota Twin Cities (hodge003@umn.edu)
# Lifeng Lin, Florida State University (lifenglin@arizona.edu)
# Haitao Chu, Pfizer Inc. (chux0051@umn.edu)

## 1. Install the R package and view the datasets

# First, installing the BayeCACE R package using the following R code:
if (!"BayesCACE" %in% rownames(installed.packages()))
  install.packages("BayesCACE")
library("BayesCACE")

# Next, open the package in your R environment and view the default datasets.
data("epidural_c", package = "BayesCACE")
head(epidural_c)

data("epidural_ic", package = "BayesCACE")
head(epidural_ic)

# Before performing the CACE analysis, one might want a visual overview of study-specific 
# noncompliance rates in both randomization arms. The function `plt.noncomp` provides a 
# forest plot of noncompliance rates in an **R** plot window. 
plt.noncomp(epidural_c, overall = TRUE)

## 2. CACE for a single trial with noncompliance
# Get customized prior distributions for single-study analysis
prior.study(re.values = list(n.s = 0.01, a.s = 0.01, alpha.s.s = 0.01, alpha.b.s = 0.01, alpha.u.s = 0.01, alpha.v.s = 0.01))

# To estimate CACE for a single study, users need to input data with the same structure as `epidural_c`, 
# containing either one row of observations for a single study, or multiple rows referring to multiple studies 
# in a meta-analysis. This function fits a model for a single study.  
set.seed(123)
out.study <- cace.study(data = epidural_c, conv.diag = TRUE, mcmc.samples = TRUE, two.step = TRUE)

# The estimates of $\theta^\text{CACE}$ for each single study (posterior mean and standard deviation, 
# posterior median, 95\% credible interval, and time-series standard error) can be obtained by
out.study$CACE
# The first sub-list from `conv.out` is
out.study$conv.out[[1]]
# If the dataset used by the function `cace.study()` has more than one study, specifying the argument
# `two.step = TRUE` causes the two-step meta-analysis for $\theta^\text{CACE}$ to be done. 
# The outcomes are saved as a sub-list object `meta`.
out.study$meta

## 3. CACE for a meta-analysis with complete compliance information
# Get a sample prior distribution for meta-analysis for complete compliance data
# Ignore the part before "# priors"; only the part afterwards indicates the prior distribution
prior.meta(random.effects = list(delta.v = FALSE))

# The function `cace.meta.c()` performs the Bayesian hierarchical model method for meta-analysis 
# when the dataset has complete compliance information for all studies. 
out.meta.c <- cace.meta.c(data = epidural_c, conv.diag = TRUE, mcmc.samples = TRUE, study.specific = TRUE)
# In this example, by calling the object `smry` from the output list `out.meta.c`, 
# posterior estimates (posterior mean, standard deviation, posterior median, 95% credible interval, 
# and time-series standard error) are displayed.
out.meta.c$smry
# Users can manually do model selection procedures by including different random effects and 
# comparing DIC from the outputs. DIC and its two components are saved as an object `DIC` in the output list.
out.meta.c$DIC

## 4. CACE for meta-analysis with incomplete compliance information
# The function `out.meta.ic()` also estimates $\theta^\text{CACE}$ using the Bayesian hierarchcal model 
# but can accommodate studies with incomplete compliance data. 
out.meta.ic <- cace.meta.ic(data = epidural_ic, conv.diag = TRUE, mcmc.samples = TRUE, study.specific = TRUE)

## 5. Functions for plots
### 5-1. Diagnostic plots
# The functions `plt.trace`, `plt.density`, and `plt.acf` provide diagnostic plots for the MCMC, 
# namely trace plots, kernel density estimation plots and auto-correlation plots. 
# In the example below we use the objects list obtained from fitting the Bayesian hierarchical model `cace.meta.ic()` 
# to generate the three plots. 
plt.trace(obj = out.meta.ic)
plt.density(obj = out.meta.ic)
plt.acf(obj = out.meta.ic)

### 5-2. Study-specific CACE in a forest plot
# A graphical overview of the results can be obtained by creating a forest plot @lewis2001forest. 
# The function `plt.forest()` draws a forest plot for $\theta^{\text{CACE}}$ estimated from the meta-analysis. 
# Users can call this function for the objects from `cace.meta.c()` or `cace.meta.ic()`.
# Here is an example using the object `out.meta.ic`:
plt.forest(data = epidural_ic, obj = out.meta.ic)

sessionInfo()




# Created 11.10.2021

# Cross validation

#---------------------------------------------------------------------------------

path2save <- "Output/"


library(INLA)
library(dplyr)
library(sf)
library(spdep)


################################################################################
# Cross validation functions
################################################################################

# function to fit INLA and store the truth and the linear predictor
inla.cv <- function(Y){
  
  in.mod <- 
    inla(formula = as.formula(Y),
         data=datCV,
         family="Poisson",  
         verbose = FALSE, 
         control.family=control.family,
         control.compute=list(config = TRUE), 
         control.mode=list(restart=TRUE),
         num.threads = round(parallel::detectCores()*.8), 
         control.predictor = list(link = 1)
    )
  
  
  
  post.samples <- inla.posterior.sample(n = 1000, result = in.mod)
  
  # store the results
  list.CV.results <- list(
    prediction = lapply(post.samples, function(X) exp(X$latent[testIndexes])),
    true_values =  datCV_firslop$deaths[testIndexes]
  )
  
  return(list.CV.results)
  
}



# Function for computing correlation & coverage probability
postpredchecks <- function(Y){
  
  pred.samples <- lapply(Y, function(X) do.call(cbind, X$prediction))
  true_values <- do.call(c, lapply(Y, function(X) X$true_values))
  
  # samples from a Poisson
  set.seed(11)
  pois.samples <- lapply(pred.samples,
                         function(Z) apply(Z, 2, function(X) rpois(n = length(X), lambda = X)))
  
  # calculate quantiles
  pois.quant <- as.data.frame(do.call(rbind, lapply(pois.samples, 
                                                    function(X) t(apply(X, 1, function(Z) quantile(Z, probs = c(0.025, 0.975)))))))
  cov.prob = mean((pois.quant$`2.5%` <= true_values) & (pois.quant$`97.5%` > true_values))
  
  
  res <- list(
    correl = apply(do.call(rbind, pois.samples), 2, function(X) cor(X, true_values)), 
    cov.prob = mean((pois.quant$`2.5%` <= true_values) & (pois.quant$`97.5%` > true_values)) # if you exclude 0s is very low
  )
  
  return(res)
  
}


# Credible Interval function
CrI <- function(X) paste0(X[1], " ", "(", X[2], ", ", X[3], ")")



################################################################################
################################################################################



# define the groups
groups4cv = as.data.frame(expand.grid(age.group = c("less40", "40-59", "60-69", "70-79", "80plus"),
                                      sex = c("F", "M")))

# load data
finaldb = readRDS("Output/findata")

shp = read_sf("data/ProvCM01012020_g_WGS84.shp")
shp = shp[order(shp$COD_PROV),]
shp$COD_RIP <- as.numeric(as.factor(shp$COD_PROV))
colnames(shp)[1] <- "id.space"

W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 


# set up formula and priors
formula = 
  deaths ~ 1 + offset(log(population)) + hol + id.year + 
  f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + # flexible function for temperature
  f(id.wkes, model='iid', hyper=hyper.iid, constr = TRUE) + # variance per week
  f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) + # seasonal effect
  f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym) # spatial effect

# Define the penalized complexity prior a c(1, 0.01) defines Pr(sigma<=1)=0.01, whereas for phi: Pr(\phi<=0.5) = 0.5
hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()



# Perform space-time cross validation
list.results <- list()
years <- 2015:2019
data = finaldb 

t_0 <- Sys.time()

for(j in 1:nrow(groups4cv)){
  
  print(j)
  
  agegroup <- as.character(groups4cv$age.group[j])
  sexg <- as.character(groups4cv$sex[j])
  
  # recreate indices
  data$id.space <- as.numeric(as.factor(data$PROV))
  data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
  data$id.tmp <- inla.group(data$mean.temp, n = 100, method = "cut", idx.only = TRUE)
  data$id.year <- data$year - 2014
  data$id.wkes <- as.numeric(factor(data$EURO_LABEL))
  data <- data[order(data$id.space),]
  
  # select age*sex group
  data %>% filter((ageg %in% agegroup) & (sex %in% sexg)) -> datCV_firslop
  
  list.CV.results.spacetime <- list()

  # run leave-out-one-year cross validation
  for(i in 1:5){
    datCV <- datCV_firslop
    testIndexes <- which(datCV$year %in% years[i], arr.ind=TRUE)
    datCV$deaths[testIndexes] <- NA
    
    list.CV.results.spacetime[[i]] <- inla.cv(Y = formula)
    gc()

  }
  
  saveRDS(list.CV.results.spacetime,
          file = paste0(path2save, "yearCV_", agegroup, "_", sexg))
  
}

t_1 <- Sys.time()
print(t_1 - t_0) # ~4h




##
## and extract the results 

list.results <- apply(groups4cv, 1,
                      function(X) readRDS(paste0(path2save,"yearCV_", X[1], "_", X[2])))
names(list.results) <- paste0(groups4cv[,1], groups4cv[,2])


t_0 <- Sys.time()
res_checks <- lapply(list.results, postpredchecks)
t_1 <- Sys.time()
t_1 - t_0 # 1.67 min

# tables of cross validation
# Correlation table
round(
  do.call(rbind, 
          lapply(lapply(res_checks, function(X) X$correl), 
                 function(X) quantile(X, probs = c(0.50, 0.025, 0.975)))
  ), 
  digits = 2
) -> correl.table
correl.table <- format(round(correl.table, digits = 2), nsmall = 2)

# Coverage probability table
cov.prob <- lapply(res_checks, function(X)X$cov.prob)  
as.data.frame(unlist(cov.prob)) %>% 
  rename(`95%CovProb` = `unlist(cov.prob)`) -> cov.prob.table

cov.prob.table <- format(round(cov.prob.table, digits = 2))


res <- data.frame(cbind(apply(correl.table, 1, CrI), cov.prob.table))
colnames(res) <- c("Correlation", "Coverage")
res

######################################################################################
######################################################################################
######################################################################################
######################################################################################








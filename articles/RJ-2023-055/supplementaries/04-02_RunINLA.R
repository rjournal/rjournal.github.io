

# Created 11.10.2021

# Run INLA 

#---------------------------------------------------------------------------------

path2save <- "Output/"

# load packages
library(INLA)
library(dplyr)
library(sf)
library(spdep)



# define the groups
groups = as.data.frame(expand.grid(age.group = c("less40", "40-59", "60-69", "70-79", "80plus"),
                                      sex = c("F", "M")))

# read the main data for analysis
finaldb = readRDS("Output/findata")

# read the shapefiles
shp = read_sf("data/ProvCM01012020_g_WGS84.shp")
shp = shp[order(shp$COD_PROV),]
shp$COD_RIP <- as.numeric(as.factor(shp$COD_PROV))
colnames(shp)[1] <- "id.space"

# convert shapefile with the INLA format about the neighboring structure.
W.nb <- poly2nb(shp)
nb2INLA("W.adj", W.nb) 



# Select data for the period 2015-2020
data = finaldb 

# Create indices
data$id.space <- as.numeric(as.factor(data$PROV)) 
data$id.time <- as.numeric(substr(data$EURO_LABEL, start = 7, stop = 8))
data$id.tmp <- inla.group(data$mean.temp, n = 100, method = "cut", idx.only = TRUE)
data$id.year <- data$year - 2014
data$id.wkes <- as.numeric(factor(data$EURO_LABEL))
data <- data[order(data$id.space),]

# define the formula for INLA
formula = 
  deaths ~ 1 + offset(log(population)) + hol + id.year + 
  f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) +
  f(id.wkes, model='iid', hyper=hyper.iid, constr = TRUE) + 
  f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE, scale.model = TRUE, cyclic = TRUE) +
  f(id.space, model='bym2', graph="W.adj", scale.model = TRUE, constr = TRUE, hyper = hyper.bym)

# PC priors
hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()


t_0 <- Sys.time()
for(j in 1:nrow(groups)){
  
  print(j)
  
  agegroup <- as.character(groups$age.group[j])
  sexg <- as.character(groups$sex[j])
  data %>% filter((ageg %in% agegroup) & (sex %in% sexg)) -> datCV_firslop

  truth <- datCV_firslop$deaths[datCV_firslop$year > 2019] 
  datCV_firslop$deaths[datCV_firslop$year > 2019] <- NA # we define as NAs the rows to predict
  
  
  in.mod = inla(formula,
                data=datCV_firslop,
                family="Poisson",  
                verbose = TRUE, 
                control.family=control.family,
                control.compute=list(config = TRUE), 
                control.mode=list(restart=T),
                num.threads = round(parallel::detectCores()*.8), 
                control.predictor = list(link = 1))
  
  res.list <- list(mod = in.mod, true_values = truth)
  saveRDS(res.list, file = paste0(path2save,
                                  paste0("res_", agegroup, "_", sexg)))
  
}

t_1 <- Sys.time()
print(t_1 - t_0) # ~30mins



# Retrieve Poisson samples from the models
res.list <- apply(groups, 1,
                  function(X) readRDS(paste0(path2save,
                                             paste0("res_", X[1], "_", X[2]))))

# with the loop bellow, we need to retrieve the linear predictor and sample from the posterior predictive, 
# which is the Poisson distribution. 
pois.samples.list <- list()
t_0 <- Sys.time()

for(j in 1:nrow(groups)){
  
  print(j)
  
  agegroup <- as.character(groups$age.group[j])
  sexg <- as.character(groups$sex[j])
  data %>% filter((ageg %in% agegroup) & (sex %in% sexg)) -> dat_tmp
  
  set.seed(11)
  post.samples <- inla.posterior.sample(n = 1000, result = res.list[[j]]$mod)
  predlist <- do.call(cbind,
                      lapply(post.samples,
                             function(X) exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  pois.samples <- apply(predlist, 2,
                        function(Z) rpois(n = length(Z), lambda = Z))
  pois.samples <- as.data.frame(pois.samples)
  
  pois.samples$EURO_LABEL <- dat_tmp$EURO_LABEL
  pois.samples$ID_space <- dat_tmp$PROV
  pois.samples$deaths <- dat_tmp$deaths 
  pois.samples$population <- dat_tmp$population 
  pois.samples$year <- dat_tmp$year
  pois.samples <- pois.samples[pois.samples$year %in% max(pois.samples$year),]
  
  pois.samples.list[[j]] <- pois.samples
  
}

t_1 <- Sys.time()
print(t_1 - t_0) # ~15mins

names(pois.samples.list) <- apply(as.data.frame(expand.grid(age.group = c("less40", "40_59", "60_69", "70_79", "80plus"),
                                                                sex = c("F", "M"))), 1, function(X) paste(X[2], X[1], sep = "_"))

saveRDS(pois.samples.list,
        file = paste0(path2save, "poisson_samples_all"))


# R code for manuscript
pois.samples.list$F_60_69 %>% 
  select(paste0("V", 1:10), EURO_LABEL, ID_space, year) %>% 
  head()

pois.samples.list$F_60_69 %>% 
  select(starts_with("V"), "ID_space") %>% 
  group_by(ID_space) %>% 
  summarise_all(sum) %>% 
  rowwise(ID_space) %>% 
  mutate(median = median(c_across(V1:V1000)), 
         LL = quantile(c_across(V1:V1000), probs= 0.025), 
         UL = quantile(c_across(V1:V1000), probs= 0.975)) %>% 
  select(ID_space, median, LL, UL)



######################################################################################
######################################################################################
######################################################################################
######################################################################################






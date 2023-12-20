# Case study (lines 6-339): This is the main case study in the paper (pp 8). You can access the most recent and reproducible version of the code here: https://www.kaggle.com/edsans/ssnbayes
# Simulations example (lines 360-738): It corresponds to the Appendix section pp 21. You can access the most recent and reproducible version of the code here:  https://www.kaggle.com/code/edsans/ssnbayes-simulated 
-------------------------------------------------------------------------------------------------------------------------------------------------


# Installation

if(!require('SSNbayes')) install.packages("SSNbayes", dependencies = T)
if(!require('SSNdata')) remotes::install_github("EdgarSantos-Fernandez/SSNdata")

#Loading the libraries we will use
library('Rcpp')
library('dplyr')
library('rstan')
library('StanHeaders')
library('SSN')
library('bayesplot')
library('lubridate')
library('viridis')
library('ggrepel')
library('RColorBrewer')
library('SSNbayes')
library('SSNdata')

rstan::rstan_options(auto_write = TRUE) # avoid recompilation
options(mc.cores = parallel::detectCores())

## Importing the spatial stream network (ssn) object
path <- system.file("extdata/clearwater.ssn", package = "SSNdata")
n <-  SSN2::ssn_import(path, predpts = "preds", overwrite  = TRUE)

## Import data.frame containing observations and covariates
clear <- readRDS(system.file("extdata/clear_obs.RDS", package = "SSNdata"))

## Create training and test datasets
seed <- '202103'
set.seed(seed)

ids <- clear[!is.na(clear$temp),]$pid
locs <- sample(ids, round(length(ids) * 0.30), replace = F )
locs <- sort(locs)

clear$y <- clear$temp_backup <- clear$temp # response variable
clear[clear$pid %in% locs,]$y <- NA
clear$dataset <- 'train'
clear$dataset[locs] <- 'test'

## Line plot test and train datasets, by location, over time
colores <- brewer.pal(3, 'Set1')[1:2]
ggplot(clear) +
  geom_line(aes(x = date, y = temp_backup, group = locID))+
  geom_point(aes(x = date, y = temp_backup, color = dataset), size = 3)+
  scale_x_date(breaks = unique(clear$date),
               labels = unique(clear$date))+
  scale_color_manual(values = colores)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(colour = "Dataset")+
  xlab('')+
  ylab("Temperature(°C)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.x = element_text(angle = 45, hjust=.5),
        strip.background =element_rect(fill='white'))

# Plotting the stream network
clear.df <- SSNbayes::collapse(n)
clear.df$addfunccol_cat <- cut(clear.df$afvArea,
                               breaks = seq(min(clear.df$afvArea),
                                            max(clear.df$afvArea),
                                            length.out=5),
                               labels = 1:4,
                               include.lowest = T)
col <- 'gray'

ggplot(clear.df) +
  geom_path(aes(X1, X2, group = slot, size = addfunccol_cat), lineend = 'round', linejoin = 'round', col = col)+
  geom_point(data = dplyr::filter(clear, clear$date == ymd('2012-08-01')) ,
             aes(x = UTM_Xcoord, y = UTM_Ycoord, col = temp_backup), size = 3)+
  geom_text_repel(data = dplyr::filter(clear, clear$date == ymd('2012-08-01')),
            aes(x = UTM_Xcoord, y = UTM_Ycoord, label = locID),size = 3)+
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  scale_color_viridis_c(option = 'C')+
  scale_shape_manual(values = c(16))+
  ylab("Northing") +
  xlab("Easting ")+
  coord_fixed()+
  theme_bw()+
  guides(size = 'none')+
  labs(size="",colour = "Temperature(°C)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.background =element_rect(fill='white'))

# creates the distance matrices

SSN2::ssn_create_distmat(n,
                         predpts = "preds" ,
                         overwrite = TRUE,
                         among_predpts  = TRUE)


# network id number

net_num <- unique(as.numeric(as.character(
  ssn_get_data(n)$netID),
  ssn_get_data(n, name = "preds")$netID))


# It takes several minutes (~10 mins) to fit this model.
# Please, be patient

# NB: If you want to skip fitting the model, you can import a stored version in the SSNdata package
# install_github("EdgarSantos-Fernandez/SSNdata")
# fit_ar <- readRDS(system.file("extdata//fit_ar.rds", package = "SSNdata"))


fit_ar <- ssnbayes(formula = y ~ SLOPE + elev + h2o_area + air_temp + sin + cos,
                   data = clear, # data frame with covariates and resp variable
                   path = path,  # path to the SSN object
                   time_method = list("ar", "date"), # temporal model to use
                   space_method = list('use_ssn', c("Exponential.taildown")), # spatial model to use
                   iter = 3000,
                   warmup = 1500,
                   chains = 3,
                   net = net_num, # second network on the ssn object
                   addfunccol='afvArea', #variable to obtain spatial weights
                   seed = seed)  #for reproducibility

saveRDS(fit_ar, 'fit_2023-11-24.RDS')

fit <- fit_ar
class(fit) <- c("stanfit") # changing the class, so we can use functions for stanfit objects

stats <- summary(fit)$summary %>%
                       data.frame()
stats <- data.frame(Variable = rownames(stats), stats)


## Create plots of chain results for seven reg coef
mcmc_dens_overlay(
  fit, # NB: if it fails use array(fit)
  pars = paste0("beta[",1:7,"]"),
  facet_args = list(nrow = 1))

## Plot the distribution of phi
mcmc_intervals(
  fit,
  pars = paste0("phi"),
  point_size = .1,
  prob_outer = 0.95
)


## Plot the nugget effect, partial sill and range
## parameter distributions for the tail-down model
mcmc_dens_overlay(
  fit,
  pars = c(
    "var_td",
    "alpha_td",
    "var_nug"),
  facet_args = list(nrow = 1)
)

## Format dataset before assessing predictive ability of the model
ypred <- data.frame(stats[grep("y\\[", row.names(stats)),])
ypred$ytrue <- clear$temp_backup
ypred$date <- rep(unique(clear$date), each = length(unique(clear$locID)) )
ypred$locID <- rep(1:length(unique(clear$locID)), times = length(unique(clear$date)))
ypred$dataset <- ifelse(ypred$sd == 0, 'obs', 'pred')
ypred$ypred <- ypred$mean
ypred$y <- clear$temp_backup
ypred$temp_pred_2.5 <- stats[grep('y',rownames(stats)),'X2.5.']
ypred$temp_pred_97.5 <- stats[grep('y',rownames(stats)),'X97.5.']
ypred$pid <- 1:nrow(ypred)


## Calculate the RMSE for the test dataset
rmse <- function(true, pred){ sqrt(mean( ( true - pred) ^ 2, na.rm= T) )}
(ypred %>% dplyr::filter(dataset == 'pred', pid %in% locs) %>%
    dplyr::summarise(rmse = rmse(ytrue , ypred)))

## Assing the prediction accuracy
## Plot the observed versus predicted temperature for the test dataset
filter(ypred, dataset == 'pred') %>% ggplot(.) +
  geom_point(aes(x = ytrue , y = ypred))+
  geom_errorbar(aes(x = ytrue, ymin = temp_pred_2.5, ymax = temp_pred_97.5))+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~locID, nrow = 3) +  coord_fixed() +
  xlab('True temperature (°C)') + ylab('Estimated temperature (°C)') +  theme_bw()+
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=13),
    legend.text=element_text(size=13),
    legend.title=element_text(size=13),
    axis.text.x = element_text(hjust=1, angle = 45), #angle = 45,
    strip.background =element_rect(fill='white'))+
  guides(size = 'none')+
  labs(size="", colour="Temp")

## Time series for 18 locations showing observations
## and predictions
cols <- brewer.pal(6, 'Paired'); cols <- c(cols[6], cols[2], cols[1], cols[1])
ggplot(ypred) +
  geom_ribbon(aes(x = date, ymin = temp_pred_2.5, ymax = temp_pred_97.5), fill = "grey70")+
  geom_point(aes(x = date, y = ypred, col = dataset),size = 2) +
  geom_line(aes(x = date, y = ypred)) +
  #geom_errorbar(aes(x = date, ymin = temp_pred_2.5, ymax = temp_pred_97.5, col = dataset), alpha = 0.4)+


  scale_color_manual(values = cols)+
  facet_wrap(~locID, nrow = 3)+
  theme_bw()+
  xlab('Date')+
  ylab('Temperature (°C)')+
  theme(
    legend.title = element_blank(),
    axis.text=element_text(size=13),
    axis.title=element_text(size=14),
    legend.text=element_text(size=14),
    #legend.title=element_text(size=13),
    axis.text.x = element_text(angle = 45, hjust=1), #
    strip.background =element_rect(fill='white'),
    strip.text.x = element_text(size = 12),
    panel.spacing.x = unit(.8, "lines"))+
  guides(size = 'none')+
  labs(size="", colour="Temp")


# Predictions
# It makes predictions using the fitted model

# reading the prediction dataset
clear_preds <- readRDS(system.file("extdata/clear_preds.RDS", package = "SSNdata"))
clear_preds$y <- NA
ys_all <- NULL
clear$temp_traintest <- clear$y
clear$y <- clear$temp_backup  # training temp values
# will impute the missing ones in the observation dataset
temps <- data.frame(stats[grep("y\\[", row.names(stats)),'mean'])
clear[is.na(clear$y),]$y <- temps[is.na(clear$y),1]

# producing predictions: it takes a few seconds
pred <- predict(object = fit_ar,
                path = path,
                obs_data = clear,
                pred_data = clear_preds,
                net = 2,
                nsamples = 100, # number of samples to use from the posterior in the stanfit object
                addfunccol = 'afvArea', # variable used for spatial weights
                locID_pred = locID_pred,
                chunk_size = 60)


ys <- reshape2::melt(pred, id.vars = c('locID0', 'locID', 'date'), value.name ='y')
ys$iter <- gsub("[^0-9.-]", "", ys$variable)
ys$variable <- NULL

# Computing the network exceedance probability
limit <- 13
ys$exc <- ifelse(ys$y > limit , 1, 0)

ys <- data.frame(ys) %>% dplyr::group_by(date, locID, locID0) %>%
  dplyr::summarise(sd = sd(y, na.rm=T),
                   y_pred = mean(y, na.rm=T),
                   prop = mean(exc, na.rm=T))
ys <- dplyr::arrange(ys, locID)
clear_preds2 <- clear_preds %>% left_join(ys, by = c('locID', 'date' ))

# plotting the predictions and observations
colores <- brewer.pal(4, 'Set1')[1:4]
ggplot(clear) +
  geom_line(aes(x = date, y = temp_backup, group = locID), color = colores[1])+
  geom_line(data= clear_preds2, aes( x = date, y = y_pred, group = locID), color = colores[2], alpha = 0.3)+
  scale_x_date(breaks = unique(clear$date),
               labels = unique(clear$date))+
  scale_color_manual(values = colores)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(colour = "Dataset")+
  xlab('')+
  ylab("Temperature(°C)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.x = element_text(angle = 90, hjust=.5),
        strip.background =element_rect(fill='white'))

# plotting the predictions on the stream network
ggplot(clear.df) +
  geom_path(aes(X1, X2, group = slot, size = addfunccol_cat), lineend = 'round', linejoin = 'round', col = col)+
  geom_point(data = dplyr::filter(clear, clear$date == ymd('2012-08-01')) ,
             aes(x = UTM_Xcoord, y = UTM_Ycoord, col = temp_backup),shape = 16, size = 2)+
  geom_point(data = dplyr::filter(clear_preds2, date == ymd('2012-08-01')) ,
             aes(x = NEAR_X, y = NEAR_Y, col = y_pred), shape = 18,size = 2.5)+
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  scale_color_viridis(option = 'C')+
  scale_shape_manual(values = c(16,18))+
  ylab("Northing") +
  xlab("Easting ")+
  coord_fixed()+
  theme_bw()+
  guides(size = 'none')+
  labs(colour = "Temperature(°C)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.background =element_rect(fill='white'))


# Plotting the network exceedance probability
dates_August <- ymd("2012-08-01", "2013-08-01")

ggplot(clear.df) +
  geom_path(aes(X1, X2, group = slot, size = addfunccol_cat), lineend = 'round', linejoin = 'round', col = col)+
  geom_point(data = dplyr::filter(clear_preds2, date %in% dates_August) ,
             aes(x = NEAR_X, y = NEAR_Y, col = prop), size = 2)+
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  facet_wrap(~date, ncol = 2)+
  scale_color_viridis(option = 'C')+
  scale_shape_manual(values = c(200))+
  xlab("x-coordinate") +
  ylab("y-coordinate")+
  coord_fixed()+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        strip.text.x = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.background =element_rect(fill='white'))+
  guides(size = 'none')+
  labs(size="", colour="exceedance")

# End of case study














## Load the packages. Note that these packages can be installed
## using the install.packages() function
library('Rcpp')
library('ggplot2')
library('dplyr')
library('RColorBrewer')
library('rstan')
library('bayesplot')
library('nlme')
library('SSN') # Note: This package has been archived.
library('viridis')
library('sf')

# it requires the old version of SSN to simulate the dataset. Note that it also requires archived packages: maptools, rgdal and rgeos

if(!require('SSNbayes')) install.packages("SSNbayes", dependencies = T)
library('SSNbayes')

## Set some useful options for modelling
rstan_options(auto_write = TRUE) # avoid recompilation
options(mc.cores = parallel::detectCores())
RNGkind(sample.kind = "Rounding")

## Set the seed for reproducibility
seed <- 202008
set.seed(seed)
path <- "./sim.ssn"

## If it does not already exist, create a SpatialStreamNetworkObject
## with 150 stream segments (edges). Use a systematic design to generate
## observed and prediction locations on the network spaced
## approximately 3 and 0.3 units apart, respectively.
if(file.exists(path)){
  ssn <- importSSN(path, "preds")
}  else{ssn <- createSSN(n = c(150),  # 150 edges
                         obsDesign = systematicDesign(spacing=3),
                         predDesign = systematicDesign(spacing=0.3),
                         importToR = TRUE,
                         path = path, # path where the .ssn object is saved
                         treeFunction = iterativeTreeLayout)}

## Plot the edges and observed locations in the SpatialStreamNetwork object
plot(ssn, lwdLineCol = "addfunccol",  lwdLineEx = 8,
     lineCol = 4,  col = 1,  pch = 16,  xlab = "x-coordinate",  ylab = "y-coordinate")
## Add the prediction locations
plot(ssn, PredPointsID = "preds", add = T, pch = 16, col = "#E41A1C")

## Create stream distance matrices
createDistMat(ssn, predpts = 'preds', o.write=TRUE, amongpreds = T)


## Extract the data.frames for the observed and prediction location data
rawDFobs <- getSSNdata.frame(ssn, Name = "Obs")
rawDFpred <- getSSNdata.frame(ssn, Name = "preds")

## Extract the geographic coordinates from the SpatialStreamNetwork
## object and add to data.frames
obs_data_coord <- data.frame(ssn@obspoints@SSNPoints[[1]]@point.coords)
obs_data_coord$pid<- as.numeric(rownames(obs_data_coord))
rawDFobs<- rawDFobs %>% left_join(obs_data_coord, by = c("pid"),
                                  keep = FALSE)
rawDFobs$point <- "Obs"

pred_data_coord <- data.frame(ssn@predpoints@SSNPoints[[1]]@point.coords)
pred_data_coord$pid<- as.numeric(rownames(pred_data_coord))
rawDFpred<- rawDFpred %>% left_join(pred_data_coord, by = "pid",
                                    keep = FALSE)
rawDFpred$point <- "pred"

## Generate 3 continous covariates at observed and prediction
## locations
set.seed(seed)
rawDFpred[,"X1"] <- rnorm(length(rawDFpred[,1]))
rawDFpred[,"X2"] <- rnorm(length(rawDFpred[,1]))
rawDFpred[,"X3"] <- rnorm(length(rawDFpred[,1]))

rawDFobs[,"X1"] <- rnorm(length(rawDFobs[,1]))
rawDFobs[,"X2"] <- rnorm(length(rawDFobs[,1]))
rawDFobs[,"X3"] <- rnorm(length(rawDFobs[,1]))

## Ensure that the rownames still match the pid values used in the
## SpatialStreamNetwork object
rownames(rawDFobs)<- as.character(rawDFobs$pid)
rownames(rawDFpred)<- as.character(rawDFpred$pid)

## Put the modified data.frames back in the SpatialStreamNetwork
## object
ssn <- putSSNdata.frame(rawDFobs,ssn, Name = 'Obs')
ssn <- putSSNdata.frame(rawDFpred, ssn , Name = 'preds')

## Simulate the response variable at observed and prediction locations
set.seed(seed)
sim.out <- SimulateOnSSN(ssn.object = ssn,
                         ObsSimDF = rawDFobs, ## observed data.frame
                         PredSimDF = rawDFpred, ## prediction data.frame
                         PredID = "preds", ## name of prediction dataset
                         formula = ~ X1 + X2 + X3,
                         coefficients = c(10, 1, 0, -1), ## regression coefficients
                         CorModels = c("Exponential.taildown"), ## covariance model
                         use.nugget = TRUE, ## include nugget effect
                         CorParms = c(3, 10, .1)) ## covariance parameters

## Extract the SpatialStreamNetwork object from the list returned by
## SimulateOnSSN and extract the observed and prediction site
## data.frames. Notice the new column Sim_Values in the data.frames
sim.ssn <- sim.out$ssn.object
simDFobs <- getSSNdata.frame(sim.ssn,"Obs")
simDFpreds <- getSSNdata.frame(sim.ssn, "preds")
summary(simDFobs)

## Fit a spatial stream network model using the Exponential tail-down function
glmssn.out <- glmssn(Sim_Values ~ X1 + X2 + X3, sim.ssn,
                     CorModels = "Exponential.taildown")
summary(glmssn.out)


## Create a data.frame containing training and test data.
df_obs <- getSSNdata.frame(sim.ssn, "Obs") ## Extract observed dataset
df_obs$dataset <- 'train' ## Create new column 'dataset' and set to 'train'
df_pred <- getSSNdata.frame(sim.ssn, "preds") ## Extract prediction dataset
df_pred$dataset <- 'test' ## Create new column 'dataset' and set to 'test'

## Expand data.frames to include 10 days per location
t <- 10 # days
df_obs <- do.call("rbind", replicate(t, df_obs, simplify = FALSE))# replicating the df
df_obs$date <- rep(1:t, each = (nrow(df_obs)/t))

df_pred <- do.call("rbind", replicate(t, df_pred, simplify = FALSE))# replicating the df
df_pred$date <- rep(1:t, each = (nrow(df_pred)/t))

## Create a copy of the pid value used in the SpatialStreamNetwork
## object and create a new pid value for use in the SSNbayes
## package. Values must be consequtively ordered from 1 to the number
## of rows in the data.frame
df_obs <- df_obs %>% mutate(pid.ssn = pid,
                            pid = rep(1:nrow(.)))
df_pred <- df_pred %>% mutate(pid.ssn = pid,
                            pid = rep(1:nrow(.)))

## Combine the training and testing datasets
df <- rbind(df_obs, df_pred)
df$dataset <- factor(df$dataset, levels = c('train', 'test'))


## Construct and initialize an autocorrelation structure of order 1
set.seed(seed)
phi <- 0.8 ## lag 1 autocorrelation value
ar1 <- corAR1(form = ~ unique(df$date), value = phi) # can also use corExp function
AR1 <- Initialize(ar1, data = data.frame(unique(df$date)))

## Create a vector of AR1 errors for each date and expand to all all locations
epsilon <- t(chol(corMatrix(AR1))) %*% rnorm(length(unique(df$date)), 0, 3)   #NB AR1 error
epsilon <- rep(epsilon, each = length(unique(df$locID)) ) +
  rnorm(length(epsilon)*length(unique(df$locID)), 0, 0.25) # for all the locations
epsilon_df <- data.frame(date = rep(unique(df$date), each = length(unique(df$locID))),
                         locID = rep(unique(df$locID), times = length(unique(df$date))),
                         epsilon = epsilon)
df <- df %>% left_join(epsilon_df, by = c('date' = 'date', 'locID' = 'locID'))


## Create a new simulated response variable, y, with error
df$y <- df$Sim_Values + df$epsilon

## Create line plots of the response over time for training and test datasets
ggplot(df) +
  geom_line(aes(x = date, y = y, group = locID, col = dataset), alpha = 0.4) +
  ylab("Simulated Temperature (\u00B0C)")+
  facet_wrap(~dataset)+
  theme_bw()

## Split the training and testing datasets. Ensure that date is numeric.
df <- df %>% dplyr::select(locID, pid, date, y, everything())
obs_data <- df[df$dataset == 'train',]
pred_data <- df[df$dataset == 'test',]

## Randomly select observations by date
set.seed(seed)
points <- length(unique(obs_data$pid))
locs <- obs_data %>% dplyr::group_by(date) %>%
   pull(pid) %>%
  sample(., round(points * 0.3), replace = F)  %>% sort()

## Create a backup for the response before setting randomly selected
## measurements to NA
obs_data$y_backup <- obs_data$y
obs_data[obs_data$pid %in% locs,]$y <- NA

## Extract stream (edge) network structure, including the additive function value
nets <- SSNbayes::collapse(SSN_to_SSN2(ssn), par = 'addfunccol')

## Create additive function value categories for plotting
nets$afv_cat <- cut(nets$addfunccol,
                                 breaks = seq(min(nets$addfunccol),
                                              max(nets$addfunccol),
                                              length.out=6),
                                 labels = 1:5,
                                 include.lowest = T)

## Plot simulated temperature, by date, with line width proportional to afv_cat
ggplot(nets) +
    geom_path(aes(X1, X2, group = slot, size = afv_cat), lineend = 'round',
              linejoin = 'round', col = 'lightblue')+
    geom_point(data = dplyr::filter(obs_data, date %in% 1:10),
               aes(x = coords.x1, y = coords.x2, col = y, shape = point),
               size = 1)+
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  facet_wrap(~date, nrow = 2)+
  scale_color_viridis(option = 'C')+
  scale_shape_manual(values = c(16,15))+
  xlab("x-coordinate") +
  ylab("y-coordinate")+
  theme_bw()

## Fit a Bayesian space-time model
start.time <- Sys.time()

fit_td <- ssnbayes(formula = y ~ X1 + X2 + X3,
                   data = obs_data,
                   path = path, ## path to the .ssn object
                   time_method = list("ar", "date"), # temporal model to use
                   space_method = list('use_ssn', c("Exponential.taildown")), # spatial model to use
                   iter = 4000, # number of MCMC iterations
                   warmup = 2000, #warm up samples
                   chains = 3, # number of chains
                   addfunccol = 'addfunccol', # additive function column
                   loglik = T) # compute the log likelihood

end.time <- Sys.time()
end.time - start.time

## Create a copy of the model fit and set class so that we can take
## advantage of plotting functions for stanfit objects
fits <- fit_td
class(fits) <- c("stanfit")

## Extract summaries of the posterior distributions for the parameter
## estimates and the predictions.
stats_td <- summary(fits)
stats_td <- stats_td$summary


## Create plots of the posterior distribution of the 3 regression
## coefficients
mcmc_dens_overlay(
  fits, #
  pars = paste0("beta[",1:3,"]"),
  facet_args = list(nrow = 1))

## Plot the posterior distribution of phi
mcmc_intervals(
  fits,
  pars = paste0("phi"),
  point_size = .1,
  prob_outer = 0.95
)

## Plot the posterior distribution for the nugget effect, partial sill.
## and range parameters in the tail-down model
mcmc_dens_overlay(
  fits,
  pars = c(
    "var_td",
    "alpha_td",
    "var_nug"),
  facet_args = list(nrow = 1)
)

## Create a data.frame containing summaries of the posterior predictive
## distributions and the true values
ypred <- data.frame(stats_td[grep("y\\[", row.names(stats_td)),])
ypred$ytrue <- obs_data$y_backup #
ypred$date <- rep(1:t, each = nrow(obs_data)/t)
ypred$dataset <- ifelse(ypred$sd == 0, 'obs', 'pred')
ypred$td_exp <- ypred$mean


## Create a plot of the predicted versus true values with 95% highest
## density interval
 filter(ypred, dataset == 'pred') %>% ggplot() +
     geom_errorbar(data = ypred, aes(x=ytrue, ymin=X97.5., ymax=X2.5.),
                   col = 2, width=0.5, size=0.5, alpha = 0.75) +
  geom_point(aes(x = ytrue , y = td_exp), col = 2)+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~date, nrow = 2) +  coord_fixed() +
     xlab('y true') + ylab('y estimated') +  theme_bw()

## Calculate the root mean square error (RMSE) between the true and
## predicted values
rmse <- sqrt(mean(((ypred$ytrue) - ypred$td_exp)^2))
rmse

# Calculate the 95% prediction coverage. Ideally, this should be close to 0.95.
ypred$cov <- ifelse(ypred$ytrue > ypred$X2.5. & ypred$ytrue<ypred$X97.5,1,0)
filter(ypred, dataset == 'pred') %>%
  group_by(dataset) %>%
  dplyr::summarize(mean(cov))

## Making predictions at a new set of prediction locations

## Set the seed for reproducibility
set.seed(seed)

## Extract the location IDs for prediction locations
locID_pred <- sort(unique(pred_data$locID))

## Define a new column containing the response variable used in
## fit_td, including missing values
obs_data$y_traintest <- obs_data$y

## Replace y with the original response variable, containing no
## missing values
obs_data$y <- obs_data$y_backup

## Produce predictions: This takes approximately 8 minutes
pred <- predict(object = fit_td, ## fitted model
                path = path, #  path to .ssn object
                obs_data = obs_data, # observed data.frame
                pred_data = pred_data, # prediction data.frame
                net = 1, # network identifier (optional)
                nsamples = 100, # number of samples to use from the posterior in the stanfit object
                addfunccol = 'addfunccol', # variable used for spatial weights
                locID_pred = locID_pred, # location identifier for predictions
                chunk_size = 60) # split the predictions into subsets of this size

## Add the pid and pid.ssn to the prediction data.frame
pred <- left_join(pred, select(pred_data, locID, date),
                 by = c("locID", "date"), keep = FALSE)

## Convert the prediction data.frame from wide to long format
ys <- reshape2::melt(pred, id.vars = c('locID0',
                                       'locID', 'date'), value.name ='y')

## Create variable representing the iteration number and set variable
## column to NULL
ys$iter <- gsub("[^0-9.-]", "", ys$variable)
ys$variable <- NULL


## Computing the exceedance probabilities

## Create an exceedance indicator based on a threshold (i.e. limit)
limit <- 13
ys$exc <- ifelse(ys$y > limit , 1, 0) ## 1== TRUE, 0== FALSE

## Calculate summary statistics for the predictions, by locID and date
## and join to prediction data.frame
ys <- data.frame(ys) %>% dplyr::group_by(date, locID) %>%
  dplyr::summarise(sd = sd(y, na.rm=T), ## prediction standard deviation
                   y_pred = mean(y, na.rm=T), ## mean temperature prediction
                   prop = mean(exc, na.rm=T)) ## exceedance probability

ys <- dplyr::arrange(ys, locID)
pred_data <- pred_data %>% left_join(ys, by = c('locID', 'date'), keep = FALSE)

## Plot the exceedance probabilities for the prediction sites on 10 dates
ggplot(nets) +
  geom_path(aes(X1, X2, group = slot, size = afv_cat), lineend = 'round',
              linejoin = 'round', col = 'gray')+
  geom_point(data = dplyr::filter(pred_data) ,
             aes(x = coords.x1, y = coords.x2, col = prop), size = 1)+
  scale_size_manual(values = seq(0.2,2,length.out = 5))+
  facet_wrap(~date, nrow = 2)+
  scale_color_viridis(option = 'C')+
  scale_shape_manual(values = c(200))+
  xlab("x-coordinate") +
  ylab("y-coordinate")+
  coord_fixed()+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13),
        strip.text.x = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust=1),
        strip.background =element_rect(fill='white'))+
  guides(size = 'none')+
    labs(size="", colour="exceedance")

# End of simulations
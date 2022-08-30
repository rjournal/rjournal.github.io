#* Lumped synthetic catchment----
library(HBV.IANIGLA)

data("lumped_hbv")

snow_module <-
  SnowGlacier_HBV(model = 1, 
                  inputData = as.matrix( lumped_hbv[ , c('T(ºC)', 'P(mm/d)')] ),
                  initCond = c(20, 2), 
                  param = c(1.20, 1.00, 0.00, 2.5) )

soil_module <-
  Soil_HBV(model = 1,
           inputData = cbind(snow_module[ , "Total"], lumped_hbv$`PET(mm/d)`),
           initCond = c(100, 1),
           param = c(200, 0.8, 1.15) )

routing_module <-
  Routing_HBV(model = 1, 
              lake = F, 
              inputData = as.matrix(soil_module[ , "Rech"]),
              initCond = c(0, 0, 0), 
              param = c(0.9, 0.01, 0.001, 0.5, 0.01) )

tf_module <-
  round( 
    UH(model = 1,
       Qg = routing_module[ , "Qg"],
       param = c(1.5) ),
    2)

# plot the "true" and simulated hydrographs
library(ggplot2)

gg_1 <-
  ggplot(data = data.frame(date = lumped_hbv[ , "Date"],
                           qsim = tf_module, qobs = lumped_hbv[ , "qout(mm/d)"]),
         aes(x = date)) +
  geom_line(aes(y = qsim), col = 'dodgerblue') +
  geom_line(aes(y = qobs), col = 'red') +
  xlab(label = '') + ylab(label = 'q(mm/d)') +
  theme_minimal() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y' ) +
  scale_y_continuous(breaks = seq(0, 15, 2.5)) +
  theme(
    title = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 90) )

gg_1


#* Semi-distributed glacier mass balance----
library(HBV.IANIGLA)

data("alerce_data")

# functions

## agruments description
  # topograpphy: data frame with the elevation bands (gl_topo in this script).
  # meteo: data frame with dates, air temperature and precipitation series.
  # z_topo: numeric vector with air temperature and precipitation gauge elevation.
  # param_tair: numeric vector with air temperature module parameters.
  # param_precip: numeric vector with precipitation module parameters.
  # param_ice: numeric vector with the glacier module parameters.
  # init_ice: numeric value with initial snow water equivalent. Default value being 00 mm.
## output
  # data frame with two columns: the date and the daily mass balance.
glacier_hbv <- function(topography,
                        meteo,
                        z_topo,
                        param_tair,
                        param_precip,
                        param_ice,
                        init_ice = 0){
  
  n_it    <- nrow(topography) # to get the number of elevation bands
  n_dates <- nrow(meteo) # to get number of rows
  
  precip_model  <- tair_model <- matrix(NA_real_, nrow = n_dates, ncol = n_it)
  glacier_model <- list()
  
  for(i in 1:n_it){
    # we first distribute the meteo forcing among the elevation bands
    precip_model[ , i] <- Precip_model(model = 1,
                                       inputData = meteo[ , "P(mm/d)"],
                                       zmeteo = z_topo[2],
                                       ztopo = topography[i , "mean"],
                                       param = param_precip)
    
    tair_model[ , i] <- Temp_model(model = 1,
                                   inputData = meteo[ , "Tair(ºC)"],
                                   zmeteo = z_topo[1],
                                   ztopo = topography[i , "mean"],
                                   param = param_tair)
    
    # now we use the extrapolated values in the glacier module
    glacier_model[[ i ]] <-
      SnowGlacier_HBV(model = 1,
                      inputData = cbind( tair_model[ , i], precip_model[ , i]),
                      initCond = c(init_ice, 1, topography[i, "area_rel"]),
                      param = param_ice )
  }
  
  # we aggregate the mass balance series for the whole glacier
  cum_mb <- lapply(X = 1:n_it, FUN = function(x){
    out <- glacier_model[[x]][ , 7] * topography[x, "area_rel"]
  })
  cum_mb <- Reduce(f = `+`, x = cum_mb)
  
  # return the column Cum = Psnow - Mtot (aggregated at glacier scale)
  cum_out <- data.frame(date = meteo[ , "Date"],
                        "cum_mb(mm)" = cum_mb,
                        check.names = FALSE)
  
  return(cum_out)
}

## arguments
  # x: data frame resulting from glacier_hbv model
  # start_date: string vector with starting dates.
  # end_date: string vector with ending dates.
## output
  # data frame with the annual mass balances

agg_mb <- function(x, start_date, end_date){
  n_it <- length(start_date)
  
  annual_mb <- c()
  for(i in 1:n_it){
    flag_start <- which(x[ , 1] == start_date[i])
    flag_end   <- which(x[ , 1] == end_date[i])
    
    annual_mb[i] <- sum( x[flag_start:flag_end, 2] )
    
  }
  
  # build output data frame
  out <- data.frame(start = start_date,
                    end = end_date,
                    "mb(mm we)" = annual_mb,
                    check.names = FALSE)
  
  return(out)
  
}

## arguments
  # obs_upp: numeric vector with the acceptable upper limit
  # obs_lwr: numeric vector with the acceptable lower limit
  # sim: numeric vector with simulated mass balance
## output
  # numeric value with the number of times that the simulation fits in between 
  # the lower and upper limits.


my_gof <- function(obs_upp, obs_lwr, sim){
  n_it <- length(sim)
  out  <- 0
  
  for(i in 1:n_it){
    
    if(sim[i] >= obs_lwr[i] & sim[i] <= obs_upp[i]){
      out <- 1 + out
      
    } else {
      out <- 0 + out
      
    }
    
  }
  
  return(out)
  
}



# now extract 
meteo_data   <- alerce_data$meteo_data   # meteorological forcing series
mass_balance <- alerce_data$mass_balance # annual glacier mass balances
mb_dates     <- alerce_data$mb_dates     # fix seasonal dates
gl_topo      <- alerce_data$topography   # elevation bands

z_tair   <- alerce_data$station_height[1] # topographic elevation air temp.
z_precip <- alerce_data$station_height[2] # topographic elevation of precipitation gauge 

# air temperature model
tair_range <- rbind(
  t_grad  = c(-9.8, -2)
)

# precip model
precip_range <- rbind(
  p_grad  = c(5, 25)
)

# glacier module
glacier_range <- rbind(
  sfcf = c(1, 2),
  tr   = c(0, 3),
  tt   = c(0, 3),
  fm   = c(1, 4),
  fi   = c(4, 8)
)

## we aggregate them in a matrix
param_range <-
  rbind(
    tair_range,
    precip_range,
    glacier_range
  )

# set the number of model runs that you want to try
n_run <- 10000

# build the matrix
n_it <- nrow(param_range)

param_sets <- matrix(NA_real_, nrow = n_run, ncol = n_it)

colnames(param_sets) <- rownames(param_range)

set.seed(123) # just for reproducibility 
for(i in 1:n_it){
  
  param_sets[ , i] <- runif(n = n_run,
                            min = param_range[i, 1],
                            max = param_range[i, 2]
  )
  
}

# goodness of fit vector
gof <- c()

# make a loop
for(i in 1:n_run){
  
  # run the model
  glacier_sim <- glacier_hbv(topography = gl_topo,
                             meteo = meteo_data,
                             z_topo = c(z_tair, z_precip),
                             param_tair = param_sets[i, rownames(tair_range)],
                             param_precip = param_sets[i, rownames(precip_range) ],
                             param_ice = param_sets[i, rownames(glacier_range)] )
  
  # aggregate the simulation
  annual_mb <- agg_mb(x = glacier_sim,
                      start_date = as.Date( mb_dates$winter[-4] ),
                      end_date = as.Date( mb_dates$winter[-1] ) - 1 )
  
  # compare the simulations with measurements
  gof[i] <- my_gof(obs_upp = mass_balance$upp,
                   obs_lwr = mass_balance$lwr,
                   sim = annual_mb[ , 3])
  
  rm(glacier_sim, annual_mb)
}

param_sets <- cbind(param_sets, gof)

# we apply a filter
param_subset <- subset(x = param_sets, subset = gof == 3)

# now we run the model again to get our simulations
n_it <- nrow(param_subset)

mb_sim <- matrix(NA_real_, nrow = 3, ncol = n_it)

for(i in 1:n_it){
  glacier_sim <- glacier_hbv(topography = gl_topo,
                             meteo = meteo_data,
                             z_topo = c(z_tair, z_precip),
                             param_tair = param_subset[i, rownames(tair_range)],
                             param_precip = param_subset[i, rownames(precip_range) ],
                             param_ice = param_subset[i, rownames(glacier_range)] )
  
  annual_mb <- agg_mb(x = glacier_sim,
                      start_date = as.Date( mb_dates$winter[-4] ),
                      end_date = as.Date( mb_dates$winter[-1] ) - 1 )
  
  mb_sim[ , i] <- annual_mb[ , 3]
  
  rm(i, glacier_sim, annual_mb)
  
}

# now we are going to make a data frame with the mean surface mass balance simulation
mean_sim <- cbind( mass_balance,
                   "mb_sim" =  rowMeans(mb_sim)    )

# make the plot
library(ggplot2)
g1 <-
  ggplot(data =  mean_sim, aes(x = year)) +
  geom_pointrange(aes(y = `mb(mm we)`, ymin = `lwr`, color = 'obs',
                      ymax = `upp` ), size = 1,  fill = "white", shape = 21) +
  geom_point(aes(y = `mb_sim`, fill = 'sim'), shape = 23,
             size = 3) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-1500, 500), breaks = seq(-1500, 500, 250) ) +
  scale_color_manual(name = '', values = c('obs' = 'blue') ) +
  scale_fill_manual(name = '', values = c('sim' = 'red') ) +
  ggtitle('') +
  xlab('') + ylab('mb (mm we)') +
  theme_minimal() +
  theme(
    title = element_text(color = "black", size = 12, face = "bold"),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text = element_text(size = 11))

g1


#* Benchmark computation----
library(HBV.IANIGLA)
library(microbenchmark)

data("alerce_data")

# now extract 
meteo_data   <- alerce_data$meteo_data   # meteorological forcing series
mass_balance <- alerce_data$mass_balance # annual glacier mass balances
mb_dates     <- alerce_data$mb_dates     # fix seasonal dates
gl_topo      <- alerce_data$topography   # elevation bands

z_tair   <- alerce_data$station_height[1] # topographic elevation air temp.
z_precip <- alerce_data$station_height[2] # topographic elevation of precipitation gauge 

# model function
## agruments description
# topograpphy: data frame with the elevation bands (gl_topo in this script).
# meteo: data frame with dates, air temperature and precipitation series.
# z_topo: numeric vector with air temperature and precipitation gauge elevation.
# param_tair: numeric vector with air temperature module parameters.
# param_precip: numeric vector with precipitation module parameters.
# param_ice: numeric vector with the glacier module parameters.
# init_ice: numeric value with initial snow water equivalent. Default value being 00 mm.
## output
# data frame with two columns: the date and the daily mass balance.

glacier_hbv <- function(topography,
                        meteo,
                        z_topo,
                        param_tair,
                        param_precip,
                        param_ice,
                        init_ice = 0){
  
  n_it    <- nrow(topography) # to get the number of elevation bands
  n_dates <- nrow(meteo) # to get number of rows
  
  precip_model  <- tair_model <- matrix(NA_real_, nrow = n_dates, ncol = n_it)
  glacier_model <- list()
  
  for(i in 1:n_it){
    # we first distribute the meteo forcing among the elevation bands
    precip_model[ , i] <- Precip_model(model = 1,
                                       inputData = meteo[ , "P(mm/d)"],
                                       zmeteo = z_topo[2],
                                       ztopo = topography[i , "mean"],
                                       param = param_precip)
    
    tair_model[ , i] <- Temp_model(model = 1,
                                   inputData = meteo[ , "Tair(ºC)"],
                                   zmeteo = z_topo[1],
                                   ztopo = topography[i , "mean"],
                                   param = param_tair)
    
    # now we use the extrapolated values in the glacier module
    glacier_model[[ i ]] <-
      SnowGlacier_HBV(model = 1,
                      inputData = cbind( tair_model[ , i], precip_model[ , i]),
                      initCond = c(init_ice, 1, topography[i, "area_rel"]),
                      param = param_ice )
  }
  
  # we aggregate the mass balance series for the whole glacier
  cum_mb <- lapply(X = 1:n_it, FUN = function(x){
    out <- glacier_model[[x]][ , 7] * topography[x, "area_rel"]
  })
  cum_mb <- Reduce(f = `+`, x = cum_mb)
  
  # return the column Cum = Psnow - Mtot (aggregated at glacier scale)
  cum_out <- data.frame(date = meteo[ , "Date"],
                        "cum_mb(mm)" = cum_mb,
                        check.names = FALSE)
  
  return(cum_out)
}

# now microbenchmark
microbenchmark(
 glacier_hbv(topography = gl_topo,
             meteo = meteo_data,
             z_topo = c(z_tair, z_precip),
             param_tair = -9.8,
             param_precip = 15,
             param_ice = c(1.2, 0, 1, 2.5, 6) ), 
 times = 1000)

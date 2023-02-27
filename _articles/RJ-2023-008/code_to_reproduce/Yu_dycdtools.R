#---
# This script is associated with the R Journal paper
# "dycdtools: an R Package for Assisting Calibration and Visualising Outputs of an Aquatic Ecosystem Model".
#
# This script reproduces the figures of the model outputs (Figure 4-6) in the paper.
#
# Author: Songyan Yu
# Date created: 03/07/2020
# Updated: 25/05/2022
#---

# Install the "dycdtools" package from CRAN
install.packages("dycdtools")  # the latest version is 0.4.2

# or install from the GitHub repo
devtools::install_github("SongyanYu/dycdtools")
unlink("dycdtools.zip")

library(dycdtools)

# run model on different combinations of parameter values
lake.temp <- read.csv("calibration_data/Obs_data_template.csv")
lake.temp$Date <- as.Date(lake.temp$Date, format = '%Y-%m-%d')

# the current setting of calib_assist will take 1 hour to run on 7 cores.
# for demo purpose only, you may want to make the following changes to shorten the running time:
# try changing 'combination' to 'random', and add a new argument 'n = 3'.
calib_assist(cal.para = "calibration_data/Calibration_parameters.csv",
             combination = "all",
             model.var = c("TEMP"),
             obs.data = lake.temp,
             objective.function = c("NSE"),
             start.date = "2002-01-23",
             end.date = "2016-12-31",
             dycd.wd = "calibration_data/DYRESM_CAEDYM_Lake-Okareka/",
             dycd.output = "calibration_data/DYRESM_CAEDYM_Lake-Okareka/DYsim.nc",
             file.name = "calibration_data/Calibration_outputs.csv",
             write.out = TRUE,
             parallel = TRUE,
             verbose = TRUE)


#---
# 1. read in model calibration outputs
#---

calibration <- read.csv("calibration_data/Calibration_outputs.csv")

#---
# 2. heap map
#---
library(ggplot2)
ggplot(calibration, aes(x = wse, y = vmc, fill = NSE.TEMP))  +
  geom_tile() +
  scale_fill_distiller(palette = "PuBu",direction = 1) +
  facet_grid(~lec, scale = 'free') +
  xlab("Wind stirring efficiency") +
  ylab("Vertical mixing coefficient") +
  labs(title = "Light extinction coefficient", fill = "NSE") +
  theme_bw() +
  theme(plot.title = element_text(size=11, hjust = 0.5))

ggsave(filename = 'Figure_03.png', width = 8, height = 4)


# The model output from calibrated DYRESM-CAEDYM is provided in the "plotting_data" folder.
# Extract temperature simulations from the model outputs.

var.values <- ext_output(dycd.output = "plotting_data/DYsim.nc",
                         var.extract = c('TEMP', 'DO'),
                         verbose = TRUE)

# Interpolate the temperature simulations across entire water column at an interval of 0.5m
temp.interpolated <- interpol(layerHeights = var.values$dyresmLAYER_HTS_Var,
                              var = var.values$dyresmTEMPTURE_Var,
                              min.dept = 0,
                              max.dept = 33,
                              by.value = 0.5)

# Read in observed water quality data
obs.okareka <- read.csv("plotting_data/Obs_data_template.csv")
obs.okareka$Date <- as.Date(obs.okareka$Date,format="%d/%m/%Y")
obs.temp <- obs.okareka[,c('Date','Depth','TEMP')]  # subset observed data to remain temperature observations

# calculate five model performance metrics
objective_fun(sim = temp.interpolated[2:4,],
              obs = obs.temp[obs.temp$Depth == 1, ],
              fun = c('NSE', 'RMSE', 'MAE', 'Pearson', 'PAE'),
              start.date = '2002-01-23',
              end.date = '2016-12-31',
              min.depth = 0.5,
              max.depth = 1.5,
              by.value = 0.5)

#---
# contour plot - Figure 4 in the M/S
#---
png(filename = 'Figure_04.png', width = 1200, height = 700)

plot_cont_comp(sim = temp.interpolated,
               obs = obs.temp,
               sim.start = "2002-01-23",
               sim.end = "2016-12-31",
               plot.start = "2002-01-23",
               plot.end = "2006-12-31",
               legend.title = "T\n(\u00B0C)",
               min.depth = 0,
               max.depth = 33,
               by.value = 0.5,
               nlevels = 20)

dev.off()

#---
# profile plot - Figure 5 in the M/S
#---
plot_prof(sim = temp.interpolated,
          obs = obs.temp,
          sim.start = "2002-01-23",
          sim.end = "2016-12-31",
          plot.start = "2002-01-23",
          plot.end = "2002-12-31",
          min.depth = 0,
          max.depth = 33,
          by.value = 0.5,
          xlabel = "Temperature \u00B0C")

ggsave(filename = "Figrue_05.png", height = 4, width = 7)

#---
# time serise plot - Figure 6 in the M/S
#---
p <- plot_ts(sim = temp.interpolated,
        obs = obs.temp,
        target.depth = c(1, 14, 30),
        sim.start = "2002-01-23",
        sim.end = "2016-12-31",
        plot.start = "2002-01-23",
        plot.end = "2012-12-31",
        min.depth = 0,
        max.depth = 33,
        by.value = 0.5,
        ylabel = "Temperature \u00B0C")

rmse_dpt01 <- objective_fun(sim = temp.interpolated[2:4,],
                           obs = obs.temp[obs.temp$Depth == 1, ],
                           fun = c('RMSE'),
                           start.date = '2002-01-23',
                           end.date = '2016-12-31',
                           min.depth = 0.5,
                           max.depth = 1.5,
                           by.value = 0.5)

rmse_dpt14 <- objective_fun(sim = temp.interpolated[28:30,],
                           obs = obs.temp[obs.temp$Depth == 14, ],
                           fun = c('RMSE'),
                           start.date = '2002-01-23',
                           end.date = '2016-12-31',
                           min.depth = 13.5,
                           max.depth = 14.5,
                           by.value = 0.5)

rmse_dpt30 <- objective_fun(sim = temp.interpolated[60:62,],
                           obs = obs.temp[obs.temp$Depth == 30, ],
                           fun = c('RMSE'),
                           start.date = '2002-01-23',
                           end.date = '2016-12-31',
                           min.depth = 29.5,
                           max.depth = 30.5,
                           by.value = 0.5)


rmse_text <- data.frame(x = as.Date('2007-06-01'),
                        y = 25,
                        Depth = c(1, 14, 30),
                        label = c(paste0('RMSE = ', round(rmse_dpt01$RMSE,2), ' \u00B0C'), 
                                  paste0('RMSE = ', round(rmse_dpt14$RMSE,2), ' \u00B0C'), 
                                  paste0('RMSE = ', round(rmse_dpt30$RMSE,2), ' \u00B0C')))

p + ylim(8,26) +
  geom_text(data = rmse_text, 
            mapping = aes(x = x, y = y, label = label))

ggsave(filename = 'Figure_06.png', height = 4, width = 7)

#---
# scatter plot - Figure 7 in the M/S
#---
plot_scatter(sim = temp.interpolated,
             obs = obs.temp,
             sim.start = "2002-01-23",
             sim.end = "2016-12-31",
             plot.start = "2002-01-23",
             plot.end = "2012-12-31",
             min.depth = 0,
             max.depth = 33,
             by.value = 0.5)

ggsave(filename = 'Figure_07.png', height = 4, width = 7)

#------END------#

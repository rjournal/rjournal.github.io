

# Created 19.11.2021


# Fig. 5


#---------------------------------------------------------------------------------


library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)
library(RColorBrewer)


load("Output/Italy.RData")

cols_exp <- brewer.pal(n = 11, name = "RdBu")
cols_exp <- cols_exp[c(2, 4, 6, 8, 10)]


# First pannel
ggplot() + geom_sf(data = d$province$age$`40<`, aes(fill = exceedance.REM.cat), size = 0.4) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) + theme_light() +
  ggtitle("A. 40<") + 
  theme(text = element_text(size = 6), 
        legend.position = 'none', 
        legend.margin=margin(0,0,0,0)) -> L1


ggplot() + geom_sf(data = d$province$age$`40-59`, aes(fill = exceedance.REM.cat), size = 0.4) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) + theme_light() +
  ggtitle("B. 40-59") + 
  theme(text = element_text(size = 6), 
        legend.position = 'none', 
        legend.margin=margin(0,0,0,0)) -> L2


ggplot() + geom_sf(data = d$province$age$`60-69`, aes(fill = exceedance.REM.cat), size = 0.4) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) + theme_light() +
  ggtitle("C. 60-69") + 
  theme(text = element_text(size = 6), 
        legend.position = 'none', 
        legend.margin=margin(0,0,0,0)) -> L3

ggplot() + geom_sf(data = d$province$age$`70-79`, aes(fill = exceedance.REM.cat), size = 0.4) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) + theme_light() +
  ggtitle("D. 70-79") + 
  theme(text = element_text(size = 6), 
        legend.position = 'none', 
        legend.margin=margin(0,0,0,0)) -> L4

ggplot() + geom_sf(data = d$province$age$`80+`, aes(fill = exceedance.REM.cat), size = 0.4) + 
  scale_fill_manual(values=cols_exp[length(cols_exp):1], name = "", drop=FALSE) + theme_light() +
  ggtitle("E. 80>") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> L5


png("Output/PosteriorProb.png", width = 17, height = 14, res = 300, units = "cm")
print(
(L1|L2|L3)/(L4|L5)
)
dev.off()


####################################################################
####################################################################
####################################################################
####################################################################


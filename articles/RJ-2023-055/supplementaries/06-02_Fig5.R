

# Created 19.11.2021


# Fig. 4


#---------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)
library(ISOweek)
library(lubridate)
library(viridis)
library(patchwork)
library(RColorBrewer)


load("Output/Italy.RData")

# get the shp
shp <- read_sf("data/ProvCM01012020_g_WGS84.shp")
link_table <- readRDS("data/link_table")

# and for NUTS2
shp %>% left_join(., link_table, by = c("SIGLA" = "SIGLA")) %>% 
  group_by(NAMNUTS2) %>% 
  summarise() -> shp_NUTS2
  

# get the iso correspondence between months for the xaxis

date.iso <- data.frame(
  iso = rep(d_week$country$sex$F$EURO_LABEL, each = 7),
  date = ISOweek2date(unlist(lapply(d_week$country$sex$F$EURO_LABEL, function(X) paste(X, 1:7, sep = "-"))))
)

date.iso$month <- month(date.iso$date)
date.iso$month_nam <- month.abb[date.iso$month]

d_week$country$sex$M %>% 
  left_join(., date.iso, by = c("EURO_LABEL" = "iso"), multiple = "all") %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) -> b

b <-  b[, c("month_nam","x")]
b <- b[-c(1:2),]

b <- b[!duplicated(b$month_nam),]



colors <- rev(brewer.pal(n = 10, name = "RdBu")[1:6])
col.highlight <- "blue"
col.highlight.cex <- 0.6

# First pannel
ggplot(d$country$none) + geom_sf(data = d$country$none, aes(fill = median.REM.cat), size = 0.4) + 
  geom_sf(fill = NA, col = col.highlight, linewidth = col.highlight.cex) + 
  scale_fill_manual(values=colors, name = "", drop=FALSE) + theme_light() +
  ggtitle("1A. National") + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> L1



d_week$country$sex$M %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("1B. Males") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P11



d_week$country$sex$F %>% mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("1C. Females") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P12

# P1/P2

L1|(P11/P12) -> M1


# Second pannel

region <- d$region$none %>% filter(median.REM == max(median.REM)) %>% select(NAMNUTS2)
region$geometry <- NULL
region <- "Puglia"




d$region$none %>% filter(NAMNUTS2 %in% region) %>% 
  select(geometry) %>% 
ggplot() +  
  geom_sf(data = d$region$none, aes(fill = median.REM.cat), size = 0.4) + 
  geom_sf(fill = NA, col = col.highlight, linewidth = col.highlight.cex) + 
  scale_fill_manual(values=colors, name = "", drop=FALSE) + theme_light() + 
  ggtitle(paste0("2A. NUTS2 regions: ", region)) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> L2


d_week$region$sex$M %>% filter(NAMNUTS2 %in% region) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("2B. Males") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P21



d_week$region$sex$F %>% filter(NAMNUTS2 %in% region) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("2C. Females") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P22


L2|(P21/P22) -> M2








# Third pannel

prov <- d$province$none %>% filter(median.REM == max(median.REM)) %>% select(ID_space)
prov$geometry <- NULL
prov <- "Foggia"


d$province$none %>% filter(DEN_UTS == prov) %>% 
  select(geometry) %>% 
  ggplot() +  
  geom_sf(data = d$province$none, aes(fill = median.REM.cat), size = 0.4) + 
  geom_sf(fill = NA, col = col.highlight, linewidth = col.highlight.cex) + 
  scale_fill_manual(values=colors, name = "", drop=FALSE) + theme_light() +
  ggtitle(paste0("3A. NUTS3 regions: ", prov)) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> L3



prov <- d$province$none %>% filter(DEN_UTS == prov) %>% select(ID_space)
prov$geometry <- NULL

d_week$province$sex$M %>% filter(ID_space %in% prov) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("3B. Males") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P31



d_week$province$sex$F %>% filter(ID_space %in% prov) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL))) %>% 
  
  ggplot() + geom_line(aes(x = x, y = median.REM)) + 
  geom_point(aes(x = x, y = median.REM), size = 0.8) + 
  geom_ribbon(aes(x = x, ymin = LL.REM, ymax = UL.REM), alpha = 0.3, fill = viridis(15)[6]) +
  geom_hline(yintercept = 0, col = "red", linetype = 2) + 
  theme_light() + ylim(c(-50, 200)) + ggtitle("3C. Females") + ylab("") + xlab("") + 
  scale_x_continuous(minor_breaks = NULL, breaks = b$x, labels = b$month_nam) + 
  theme(text = element_text(size = 6),  
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.2, 'cm'), 
        plot.margin = margin(0, 0, 0, 0), 
        legend.margin=margin(0,0,0,0)) -> P32


L3|(P31/P32) -> M3

png("Output/SpatiotemporalRegions.png", width = 16.5, height = 22, res = 300, units = "cm")
print(
M1/M2/M3
)
dev.off()




####################################################################
####################################################################
####################################################################
####################################################################
####################################################################



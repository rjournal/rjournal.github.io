## This file generates Figure 2 of our manuscript, and should be run after ##
## each of the fig2*.R scripts have been run.                              ##

library(ggplot2)
library(patchwork)

plot1 <- readRDS(file = "data/fig2a.rds")
plot2 <- readRDS(file = "data/fig2b.rds")
plot3 <- readRDS(file = "data/fig2c.rds")
plot4 <- readRDS(file = "data/fig2d.rds")

pplt <- (plot1 + plot2) / (plot3 + plot4) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')',
                  theme = theme(plot.margin = margin(0, 0, 0, -5))) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = pplt, filename = "fig2.png", width = 11, height = 7.5, dpi = 450)

## This file generates Figure 5 of our manuscript, and should be run after ##
## each of the fig5*.R scripts have been run.                              ##

library(ggplot2)
library(patchwork)

plot1 <- readRDS(file = "data/fig5a.rds")
plot2 <- readRDS(file = "data/fig5b.rds")
plot3 <- readRDS(file = "data/fig5c.rds")
plot4 <- readRDS(file = "data/fig5d.rds")

pplt <- (plot1 + plot2) / (plot3 + plot4) +
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')',
                    theme = theme(plot.margin = margin(0, 0, 0, -5))) +
    plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(plot = pplt, filename = "fig5.png", width = 11, height = 7.5, dpi = 450)

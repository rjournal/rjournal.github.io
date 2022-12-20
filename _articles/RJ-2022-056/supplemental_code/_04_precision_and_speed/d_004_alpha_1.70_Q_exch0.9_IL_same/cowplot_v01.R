library(ggplot2)
library(cowplot)

gg_5 <- readRDS("gg_rel.tol.si_1e-05_pmv_sim_val_2e+06.RDS")
gg_4 <- readRDS("gg_rel.tol.si_1e-04_pmv_sim_val_2e+06.RDS")
gg_3 <- readRDS("gg_rel.tol.si_0.001_pmv_sim_val_2e+06.RDS")

combined_plot <- plot_grid(gg_3 + theme(legend.position = "none") + coord_cartesian(xlim=c(.1,70)),
                           gg_4 + theme(legend.position = "none") + coord_cartesian(xlim=c(.1,70)), 
                           gg_5 + theme(legend.position = "none") + coord_cartesian(xlim=c(.1,70)),
                           labels = c('1e-3', '1e-4', '1e-5'), label_size = 12, ncol=3)

legend <- get_legend(
  # create some space to the left of the legend
  gg_4 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_row <- plot_grid(combined_plot,
                      legend,
                      ncol = 1, 
                      rel_heights = c(1, .1))
plot_row

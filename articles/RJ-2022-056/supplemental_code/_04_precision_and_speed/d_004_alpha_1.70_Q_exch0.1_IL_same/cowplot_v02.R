library(ggplot2)
library(cowplot)

## gg_5 <- readRDS("gg_rel.tol.si_1e-05_pmv_sim_val_2e+06.RDS")
gg_4 <- readRDS("gg_rel.tol.si_1e-04_pmv_sim_val_9.6e+07.RDS")
gg_3 <- readRDS("gg_rel.tol.si_0.001_pmv_sim_val_960000.RDS")

TRUTHVAL <- 0.5148227
PTSIZE = 6
TXTSIZE = 20

combined_plot <- plot_grid(gg_3 + 
                             theme(legend.position = "none") + 
                             coord_cartesian(xlim=c(.1,700), 
                                             ylim=c(TRUTHVAL-1e-3,
                                                    TRUTHVAL+1e-3)) + 
                             theme(text = element_text(size = TXTSIZE)) +
                             geom_point(aes(fill=approach,
                                            y=outcome, 
                                            x=median),
                                        size=PTSIZE,
                                        color="black",
                                        pch=21),
                           gg_4  + 
                             theme(legend.position = "none") + 
                             coord_cartesian(xlim=c(.1,700), 
                                             ylim=c(TRUTHVAL-1e-3,
                                                    TRUTHVAL+1e-3)) + 
                             theme(text = element_text(size = TXTSIZE)) +
                             geom_point(aes(fill=approach, 
                                            y=outcome, 
                                            x=median),
                                        size=PTSIZE,
                                        color="black",
                                        pch=21),
                           labels = c('A) 1e-3 rho=0.1', 
                                      'B) 1e-4 rho=0.1'), 
                           label_size = 16, 
                           hjust=-0.5,
                           vjust=1.1,
                           ncol=2)

legend <- get_legend(
  # create some space to the left of the legend
  gg_4 + labs(color=NULL)+
    guides(color = guide_legend(nrow = 1,override.aes = list(size = PTSIZE))) +
    theme(legend.position = "bottom", text=element_text(size=TXTSIZE))
)

plot_row <- plot_grid(combined_plot,
                      legend,
                      ncol = 1, 
                      rel_heights = c(1, .1))
plot_row

library(ggplot2)
library(RColorBrewer)
library(GGally)

load("exp1cpoutput.RData")
load("exp2cpoutput.RData")
load("exp3cpoutput.RData")
load("exp4cpoutput.RData")
load("weib1cpoutput.RData")
load("weib2cpoutput.RData")
load("weib3cpoutput.RData")
load("weib4cpoutput.RData")
load("resultsfns.RData")

sim <- 10000
palette <- brewer.pal(3, "Set1") 

## Tau ####
## 1cp results ----
exp1tautable <- tautable(exp1cpout, 1, "exp", sim, param1cp.exp)
exp1tautable$invtauinci <- ifelse(exp1tautable$tau > exp1tautable$inv_tau - 1.96 * exp1tautable$inv_sd/sqrt(as.numeric(exp1tautable$N)),
                                  ifelse(exp1tautable$tau < exp1tautable$inv_tau + 1.96 * exp1tautable$inv_sd/sqrt(as.numeric(exp1tautable$N)), 1, 0), 0)
exp1tautable$memtauinci <- ifelse(exp1tautable$tau > exp1tautable$mem_tau - 1.96 * exp1tautable$mem_sd/sqrt(as.numeric(exp1tautable$N)),
                                  ifelse(exp1tautable$tau < exp1tautable$mem_tau + 1.96 * exp1tautable$mem_sd/sqrt(as.numeric(exp1tautable$N)), 1, 0), 0)


weib1tautable <- tautable(weib1cpout, 1, "weib", sim, param1cp.weib)
weib1tautable$invtauinci <- ifelse(weib1tautable$tau > weib1tautable$inv_tau - 1.96 * weib1tautable$inv_sd/sqrt(as.numeric(weib1tautable$N)),
                                  ifelse(weib1tautable$tau < weib1tautable$inv_tau + 1.96 * weib1tautable$inv_sd/sqrt(as.numeric(weib1tautable$N)), 1, 0), 0)
weib1tautable$memtauinci <- ifelse(weib1tautable$tau > weib1tautable$mem_tau - 1.96 * weib1tautable$mem_sd/sqrt(as.numeric(weib1tautable$N)),
                                  ifelse(weib1tautable$tau < weib1tautable$mem_tau + 1.96 * weib1tautable$mem_sd/sqrt(as.numeric(weib1tautable$N)), 1, 0), 0)


# parallel line plots
exp1_plplot <- ggparcoord(exp1tautable, columns = c(11, 12), groupColumn = 1, 
                          scale="globalminmax", showPoints = TRUE) + 
               scale_color_manual(values = palette) +
               scale_x_discrete(labels=c("inv_in_range" = expression(paste("Inv ", tau)), 
                                         "mem_in_range" = expression(paste("Mem ", tau)))) +
               theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
               labs(x = "", y = "Proportion within \u00B1 5", 
                    color = "Sample size") +
               ylim(c(0,1))

weib1_plplot <- ggparcoord(weib1tautable, columns = c(11, 12), groupColumn = 1, 
                           scale="globalminmax", showPoints = TRUE) + 
                scale_color_manual(values = palette) +
                scale_x_discrete(labels=c("inv_in_range" = expression(paste("Inv ", tau)), 
                                          "mem_in_range" = expression(paste("Mem ", tau)))) +
                theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                labs(x = "", y = "Proportion within \u00B1 5", 
                     color = "Sample size") +
                ylim(c(0,1))

ggsave(filename = "exp1_plplot.pdf", plot = exp1_plplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib1_plplot.pdf", plot = weib1_plplot, width = 5, height = 4, units = "in")

## 2cp results ----
exp2tautable <- tautable(exp2cpout, 2, "exp", sim, param2cp.exp)
exp2tautable$invtau1inci <- ifelse(exp2tautable$tau1 > exp2tautable$inv_tau1 - 1.96 * exp2tautable$inv_sd1/sqrt(as.numeric(exp2tautable$N)),
                                  ifelse(exp2tautable$tau1 < exp2tautable$inv_tau1 + 1.96 * exp2tautable$inv_sd1/sqrt(as.numeric(exp2tautable$N)), 1, 0), 0)
exp2tautable$memtau1inci <- ifelse(exp2tautable$tau1 > exp2tautable$mem_tau1 - 1.96 * exp2tautable$mem_sd1/sqrt(as.numeric(exp2tautable$N)),
                                  ifelse(exp2tautable$tau1 < exp2tautable$mem_tau1 + 1.96 * exp2tautable$mem_sd1/sqrt(as.numeric(exp2tautable$N)), 1, 0), 0)
exp2tautable$invtau2inci <- ifelse(exp2tautable$tau2 > exp2tautable$inv_tau2 - 1.96 * exp2tautable$inv_sd2/sqrt(as.numeric(exp2tautable$N)),
                                    ifelse(exp2tautable$tau2 < exp2tautable$inv_tau2 + 1.96 * exp2tautable$inv_sd2/sqrt(as.numeric(exp2tautable$N)), 1, 0), 0)
exp2tautable$memtau2inci <- ifelse(exp2tautable$tau2 > exp2tautable$mem_tau2 - 1.96 * exp2tautable$mem_sd2/sqrt(as.numeric(exp2tautable$N)),
                                    ifelse(exp2tautable$tau2 < exp2tautable$mem_tau2 + 1.96 * exp2tautable$mem_sd2/sqrt(as.numeric(exp2tautable$N)), 1, 0), 0)


weib2tautable <- tautable(weib2cpout, 2, "weib", sim, param2cp.weib)
weib2tautable$invtau1inci <- ifelse(weib2tautable$tau1 > weib2tautable$inv_tau1 - 1.96 * weib2tautable$inv_sd1/sqrt(as.numeric(weib2tautable$N)),
                                   ifelse(weib2tautable$tau1 < weib2tautable$inv_tau1 + 1.96 * weib2tautable$inv_sd1/sqrt(as.numeric(weib2tautable$N)), 1, 0), 0)
weib2tautable$memtau1inci <- ifelse(weib2tautable$tau1 > weib2tautable$mem_tau1 - 1.96 * weib2tautable$mem_sd1/sqrt(as.numeric(weib2tautable$N)),
                                   ifelse(weib2tautable$tau1 < weib2tautable$mem_tau1 + 1.96 * weib2tautable$mem_sd1/sqrt(as.numeric(weib2tautable$N)), 1, 0), 0)
weib2tautable$invtau2inci <- ifelse(weib2tautable$tau2 > weib2tautable$inv_tau2 - 1.96 * weib2tautable$inv_sd2/sqrt(as.numeric(weib2tautable$N)),
                                    ifelse(weib2tautable$tau2 < weib2tautable$inv_tau2 + 1.96 * weib2tautable$inv_sd2/sqrt(as.numeric(weib2tautable$N)), 1, 0), 0)
weib2tautable$memtau2inci <- ifelse(weib2tautable$tau2 > weib2tautable$mem_tau2 - 1.96 * weib2tautable$mem_sd2/sqrt(as.numeric(weib2tautable$N)),
                                    ifelse(weib2tautable$tau2 < weib2tautable$mem_tau2 + 1.96 * weib2tautable$mem_sd2/sqrt(as.numeric(weib2tautable$N)), 1, 0), 0)


exp2_plplot <- ggparcoord(exp2tautable, columns = c(12, 13, 21, 22), groupColumn = 1, scale="globalminmax",
                          showPoints = TRUE) + 
               scale_color_manual(values = palette) +
               scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                         "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                         "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                         "mem_in_range2" = expression(paste("Mem ", tau[2])))) +
               theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
               labs(x = "", y = "Proportion within \u00B1 5", 
                    color = "Sample size") +
               ylim(c(0,1))

weib2_plplot <- ggparcoord(weib2tautable, columns = c(12, 13, 21, 22), groupColumn = 1, scale="globalminmax",
                           showPoints = TRUE) + 
                scale_color_manual(values = palette) +
                scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                          "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                          "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                          "mem_in_range2" = expression(paste("Mem ", tau[2])))) +
                theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                labs(x = "", y = "Proportion within \u00B1 5", 
                     color = "Sample size") +
                ylim(c(0,1))

ggsave(filename = "exp2_plplot.pdf", plot = exp2_plplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib2_plplot.pdf", plot = weib2_plplot, width = 5, height = 4, units = "in")


## 3cp results ----
exp3tautable <- tautable(exp3cpout, 3, "exp", sim, param3cp.exp)
exp3tautable$invtau1inci <- ifelse(exp3tautable$tau1 > exp3tautable$inv_tau1 - 1.96 * exp3tautable$inv_sd1/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau1 < exp3tautable$inv_tau1 + 1.96 * exp3tautable$inv_sd1/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)
exp3tautable$memtau1inci <- ifelse(exp3tautable$tau1 > exp3tautable$mem_tau1 - 1.96 * exp3tautable$mem_sd1/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau1 < exp3tautable$mem_tau1 + 1.96 * exp3tautable$mem_sd1/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)
exp3tautable$invtau2inci <- ifelse(exp3tautable$tau2 > exp3tautable$inv_tau2 - 1.96 * exp3tautable$inv_sd2/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau2 < exp3tautable$inv_tau2 + 1.96 * exp3tautable$inv_sd2/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)
exp3tautable$memtau2inci <- ifelse(exp3tautable$tau2 > exp3tautable$mem_tau2 - 1.96 * exp3tautable$mem_sd2/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau2 < exp3tautable$mem_tau2 + 1.96 * exp3tautable$mem_sd2/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)
exp3tautable$invtau3inci <- ifelse(exp3tautable$tau3 > exp3tautable$inv_tau3 - 1.96 * exp3tautable$inv_sd3/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau3 < exp3tautable$inv_tau3 + 1.96 * exp3tautable$inv_sd3/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)
exp3tautable$memtau3inci <- ifelse(exp3tautable$tau3 > exp3tautable$mem_tau3 - 1.96 * exp3tautable$mem_sd3/sqrt(as.numeric(exp3tautable$N)),
                                   ifelse(exp3tautable$tau3 < exp3tautable$mem_tau3 + 1.96 * exp3tautable$mem_sd3/sqrt(as.numeric(exp3tautable$N)), 1, 0), 0)


weib3tautable <- tautable(weib3cpout, 3, "weib", sim, param3cp.weib)
weib3tautable$invtau1inci <- ifelse(weib3tautable$tau1 > weib3tautable$inv_tau1 - 1.96 * weib3tautable$inv_sd1/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau1 < weib3tautable$inv_tau1 + 1.96 * weib3tautable$inv_sd1/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)
weib3tautable$memtau1inci <- ifelse(weib3tautable$tau1 > weib3tautable$mem_tau1 - 1.96 * weib3tautable$mem_sd1/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau1 < weib3tautable$mem_tau1 + 1.96 * weib3tautable$mem_sd1/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)
weib3tautable$invtau2inci <- ifelse(weib3tautable$tau2 > weib3tautable$inv_tau2 - 1.96 * weib3tautable$inv_sd2/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau2 < weib3tautable$inv_tau2 + 1.96 * weib3tautable$inv_sd2/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)
weib3tautable$memtau2inci <- ifelse(weib3tautable$tau2 > weib3tautable$mem_tau2 - 1.96 * weib3tautable$mem_sd2/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau2 < weib3tautable$mem_tau2 + 1.96 * weib3tautable$mem_sd2/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)
weib3tautable$invtau3inci <- ifelse(weib3tautable$tau3 > weib3tautable$inv_tau3 - 1.96 * weib3tautable$inv_sd3/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau3 < weib3tautable$inv_tau3 + 1.96 * weib3tautable$inv_sd3/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)
weib3tautable$memtau3inci <- ifelse(weib3tautable$tau3 > weib3tautable$mem_tau3 - 1.96 * weib3tautable$mem_sd3/sqrt(as.numeric(weib3tautable$N)),
                                   ifelse(weib3tautable$tau3 < weib3tautable$mem_tau3 + 1.96 * weib3tautable$mem_sd3/sqrt(as.numeric(weib3tautable$N)), 1, 0), 0)

exp3_plplot <- ggparcoord(exp3tautable, columns = c(13, 14, 22, 23, 31, 32), groupColumn = 1,
                          scale = "globalminmax", showPoints = TRUE) +
               scale_color_manual(values = palette) +
               scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                         "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                         "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                         "mem_in_range2" = expression(paste("Mem ", tau[2])),
                                         "inv_in_range3" = expression(paste("Inv ", tau[3])), 
                                         "mem_in_range3" = expression(paste("Mem ", tau[3])))) +
               theme_minimal() + theme(legend.position="bottom", text = element_text(size = 12)) +
               labs(x = "", y = "Proportion within \u00B1 5", 
                    color = "Sample size") +
               ylim(c(0,1))

weib3_plplot <- ggparcoord(weib3tautable, columns = c(13, 14, 22, 23, 31, 32), groupColumn = 1,
                           scale = "globalminmax", showPoints = TRUE) +
                scale_color_manual(values = palette) +
                scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                          "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                          "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                          "mem_in_range2" = expression(paste("Mem ", tau[2])),
                                          "inv_in_range3" = expression(paste("Inv ", tau[3])), 
                                          "mem_in_range3" = expression(paste("Mem ", tau[3])))) +
                theme_minimal() + theme(legend.position="bottom", text = element_text(size = 12)) +
                labs(x = "", y = "Proportion within \u00B1 5", 
                     color = "Sample size") +
                ylim(c(0,1))

ggsave(filename = "exp3_plplot.pdf", plot = exp3_plplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib3_plplot.pdf", plot = weib3_plplot, width = 5, height = 4, units = "in")


## 4cp results ----
exp4tautable <- tautable(exp4cpout, 4, "exp", sim, param4cp.exp)
exp4tautable$invtau1inci <- ifelse(exp4tautable$tau1 > exp4tautable$inv_tau1 - 1.96 * exp4tautable$inv_sd1/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau1 < exp4tautable$inv_tau1 + 1.96 * exp4tautable$inv_sd1/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$memtau1inci <- ifelse(exp4tautable$tau1 > exp4tautable$mem_tau1 - 1.96 * exp4tautable$mem_sd1/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau1 < exp4tautable$mem_tau1 + 1.96 * exp4tautable$mem_sd1/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$invtau2inci <- ifelse(exp4tautable$tau2 > exp4tautable$inv_tau2 - 1.96 * exp4tautable$inv_sd2/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau2 < exp4tautable$inv_tau2 + 1.96 * exp4tautable$inv_sd2/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$memtau2inci <- ifelse(exp4tautable$tau2 > exp4tautable$mem_tau2 - 1.96 * exp4tautable$mem_sd2/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau2 < exp4tautable$mem_tau2 + 1.96 * exp4tautable$mem_sd2/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$invtau3inci <- ifelse(exp4tautable$tau3 > exp4tautable$inv_tau3 - 1.96 * exp4tautable$inv_sd3/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau3 < exp4tautable$inv_tau3 + 1.96 * exp4tautable$inv_sd3/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$memtau3inci <- ifelse(exp4tautable$tau3 > exp4tautable$mem_tau3 - 1.96 * exp4tautable$mem_sd3/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau3 < exp4tautable$mem_tau3 + 1.96 * exp4tautable$mem_sd3/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$invtau4inci <- ifelse(exp4tautable$tau4 > exp4tautable$inv_tau4 - 1.96 * exp4tautable$inv_sd4/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau4 < exp4tautable$inv_tau4 + 1.96 * exp4tautable$inv_sd4/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)
exp4tautable$memtau4inci <- ifelse(exp4tautable$tau4 > exp4tautable$mem_tau4 - 1.96 * exp4tautable$mem_sd4/sqrt(as.numeric(exp4tautable$N)),
                                   ifelse(exp4tautable$tau4 < exp4tautable$mem_tau4 + 1.96 * exp4tautable$mem_sd4/sqrt(as.numeric(exp4tautable$N)), 1, 0), 0)


weib4tautable <- tautable(weib4cpout, 4, "weib", sim, param4cp.weib)
weib4tautable$invtau1inci <- ifelse(weib4tautable$tau1 > weib4tautable$inv_tau1 - 1.96 * weib4tautable$inv_sd1/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau1 < weib4tautable$inv_tau1 + 1.96 * weib4tautable$inv_sd1/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$memtau1inci <- ifelse(weib4tautable$tau1 > weib4tautable$mem_tau1 - 1.96 * weib4tautable$mem_sd1/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau1 < weib4tautable$mem_tau1 + 1.96 * weib4tautable$mem_sd1/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$invtau2inci <- ifelse(weib4tautable$tau2 > weib4tautable$inv_tau2 - 1.96 * weib4tautable$inv_sd2/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau2 < weib4tautable$inv_tau2 + 1.96 * weib4tautable$inv_sd2/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$memtau2inci <- ifelse(weib4tautable$tau2 > weib4tautable$mem_tau2 - 1.96 * weib4tautable$mem_sd2/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau2 < weib4tautable$mem_tau2 + 1.96 * weib4tautable$mem_sd2/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$invtau3inci <- ifelse(weib4tautable$tau3 > weib4tautable$inv_tau3 - 1.96 * weib4tautable$inv_sd3/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau3 < weib4tautable$inv_tau3 + 1.96 * weib4tautable$inv_sd3/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$memtau3inci <- ifelse(weib4tautable$tau3 > weib4tautable$mem_tau3 - 1.96 * weib4tautable$mem_sd3/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau3 < weib4tautable$mem_tau3 + 1.96 * weib4tautable$mem_sd3/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$invtau4inci <- ifelse(weib4tautable$tau4 > weib4tautable$inv_tau4 - 1.96 * weib4tautable$inv_sd4/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau4 < weib4tautable$inv_tau4 + 1.96 * weib4tautable$inv_sd4/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)
weib4tautable$memtau4inci <- ifelse(weib4tautable$tau4 > weib4tautable$mem_tau4 - 1.96 * weib4tautable$mem_sd4/sqrt(as.numeric(weib4tautable$N)),
                                    ifelse(weib4tautable$tau4 < weib4tautable$mem_tau4 + 1.96 * weib4tautable$mem_sd4/sqrt(as.numeric(weib4tautable$N)), 1, 0), 0)


exp4_plplot <- ggparcoord(exp4tautable, columns = c(14, 15, 23, 24, 32, 33, 41, 42), groupColumn = 1,
                          scale = "globalminmax", showPoints = TRUE) +
               scale_color_manual(values = palette) +
               scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                         "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                         "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                         "mem_in_range2" = expression(paste("Mem ", tau[2])),
                                         "inv_in_range3" = expression(paste("Inv ", tau[3])), 
                                         "mem_in_range3" = expression(paste("Mem ", tau[3])),
                                         "inv_in_range4" = expression(paste("Inv ", tau[4])), 
                                         "mem_in_range4" = expression(paste("Mem ", tau[4])))) +
               theme_minimal() +  theme(legend.position="bottom", text = element_text(size = 12)) +
               labs(x = "", y = "Proportion within \u00B1 5", 
                    color = "Sample size") +
               ylim(c(0,1))

weib4_plplot <- ggparcoord(weib4tautable, columns = c(14, 15, 23, 24, 32, 33, 41, 42), groupColumn = 1,
                           scale = "globalminmax", showPoints = TRUE) +
                scale_color_manual(values = palette) +
                scale_x_discrete(labels=c("inv_in_range1" = expression(paste("Inv ", tau[1])), 
                                          "mem_in_range1" = expression(paste("Mem ", tau[1])),
                                          "inv_in_range2" = expression(paste("Inv ", tau[2])), 
                                          "mem_in_range2" = expression(paste("Mem ", tau[2])),
                                          "inv_in_range3" = expression(paste("Inv ", tau[3])), 
                                          "mem_in_range3" = expression(paste("Mem ", tau[3])),
                                          "inv_in_range4" = expression(paste("Inv ", tau[4])), 
                                          "mem_in_range4" = expression(paste("Mem ", tau[4])))) +
                theme_minimal() + theme(legend.position="bottom", text = element_text(size = 12)) + 
                labs(x = "", y = "Proportion within \u00B1 5", 
                     color = "Sample size") +
                ylim(c(0,1))

ggsave(filename = "exp4_plplot.pdf", plot = exp4_plplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib4_plplot.pdf", plot = weib4_plplot, width = 5, height = 4, units = "in")


## Gamma ####


weib1_gtable <- gammatable(weib1cpout, 1, "weib", sim, param1cp.weib)
weib2_gtable <- gammatable(weib2cpout, 2, "weib", sim, param2cp.weib)
weib3_gtable <- gammatable(weib3cpout, 3, "weib", sim, param3cp.weib)
weib4_gtable <- gammatable(weib4cpout, 4, "weib", sim, param4cp.weib)

weib1_gplot <- ggplot(weib1_gtable, aes(x = N, y = ghat, 
                                        ymax = ciup, 
                                        ymin = cilow,
                                        color = method)) + 
               geom_hline(yintercept = 2) +
               geom_pointrange(position = position_dodge(width = 1)) + 
               scale_color_manual(name = "Simulation method", 
                                  labels = c("Inverse hazard", "Memoryless"),
                                  values = palette[2:3]) +
               theme_minimal() +
               ylim(c(1.15, 2.4)) +
               theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
                     legend.position="bottom", 
                     text = element_text(size = 12)) +
               labs(x = "Sample size", y = expression(hat(gamma)))
               
weib2_gplot <- ggplot(weib2_gtable, aes(x = N, y = ghat, 
                                        ymax = ciup, 
                                        ymin = cilow,
                                        color = method)) + 
               geom_hline(yintercept = 2) +
               geom_pointrange(position = position_dodge(width = 1)) + 
               scale_color_manual(name = "Simulation method", 
                                  labels = c("Inverse hazard", "Memoryless"),
                                  values = palette[2:3]) + 
               theme_minimal() +
               ylim(c(1.15, 2.4)) +
               theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
                     legend.position="bottom", 
                     text = element_text(size = 12)) +
               labs(x = "Sample size", y = expression(hat(gamma)))

weib3_gplot <- ggplot(weib3_gtable, aes(x = N, y = ghat, 
                                        ymax = ciup, 
                                        ymin = cilow,
                                        color = method)) + 
               geom_hline(yintercept = 2) +
               geom_pointrange(position = position_dodge(width = 1)) +  
               scale_color_manual(name = "Simulation method", 
                                  labels = c("Inverse hazard", "Memoryless"),
                                  values = palette[2:3]) + 
               theme_minimal() +
               ylim(c(1.15, 2.4)) +
               theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
                     legend.position="bottom", 
                     text = element_text(size = 12)) +
               labs(x = "Sample size", y = expression(hat(gamma))) 

weib4_gplot <- ggplot(weib4_gtable, aes(x = N, y = ghat, 
                                        ymax = ciup, 
                                        ymin = cilow,
                                        color = method)) + 
               geom_hline(yintercept = 2) +
               geom_pointrange(position = position_dodge(width = 1)) +  
               scale_color_manual(name = "Simulation method", 
                                  labels = c("Inverse hazard", "Memoryless"),
                                  values = palette[2:3]) +  
               theme_minimal() +
               ylim(c(1.15, 2.4)) +
               theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
                     legend.position="bottom", 
                     text = element_text(size = 12)) +
               labs(x = "Sample size", y = expression(hat(gamma)))

ggsave(filename = "weib1_gplot.pdf", plot = weib1_gplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib2_gplot.pdf", plot = weib2_gplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib3_gplot.pdf", plot = weib3_gplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib4_gplot.pdf", plot = weib4_gplot, width = 5, height = 4, units = "in")

## Theta ####
exp1thetatable <- thetatable(exp1cpout, 1, "exp", sim, param1cp.exp)

exp1_thetaplot <- ggparcoord(exp1thetatable, columns = c(4, 5, 7, 8), groupColumn = 2, 
                             scale="globalminmax", showPoints = TRUE) + 
                  scale_color_manual(values = palette) +
                  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                                            "mem_pbias2" = expression(paste("Mem ", theta[2])))) +
                  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                  labs(x = "", y = "Proportion bias", 
                       color = "Sample size") +
                  ylim(0, 2)

exp2thetatable <- thetatable(exp2cpout, 2, "exp", sim, param2cp.exp)

exp2_thetaplot <- ggparcoord(exp2thetatable, columns = c(4, 5, 7, 8, 10, 11), groupColumn = 2, 
                             scale="globalminmax", showPoints = TRUE) + 
                  scale_color_manual(values = palette[2:3]) +
                  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                                            "mem_pbias2" = expression(paste("Mem ", theta[2])),
                                            "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                                            "mem_pbias3" = expression(paste("Mem ", theta[3])))) +
                  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                  labs(x = "", y = "Proportion bias", 
                       color = "Sample size") +
                  ylim(0, 2)

exp3thetatable <- thetatable(exp3cpout, 3, "exp", sim, param3cp.exp)

exp3thetaplot <- ggparcoord(exp3thetatable, columns = c(4, 5, 7, 8, 10, 11, 13, 14), 
                            groupColumn = 2, scale="globalminmax", showPoints = TRUE) + 
                 scale_color_manual(values = palette[2:3]) +
                 scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                                           "mem_pbias1" = expression(paste("Mem ", theta[1])),
                                           "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                                           "mem_pbias2" = expression(paste("Mem ", theta[2])),
                                           "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                                           "mem_pbias3" = expression(paste("Mem ", theta[3])),
                                           "inv_pbias4" = expression(paste("Inv ", theta[4])), 
                                           "mem_pbias4" = expression(paste("Mem ", theta[4])))) +
                 theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                 labs(x = "", y = "Proportion bias", 
                      color = "Sample size") +
                 ylim(0, 2)

exp4thetatable <- thetatable(exp4cpout, 4, "exp", sim, param4cp.exp)

exp4thetaplot <- ggparcoord(exp4thetatable, columns = c(4, 5, 7, 8, 10, 11, 13, 14, 16, 17), 
                            groupColumn = 2, scale="globalminmax", showPoints = TRUE) + 
                 scale_color_manual(values = palette[3]) +
                 scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                                           "mem_pbias1" = expression(paste("Mem ", theta[1])),
                                           "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                                           "mem_pbias2" = expression(paste("Mem ", theta[2])),
                                           "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                                           "mem_pbias3" = expression(paste("Mem ", theta[3])),
                                           "inv_pbias4" = expression(paste("Inv ", theta[4])), 
                                           "mem_pbias4" = expression(paste("Mem ", theta[4])),
                                           "inv_pbias5" = expression(paste("Inv ", theta[5])), 
                                           "mem_pbias5" = expression(paste("Mem ", theta[5])))) +
                 theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
                 labs(x = "", y = "Proportion bias", 
                      color = "Sample size") +
                 ylim(0, 2)

ggsave(filename = "exp1_tplot.pdf", plot = exp1_thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "exp2_tplot.pdf", plot = exp2_thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "exp3_tplot.pdf", plot = exp3thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "exp4_tplot.pdf", plot = exp4thetaplot, width = 5, height = 4, units = "in")

weib1thetatable <- thetatable(weib1cpout, 1, "weib", sim, param1cp.weib)

weib1_thetaplot <- ggparcoord(weib1thetatable, columns = c(4, 5, 7, 8), groupColumn = 2, 
                              scale="globalminmax", showPoints = TRUE) + 
  scale_color_manual(values = palette) +
  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                            "mem_pbias2" = expression(paste("Mem ", theta[2])))) +
  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
  labs(x = "", y = "Proportion bias", 
       color = "Sample size") +
  ylim(0, 2)

weib2thetatable <- thetatable(weib2cpout, 2, "weib", sim, param2cp.weib)

weib2_thetaplot <- ggparcoord(weib2thetatable, columns = c(4, 5, 7, 8, 10, 11), groupColumn = 2, 
                              scale="globalminmax", showPoints = TRUE) + 
  scale_color_manual(values = palette[3]) +
  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                            "mem_pbias2" = expression(paste("Mem ", theta[2])),
                            "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                            "mem_pbias3" = expression(paste("Mem ", theta[3])))) +
  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
  labs(x = "", y = "Proportion bias", 
       color = "Sample size") +
  ylim(0, 2)

weib3thetatable <- thetatable(weib3cpout, 3, "weib", sim, param3cp.weib)

weib3thetaplot <- ggparcoord(weib3thetatable, columns = c(4, 5, 7, 8, 10, 11, 13, 14), 
                             groupColumn = 2, scale="globalminmax", showPoints = TRUE) + 
  scale_color_manual(values = palette[3]) +
  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                            "mem_pbias2" = expression(paste("Mem ", theta[2])),
                            "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                            "mem_pbias3" = expression(paste("Mem ", theta[3])),
                            "inv_pbias4" = expression(paste("Inv ", theta[4])), 
                            "mem_pbias4" = expression(paste("Mem ", theta[4])))) +
  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
  labs(x = "", y = "Proportion bias", 
       color = "Sample size") +
  ylim(0, 2)

weib4thetatable <- thetatable(weib4cpout, 4, "weib", sim, param4cp.weib)

weib4thetaplot <- ggparcoord(weib4thetatable, columns = c(4, 5, 7, 8, 10, 11, 13, 14, 16, 17), 
                             groupColumn = 2, scale="globalminmax", showPoints = TRUE) + 
  scale_color_manual(values = palette) +
  scale_x_discrete(labels=c("inv_pbias1" = expression(paste("Inv ", theta[1])), 
                            "mem_pbias1" = expression(paste("Mem ", theta[1])),
                            "inv_pbias2" = expression(paste("Inv ", theta[2])), 
                            "mem_pbias2" = expression(paste("Mem ", theta[2])),
                            "inv_pbias3" = expression(paste("Inv ", theta[3])), 
                            "mem_pbias3" = expression(paste("Mem ", theta[3])),
                            "inv_pbias4" = expression(paste("Inv ", theta[4])), 
                            "mem_pbias4" = expression(paste("Mem ", theta[4])),
                            "inv_pbias5" = expression(paste("Inv ", theta[5])), 
                            "mem_pbias5" = expression(paste("Mem ", theta[5])))) +
  theme_minimal()+ theme(legend.position="bottom", text = element_text(size = 12)) +
  labs(x = "", y = "Proportion bias", 
       color = "Sample size") +
  ylim(0, 2)

ggsave(filename = "weib1_tplot.pdf", plot = weib1_thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib2_tplot.pdf", plot = weib2_thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib3_tplot.pdf", plot = weib3thetaplot, width = 5, height = 4, units = "in")
ggsave(filename = "weib4_tplot.pdf", plot = weib4thetaplot, width = 5, height = 4, units = "in")
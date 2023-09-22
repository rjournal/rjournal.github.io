rm(list = ls())
library(ggplot2)
library(xtable)
#Be sure to use correct data directory
# .../RJournal_Rfiles/section5/


timing <- list()
for(i in 1:7){
  load(paste0("results/pen_simulation_", i, ".RData"))
  timing[[i]] <- data.frame(i = as.factor(1:6),
                          penalty = as.factor(rep(c("MCP", "SCAD", "LASSO"), 2)),
                          method = as.factor(rep(c("crrp", "fastCrrp"), each = 3)),
                          n = nobs,
                          time =
                            c(apply(crrp_mcp_time, 2, mean)[3],
                              apply(crrp_scad_time, 2, mean)[3],
                              apply(crrp_lasso_time, 2, mean)[3],
                              apply(fast_mcp_time, 2, mean)[3],
                              apply(fast_scad_time, 2, mean)[3],
                              apply(fast_lasso_time, 2, mean)[3]))
}

#####################################################################################
# FIGURE 4
timing <- do.call(rbind, timing)

legend.order <- interaction(timing$method, timing$penalty, sep = ": ")
legend.order <- factor(legend.order, levels = levels(legend.order)[c(3, 5, 1, 4, 6, 2)])
legend.order
ggplot(data = timing, aes(x = log10(n), y = log10(time), group = i,
                          shape = legend.order,
                          linetype = legend.order)) +
  theme(legend.position = "right", legend.key.size =  unit(0.7, "in"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = 12)) +
  ylab(expression(log[10](Seconds))) +
  xlab(expression(log[10](Sample~size))) +
  geom_line() +
  geom_point(size = 2.4) +
  scale_linetype_manual(values = rep(c(1, 3), each = 3), name = "Method: Penalty") +
  scale_shape_manual(values = rep(15:17, 2), name = "Method: Penalty")

#####################################################################################
# Appendix figure (un logged Figure 4)
ggplot(data = timing, aes(x = n, y = time, group = i,
                          shape = legend.order,
                          linetype = legend.order)) +
  theme(legend.position = "right", legend.key.size =  unit(0.7, "in"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = 12)) +
  ylab(expression(Seconds)) +
  xlab(expression(Sample~size)) +
  geom_line() +
  geom_point(size = 2.4) +
  scale_linetype_manual(values = rep(c(1, 3), each = 3), name = "Method: Penalty") +
  scale_shape_manual(values = rep(15:17, 2), name = "Method: Penalty")

#####################################################################################
# Regression for linearity
#MCP
tmp1 <- timing %>% filter(method == "crrp" & penalty == "MCP")
summary(lm(log10(time) ~ log10(n), data = tmp1))

tmp2 <- timing %>% filter(method == "fastCrrp" & penalty == "MCP")
summary(lm(log10(time) ~ log10(n), data = tmp2))

#SCAD
tmp1 <- timing %>% filter(method == "crrp" & penalty == "SCAD")
summary(lm(log10(time) ~ log10(n), data = tmp1))

tmp2 <- timing %>% filter(method == "fastCrrp" & penalty == "SCAD")
summary(lm(log10(time) ~ log10(n), data = tmp2))

#LASSO
tmp1 <- timing %>% filter(method == "crrp" & penalty == "LASSO")
summary(lm(log10(time) ~ log10(n), data = tmp1))

tmp2 <- timing %>% filter(method == "fastCrrp" & penalty == "LASSO")
summary(lm(log10(time) ~ log10(n), data = tmp2))


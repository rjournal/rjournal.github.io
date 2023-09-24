rm(list = ls())
library(ggplot2)
library(reshape2)
library(xtable)
library(dplyr)
#Be sure to use correct data directory
# .../RJournal_Rfiles/section5/

timing <- list()
NOBS <- NULL
for(i in 1:7){
  #Combine the two results here:
  load(paste0("results/np_simulation_", i, ".RData"))
  NOBS[i] = nobs
  timing[[i]] <- data.frame(i = as.factor(4:1),
                            n = nobs,
                            method = rep(c("crr", "fastCrr"), each = 2),
                            var = rep(c("(var)", "(no var)"), 2),
                            time =
                              c(apply(cmprsk_time_var, 2, mean)[3],
                                apply(cmprsk_time_no_var, 2, mean)[3],
                                apply(fast_time_var, 2, mean)[3],
                                apply(fast_time_no_var, 2, mean)[3]))
}


#####################################################################################
# FIGURE 3
timing <- do.call(rbind, timing)
legend.order <- interaction(timing$method, timing$var, sep = " ")
legend.order <- factor(legend.order, levels = levels(legend.order)[c(3, 1, 4, 2)])
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
  scale_linetype_manual(values = rep(c(1, 2), each = 2), name = "Method") +
  scale_shape_manual(values = rep(15:16, 2), name = "Method")

#####################################################################################
# TABLE 4 (Coverage probabilities)
coverage <- function(a, b, c) {
  ifelse(a > b & a < c, 1, 0)
}

cover <- list()
se.est <- list()
beta.idx <- 1
for(i in 1:7){
  #Combine the two results here:
  load(paste0("results/np_simulation_", i, ".RData"))
  crr_se     <- unlist(lapply(cmprsk_var, function(x) x[beta.idx]))
  fastCrr_se <- unlist(lapply(fast_var, function(x) x[beta.idx]))
  
  #crr_beta     <- cmprsk_beta[, beta.idx]
  #fastCrr_beta <- fast_beta[, beta.idx]
  cover[[i]] <- data.frame(method = c("crr", "fastCrr"),
                           res = c(mean(coverage(beta1[beta.idx], cmprsk_beta[, beta.idx] - 1.96 * crr_se, cmprsk_beta[, beta.idx] + 1.96 * crr_se)),
                                   mean(coverage(beta1[beta.idx], fast_beta[, beta.idx] - 1.96 * fastCrr_se, fast_beta[, beta.idx] + 1.96 * fastCrr_se))))
  
  se.est[[i]] <- c(sd(fast_beta[, beta.idx]), mean(fastCrr_se), mean(crr_se))
}

# Table 3
tmp <- t(do.call(rbind, se.est)) 
rownames(tmp) <- c("Empirical", "Bootstrap", "Asymptotic")
xtable(tmp[, c(1, 3, 5, 7)])

# Table 4
cover <- do.call(rbind, cover)
cover$se <- paste0(cover$res, " (", round(sqrt(cover$res * (1 - cover$res) / NUM), 2), ")")
tmp <- dcast(cover,  method ~ i, value.var = "se")
xtable(tmp[, c(1, 3, 5, 7)])


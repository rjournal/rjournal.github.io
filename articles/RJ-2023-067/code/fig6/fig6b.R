## This file generates Figure 6b of our manuscript. ##
## Estimated run time: 9 hours                      ##

library(fasano.franceschini.test)
library(cramer)
library(diproperm)
library(MASS)
library(ggplot2)
set.seed(0)

# Significance level to perform tests at
alpha <- 0.05
# Sample sizes
n <- c(40, 40)
# Number of replicates to use to estimate power
nrep <- 1000
# What dimensions to use
d <- c(seq(2, 8), 12, 16)

title <- "Dispersion"
dist_sample1 <- function(n, d) {
    mvrnorm(n, mu = rep(0, d), Sigma = diag(d))
}
dist_sample2 <- function(n, d) {
    mvrnorm(n, mu = rep(0, d), Sigma = diag(d) + 1.5)
}

# Perform tests
ff_pwr <- rep(0, length(d))
cr_pwr <- rep(0, length(d))
dp_pwr <- rep(0, length(d))
for (i in 1:length(d)) {
    # Generate samples
    S1 <- list()
    S2 <- list()
    for (j in 1:nrep) {
        S1[[j]] <- dist_sample1(n[1], d[i])
        S2[[j]] <- dist_sample2(n[2], d[i])
    }
    clab <- c(rep(1, n[1]), rep(-1, n[2]))
    # Compute empirical power
    for (j in 1:nrep) {
        ff_test <- fasano.franceschini.test(S1[[j]], S2[[j]], threads = 28)
        cr_test <- cramer.test(S1[[j]], S2[[j]])
        
        ff_pwr[i] <- ff_pwr[i] + (ff_test$p.value <= alpha)
        cr_pwr[i] <- cr_pwr[i] + (cr_test$p.value <= alpha)
    
        # This is necessary to stop a ton of messages from printing
        sink <- capture.output(p <- DiProPerm(rbind(S1[[j]], S2[[j]]), clab,
                                              cores = 28)$pvalue)
        # For whatever reason, p-values are returned as strings
        p <- if (p == "<0.001") 0.001 else as.numeric(p)
        dp_pwr[i] <- dp_pwr[i] + (p <= alpha)
    }
    print(paste("Percent complete =", i/length(d)))
}

df <- data.frame(x = d, y_ff = ff_pwr/nrep, y_cr = cr_pwr/nrep,
                 y_dp = dp_pwr/nrep)
pplt <- ggplot(data = df, aes(x = x)) +
    geom_line(aes(y = y_ff, color = "Fasano-Franceschini",
                  linetype = "Fasano-Franceschini")) +
    geom_line(aes(y = y_cr, color = "Cramer", linetype = "Cramer")) +
    geom_line(aes(y = y_dp, color = "DiProPerm", linetype = "DiProPerm")) +
    geom_point(aes(y = y_ff, color = "Fasano-Franceschini",
                   shape = "Fasano-Franceschini", size = "Fasano-Franceschini")) +
    geom_point(aes(y = y_cr, color = "Cramer", shape = "Cramer",
                   size = "Cramer")) +
    geom_point(aes(y = y_dp, color = "DiProPerm", shape = "DiProPerm",
                   size = "DiProPerm")) +
    scale_linetype_manual(
        name = "legend",
        values = c("Fasano-Franceschini" = 1, "Cramer" = 2, "DiProPerm" = 6)
    ) +
    scale_color_manual(
        name = "legend",
        values = c("Fasano-Franceschini" = "blue", "Cramer" = "red", "DiProPerm" = "green")
    ) +
    scale_shape_manual(
        name = "legend",
        values = c("Fasano-Franceschini" = 16, "Cramer" = 17, "DiProPerm" = 18)
    ) +
    scale_size_manual(
        name = "legend",
        values = c("Fasano-Franceschini" = 1.5, "Cramer" = 1.5, "DiProPerm" = 2.0)
    ) +
    geom_hline(yintercept = alpha, linetype = "dashed", color = "gray55") +
    ggtitle(title) +
    xlab(NULL) +
    ylab(NULL) +
    ylim(0, 1) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 13))
saveRDS(pplt, file = "data/fig6b.rds")

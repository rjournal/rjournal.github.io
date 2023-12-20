## This file generates Figure 3 of our manuscript.  ##
## Estimated run time: 2 hours                      ##

# This script requires computation of two different p-values. This is not a feature
# we want on the final CRAN package. Instead, we install an old commit from the Github
# repo. The only difference between this version and the final CRAN version is that
# both p-values are returned.
devtools::install_github("braunlab-nu/fasano.franceschini.test", ref = "8f6d027")

library(fasano.franceschini.test)
library(MASS)
library(ggplot2)
library(latex2exp)
set.seed(1)

n <- c(10, 10)
nrep <- 10^5
alpha <- 0.05

dvals <- seq(2, 10)

err1 <- c()
err2 <- c()
for (i in 1:length(dvals)) {
    print(paste("Percent done =", i/length(dvals)))
    d <- dvals[i]
    pvals1 <- c()
    pvals2 <- c()
    for (j in 1:nrep) {
        S1 <- mvrnorm(n[1], mu = rep(0, d), Sigma = diag(d))
        S2 <- mvrnorm(n[2], mu = rep(0, d), Sigma = diag(d))
        ff_test <- fasano.franceschini.test(S1, S2, threads = 4)
        pvals1[length(pvals1) + 1] <- ff_test$p.value
        pvals2[length(pvals2) + 1] <- ff_test$p.value.old
    }
    err1[i] <- mean(pvals1 <= alpha)
    err2[i] <- mean(pvals2 <= alpha)
}

df <- data.frame(x = dvals, y1 = err2, y2 = err1)
pplt <- ggplot(data = df, aes(x)) +
    geom_point(aes(y = y1, color = "pval1", shape = "pval1"), size = 3) +
    geom_point(aes(y = y2, color = "pval2", shape = "pval2"), size = 3) +
    geom_hline(yintercept = alpha, color = "darkgrey", linetype = 2) +
    scale_color_manual(name = "legend",
                       values = c("pval1" = "blue", "pval2" = "red"),
                       labels = unname(TeX(c("$p_{M}$", "$p_{M}'$")))) +
    scale_shape_manual(name = "legend",
                       values = c("pval1" = 16, "pval2" = 17),
                       labels = unname(TeX(c("$p_{M}$", "$p_{M}'$")))) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    xlab("Dimension") +
    ylab("Type I error rate") +
    ylim(0, NA) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "right",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 13),
          legend.text.align = 0)
ggsave(plot = pplt, filename = "fig3.png", width = 8, height = 5.5, dpi = 450)

# Overwrite the old package with the current version
devtools::install_github("braunlab-nu/fasano.franceschini.test")

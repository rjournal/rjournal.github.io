## This file generates Figure 2c of our manuscript. ##
## Estimated run time: 3 hours                      ##

library(MASS)
library(ggplot2)
library(microbenchmark)
library(fasano.franceschini.test)
set.seed(0)

nrep <- 200
d <- 4
nvals <- c(25, 800, 1600, 2400, 3200, 4000)

times_rt <- rep(0, length(nvals))
times_bf <- rep(0, length(nvals))
for (i in 1:length(nvals)) {
    print(paste("Percent complete =", i/length(nvals)))
    S1 <- mvrnorm(n = nvals[i], mu = rep(0, d), Sigma = diag(d))
    S2 <- mvrnorm(n = nvals[i], mu = rep(0, d), Sigma = diag(d))
    times_rt[i] <- mean(microbenchmark(
        fasano.franceschini.test(S1, S2, nPermute = 0, method = 'r'),
        times = nrep)$time)
    times_bf[i] <- mean(microbenchmark(
        fasano.franceschini.test(S1, S2, nPermute = 0, method = 'b'),
        times = nrep)$time)
}

times_df <- data.frame(x = nvals, y_rt = times_rt/10^9, y_bf = times_bf/10^9)
pplt <- ggplot(data = times_df, aes(x = x)) +
    geom_point(aes(y = y_rt, color = "Range tree", shape = "Range tree"),
               size = 1.9) +
    geom_point(aes(y = y_bf, color = "Brute force", shape = "Brute force"),
               size = 1.9) +
    geom_line(aes(y = y_rt, color = "Range tree", linetype = "Range tree"),
              linewidth = 0.65) +
    geom_line(aes(y = y_bf, color = "Brute force", linetype = "Brute force"),
              linewidth = 0.65) +
    scale_color_manual(name = "legend",
                       values = c("Range tree" = "blue", "Brute force" = "red")) +
    scale_shape_manual(name = "legend",
                       values = c("Range tree" = 16, "Brute force" = 17)) +
    scale_linetype_manual(name = "legend",
                          values = c("Range tree" = "solid",
                                     "Brute force" = "longdash")) +
    xlab("Sample size") +
    ylab("Time (s)") +
    ggtitle("Four dimensions") +
    theme_bw() +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 13),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-10, 0, -16, 0),
          legend.key.size = unit(3, "lines"),
          legend.position = "bottom",
          legend.text = element_text(size = 13),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14))
saveRDS(pplt, file = "data/fig2c.rds")

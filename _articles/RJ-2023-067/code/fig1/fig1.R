## This file generates Figure 1 in our manuscript. ##
## Estimated run time: <10 seconds                 ##

library(MASS)
library(ggplot2)
library(patchwork)
set.seed(10)

S1 <- mvrnorm(n = 10, mu = c(-0.1, -0.1), Sigma = diag(2))
S2 <- mvrnorm(n = 10, mu = c(0.1, 0.1), Sigma = diag(2))

S1_df <- data.frame(x = S1[,1], y = S1[,2])
S2_df <- data.frame(x = S2[,1], y = S2[,2])

npt <- 10
m <- matrix(data = c(c("0.0", "0.0", "0.2", "0.1", "0.7", "0.9", "0.0", "0.0"),
                     c("0.1", "0.0", "0.2", "0.3", "0.4", "0.6", "0.2", "0.1"),
                     c("0.7", "0.2", "0.1", "0.6", "0.0", "0.1", "0.1", "0.1"),
                     c("0.1", "0.0", "0.5", "0.6", "0.3", "0.4", "0.0", "0.0"),
                     c("0.0", "0.0", "0.1", "0.0", "0.4", "0.9", "0.4", "0.1"),
                     c("0.0", "0.0", "0.0", "0.0", "0.3", "0.8", "0.6", "0.2"),
                     c("0.7", "0.4", "0.0", "0.4", "0.0", "0.0", "0.2", "0.2"),
                     c("0.5", "0.0", "0.0", "0.3", "0.2", "0.4", "0.2", "0.3"),
                     c("0.2", "0.0", "0.7", "0.8", "0.0", "0.2", "0.0", "0.0"),
                     c("0.3", "0.0", "0.1", "0.3", "0.3", "0.6", "0.2", "0.1")),
            nrow = npt, byrow = TRUE)
title <- c(expression(paste("D(", X["1"], "|X,Y) = |0.7 - 0.9| = 0.2", sep = '')),
           expression(paste("D(", X["2"], "|X,Y) = |0.4 - 0.6| = 0.2", sep = '')),
           expression(paste("D(", X["3"], "|X,Y) = |0.7 - 0.2| = 0.5", sep = '')),
           expression(paste("D(", X["4"], "|X,Y) = |0.1 - 0.0| = 0.1", sep = '')),
           expression(paste("D(", X["5"], "|X,Y) = |0.4 - 0.9| = 0.5", sep = '')),
           expression(paste("D(", X["6"], "|X,Y) = |0.3 - 0.8| = 0.5", sep = '')),
           expression(paste("D(", X["7"], "|X,Y) = |0.0 - 0.4| = 0.4", sep = '')),
           expression(paste("D(", X["8"], "|X,Y) = |0.5 - 0.0| = 0.5", sep = '')),
           expression(paste("D(", X["9"], "|X,Y) = |0.2 - 0.0| = 0.2", sep = '')),
           expression(paste("D(", X["10"], "|X,Y) = |0.3 - 0.0| = 0.3", sep = '')))
colors <- c("#440154FF", "black", "#B4DE2CFF")

xc <- c(-2.00, -1.75, -1.50, 1.70, 1.95, 2.20)
yc <- c(0.5, -2.3)
plots <- list()
for (i in 1:npt) {
    plots[[i]] <- ggplot() +
        geom_vline(xintercept = S1[i, 1], color = "gray", linetype = "dashed",
                   linewidth = 0.9) +
        geom_hline(yintercept = S1[i, 2], color = "gray", linetype = "dashed",
                   linewidth = 0.9) +
        annotate("text", x = xc[1], y = yc[1], label = m[i, 1], size = 5,
                 color = colors[1]) +
        annotate("text", x = xc[2], y = yc[1], label = "|", size = 5,
                 color = colors[2]) +
        annotate("text", x = xc[3], y = yc[1], label = m[i, 2], size = 5,
                 color = colors[3]) +
        annotate("text", x = xc[4], y = yc[1], label = m[i, 3], size = 5,
                 color = colors[1]) +
        annotate("text", x = xc[5], y = yc[1], label = "|", size = 5,
                 color = colors[2]) +
        annotate("text", x = xc[6], y = yc[1], label = m[i, 4], size = 5,
                 color = colors[3]) +
        annotate("text", x = xc[4], y = yc[2], label = m[i, 5], size = 5,
                 color = colors[1]) +
        annotate("text", x = xc[5], y = yc[2], label = "|", size = 5,
                 color = colors[2]) +
        annotate("text", x = xc[6], y = yc[2], label = m[i, 6], size = 5,
                 color = colors[3]) +
        annotate("text", x = xc[1], y = yc[2], label = m[i, 7], size = 5,
                 color = colors[1]) +
        annotate("text", x = xc[2], y = yc[2], label = "|", size = 5,
                 color = colors[2]) +
        annotate("text", x = xc[3], y = yc[2], label = m[i, 8], size = 5,
                 color = colors[3]) +
        geom_point(data = S1_df, aes(x, y), size = 3, color = colors[1]) +
        geom_point(data = S2_df, aes(x, y), size = 3, color = colors[3]) +
        geom_point(data = S1_df[i,], aes(x, y), pch = 1, size = 5.5,
                   color = "black") +
        ggtitle(title[i]) +
        scale_x_continuous(limits = c(-2.0, 2.2), expand = c(0.04, 0.04)) +
        scale_y_continuous(limits = c(-2.3, 0.5), expand = c(0.02, 0.02)) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
              plot.margin = margin(14, 14, 14, 14))
}

pplt <- (plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]] | plots[[5]]) /
        (plots[[6]] | plots[[7]] | plots[[8]] | plots[[9]] | plots[[10]]) +
        plot_annotation(
            title = expression(paste(bold(D["1"]), bold(" = "), bold(max["i"]),
                                     bold(" D("), bold(X["i"]), bold("|X,Y) = 0.5"),
                                     sep = '')),
            theme = theme(
                plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
                plot.margin = margin(0, 0, 0, -10)
            )
        )

ggsave(plot = pplt, filename = "fig1.png", width = 22, height = 11, dpi = 450)

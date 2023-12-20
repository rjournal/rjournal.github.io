## This file generates Figure 4 of our manuscript. ##
## Estimated run time: <10 seconds                 ##

## NOTE: This script uses the `copula` package, which requires an
## installation of the GNU Scientific Library (GSL).

library(MASS)
library(copula)
library(ggplot2)
library(patchwork)
set.seed(0)

dist_sample1 <- function(param, n) {
    mvrnorm(n, mu = rep(param, 2), Sigma = diag(2))
}
dist_sample2 <- function(param, n) {
    mvrnorm(n, mu = rep(0, 2), Sigma = diag(2) + param)
}
dist_sample3 <- function(param, n) {
    rMvdc(n, mvdc(normalCopula(param, dim = 2), rep("norm", 2),
                  rep(list(list(mean = 0, sd = 1)), 2)))
}
dist_sample4 <- function(param, n) {
    rMvdc(n, mvdc(claytonCopula(param, dim = 2), rep("norm", 2),
                  rep(list(list(mean = 0, sd = 1)), 2)))
}
dist_samples <- list(dist_sample1, dist_sample2, dist_sample3, dist_sample4)
param1 <- c(0, 0, 0, 1)
param2 <- c(0.4, 1.5, 0.6, 8)
n <- 10000

plots <- list()
titles <- c("Location", "Dispersion", "Gaussian copula", "Clayton copula")
for (i in 1:4) {
	S1 <- dist_samples[[i]](param1[i], n)
	S2 <- dist_samples[[i]](param2[i], n)
	plots[[i]] <-
		ggplot() +
		geom_point(data = data.frame(x = S1[,1], y =  S1[,2]), aes(x, y),
		           color = "blue", size = 0.15, alpha = 0.4) +
		geom_point(data = data.frame(x = S2[,1], y =  S2[,2]), aes(x, y),
		           color = "red", size = 0.15, alpha = 0.4) +
		xlab(NULL) +
		ylab(NULL) +
	    ggtitle(titles[[i]]) +
	    theme_bw() +
	    theme(plot.title = element_text(hjust = 0.5),
	          panel.grid.major.x = element_blank(),
	          panel.grid.major.y = element_blank(),
	          panel.grid.minor.x = element_blank(),
	          panel.grid.minor.y = element_blank())
}
pplt <- (plots[[1]] + plots[[2]])/(plots[[3]] + plots[[4]]) +
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')',
					theme = theme(plot.margin = margin(0, 0, 0, -5))) +
	plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(plot = pplt, filename = "fig4.png", width = 11, height = 7.5, dpi = 450)

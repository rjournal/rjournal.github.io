## Figure 1
rm(list = ls())
require(biglasso)
require(ncvreg)
require(glmnet)
require(ggplot2)
require(picasso)

# Assume current working directory set to be folder "biglasso_reproduce/"
# setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
load("./Section_2-2/bcTCGA.RData")
p <- ncol(X)
n <- nrow(X)
x.bm <- as.big.matrix(X, type = 'double')

## replicate figure 1
# ---------------------------------------------------------------------
eps <- 1e-8
lam.min <- 0.1
fit.hsr.bedpp <- biglasso(x.bm, y, family = 'gaussian',
                          screen = "SSR-BEDPP",
                          safe.thresh = 0,
                          lambda.min = lam.min,
                          output.time = F, eps = eps,
                          ncores = 1, lambda.log.scale = F)
fit.edpp <- biglasso(x.bm, y, family = 'gaussian', screen = 'SEDPP',
                     lambda.min = lam.min,
                     eps = eps,
                     lambda.log.scale = F, output.time = F)
fit.hsr <- biglasso(x.bm, y, family = 'gaussian', screen = 'SSR',
                    lambda.min = lam.min,
                    eps = eps,
                    lambda.log.scale = F, output.time = F)
lamb.ratio <- fit.hsr.bedpp$lambda / fit.hsr.bedpp$lambda[1]

rejections <- c(fit.hsr$rejections, fit.edpp$rejections, fit.hsr.bedpp$safe_rejections)
lam.ratio <- lamb.ratio

rej.mean.df <- as.data.frame(matrix(rejections / p, ncol = 1))
names(rej.mean.df) <- 'Reject_percent'
rej.mean.df$lam.ratio <- rep(lam.ratio, 3)
rej.mean.df$Rule <- rep(c("SSR", 'SEDPP', 'BEDPP'), each = 100)
rej.mean.df$Rule <- factor(rej.mean.df$Rule, c("SSR", 'SEDPP', 'BEDPP'))

gp <- ggplot(rej.mean.df, aes(x = lam.ratio, y = Reject_percent, color = Rule)) +
  #   geom_ribbon(aes(ymin = Reject_percent, ymax = 1, linetype = NA,
  #                   fill = Rule),
  #               alpha = 0.4) +
  geom_line(size = 1) +
  xlab(expression(lambda/lambda[max])) +
  ylab("Percent of discarded features") +
  # scale_x_continuous(limits = c(0.1, 1),
  #                    breaks = seq(0.1, 1, by = 0.1)) +
  scale_x_reverse(limits = c(1, 0.09), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  geom_hline(aes(yintercept=1), linetype = 5, color = 'black') +
  # geom_hline(aes(yintercept=0.92), linetype = 5, color = 'red') +
  theme_bw() +
  theme(legend.position = c(.78, .5),
        # axis.text = element_text(size = 16, face = "bold"),
        # axis.title = element_text(size = 16, face = "bold"),
        # legend.title = element_text(size = 16, face = "bold"),
        # legend.text = element_text(size = 16)
        legend.background = element_rect(colour = "darkgray")
  )

pdf(file = paste0("Fig_1_three_rules_breast.pdf"), width = 5, height = 4)
print(gp)
dev.off()

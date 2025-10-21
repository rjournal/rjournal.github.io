library(ggtext)
library(signs)

## Set up "data"
mu <- 100
sigma <- 15
plot_height <- dnorm(mu, mu, sigma)
lb <- -4 * sigma + mu
ub <- 4 * sigma + mu

## Set up main plot
## NOTE need some data for grid_panel() to be called at all
ggSchneider <- ggplot() +
    stat_function(fun=function(x) dnorm(x, mean=mu, sd=sigma),
                  geom="area",
                  n=1000,
                  fill="dodgerblue",
                  alpha=.5) +
    theme_classic(base_family="Roboto Condensed",
                  base_size=18) +
    theme(axis.text.x=element_markdown(),
          axis.title.x=element_markdown(),
          axis.line=element_blank())  +
    scale_x_continuous("Observed Score *X*<br>*z*",
                       breaks=seq(lb, ub, sigma),
                       limits=c(lb, ub),
                       labels=function(x)
                           paste0(signs(x),
                                  "<br>",
                                  ifelse(x == mu,
                                         "<em>&mu;</em>",
                                         paste0(signs((x - mu) / sigma,
                                                      add_plusses=T,
                                                      label_at_zero="none"),
                                                "<em>&sigma;</em>")))) +
    scale_y_continuous(NULL,
                       limits=c(0, plot_height),
                       expand=expansion(),
                       breaks=NULL) 


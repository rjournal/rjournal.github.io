# Working directory needs to be set to `/code` directory
getwd()
# Use renv to synchronize package environment for reproducibility
library(renv)
renv::activate()
# Install project library
renv::restore()
# renv project should be in consistent state
renv::status()
# Load packages
library(fmeffects)
library(iml)
library(randomForest)
library(mlr3verse)
library(mlr3extralearners)
library(ggplot2)
library(furrr)
library(hexbin)
# Colorblind palette:
cbb.palette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data
data(bikes, package = "fmeffects")

# Example of how one would train a random forest using mlr3:
set.seed(123)
forest = lrn("regr.randomForest")
forest$train(as_task_regr(x = bikes, id = "bikes", target = "count"))

# Univariate FMEs of increasing temperature by 5 degrees Celsius:
effects.univariate.temp = fme(
  model = forest,
  data = bikes,
  features = list("temp" = 5),
  ep.method = "envelope")

forest

summary(effects.univariate.temp)

# Searching for observation associated with largest FME:
fme.data = effects.univariate.temp$results
fme.data$temp = bikes[fme.data$obs.id, "temp"] 
max(fme.data$fme) # FME of 2563 additional bike rentals on this day
obs.id = as.integer(fme.data[which.max(fme.data$fme), "obs.id"])
bikes[obs.id, ]

# Figure 3 (univariate FMEs due to increase in temperature by 5 degrees Celsius):
pfme.temp = plot(effects.univariate.temp)
pfme.temp
# ggsave(pfme.temp, file = "../figures/univariate_fme_plot_temp.pdf", width = 5, height = 4)

# Figure 4 (comparison to ICE and PD plot):
mod = Predictor$new(forest, data = bikes)
eff <- FeatureEffect$new(mod, feature = "temp", method = "pdp+ice")
iceplot = eff$plot()
iceplot = ggplot(
  iceplot$data,
  aes(
    x = temp, y = .value, group = .id, linewidth = .type, color = .type, 
    linetype = .type)) +
  geom_line() +
  scale_linewidth_manual("", labels = c("ICE", "PD"), values = c(0.2, 2)) +
  scale_linetype_manual("", labels = c("ICE", "PD"), values = c("solid", "dashed")) +
  scale_color_manual("", labels = c("ICE", "PD"), values = c(cbb.palette[3], cbb.palette[1])) +
  ylab("Predicted target") +
  theme_bw()
iceplot
# ggsave(iceplot, file = "../figures/iceplot.pdf", width = 5, height = 4)

# Figure 5 (univariate FME due to decrease in humidity by 10 percentage points):
effects.univariate.humidity = fme(
  model = forest,
  data = bikes,
  features = list("humidity" = -0.1),
  ep.method = "envelope")

summary(effects.univariate.humidity)

pfme.humidity = plot(effects.univariate.humidity)
pfme.humidity
# ggsave(pfme.humidity, file = "../figures/univariate_fme_plot_humidity.pdf", width = 5, height = 4)

# Computing bivariate FMEs of increasing temperature by 5 degrees Celsius
# and lowering humidity by 10 percentage points:
effects.bivariate = fme(
  model = forest,
  data = bikes,
  features = list("temp" = 5, "humidity" = -0.1),
  ep.method = "envelope")

summary(effects.bivariate)

# Figure 6 (bivariate FMEs):
pbiv = plot(effects.bivariate)
pbiv
# ggsave(pbiv, filename = "../figures/pbiv.pdf", width = 5, height = 4)

# Figure 7 (comparison to bivariate PD plot):
eff.biv <- FeatureEffect$new(mod, feature = c("temp", "humidity"), method = "pdp")
pdplot.biv = eff.biv$plot() + theme_bw()
pdplot.biv
# ggsave(pdplot.biv, file = "../figures/pdplot_bivariate.pdf", width = 5, height = 4)

# Adding NLM computations to bivariate FMEs:
# Note: NLM computations take some time
effects.bivariate.nlm = fme(
  model = forest,
  data = bikes,
  features = list("temp" = 5, "humidity" = -0.1),
  ep.method = "envelope",
  compute.nlm = TRUE)

effects.bivariate.nlm

# Figure 8 (adding NLM visualization to bivariate FME plot):
pbiv.nlm = plot(effects.bivariate.nlm, with.nlm = TRUE)
pbiv.nlm
# ggsave(pbiv.nlm, filename = "../figures/pbiv_nlm.pdf", width = 10, height = 4)


# Subsetting linear FMEs

linear.set = effects.bivariate.nlm$results[(effects.bivariate.nlm$results$nlm > 0.9), ]
linear.set$temp = bikes$temp[linear.set$obs.id]
linear.set$humidity = bikes$humidity[linear.set$obs.id]

min.x1 = min(linear.set$temp)
max.x1 = max(linear.set$temp)

min.x2 = min(linear.set$humidity)
max.x2 = max(linear.set$humidity)

range.x1 = diff(range(linear.set$temp))
range.x2 = diff(range(linear.set$humidity))

ggplot2::ggplot(linear.set, ggplot2::aes(x = temp, y = humidity)) +
  ggplot2::stat_summary_hex(ggplot2::aes(z = fme), fun = mean) +
  ggplot2::scale_fill_gradient2(
    name = "FME",
    low = "#D55E00", mid = "#ECF4F9", high = "#0072B2",
    midpoint = 0,
    breaks = function(x) {pretty(x, n = 5)}
  ) +
  ggplot2::xlim(min.x1 - 0.06 * range.x1, NA) +
  ggplot2::ylim(min.x2 - 0.06 * range.x2, NA) +
  ggplot2::geom_rug(length = ggplot2::unit(0.015, "npc")) +
  ggplot2::xlab("temp") +
  ggplot2::ylab("humidity") +
  ggplot2::theme_bw() +
  ggplot2::annotate("segment", x = (0.5 * min.x1 + 0.5 * max.x1 - 0.5 * 5),
                    xend = (0.5 * min.x1 + 0.5 * max.x1 + 0.5 * 5),
                    y = min.x2 - 0.06 * range.x2,
                    yend = min.x2 - 0.06 * range.x2,
                    colour = 'black', linewidth = 1,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                    lineend = "round", linejoin = "mitre") +
  ggplot2::annotate("segment", y = (0.5 * min.x2 + 0.5 * max.x2 - 0.5 * -0.1),
                    yend = (0.5 * min.x2 + 0.5 * max.x2 + 0.5 * -0.1),
                    x = min.x1 - 0.06 * range.x1,
                    xend = min.x1 - 0.06 * range.x1,
                    colour = 'black', linewidth = 1,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                    lineend = "round", linejoin = "mitre") +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.7),
                 axis.title = ggplot2::element_text(size = 12),
                 axis.text   = ggplot2::element_text(colour = "black", size = 10),
                 legend.title = ggplot2::element_text(color = "black", size = 12),
                 legend.text = ggplot2::element_text(color = "black", size = 10))

# ggsave(filename = "../figures/linear_fme_bivariate.pdf", width = 5, height = 4)

# Figure 9 (trivariate FME with additional decrease in windspeed by 5mph):
effects.trivariate.nlm = fme(
  model = forest,
  data = bikes,
  features = list("temp" = 5, "humidity" = -0.1, "windspeed" = -5),
  ep.method = "envelope",
  compute.nlm = TRUE)

summary(effects.trivariate.nlm)

ptriv.nlm = plot(effects.trivariate.nlm, with.nlm = TRUE)
ptriv.nlm
# ggsave(ptriv.nlm, file = "../figures/ptriv_nlm.pdf", width = 10, height = 4)

# Figure 8 (computing categorical FMEs of switching weather status to rain):
effects.categ = fme(
  model = forest,
  data = bikes,
  features = list("weather" = "rain"))

summary(effects.categ)

p.categ = plot(effects.categ)
p.categ
# ggsave(p.categ, filename = "../figures/pcateg.pdf", width = 5, height = 4)


# Figure 9 (visualizing tree with subgroups and cAMEs):
subspaces = came(effects = effects.univariate.temp, number.partitions = 2) # searching for exactly 2 subgroups
summary(subspaces)

psubspaces = plot(subspaces)
psubspaces
# ggsave(psubspaces, file = "../figures/subspaces_plot.pdf", width = 8, height = 5)

# Compact model summary with AMEs:
ame.results = ame(model = forest, data = bikes)
summary(ame.results)


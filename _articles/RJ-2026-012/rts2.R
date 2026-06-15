##############################
# Reproduction materials for #
# Watson (2024) rts2  0.10.2 #
# Version: 6                 #
# Date: 14 Jan 2026          #
##############################

# Load required package
if(!require(rts2))install.packages("rts2") # load or install package from CRAN

####################################################
# EXAMPLE 0: A very basic, minimal example         #
####################################################
# Note that this example is quick, but completely meaningless and just showcases the basic workflow

b1 <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0))))))
g1 <- grid$new(b1,0.5)
dp <- data.frame(y=runif(10,0,3),x=runif(10,0,3),date=paste0("2021-01-",11:20))
dp <- create_points(dp,pos_vars = c('y','x'),t_var='date')
cov1 <- grid$new(b1,0.8)
cov1$grid_data$cov <- runif(nrow(cov1$grid_data))
g1$add_covariates(cov1$grid_data,
                  zcols="cov")
g1$points_to_grid(dp, laglength=5)
g1$lgcp_ml(popdens="cov")
fit <- g1$model_fit()
summary(fit)
g1$extract_preds("rr")
g1$hotspots(threshold = 1.2, stat = "rr")

# plot some of the outputs
g1$plot("rr")
g1$plot("hotspot_prob")


# Main examples - Data

# Load the example data into the global environment

data("example_points")# point data
data("boundary") # boundary
data("birmingham_crime") # covariates and region outcome data

####################################################
# EXAMPLE 1: Spatial only analysis with point data #
####################################################

# to visualise the boundary
plot(boundary)

# create a new grid object with the boundary
g1 <- grid$new(boundary,cellsize=0.008)

# plot the grid
g1$plot()

# add the points to the grid
# the CRS of the generated point data will need updating, the function
# will do this automatically  but generate a warning.
g1$points_to_grid(point_data = create_points(example_points,
                                             pos_vars = c("Y","X")))

# we can plot the new counts
g1$plot("y")

# now we will add the population density data, 
# we will use the population counts from the MSOAs described in 
# the Birmingham crime data used in the second example
# however these data are in Eastings and Northings - we will 
#'generate a new transformed data set
msoa <- sf::st_transform(birmingham_crime,crs = 4326)
sf::st_crs(msoa) <- sf::st_crs(g1$grid_data) # ensure crs matches
g1$add_covariates(msoa,
                  zcols="pop",
                  weight_type="area")

# plot the new population data
g1$plot("pop")

# start with the maximum likelihood model fit 
fit_ml <- g1$lgcp_ml("pop")

g1$extract_preds(type = c("rr"), popdens = "pop")
g1$plot("rr")

# or to use ggplot, an example would be as follows. We use a colour palette from 
# the package scico as used in the article.
require(ggplot2)

p1 <- ggplot(g1$grid_data)+
  geom_sf(aes(fill = rr),color=NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  theme_bw()+
  ggtitle("RR ML")

# bayesian model example with approximation

# for the Bayesian model fits we set some prior values
g1$priors <- list(
  prior_lscale=c(0,1),
  prior_var=c(1,1),
  prior_linpred_mean=c(0),
  prior_linpred_sd=c(3)
)

fit_mcmc_hsgp <- g1$lgcp_bayes("pop",
                               approx = "hsgp", 
                               m = 10, 
                               L = 1.5)

# rename the ml fit variables 
colnames(g1$grid_data)[(ncol(g1$grid_data)-1):ncol(g1$grid_data)] <- c("rr_ml","rr_ml_se")

# extract the Bayesian HSGP relative risk predictions
g1$extract_preds(type = c("rr"), popdens = "pop")

p2 <- ggplot(g1$grid_data)+
  geom_sf(aes(fill = rr),color=NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  theme_bw()+
  ggtitle("RR Bayes (HSGP)")

# finally, try the NNGP approximation - this is the slowest approximation
# we reorder the computational grid for the NNGP model fits
g1$reorder("minimax")
fit_mcmc_nngp <- g1$lgcp_bayes("pop",
                               approx = "nngp",
                               m = 10)

# rename the ml fit variables 
colnames(g1$grid_data)[(ncol(g1$grid_data)-1):ncol(g1$grid_data)] <- c("rr_hsgp","rr_hsgp_se")

# extract the Bayesian HSGP relative risk predictions
g1$extract_preds(type = c("rr"), popdens = "pop")

p3 <- ggplot(g1$grid_data)+
  geom_sf(aes(fill = rr),color=NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  theme_bw()+
  ggtitle("RR Bayes (NNGP)")

require(patchwork)
layout <- "
ab
c#
"
p1 + p2 +  p3 +plot_layout(design = layout)

########################################################################
# EXAMPLE 2: Spatial and spatio-temporal example with spatially aggregated counts  #
########################################################################
# The data `msoa` created above includes counts of burglaries for the middle-layer super 
# output areas of Birmingham, UK for each month of 2022 in the columns t1 - t12:
head(msoa)
# note that we use the data reprojected to longitude and latitude as this is more 
# familiar to most users. We can use the original data in Eastings and Northings but
# changing the cellsize appropriately below.
sf_use_s2(FALSE) # this is set to prevent an error due to clipping boundaries with spherical geometry

# first consider only a spatial model
g2 <- grid$new(msoa,cellsize = 0.008) 
g2$region_data$y <- g2$region_data$t12
g2$region_data <- g2$region_data[,-c(9:20)]
fitt <- g2$lgcp_bayes("pop",
                      approx = "hsgp", 
                      m = 15,
                      L = 1.5,
                      iter_warmup = 100,chains = 1,
                      iter_sampling = 250)
g2$extract_preds(type = c("rr"),popdens = "pop")
g2$hotspots(threshold = 1.5, stat = "rr", popdens = "pop")
colnames(g2$grid_data)[(ncol(g2$grid_data)-2):ncol(g2$grid_data)] <- c("rr_hsgp", "rr_hsgp_sd","hotspot_prob_hsgp")
colnames(g2$region_data)[c(ncol(g2$region_data)-1,ncol(g2$region_data))] <- c("irr_hsgp", "irr_hsgp_sd")

p2 <- ggplot(g2$grid_data)+
  geom_sf(aes(fill=rr_hsgp), color = NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  ggtitle("RR Bayes (HSGP)")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

g3 <- g2$aggregate_output(new_geom = g2$region_data,zcols = "rr_hsgp",verbose = TRUE)

p2a <- ggplot(g3)+
  geom_sf(aes(fill=rr_hsgp), color = NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  ggtitle("RR Bayes (HSGP) Aggregated")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

p2b <- ggplot(g2$grid_data)+
  geom_sf(aes(fill=hotspot_prob_hsgp), color = NA)+
  scico::scale_fill_scico(name = "Prob.", palette = "vik",limits = c(0,1))+
  ggtitle("Pr(RR > 1.5) Bayes (HSGP)")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

## now let's compare with the full ML model with all 12 time periods

g2 <- grid$new(msoa,cellsize = 0.008) 
# we will add time period indicators to the data
g2$add_time_indicators()

# fit the data with the matern-1 model
# on a laptop, this may take around 10 minutes - the slowest step is updating the
# samples of the random effects - times will be reported
fit2_ml <- g2$lgcp_ml("pop",
                      model = "matern1",
                       covs = paste0("time",2:12,"i"),
                       iter_sampling = 100)

# extract the predictions - relative risk and IRR relative to 11 months prior. 
# we rename the columns in the data to accommodate predictions from different
# model fits.

# HSGP
g2$model_fit(fit2_ml)
g2$extract_preds(type = c("rr","irr"),popdens = "pop",irr.lag = 11)
g2$hotspots(threshold = 1.5, stat = "rr", popdens = "pop")
colnames(g2$grid_data)[(ncol(g2$grid_data)-2):ncol(g2$grid_data)] <- c("rr_ml", "rr_ml_se", "hotspot_prob_ml")
colnames(g2$region_data)[c(ncol(g2$region_data)-1,ncol(g2$region_data))] <- c("irr_ml", "irr_ml_sd")


## aggregation of the grid data to the region data geography
g3 <- g2$aggregate_output(new_geom = g3,zcols = "rr_ml",verbose = TRUE)

## generate the plots shown in the article using ggplot2
require(ggplot2)

p1 <- ggplot(g2$grid_data)+
  geom_sf(aes(fill=rr_ml), color = NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  ggtitle("RR ML")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

p1a <- ggplot(g3)+
  geom_sf(aes(fill=rr_ml), color = NA)+
  scico::scale_fill_scico(name = "RR", palette = "vik")+
  ggtitle("RR ML Aggregated")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

p1b <- ggplot(g2$grid_data)+
  geom_sf(aes(fill=hotspot_prob_ml), color = NA)+
  scico::scale_fill_scico(name = "Prob.", palette = "vik",limits = c(0,1))+
  ggtitle("Pr(RR > 1.5) ML")+
  theme_bw()+
  theme(axis.text = element_blank(), panel.grid = element_blank())

require(patchwork)

q1 <- (p1 + p2) / (p1a + p2a) #+ plot_layout(guides = "collect")

q2 <- (p1b + p2b) + plot_layout(guides = "collect")

q1 / q2

############################################
# EXAMPLES COMPARING INLA/INLABRU AND RTS2 #
############################################

# INLA can be downloaded and installed, see https://www.r-inla.org/download-install

# gorillas data
# for more information on this example see: https://inlabru-org.github.io/inlabru/articles/2d_lgcp_covars.html
require(inlabru)
require(ggplot2)

data <- gorillas_sf
data$gcov <- gorillas_sf_gcov()

# mean standardise the elevation data
elev <- data$gcov$elevation
data$gcov$elev <- elev - mean(terra::values(elev), na.rm = TRUE)

# function to return the elevation at the model locations
f.elev <- function(where) {
  v <- eval_spatial(data$gcov$elev, where, layer = "elev")
  v
}

# priors
matern <- INLA::inla.spde2.pcmatern(data$mesh,
                                    prior.sigma = c(0.1, 0.01),
                                    prior.range = c(0.1, 0.01)
)

# specify the formula
ecomp <- geometry ~ elev(f.elev(.data.), model = "linear") +
  mySmooth(geometry, model = matern) + Intercept(1)

# fit the model
efit <- lgcp(ecomp, data$nests, samplers = data$boundary, domain = list(geometry = data$mesh))

# extract the predictions
pred.df <- fm_pixels(data$mesh, mask = data$boundary)
e.pred <- predict(
  efit,
  pred.df,
  ~ list(
    int = exp(mySmooth + elev + Intercept),
    int.log = mySmooth + elev + Intercept,
    rr = exp(mySmooth)
  )
)

# plot the results
p1 <- ggplot() +
  gg(e.pred$int, aes(fill = mean), geom = "tile") +
  scale_fill_gradientn(
    name = "Intensity",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("inlabru LGCP Intensity")

p1b <- ggplot() +
  gg(e.pred$int, aes(fill = log(sd)), geom = "tile") +
  scale_fill_gradientn(
    name = "log(SD)",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("inlabru LGCP Intensity")

p2 <- ggplot() +
  gg(e.pred$rr, aes(fill = mean), geom = "tile") +
  scale_fill_gradientn(
    name = "RR",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("inlabru LGCP RR")

p2b <- ggplot() +
  gg(e.pred$rr, aes(fill = log(sd)), geom = "tile") +
  scale_fill_gradientn(
    name = "log(SD)",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("inlabru LGCP RR")


## rts2
# set up a new model
mod <- grid$new(
  data$boundary,
  cellsize = 0.075
)
# map the point locations
mod$points_to_grid(data$nests)
# add elevation covariate
mod$add_covariates(cov_data = data$gcov$elev,zcols = "elev")
mod$plot("elev")
# fit the model
fit3 <- mod$lgcp_ml(covs = c("elev"))
#extract the predictions
mod$extract_preds(c("pred","rr"))

# plot the results
p3 <- ggplot() +
  gg(mod$grid_data, aes(fill = pred_mean_total), geom = "tile") +
  scale_fill_gradientn(
    name = "Intensity",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP Intensity")

p3b <- ggplot() +
  gg(mod$grid_data, aes(fill = log(pred_mean_pp_se)), geom = "tile") +
  scale_fill_gradientn(
    name = "log(SD)",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP Intensity")

p4 <- ggplot() +
  gg(mod$grid_data, aes(fill = rr), geom = "tile") +
  scale_fill_gradientn(
    name = "RR",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP RR")

p4b <- ggplot() +
  gg(mod$grid_data, aes(fill = log(rr_se)), geom = "tile") +
  scale_fill_gradientn(
    name = "log(SD)",
    colors = colorRamps::blue2red(10)
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP RR")

require(patchwork)

p1 + p1b + p2 + p2b + p3 + p3b + p4 + p4b + plot_layout(ncol = 2)

## INLA SIDs data example

nc.sids <- sf::st_read(system.file("shapes/sids.gpkg", package="spData")[1])

# process the first time period data
r74 <- sum(nc.sids$SID74) / sum(nc.sids$BIR74)
nc.sids$EXP74 <- r74 * nc.sids$BIR74
nc.sids$SMR74 <- nc.sids$SID74 / nc.sids$EXP74

# Second time period
r79 <- sum(nc.sids$SID79) / sum(nc.sids$BIR79)
nc.sids$EXP79 <- r79 * nc.sids$BIR79
nc.sids$SMR79 <- nc.sids$SID79 / nc.sids$EXP79

d <- data.frame(OBS = c(nc.sids$SID74, nc.sids$SID79),
                PERIOD = c(rep("74", 100), rep("79", 100)), 
                gr = c(rep(1, 100), rep(2, 100)), 
                EXP = c(nc.sids$EXP74, nc.sids$EXP79))

# County-period index
d$idx <- 1:length(d$OBS)
d$idarea <- rep(1:100,2)

nb <- spdep::poly2nb(nc.sids)
spdep::nb2INLA("map.adj",nb)

require(INLA)

g <- inla.read.graph(filename = "map.adj")

res <- inla(OBS ~ f(idarea, model = "bym2", graph = g) + 
              PERIOD,
            family = "poisson", 
            data = d,
            E = EXP, 
            control.predictor = list(compute = TRUE))

nc.sids$rr_inla <- res$summary.random$idarea[, "mean"][101:200]
nc.sids$rr_lci_inla <- res$summary.random$idarea[, "0.025quant"][101:200]
nc.sids$rr_uci_inla <- res$summary.random$idarea[, "0.975quant"][101:200]
nc.sids$prob_inla <- 1 - pnorm(log(1.5), nc.sids$rr_inla, res$summary.random$idarea[, "sd"][101:200])
nc.sids$pred_inla <- res$summary.fitted.values[, "mean"][101:200]


#rts 2
mod <- grid$new(nc.sids, cellsize = 0.15) # new grid object
colnames(mod$region_data)[c(10,13)] <- c("t1","t2") # rename outcomes for model fitting
colnames(mod$region_data)[c(24,26)] <- c("exp1","exp2") # rename covariates for model fitting
mod$add_time_indicators()
mod$add_covariates(mod$region_data, zcols = c("exp1", "exp2"))
mod$plot("exp2")
res3 <- mod$lgcp_ml("exp", model = "fexp", covs = c("time2i"), iter_sampling = 250)

summary(res3)
mod$extract_preds(c("pred","rr"))
mod$hotspots(threshold = 1.5, stat = "rr")
# generate a fitted value net of offset
# mod$grid_data$pred_fitted <- mod$grid_data$pred_mean_pp/mod$grid_data$exp2

# generate confidence intervals for prediction using the standard errors
mod$region_data$pred_lci <- mod$region_data$pred_mean_total - 
  qnorm(0.975)*mod$region_data$pred_mean_total_se
mod$region_data$pred_lci <- ifelse(mod$region_data$pred_lci >= 0, mod$region_data$pred_lci, 0)
mod$region_data$pred_uci <- mod$region_data$pred_mean_total + 
  qnorm(0.975)*mod$region_data$pred_mean_total_se

# attach to the original data file
nc.sids$pred <- mod$region_data$pred_mean_total
nc.sids$pred_lci <- mod$region_data$pred_lci
nc.sids$pred_uci <- mod$region_data$pred_uci

## visualise all the results
require(colorRamps)


p1 <- ggplot(mod$grid_data) +
  geom_sf(aes(fill = log(rr)), color = NA)+
  scale_fill_gradientn(
    name = "Log RR",
    colors = blue2red(10),
    limits = c(-1.5, 2.75),
    values = scales::rescale(c(seq(0.4,1,length.out = 5),seq(1.1,2.1,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP")

p2 <- ggplot(nc.sids) +
  geom_sf(aes(fill = rr_inla), color = NA)+
  scale_fill_gradientn(
    name = "Log RR",
    colors = blue2red(10), 
    limits = c(-1.5, 2.75),
    values = scales::rescale(c(seq(0.4,1,length.out = 5),seq(1.1,2.1,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("INLA LGCP")

q1 <- p1 + p2 + plot_layout(guides = "collect") #&

p1a <- ggplot(mod$grid_data) +
  geom_sf(aes(fill = pred_mean_pp), color = NA)+
  scale_fill_gradientn(
    name = "RR",
    colors = blue2red(10) ,
    limits = c(0,3),
    values = scales::rescale(c(seq(0.4,1.0,length.out = 5),seq(1.1,3.1,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP, SMR")

p2a <- ggplot(nc.sids) +
  geom_sf(aes(fill = pred_inla), color = NA)+
  scale_fill_gradientn(
    name = "RR",
    colors = blue2red(10) ,
    limits = c(0,3),
    values = scales::rescale(c(seq(0.4,1.0,length.out = 5),seq(1.1,3.1,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("INLA LGCP, SMR")

q2 <- p1a + p2a + plot_layout(guides = "collect")

p1b <- ggplot(mod$grid_data) +
  geom_sf(aes(fill = hotspot_prob), color = NA)+
  scale_fill_gradientn(
    name = "RR",
    colors = blue2red(10), 
    limits = c(0,1),
    values = scales::rescale(c(seq(0,0.5,length.out = 5),seq(0.5,1.0,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("rts2 LGCP, Pr(RR > 1.5)")

p2b <- ggplot(nc.sids) +
  geom_sf(aes(fill = prob_inla), color = NA)+
  scale_fill_gradientn(
    name = "RR",
    colors = blue2red(10), 
    limits = c(0, 1),
    values = scales::rescale(c(seq(0,0.5,length.out = 5),seq(0.5,1.0,length.out = 5)))
  )+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank())+
  ggtitle("INLA LGCP, Pr(RR > 1.5)")

q3 <- p1b + p2b + plot_layout(guides = "collect")


q1 / q2 / q3


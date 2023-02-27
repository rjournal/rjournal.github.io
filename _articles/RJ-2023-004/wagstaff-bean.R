library(maps)
library(gridExtra)
library(nngeo)
library(cowplot)

# ==============================================================================
# CODE EXAMPLES IN TEXT (INCLUDING FIGURE 7)
# ==============================================================================
library(tidyverse)
library(sf)
library(raster)
library(mgcv)
library(automap)
library(gstat)
library(remap)


# Load supplementary data
load("wagstaff-bean.RData")

# Simplify polygons
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 10000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

# Make distance matrix
eco3_dist <- redist(loads, regions = eco3_simp, region_id = ECO3, 
                    progress = TRUE)

# Linear model
lmod <- remap(loads, regions = eco3_simp, region_id = ECO3,
              buffer = 50, min_n = 150,
              distances = eco3_dist,
              model_function = stats::lm,
              formula = log(EVENT50) ~ ELEVATION,
              progress = TRUE)

lmod

head(names(lmod$models), 3)

class(lmod$models[[1]])

head(lmod$regions, 3)

# GAM
gam_wrap <- function(data, ...) {
  model <- mgcv::gam(data, ...)
  class(model) <- "gam_wrap"
  return(model)
}

predict.gam_wrap <- function(object, data, se.fit = FALSE) {
  class(object) <- "gam"
  if (se.fit) {
    # return standard errors
    return(predict(object, data, se.fit = TRUE)$se.fit)
  } else {
    # return predictions
    return(predict(object, data))
  }
}

gm <- remap(loads,
            regions = eco3_simp, region_id = ECO3,
            buffer = 50, min_n = 150,
            distances = eco3_dist,
            model_function = gam_wrap,
            formula = log(EVENT50) ~ s(ELEVATION, k = 15) +
              s(LATITUDE, LONGITUDE, bs = 'sos', k = 75),
            family = gaussian,
            progress = TRUE)

predict(gm, loads[1:3, ], smooth = 25)

predict(gm, loads[1:3, ], smooth = 25, se = TRUE, se.fit = TRUE)

# Kriging model functions
projection <- sf::st_crs("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

krig <- function(data, formula) {
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  out <- list(data = data, formula = formula)
  class(out) <- "krig"
  
  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) == 0) return(NULL)
  
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  variogram_object <- automap::autofitVariogram(
    formula = object$formula,
    input_data = object$data,
    model = "Sph")
  
  k <- gstat::krige(formula = object$formula,
                    locations = object$data,
                    newdata = data,
                    model = variogram_object$var_model,
                    debug.level = 0)
  
  return(k$var1.pred)
}

kg <- remap(loads,
            regions = eco3_simp, region_id = ECO3,
            buffer = 50, min_n = 150,
            distances = eco3_dist,
            model_function = krig,
            formula = log(EVENT50) ~ ELEVATION,
            progress = TRUE)

loads_us <- sf::st_filter(loads, cont_us)

kg_preds <- exp(predict(kg, loads_us, smooth = 25, progress = TRUE))

all(dplyr::near(kg_preds, loads_us$EVENT50))

# Distances and predictions
ag_grd <- grd %>%
  raster::aggregate(9) %>%
  raster::rasterToPoints() %>%
  as.data.frame() %>%
  mutate(LONGITUDE = x, LATITUDE = y) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  filter(sf::st_intersects(., cont_us, sparse = FALSE)[,1])

grd_dist <- redist(ag_grd, regions = eco3_simp, region_id = ECO3,
                   max_dist = 25, progress = TRUE)

gm_preds <- predict(gm, ag_grd, smooth = 25, distances = grd_dist, 
                    progress = TRUE)
gm_preds <- exp(gm_preds)

range(gm_preds)
range(loads$EVENT50)

gm_preds[gm_preds > 40.2] <- 40.2
gm_preds[gm_preds < 0.1] <- 0.1

# Figure 7
ggplot(cont_us) +
  geom_tile(data = ag_grd %>% dplyr::mutate(EVENT50 = gm_preds),
            aes(x = LONGITUDE, y = LATITUDE, fill = EVENT50)) +
  geom_sf(fill = NA, color = NA) +
  scale_fill_viridis_c(option = "inferno",
                       trans = "log10",
                       breaks = c(0.1, 1, 7, 40),
                       labels = c("0.1 kPa", "1 kPa", "7 kPa", "40 kPa"),
                       name = "50-year event") +
  theme_void()

# Utah snowpack data
utsp2011 <- utapr1 %>%
  dplyr::filter(YEAR == 2011) %>%
  dplyr::mutate(WESD = WESD + 1)

utlmod <- remap(utsp2011,
                regions = utws, region_id = HUC2,
                buffer = 20, min_n = 30,
                model_function = stats::lm,
                formula = log(WESD) ~ ELEVATION,
                progress = TRUE)

utgm <- remap(utsp2011,
              regions = utws, region_id = HUC2,
              buffer = 20, min_n = 30,
              model_function = mgcv::gam,
              formula = log(WESD) ~ s(ELEVATION, k = 5) +
                s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
              family = gaussian,
              progress = TRUE)

utkg <- remap(utsp2011,
              regions = utws, region_id = HUC2,
              buffer = 20, min_n = 30,
              model_function = krig,
              formula = log(WESD) ~ ELEVATION,
              progress = TRUE)





# ==============================================================================
# CODE FOR FIGURES 1, 3, and 4
# ==============================================================================

# Data for figures 1, 3, and 4
regions_example <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 2, 0, 6, 3, 4, 10, 0, 10, 0, 0)*.1, 
                                ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(2, 0, 10, 0, 10, 4, 6, 3, 2, 0)*.1, 
                                ncol = 2, byrow = TRUE))),
  3, sf::st_polygon(list(matrix(c(4, 10, 6, 3, 10, 4, 10, 10, 4, 10)*.1, 
                                ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

regions_gap <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 2, 0, 5.5, 2.625, 0, 10, 0, 0)*.1, 
                                ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(2, 0, 10, 0, 10, 4, 6, 3, 2, 0)*.1, 
                                ncol = 2, byrow = TRUE))),
  3, sf::st_polygon(list(matrix(c(8, 10, 6.5, 3.125, 10, 4, 10, 10, 8, 10)*.1, 
                                ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

regions_nondif <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 10, 0, 10, 10, 9, 10, 5, 1, 1, 10, 0, 
                                  10, 0, 0)*.1, 
                                ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(1, 10, 5, 1, 9, 10, 1, 10)*.1, 
                                ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

res <- 100
points <- data.frame(
  LONGITUDE = rep(0:res / res, times = res + 1),
  LATITUDE = rep(0:res / res, each = res + 1)
) %>%
  dplyr::mutate(x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)



# Make dummy models for figures 1, 3, and 4
make_model <- function(x, const) {
  if (missing(const)) {
    class(x) <- "dummy_model"
  } else {
    class(x) <- "dummy_const"
  }
  return(x)
}
predict.dummy_model <- function(object, data) {
  x <- sf::st_coordinates(data)[, "X"]
  y <- sf::st_coordinates(data)[, "Y"]
  if (object[[1]] == 1) {
    x - y + 1
  } else if (object[[1]] == 2) {
    y - x + 1.4
  } else {
    x - y + 0.7
  }
}
predict.dummy_const <- function(object, data) {
  rep(object[[1]], nrow(data))
}

model_example <- list(
  models = list("1" = make_model(1),
                "2" = make_model(2),
                "3" = make_model(3)),
  regions = regions_example,
  region_id = "value"
)
class(model_example) <- "remap"

model_gap <- list(
  models = list("1" = make_model(0, "const"),
                "2" = make_model(2, "const"),
                "3" = make_model(4, "const")),
  regions = regions_gap,
  region_id = "value"
)
class(model_gap) <- "remap"

model_nondif <- list(
  models = list("1" = make_model(1, "const"),
                "2" = make_model(2, "const")),
  regions = regions_nondif,
  region_id = "value"
)
class(model_nondif) <- "remap"

points <- points %>%
  dplyr::mutate(example = predict(model_example, points, 0.01),
                example_smooth = predict(model_example, points, 30),
                gap = predict(model_gap, points, 35),
                nondif = predict(model_nondif, points, 40))


# Plot figure 1
png("example.png", width = 880, height = 750)
gridExtra::grid.arrange(
  ggplot(regions_example) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = example)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.7, 0.7)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle("Regional models without smoothing"),
  
  ggplot(points %>% filter(LATITUDE == 0.7),
         aes(x = LONGITUDE, y = example)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    ggtitle("") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  
  ggplot2::ggplot(regions_example) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = example_smooth)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.7, 0.7)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         name = "Prediction", 
                         limits = c(0, 1.25)) +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle("Regional models with smoothing"),
  
  ggplot(points %>% filter(LATITUDE == 0.7),
         aes(x = LONGITUDE, y = example_smooth)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    ggtitle("") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()

# Plot figure 3
png("gap.png", width = 880, height = 360)
gridExtra::grid.arrange(
  ggplot(regions_gap) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = gap)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.6, 0.6)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.95, 0.95)),
              aes(x = x, y = y), color = "gray40", size = 1.1,
              linetype = "dashed") +
    scale_fill_viridis_c(option = "inferno",
                         begin = .3, end = 1,
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)),

  ggplot(points %>% filter(LATITUDE == 0.6),
         aes(x = LONGITUDE, y = gap)) +
    geom_line(color = "gray40", size = 1.1) +
    geom_line(data = points %>% filter(LATITUDE == 0.95),
              aes(x = LONGITUDE, y = gap),
              color = "gray40", size = 1.1,
              linetype = "dashed") +
    ylab("Prediction") +
    xlab("Longitude") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()

# Plot figure 4
png("nondiff.png", width = 880, height = 360)
gridExtra::grid.arrange(
  ggplot(regions_nondif) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = nondif)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.5, 0.5)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         begin = .3, end = 1,
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)),

  ggplot(points %>% filter(LATITUDE == 0.5),
         aes(x = LONGITUDE, y = nondif)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()





# ==============================================================================
# CODE FOR FIGURE 2
# ==============================================================================
eco3_subset <- eco3 %>%
  dplyr::filter(ECO3 %in% c("10.1.4", "10.1.6", "10.1.7", "6.2.14", 
                            "9.4.1",  "9.4.1",  "9.4.3")) %>%
  dplyr::mutate(AREA = sf::st_area(.)) %>%
  dplyr::filter(as.numeric(AREA) > 2e9) %>%
  nngeo::st_remove_holes() 

set.seed(40)
contrived_points <- data.frame(
  LAT = runif(350, 31, 45),
  LON = runif(350, -114, -98)
) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  dplyr::filter(apply(sf::st_within(., eco3_subset, sparse = FALSE), 1, any))

mnt <- eco3_subset %>% filter(ECO3 == "6.2.14")

# st_buffer uses meters
mnt_buf <- mnt %>% sf::st_buffer(50000) %>%
  sf::st_union() %>%
  sf::st_as_sf()

points_buf <- contrived_points %>%
  filter(apply(sf::st_within(contrived_points, mnt_buf, sparse = FALSE), 
               1, any))

png("buffer_example.png", width = 650, height = 600)
ggplot(eco3_subset) +
  geom_sf(fill = "NA") +
  geom_sf(data = mnt, color = "brown", fill = "NA") +
  geom_sf(data = mnt_buf, color = "brown", fill = "NA", 
          linetype = "dashed", size = .8) +
  geom_sf(data = contrived_points, size = 1.1) +
  geom_sf(data = points_buf, size = 1.1, color = "brown") +
  theme_void()
dev.off()





# ==============================================================================
# CODE FOR FIGURE 5
# ==============================================================================
png("elevation_plot.png", width = 750, height = 600)
loads %>%
  dplyr::filter(STATE %in% c("WA", "MT")) %>%
  dplyr::mutate(STATE = dplyr::if_else(STATE == "WA", "Washington", "Montana"),
                STATE = factor(STATE, levels = c("Washington", "Montana"))) %>%
  ggplot(aes(x = ELEVATION, y = EVENT50)) +
  facet_wrap(vars(STATE), ncol = 1) +
  geom_smooth(method = "gam", se = FALSE, color = "brown") +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  xlab("Elevation (m)") +
  ylab("50 Year Load (kPa)") +
  theme(text = element_text(size = 20))
dev.off()





# ==============================================================================
# CODE FOR FIGURE 6
# ==============================================================================
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 10000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

# The code for figure 6 gives some warnings about assumptions of spatially 
# constant geometries. These can be ignored since we are just making plots.
full_small <- ggplot(sf::st_intersection(eco3, cont_us)) +
  geom_sf() +
  coord_sf(xlim = c(-118, -112), ylim = c(43, 47)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "brown", fill=NA, size=2))

full_large <- ggplot(sf::st_intersection(eco3, cont_us)) +
  geom_sf() +
  coord_sf(xlim = c(-150, -67), ylim = c(22, 50)) +
  geom_segment(x = -140.75, xend = -138.2, y = 27.1, yend = 27.1, size = 1) +
  geom_rect(xmin = -118, xmax = -112, ymin = 43, ymax = 47, 
            color = "brown", fill = NA, size = 1) +
  annotate("text", x=-139.5, y=26, label= "50 km", size = 6.5) + 
  ggtitle("Original polygons") +
  theme_void() +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))


simp_small <- ggplot(sf::st_intersection(eco3_simp, cont_us)) +
  geom_sf() +
  coord_sf(xlim = c(-118, -112), ylim = c(43, 47)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "brown", fill=NA, size=2))

simp_large <- ggplot(sf::st_intersection(eco3_simp, cont_us)) +
  geom_sf() +
  coord_sf(xlim = c(-150, -67), ylim = c(22, 50)) +
  geom_segment(x = -140.75, xend = -138.2, y = 27.1, yend = 27.1, size = 1) +
  geom_rect(xmin = -118, xmax = -112, ymin = 43, ymax = 47, 
            color = "brown", fill = NA, size = 1) +
  annotate("text", x=-139.5, y=26, label= "50 km", size = 6.5) + 
  ggtitle("Simplified polygons") +
  theme_void() +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

png(file = "polygon_simplification.png", width = 880, height = 770)
gridExtra::grid.arrange(
  cowplot::ggdraw() +
    cowplot::draw_plot(full_large) +
    cowplot::draw_plot(full_small, x = 0.02, y = 0.24, 
                       height = .62, width = .3),
  
  cowplot::ggdraw() +
    cowplot::draw_plot(simp_large) +
    cowplot::draw_plot(simp_small, x = 0.02, y = 0.24, 
                       height = .62, width = .3),
  
  ncol = 1
)
dev.off()




# ==============================================================================
# CODE FOR FIGURE 8
# ==============================================================================
ut <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(ID == "utah")

ut_only_ws  <- sf::st_intersection(utws, ut)

png("utah_regions.png", width = 500, height = 500)
ggplot(ut) +
  geom_sf(data = ut_only_ws, aes(fill = HUC2), size = 1) +
  geom_sf(fill = NA, size = 1, color = "black") +
  scale_fill_manual(values = c("gray95", "gray80", "gray65", "gray50")) +
  theme_void() +
  theme(text = element_text(size = 20),
        legend.position = "none")
dev.off()





# ==============================================================================
# CODE FOR TABLE 1
# ==============================================================================
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 10000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

eco3_dist <- redist(loads, regions = eco3_simp, region_id = ECO3)

set.seed(42)
loads$V <- sample(1:10, nrow(loads), replace = TRUE)

# Cross validation function for eco-region remap model
# Parameter - modeling function
# Returns - cross validated MSE
regional_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(loads))
  
  for (k in 1:max(as.numeric(loads$V))) {
    pred_index <- loads$V == k
    
    models <- remap::remap(loads[!pred_index, ],
                           regions = eco3_simp, region_id = ECO3,
                           model_function = fun,
                           buffer = 50, min_n = 150,
                           distances = eco3_dist[!pred_index, ],
                           ...)
    
    preds[pred_index] <- predict(models,
                                 loads[pred_index, ],
                                 smooth = 25,
                                 distances = eco3_dist[pred_index, ])
  }
  
  # Return MSE
  c(MSE = mean((preds - log(loads$EVENT50))^2))
}

# Cross validation function for national level snow load model
# Parameter - modeling function
# Returns - cross validated MSE
national_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(loads))
  
  for (k in 1:max(as.numeric(loads$V))) {
    pred_index <- loads$V == k
    
    model <- fun(loads[!pred_index, ], ...)
    
    preds[pred_index] <- predict(model, loads[pred_index, ])
  }
  
  # Return MSE
  c(MSE = mean((preds - log(loads$EVENT50))^2))
}

# Kriging model functions
projection <- sf::st_crs("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

krig <- function(data, formula) {
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  out <- list(data = data, formula = formula)
  class(out) <- "krig"
  
  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) == 0) return(NULL)
  
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  variogram_object <- automap::autofitVariogram(
    formula = object$formula,
    input_data = object$data,
    model = "Sph")
  
  k <- gstat::krige(formula = object$formula,
                    locations = object$data,
                    newdata = data,
                    model = variogram_object$var_model,
                    debug.level = 0)
  
  return(k$var1.pred)
}

# Do cross validation for different modeling approaches
cv <- data.frame(
  Model = rep("", 3),
  National = rep(as.numeric(NA), 3),
  Regional = rep(as.numeric(NA), 3),
  stringsAsFactors = FALSE
)

cv[1, 1] <- "GAM"
cv[1, 2] <- national_cv(mgcv::gam, 
                        formula = log(EVENT50) ~ s(ELEVATION, k = 50) + 
                          s(LATITUDE, LONGITUDE, bs = 'sos', k = 500),
                        family = gaussian)
cv[1, 3] <- regional_cv(mgcv::gam, 
                        formula = log(EVENT50) ~ s(ELEVATION, k = 15) +
                          s(LATITUDE, LONGITUDE, bs = 'sos', k = 75),
                        family = gaussian)

cv[2, 1] <- "Kriging"
cv[2, 2] <- national_cv(krig, formula = log(EVENT50) ~ ELEVATION)
cv[2, 3] <- regional_cv(krig, formula = log(EVENT50) ~ ELEVATION)

cv[3, 1] <- "OLS"
cv[3, 2] <- national_cv(lm, formula = log(EVENT50) ~ ELEVATION)
cv[3, 3] <- regional_cv(lm, formula = log(EVENT50) ~ ELEVATION)

# Format results
cv[["Improvement"]] <- round(c(100 * (cv[, 2] - cv[, 3]) / cv[, 2]))
cv[, 2] <- round(100 * cv[, 2], 1)
cv[, 3] <- round(100 * cv[, 3], 1)

cv





# ==============================================================================
# CODE FOR TABLE 2
# ==============================================================================

### WARNING ###
# The code for table 2 will take several hours to run.

eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 10000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

grd <- grd %>%
  raster::rasterToPoints() %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

set.seed(42)
grd_sample <- grd %>%
  filter(1:nrow(.) %in% sample(1:nrow(.), 10000))

t3 <- data.frame(
  step = c("None", 
           "Simplify polygons", 
           " + set max_dist to 25 km",
           " + run in parallel on 4 cores"),
  geographic = rep(as.numeric(NA), 4),
  geographicS2 = rep(as.numeric(NA), 4),
  projected = rep(as.numeric(NA), 4)
)


# Geographic times
# ==============================================================================
sf::sf_use_s2(FALSE)
times <- list()

# None
Sys.time()
times[[1]] <- Sys.time()
remap::redist(grd_sample, eco3, ECO3)

# Simplify polygons
Sys.time()
times[[2]] <- Sys.time()
remap::redist(grd_sample, eco3_simp, ECO3)

# + set max_dist to 25 km
Sys.time()
times[[3]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25)
}

# + run in parallel on 4 cores
Sys.time()
times[[4]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25, cores = 4)
}
Sys.time()
times[[5]] <- Sys.time()

# Process times into geographic column of Table 2
for (i in 2:length(times)) {
  if (i %in% c(2, 3)) {
    time <- (nrow(grd) / nrow(grd_sample)) * 
      difftime(times[[i]], times[[i-1]], units = "hours")
  } else {
    time <- difftime(times[[i]], times[[i-1]], units = "hours")
  }
  t3[i - 1, 2] <- signif(time, digits = 2)
}

# Geographic (S2) times
# ==============================================================================
sf::sf_use_s2(TRUE)

times2 <- list()

# None
Sys.time()
times2[[1]] <- Sys.time()
remap::redist(grd_sample, eco3, ECO3)

# Simplify polygons
Sys.time()
times2[[2]] <- Sys.time()
remap::redist(grd_sample, eco3_simp, ECO3)

# + set max_dist to 25 km
Sys.time()
times2[[3]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25)
}

# + run in parallel on 4 cores
Sys.time()
times2[[4]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25, cores = 4)
}
Sys.time()
times2[[5]] <- Sys.time()

# Process times into geographic column of Table 2
for (i in 2:length(times2)) {
  if (i %in% c(2, 3)) {
    time <- (nrow(grd) / nrow(grd_sample)) * 
      difftime(times2[[i]], times2[[i-1]], units = "hours")
  } else {
    time <- difftime(times2[[i]], times2[[i-1]], units = "hours")
  }
  t3[i - 1, 3] <- signif(time, digits = 2)
}

# Projected times
# ==============================================================================

# Project data
eco3_trans <- eco3 %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

eco3_simp_trans <- eco3_simp %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

grd_trans <- grd %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

grd_sample_trans <- grd_sample %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

times3 <- list()

# None
Sys.time()
times3[[1]] <- Sys.time()
remap::redist(grd_sample_trans, eco3_trans, ECO3)

# Simplify polygons
Sys.time()
times3[[2]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3)
}

# + set max_dist to 25 km
Sys.time()
times3[[3]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3, max_dist = 25)
}

# + run in parallel on 4 cores
Sys.time()
times3[[4]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3, max_dist = 25, cores = 4)
}
Sys.time()
times3[[5]] <- Sys.time()

# Process times into projected column of Table 2
for (i in 2:length(times3)) {
  if (i == 2) {
    time <- (nrow(grd) / nrow(grd_sample)) * 
      difftime(times3[[i]], times3[[i-1]], units = "hours")
  } else {
    time <- difftime(times3[[i]], times3[[i-1]], units = "hours")
  }
  t3[i - 1, 4] <- signif(time, digits = 2)
}

t3





# ==============================================================================
# CODE FOR TABLE 3
# ==============================================================================
set.seed(42)
utapr1$V <- sample(1:10, nrow(utapr1), replace = TRUE)
utapr1$WESDp1 <- utapr1$WESD + 1

ws_simp <- utws %>%
  sf::st_simplify(dTolerance = 5000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

# Cross validation function for HUC remap model
# Parameter - modeling function
# Returns - cross validated MSE
regional_sp_cv <- function(fun, huc, dist, ...) {
  
  preds <- rep(as.numeric(NA), nrow(utapr1))
  
  years <- sort(unique(utapr1$YEAR))
  
  for (i in 1:length(years)) {
    # Make a subset index for each year
    year_index <- utapr1$YEAR == years[i]
    
    # do cross validation for each year
    for (k in 1:max(as.numeric(utapr1$V))) {
      pred_index <- utapr1$V == k
      
      models <- remap::remap(utapr1[year_index & !pred_index, ],
                             regions = ws_simp, region_id = huc,
                             model_function = fun,
                             buffer = 20, min_n = 30,
                             distances = dist[year_index & !pred_index, ],
                             ...)
      
      preds[year_index & pred_index] <- predict(
        models, 
        utapr1[year_index & pred_index, ],
        smooth = 10, 
        distances = dist[year_index & pred_index, ]
      )
    }
  }
  
  c(MSE = mean((preds - log(utapr1$WESDp1))^2))
}

# Cross validation function for state level model
# Parameter - modeling function
# Returns - cross validated MSE
state_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(utapr1))
  
  years <- sort(unique(utapr1$YEAR))
  
  for (i in 1:length(years)) {
    # Make a subset index for each year
    year_index <- utapr1$YEAR == years[i]
    
    # do cross validation for each year
    for (k in 1:max(as.numeric(utapr1$V))) {
      pred_index <- utapr1$V == k
      
      model <- fun(utapr1[year_index & !pred_index, ], ...)
      
      preds[year_index & pred_index] <- predict(
        model, 
        utapr1[year_index & pred_index, ]
      )
    }
  }
  
  c(MSE = mean((preds - log(utapr1$WESDp1))^2))
}

# A bounded prediction function that uses the highest value in the modeling
# data as a bound
predict.bound <- function(object, data) {
  preds <- predict(object$model, data)
  preds[preds < log(1)] <- log(1)
  preds[preds > log(object$ubound)] <- log(object$ubound)
  return(preds)
}

# OLS model functions for Table 3
ols_ut <- function(data, ...) {
  ubound <- max(data$WESDp1)
  
  model <- lm(data, ...)
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
  return(out)
}

# GAM model functions for Table 3
gam_ut <- function(data, ...) {
  ubound <- max(data$WESDp1)
  
  model <- mgcv::gam(data, ...)
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
  return(out)
}

# Kriging model functions for Table 3
projection <- sf::st_crs("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

krig_ut <- function(data, formula) {
  ubound <- max(data$WESDp1)
  
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  model <- list(data = data, formula = formula)
  class(model) <- "krig"
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) == 0) return(NULL)
  
  data <- data %>%
    sf::st_transform(projection) %>%
    sf::as_Spatial()
  
  variogram_object <- automap::autofitVariogram(
    formula = object$formula,
    input_data = object$data,
    model = "Sph")
  
  k <- gstat::krige(formula = object$formula,
                    locations = object$data,
                    newdata = data,
                    model = variogram_object$var_model,
                    debug.level = 0)
  
  return(k$var1.pred)
}

# Make distance matrices
dists2 <- remap::redist(utapr1, ws_simp, HUC2)
dists4 <- remap::redist(utapr1, ws_simp, HUC4)


# Do cross validation for different modeling approaches
utcv <- data.frame(
  Model = rep("", 3),
  State = rep(as.numeric(NA), 3),
  HUC2 = rep(as.numeric(NA), 3),
  HUC4 = rep(as.numeric(NA), 3),
  stringsAsFactors = FALSE
)

utcv[1, 1] <- "GAM"
utcv[1, 2] <- state_cv(gam_ut, 
                       formula = log(WESDp1) ~ s(ELEVATION, k = 15) + 
                         s(LATITUDE, LONGITUDE, bs = 'sos', k = 45),
                       family = gaussian)
utcv[1, 3] <- regional_sp_cv(gam_ut, "HUC2", dists2,
                             formula = log(WESDp1) ~ s(ELEVATION, k = 5) +
                               s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
                             family = gaussian)
utcv[1, 4] <- regional_sp_cv(gam_ut, "HUC4", dists4,
                             formula = log(WESDp1) ~ s(ELEVATION, k = 5) +
                               s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
                             family = gaussian)

utcv[2, 1] <- "Kriging"
utcv[2, 2] <- state_cv(krig_ut, formula = log(WESDp1) ~ ELEVATION)
utcv[2, 3] <- regional_sp_cv(krig_ut, "HUC2", dists2, 
                             formula = log(WESDp1) ~ ELEVATION)
utcv[2, 4] <- regional_sp_cv(krig_ut, "HUC4", dists4, 
                             formula = log(WESDp1) ~ ELEVATION)

utcv[3, 1] <- "OLS"
utcv[3, 2] <- state_cv(ols_ut, formula = log(WESDp1) ~ ELEVATION)
utcv[3, 3] <- regional_sp_cv(ols_ut, "HUC2", dists2,
                             formula = log(WESDp1) ~ ELEVATION)
utcv[3, 4] <- regional_sp_cv(ols_ut, "HUC4", dists4,
                             formula = log(WESDp1) ~ ELEVATION)

# Format and save file
utcv[, 2] <- round(utcv[, 2] * 100)
utcv[, 3] <- round(utcv[, 3] * 100)
utcv[, 4] <- round(utcv[, 4] * 100)

utcv





####################################################################
# Species Distribution Modeling using Spatial Point Processes:
# a Case Study of Sloth Occurrence in Costa Rica
# by Paula Moraga
####################################################################


# Installing packages

install.packages(c("spocc", "sp", "tmap", "raster", "rnaturalearth", "rgeos", "ggplot2"))

# The R-INLA package is not on CRAN because it uses some external C libraries that make difficult to build the binaries.
# Therefore, we need to install it adding the URL of the INLA repository.
# We also add https://cloud.r-project.org to enable the installation of CRAN dependencies.

install.packages("INLA", repos = c("https://inla.r-inla-download.org/R/stable",
                                   "https://cloud.r-project.org"), dep = TRUE)

# Installing the Bioconductor packages that R-INLA suggests

install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep = TRUE)


# Sloth occurrence data

library("spocc")
df <- occ(query = "Bradypus variegatus", from = "gbif",
          date = c("2000-01-01", "2019-12-31"),
          gbifopts = list(country = "CR"),
          has_coords = TRUE, limit = 1000)
names(df)
d <- occ2df(df)
summary(d)

library(sp)
dpts <- SpatialPoints(d[, c("longitude", "latitude")])

library(tmap)
tmap_mode("view")
tm_basemap(leaflet::providers$OpenStreetMap) +
  tm_shape(dpts) + tm_dots()

# Spatial climatic covariates

library(raster)
rmonth <- getData(name = "worldclim", var = "tmin", res = 10)
rcov <- mean(rmonth)

# Computational grid

library(rnaturalearth)
map <- ne_countries(type = "countries",
                    country = "Costa Rica", scale = "medium")
resolution <- 0.1
r <- raster(map, resolution = resolution)
(nrow <- nrow(r))
(ncol <- ncol(r))
nrow*ncol

r[] <- 0
tab <- table(cellFromXY(r, dpts))
r[as.numeric(names(tab))] <- tab
grid <- rasterToPolygons(r)

# Data

grid <- grid[as.vector(t(matrix(1:nrow(grid), nrow = ncol, ncol = nrow))), ]

grid$id <- 1:nrow(grid)
grid$Y <- grid$layer
grid$cellarea <- resolution*resolution
grid$cov <- extract(rcov, coordinates(grid))

gridmap <- raster::intersect(grid, map)
grid <- grid[grid$id %in% gridmap$id, ]
summary(grid)

indNA <- which(is.na(grid$cov))
indNA
grid$cov[indNA] <- grid$cov[indNA+1]

library(rgeos)
gridborder <- gUnaryUnion(grid)

tmap_mode("plot")
tm_shape(grid) +
  tm_polygons(col = c("Y", "cov"), border.col = "transparent") +
  tm_shape(gridborder) + tm_borders() +
  tm_facets(ncol = 2) + tm_legend(legend.position = c("left", "bottom"))


# Fitting the model using R-INLA

library(INLA)
grid$id2 <- grid$id
formula <- Y ~ 1 + cov +
  f(id, model="rw2d", nrow = nrow, ncol = ncol) +
  f(id2, model="iid")
res <- inla(formula, family = "poisson", data = grid@data,
            E = cellarea, control.predictor = list(compute = TRUE))

# Results

summary(res)

library(ggplot2)
marginal <- inla.smarginal(res$marginals.fixed$cov)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") + theme_bw()

grid$respa <- res$summary.random$id[grid$id, "mean"]
grid$reiid <- res$summary.random$id2[, "mean"]

tm_shape(grid) +
  tm_polygons(col = c("respa", "reiid"), style = "cont", border.col = "transparent") +
  tm_shape(gridborder) + tm_borders() +
  tm_facets(ncol = 2) + tm_legend(legend.position = c("left", "bottom"))

cellarea <- resolution*resolution
grid$NE <- res$summary.fitted.values[, "mean"] * cellarea
grid$LL <- res$summary.fitted.values[, "0.025quant"] * cellarea
grid$UL <- res$summary.fitted.values[, "0.975quant"] * cellarea

tm_shape(grid) +
  tm_polygons(col = c("NE", "LL", "UL"),
              style = 'fixed', border.col = "transparent",
              breaks = c(0, 10, 50, 100, ceiling(max(grid$UL)))) +
  tm_shape(gridborder) + tm_borders() +
  tm_facets(ncol = 3) + tm_legend(legend.position = c("left", "bottom"))

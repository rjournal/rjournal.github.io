## ----defaults, echo=FALSE, message=FALSE, warning=FALSE, comment = FALSE----
knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE, 
                      comment = FALSE,
                      echo = FALSE,
                      eval = TRUE,
                      out.width = "95%",
                      retina = 3,
                      dev = "png",
                      dpi = 300)


## ----setup----------------------------------------------------
library(cartogram)
library(cowplot)
library(gridExtra)
library(ggthemes)
library(knitcitations)
library(kableExtra)
library(sp)
library(sf)
library(sugarbag)
library(tidyverse)

# Rda files sourced from: https://github.com/wfmackey/absmapsdata
load("data/sa22011.Rda")
load("data/state2011.Rda")

library(RefManageR)
options("citation_format" = "pandoc")
BibOptions(check.entries = FALSE, style = "markdown", bib.style = "alphabetic", cite.style = 'alphabetic')


invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white")
  )

# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 <  0.74 ~ "#33809d",
    sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
    sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
    sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
    sir_p50 >= 1.45 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}


## ----thyroiddata----------------------------------------------
sa2 <- sa22011 %>% 
  filter(!st_is_empty(geometry)) %>% 
  filter(!state_name_2011 == "Other Territories") %>% 
  filter(!sa2_name_2011 == "Lord Howe Island")

SIR <- read_csv("data/SIR Downloadable Data.csv") %>% 
  filter(SA2_name %in% sa2$sa2_name_2011) %>% 
  dplyr::select(Cancer_name, SA2_name, Sex_name, p50) %>% 
  filter(Cancer_name == "Thyroid", Sex_name == "Females")
ERP <- read_csv("data/ERP.csv") %>%
  filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) %>% 
  dplyr::select(Region, Value)
# Alternative maps
# Join with sa2 sf object
sa2thyroid_ERP <- SIR %>% 
  left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) %>%
  left_join(., ERP %>% 
              dplyr::select(Region, 
              Population = Value), by = c("sa2_name_2011"= "Region")) %>% 
  filter(!st_is_empty(geometry))
sa2thyroid_ERP <- sa2thyroid_ERP %>% 
  #filter(!is.na(Population)) %>% 
  filter(!sa2_name_2011 == "Lord Howe Island") %>% 
  mutate(SIR = map_chr(p50, aus_colours)) %>% 
  st_as_sf() 


## ----choro, fig.cap = "A choropleth map of thyroid incidence among females across the Statistical Areas of Australia at Level 2. Blue indicates lower than average and red indicates higher than average incidence. A cluster of high incidence is visible on the east coast."----
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm
aus_ggchoro


## ----fullhexmap, fig.cap = "A hexagon tile map of Australia at SA2 level. The colours communicate the value of the estimated SIR of thyroid cancer for females, ranging from much lower than average (blue) to much higher than average (red)"----
if (!file.exists("data/aus_hexmap.rda")) {

## Check the projection uses long and lat
## Create centroids set
sa2centroids <- sa2 %>% 
  create_centroids(., "sa2_name_2011")
## Create hexagon grid
# For speed, consider if the buffer distance beyond the centroids is necessary
grid <- create_grid(centroids = sa2centroids,
                    hex_size = 0.1,
                    buffer_dist = 4, verbose = TRUE)
## Allocate polygon centroids to hexagon grid points
# This is a time consuming process for a large data set
aus_hexmap <- allocate(
  centroids = sa2centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.1,
  hex_filter = 8,
  focal_points = capital_cities,
  width = 25,
  verbose = TRUE)
save(aus_hexmap, file = "data/aus_hexmap.rda") 
}

load("data/aus_hexmap.rda")

## Prepare to plot
fort_hex <- fortify_hexagon(data = aus_hexmap,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.2) %>% 
            left_join(sa2thyroid_ERP %>% select(sa2_name_2011, SIR, p50))
fort_aus <- sa2thyroid_ERP %>%
  fortify_sfc()
## Make a plot
aus_hexmap_plot <- ggplot() +
  geom_polygon(aes(x = long,  y = lat,  group = interaction(sa2_name_2011, polygon)),
               fill = "black",  colour = "darkgrey",  linewidth = 0.1, data = fort_aus) +
  geom_polygon(aes(x = long, y = lat, group = hex_id, fill = SIR), data = fort_hex) +
  scale_fill_identity() +
  invthm + coord_equal()


## ----contcart-------------------------------------------------
# Projected data 
sa2thyroid_ERP <- st_transform(sa2thyroid_ERP, crs = 3112) %>% 
  filter(state_name_2011 == "Tasmania")
sa2thyroid_ERP <- sa2thyroid_ERP %>% 
  mutate(sva = sqrt(as.numeric(Population)/as.numeric(areasqkm_2011)))

# Contiguous Cartograms
if (!file.exists("data/ncont.rda")) {
  cont <- sa2thyroid_ERP %>% 
    mutate(Population = Population + 1) %>% 
    cartogram_cont(., weight = "Population", itermax = 20) %>%
    st_as_sf()
  save(cont, file = "data/cont.rda") 
}

load("data/cont.rda")

tas_ggcont <- ggplot(cont) + 
  geom_sf(fill = "#D3D3D3", colour = "white", data = sa2thyroid_ERP) +
  geom_sf(aes(fill = SIR), colour = "grey", size = 0.01) + 
  scale_fill_identity() + guides(fill = "none") +
  theme_map()

## ----ncontcart------------------------------------------------
# Non - Contiguous Cartograms
if (!file.exists("data/ncont.rda")) {
  ncont <- cartogram_ncont(sa2thyroid_ERP, k = 1.5,
                         weight = "Population") %>% 
                          st_as_sf()
  save(ncont, file = "data/ncont.rda")
}

load("data/ncont.rda")

tas_ggncont <- ggplot(ncont %>% filter(state_name_2011 == "Tasmania")) + 
  geom_sf(fill = "#D3D3D3", colour = "white",  data = sa2thyroid_ERP) +
  geom_sf(aes(fill = SIR), colour = NA) + 
  scale_fill_identity() + 
  guides(fill = "none") +
  theme_map() 


## ----dorl-----------------------------------------------------
# Non - Contiguous Dorling Cartograms
if (!file.exists("data/dorl.rda")) {
  dorl <- sa2thyroid_ERP %>% 
    filter(!is.na(Population)) %>% 
  # mutate(pop = (Population/max(Population))*10) %>% # helpful for all of Australia
    cartogram_dorling(., weight = "Population", k = 0.2, m_weight = 1) %>% st_as_sf()
  save(dorl, file = "data/dorl.rda")
}

load("data/dorl.rda")
d <- st_bbox(dorl)
tas_ggdorl <- ggplot(dorl) +
  geom_sf(fill = "#D3D3D3", colour = "white",  data = sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), colour = "grey", size = 0.01) + 
  scale_fill_identity() +
  guides(fill = "none") +
  theme_map() #+ ggtitle("c")


## ----tasdisplays, out.width="100%", fig.width = 8, fig.height = 3, layout = "l-body", fig.cap = "The three displays show alternative maps of the Australian state of Tasmania at SA2 level: (a) contiguous cartogram, (b) non-contiguous cartogram and (c) Dorling cartogram of Tasmania. The contiguous cartogram looks like the state has an hourglass figure, while the non-contiguous cartogram shrinks areas into invisibility. The Dorling expands the metropolitan regions."----
cowplot::plot_grid(tas_ggcont, 
                   tas_ggncont, 
                   tas_ggdorl, ncol=3,
                   labels = c("a", "b", "c"))


## ----hexmap, fig.cap = "A hexagon tile map of female thyroid cancer incidence in Australia, the same data as shown in the choropleth map in Figure 1. The high incidence in several of the metropolitan regions (Brisbane, Sydney and Perth) can now be seen, along with numerous isolated spots."----
aus_hexmap_plot


## ----sugarbagflow, out.width = "100%", layout = "l-body-outset", fig.cap = "A flow diagram detailing the steps taken to create a hexagon tile map. There are two basic processes, one to make the grid, and the other to allocate centroids to grid points.", fig.alt="The steps of the algorithm to creating a hexagon grid with buffer, and the allocation of spatial polygon centroids to hexagon. This is the same information as provided in the text of the paper."----
knitr::include_graphics("figs/sugarbag_flow.png")


## ----eval=FALSE, echo = TRUE----------------------------------
## install.packages("sugarbag")


## ----eval=FALSE, echo = TRUE----------------------------------
## devtools::install_github("srkobakian","sugarbag")


## ----capital_cities-------------------------------------------
data(capital_cities)


## ----tas_centroids, fig.cap = "The Australian Cancer Atlas data has determined the colour of each Statistical Area of Australian at Level 2. A choropleth map (a) of Standardised Incidence Rates (SIRs) is paired with a hexagon tile map (b) to contrast the colours that are made obvious when every SA2 is equally represented."----
centroids <- create_centroids(
  shp_sf = sa2 %>% filter(state_name_2011 == "Tasmania"), 
  sf_id = "sa2_name_2011")
hex_size <- .2
buffer_dist <- 2
bbox <- tibble(min = c(min(centroids$longitude), min(centroids$latitude)),
  max = c(max(centroids$longitude), max(centroids$latitude)))
grid <- tibble::as_tibble(expand.grid(hex_long = seq(bbox$min[1] - buffer_dist,
  bbox$max[1] + buffer_dist,
  hex_size),
  hex_lat = seq(bbox$min[2] - buffer_dist,
    bbox$max[2] + buffer_dist,
    hex_size)))

fort_tas <- fort_aus %>% filter(state_name_2011 == "Tasmania")

tas_centroids <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "#D3D3D3", colour = "white", size = 0.1) + 
  geom_point(aes(x=longitude, y = latitude), size = 2, shape = 19, colour = "white",
             data = centroids) +
  scale_colour_identity() +
  theme_void() + 
  coord_equal()

tas_hexmap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "#D3D3D3", colour = "white", size = 0.1) + 
  geom_polygon(aes(x = long, y = lat, group = hex_id, fill = SIR),
               data = fort_hex %>% filter(sa2_name_2011 %in% fort_tas$sa2_name_2011)) +
  scale_fill_identity() +
  theme_void() + 
  coord_equal()


## ----grid, fig.cap = "Complete set of grid points to create a tile map.", height = 4----
# Find every second latitude
shift_lat <- grid %>% dplyr::select(hex_lat) %>%
  dplyr::distinct() %>%
  dplyr::filter(dplyr::row_number() %% 2 == 1) %>% unlist()
# Shift the longitude of every second latitude to the right to make hex structure
grid <- grid %>%
  dplyr::mutate(hex_long = ifelse(hex_lat %in% shift_lat, hex_long,
    hex_long + (hex_size / 2))) %>%
  dplyr::mutate(id=1:NROW(.))  %>%
  dplyr::mutate(assigned=FALSE)


## ----filtergrid, fig.cap = "Each key step in the hexagon allocation process. The full hexagon grid in a) is created first. The buffer is applied and the remaining points are shown as larger, dark green circles, the potential hexagon points that were removed are shown as smaller, light green points. For each centroid in the set, c) shows the points left after the closest points are selected as potential locations. The relationship between Hobart (the cross) and the centroid (the yellow triangle is used to filter the potential locations within a wedge."----
g1 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "#D3D3D3", colour = "white", size = 0.1) + 
   theme_void() + 
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#09661f", data = grid, size = 1) +
  coord_equal()

full_grid <- grid <- grid %>%
  mutate(hex_long_int = dense_rank(hex_long)-1,
    hex_lat_int = dense_rank(hex_lat)-1)
nlong <- length(unique(grid$hex_long))
nlat <- length(unique(grid$hex_lat))
  
# Add grid row and column values to centroids data set 
centroids <- centroids %>%
  mutate(
    # long int for integer value of longitude column
    long_int = round((longitude - min(grid$hex_long)) / 
                       (max(grid$hex_long) - min(grid$hex_long)) * nlong, 0),
    # lat int for integer value of latitude column
    lat_int = round((latitude - min(grid$hex_lat)) / 
                      (max(grid$hex_lat) - min(grid$hex_lat)) * nlat, 0)
  )
# Amount of lats and longs in each group of rows and columns
lat_size <- round(nlat / 20, 0)
long_size <- round(nlong / 20, 0)
# Make a list of the min and max of the groups
# Effectively creates manual sliding windows
nlat_list <- purrr::map2(seq(1:nlat), lat_size + seq(1:nlat), c)
nlong_list <- purrr::map2(seq(1:nlong), long_size + seq(1:nlong), c)
# LATITUDE ROWS FILTER


# Function to return the centroids that fall in the latitude window
lat_window <- function(bounds, cents = centroids, maximum = nlat) {
  
  max_int <- min(bounds[2], maximum)
  cents_in <- filter(cents, between(lat_int, bounds[1], max_int))
  return(cents_in)
}

# Function to return the centroids that fall in the longitude window
long_window <- function(bounds, cents = centroids, maximum = nlong) {
  
  max_int <- bounds[2]
  
  while (max_int > maximum) {
    max_int <- max_int - 1
  }
  cents_in <- filter(cents, between(long_int, bounds[1], max_int))
  return(cents_in)
}

# amount of latitude in sliding window
lat_windows <- purrr::map(.x = nlat_list, .f = lat_window, centroids, nlat)

# LONGITUDE COLS FILTER
long_windows <- purrr::map(.x = nlong_list, .f = long_window, centroids, nlong)

#########################################################
###                ROLLING MIN & MAX                  ###

# DEFINE FUNCTION
# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(
  .x = lat_windows,
  .f = function(data) {
    data %>%
      dplyr::summarise(
        long_min = ifelse(purrr::is_empty(long_int), NA, min(data$long_int)),
        long_max = ifelse(purrr::is_empty(long_int), NA, max(data$long_int))
      )
  }
)
# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(data) {
  data %>%
    dplyr::summarise(
      lat_min = ifelse(purrr::is_empty(lat_int), NA, min(data$lat_int)),
      lat_max = ifelse(purrr::is_empty(lat_int), NA, max(data$lat_int))
    )
})

# find the min and max longitude for each latitude
range_rows <- purrr::map_dfr(.x = lat_windows, .f = function(data) {
    data %>%
      dplyr::summarise(
        long_min = ifelse(purrr::is_empty(long_int), NA, min(data$long_int)),
        long_max = ifelse(purrr::is_empty(long_int), NA, max(data$long_int))
      )
  }
)

# find the min and max longitude for each latitude
range_cols <- purrr::map_dfr(.x = long_windows, .f = function(data) {
  data %>%
    dplyr::summarise(
      lat_min = ifelse(purrr::is_empty(lat_int), NA, min(data$lat_int)),
      lat_max = ifelse(purrr::is_empty(lat_int), NA, max(data$lat_int))
    )
})


#########################################################
###                ROLLING AVERAGES                   ###


mean_range <- function(bounds, data) {
  data[bounds[1]:min(bounds[2], NROW(data)), ] %>%
    dplyr::summarise(across(ends_with("min"), 
                       ~mean(.x, na.rm = TRUE), .names = "mean_{col}"),
                     across(ends_with("max"), 
                            ~mean(.x, na.rm = TRUE), .names = "mean_{col}")) %>% 
    # in cases where all values are NA mean is Nan
    # make mean value of NaN be NA
    dplyr::summarise(across(starts_with("mean"), ~ifelse(.x == "NaN", NA, .x)))
}

# smooth the minimums
av_range_rows <- purrr::map_dfr(.x = nlat_list, mean_range, range_rows) %>%
  bind_cols(lat_id = c(seq(1:nlat) + lat_size), .)

# smooth the minimums
av_range_cols <- purrr::map_dfr(.x = nlong_list, mean_range, range_cols) %>%
  bind_cols(long_id = c(seq(1:nlong) + round(long_size / 2)), .)
# APPLY A BUFFER
# change buffer to amount of hexagons (ints) either side
hex_buffer <- floor(buffer_dist / hex_size)
  
grid <- grid %>%
    left_join(., av_range_rows, by = c("hex_lat_int" = "lat_id")) %>%
    left_join(., av_range_cols, by = c("hex_long_int" = "long_id")) %>%
    rowwise() %>%
    mutate(long_buffer = ifelse(between(
      hex_long_int, mean_long_min - hex_buffer,
      mean_long_max + hex_buffer), "in", "out")) %>%
    mutate(lat_buffer = ifelse(between(
      hex_lat_int, mean_lat_min - hex_buffer,
      mean_lat_max + hex_buffer), "in", "out")) %>%
    filter(lat_buffer == "in" | long_buffer == "in")

g2 <- ggplot() + 
   geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "#D3D3D3", colour = "white", size = 0.1) +  
  theme_void() + coord_equal() +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = full_grid, size = 0.5) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#09661f", data = grid, size = 1)



## ----centroids, eval = FALSE----------------------------------
## hexmap_allocation <- allocate(
##   centroids = centroids %>% select(sa2_name_2011, longitude, latitude),
##   sf_id = "sa2_name_2011",
##   hex_grid = grid,
##   hex_size = 0.2, ## same size used in create_grid
##   hex_filter = 10,
##   width = 35,
##   focal_points = capital_cities,
##   verbose = TRUE)


## ----buffers, fig.cap = "Filter for grid points within a square, then circular, distance for those closest to the centroid.", echo = FALSE, out.width = "100%", height = 4----
centroids <- centroids %>%
        group_nest(sa2_name_2011) %>%
        mutate(closest = purrr::map(data, closest_focal_point, 
                                    focal_points = capital_cities)) %>%
        unnest(c("data", "closest")) %>%
        arrange(focal_distance) %>% 
        mutate(rownumber = row_number())


# Choose a centroid for allocation:
f_centroid <- centroids %>%
        filter(rownumber == 8)


# filter_grid_points
distance <- (((f_centroid$latitude - f_centroid$focal_latitude) ^ 2) + 
         ((f_centroid$longitude - f_centroid$focal_longitude) ^ 2)) ^ (1 / 2)
flong <- f_centroid$longitude
flat <- f_centroid$latitude
f_dist <- 10 * hex_size
angle_width <- 35
  
f_grid <- grid %>%
  ungroup() %>%
  filter((flat - f_dist) < hex_lat & hex_lat < (flat + f_dist)) %>%
  filter((flong - f_dist) < hex_long & hex_long < (flong + f_dist))


f_grid <- f_grid %>%
  group_by(id) %>%
  mutate(
    hex_lat_c = (hex_lat - flat),
    hex_long_c = (hex_long - flong)) %>%
  mutate(hyp = ((hex_lat_c^2) + (hex_long_c^2))^(1 / 2))
f_angle <- f_centroid %>%
      mutate(atan = atan2(latitude - focal_latitude, longitude - focal_longitude),
        angle = (atan * 180 / pi),
        pangle = ifelse(angle < 0, angle + 360, angle)) %>%
      pull(pangle)

f_grid <- f_grid %>%
  # create circle of radius: f_dist
  filter(hyp < f_dist) %>%
  mutate(
    # geosphere takes a long time
    angle = f_angle,
    angle_plus = (angle + angle_width) %% 360,
    angle_minus = (angle - angle_width) %% 360,
    atan = atan2(hex_lat_c, hex_long_c),
    hex_angle = (atan * 180 / pi),
    hex_angle = ifelse(hex_angle < 0, hex_angle + 360, hex_angle))

allocated_list <- aus_hexmap %>% 
                  filter(points == "Hobart") %>% 
                  slice_min(rownumber, n = 7)

fort_allocated <- allocated_list %>% 
  fortify_hexagon(., sf_id = "sa2_name_2011", hex_size = 0.2)


full_grid <- full_grid %>% filter(hex_long > bbox$min[1], hex_long < bbox$max[1],
                                  hex_lat > bbox$min[2], hex_lat < bbox$max[2])

allocated_list <- aus_hexmap %>% 
                  filter(points == "Hobart") %>% 
                  slice_min(rownumber, n = 7)

fort_allocated <- allocated_list %>% 
  fortify_hexagon(., sf_id = "sa2_name_2011", hex_size = 0.2)
  
# Check that there were available points within hyp distance
f_grid <- f_grid %>%
        # create slice of width *2 degrees from centroid
        filter(angle_minus < hex_angle & hex_angle < angle_plus)


f1 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas, fill = "#D3D3D3", colour = "white", size = 0.1) +
  theme_void() + coord_equal(ylim = c(-45.1, -40.5), xlim = c(144.5, 149)) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#55b56c", data = grid, size = 0.5) +
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#09661f", data = f_grid, size = 1) +
  # allocated areas
    geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011)), data = fort_allocated, fill = "#353b38", colour = "black", size = 0.01, alpha = 0.7) +
  #Hobart
  geom_point(aes(x = focal_longitude, y = focal_latitude), data = f_centroid, colour = "#e9f2ed", size = 4, shape = 3) + 
  geom_point(aes(x=longitude, y = latitude), data = f_centroid, fill = "#beaed4", shape = 25) 



## ----filterprocess, fig.cap = "Illustration of key steps of the algorithm: (1a) full hexagon grid is created first; (1b) buffer is applied, shown as dark green circles, to accommodate irregularly shaped regions; (2a, 2b) allocation process, relative the center of Hobart, showing the 8th centroid to be allocated. The relationship between Hobart (the cross) and the centroid (the purple triangle) is used to filter the potential locations from the grid within a wedge. Hexagons already allocated are shown in black, and the purple circle indicates the hexagon to be assigned to the 8th centroid.", dpi = 600, echo = FALSE, height = 12----

hex <- f_grid %>%
        ungroup() %>%
        filter(hyp == min(hyp)) %>%
        select(hex_long, hex_lat, hex_id = id)

f2 <- ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011, polygon)), data = fort_tas %>% filter(lat < -40.3), fill = "#D3D3D3", colour = "white", size = 0.1) +
  geom_polygon(aes(x = long, y = lat, group = interaction(sa2_name_2011)), data = fort_allocated, fill = "#353b38", colour = "black", size = 0.01, alpha = 0.7) +
  #Hobart
  geom_point(aes(x = focal_longitude, y = focal_latitude), data = f_centroid, colour = "#e9f2ed", size = 4, shape = 3) + 
  geom_point(aes(x=longitude, y = latitude), data = f_centroid, fill = "#beaed4", shape = 25, size = 2) + 
  theme_void() + coord_equal(xlim = c(146.9, 148), ylim = c(-43.6, -42.5)) +
  # possible choices to select
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#09661f", data = f_grid, size = 2.5) +
  # selected hexagon
  geom_point(aes(x = hex_long, y = hex_lat), colour = "#09661f", fill = "#beaed4", data = hex, size = 3.5, shape = 21)
 
# gridExtra::grid.arrange(g1, g2,
#                         f1, f2)

cowplot::plot_grid(g1, g2,
                   f1, f2,
                   labels = c("1a", "1b", "2a", "2b"))


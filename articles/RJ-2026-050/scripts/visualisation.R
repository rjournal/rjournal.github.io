
# Data preparation -----------------------------------------------

vic_temp <- aus_temp |>
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143"))

nsw_temp <- aus_temp |>
  filter(id %in% c("ASN00055325", "ASN00049000"))

sample <- c("South Yarra", "Carnegie",
            "Moonee Pond", "Footscray", "Essendon", "Camberwell",
            "Gardiner", "Essendon", "Moreland", "Flagstaff")

zone1 <- c("Seaford", "Yarraville", "Windsor"	,"Willison",	"Williamstown Beach",
           "Williamstown",	"Westgarth", "West Richmond",	"West Footscray",	"Victoria Park",
           "Tottenham",	"Tooronga",	"Toorak",	"Thornbury",	"Strathmore",	"Spotswood",
           "Southern Cross",	"South Yarra",	"South Kensington",	"Showgrounds",	"Seddon",
           "Seaholme",	"Rushall",	"Royal Park",	"Riversdale",	"Ripponlea",	"Richmond",
           "Prahran",	"Parliament",	"Northcote",	"North Williamstown",	"North Richmond",
           "North Melbourne",	"Newport",	"Newmarket",	"Murrumbeena",	"Moreland",	"Moonee Ponds",
           "Middle Footscray",	"Merri",	"Melbourne Central",	"Malvern",	"Macaulay",	"Kooyong",
           "Kensington",	"Jolimont",	"Jewell",	"Heyington",	"Hawthorn", "Hawksburn",	"Hartwell",
           "Glenferrie",	"Glenbervie",	"Glen Iris","Glen Huntly",	"Gardiner",	"Gardenvale",	"Footscray",
           "Flinders Street"	,"Flemington Racecourse",	"Flemington Bridge",	"Flagstaff",	"Fairfield",
           "Essendon"	,"Elsternwick",	"East Richmond",	"East Camberwell",	"Dennis",	"Darebin",	"Croxton",
           "Collingwood",	"Coburg",	"Clifton Hill",	"Caulfield"	,"Carnegie",	"Camberwell",	"Burwood"	,
           "Burnley",	"Brunswick", "Bell Balaclava"	,"Auburn",	"Aspendale",	"Ashburton",	"Ascot Vale",
           "Armadale",	"Anstey",	"Alphington","Alamein")


# Examples ------------------------------------------------------------------

# Comparison between ribbon glyph and segment glyph -------------------------
# Define a color palette
color_palette <- c("deepskyblue4", "coral3")

p1 <- vic_temp |>
  ggplot(aes(x_major = long,
             y_major = lat,
             x_minor = month,
             ymin_minor = tmin,
             ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
          fill = "antiquewhite", color = "white", inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
  add_glyph_boxes(width = rel(2.5), height = rel(1.5),
                  color = color_palette[1]) +
  add_ref_lines(width = rel(2.5), height = rel(1.5),
                color = color_palette[1]) +
  geom_glyph_ribbon(width = rel(2.5), height = rel(1.5),
                    color = color_palette[1], fill = color_palette[1]) +
  # Theme and aesthetic
  theme_glyph() +
  labs(title = "geom_glyph_ribbon()") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(color = color_palette[1],
                             family  = "mono"))

p2 <- vic_temp |>
  ggplot(aes(x_major = long,
             y_major = lat,
             x_minor = month,
             y_minor = tmin,
             yend_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
          fill = "antiquewhite", color = "white", inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
  add_glyph_boxes(width = rel(2.5), height = rel(1.5),
                  color = color_palette[2]) +
  add_ref_lines(width = rel(2.5), height = rel(1.5),
                color = color_palette[2]) +
  geom_glyph_segment(width = rel(2.5), height = rel(1.5),
                     color = color_palette[2]) +
  # Theme and aesthetic
  theme_glyph() +
  labs(title = "geom_glyph_segment()") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(color = color_palette[2]))

ribbon_segment <- grid.arrange(p1, p2, ncol = 2)

ggsave("ribbon_segment.png", ribbon_segment,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)


# Custom Rescale --------------------------------------------------

# Default rescale
def_rescale <- nsw_temp |>
  ggplot(aes(x_major = long,
             y_major = lat,
             x_minor = month,
             ymin_minor = tmin,
             ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "New South Wales"),
          fill = "antiquewhite", color = "white",
          inherit.aes = FALSE) +
  geom_glyph_ribbon() +
  theme_glyph() +
  labs(title = "Default Rescale") +
  coord_sf(xlim = c(140,155))


# Custom rescale function
custom_rescale <- function(dx) {
  rng <- range(dx, na.rm = TRUE)
  # Rescale dx to [0,1]
  rescaled <- (dx - rng[1]) / (rng[2] - rng[1])
}

# Customized rescale function
cus_rescale <- nsw_temp |>
  ggplot(aes(x_major = long,
             y_major = lat,
             x_minor = month,
             ymin_minor = tmin,
             ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "New South Wales"),
          fill = "antiquewhite", color = "white",
          inherit.aes = FALSE) +
  geom_glyph_ribbon(x_scale = custom_rescale,
                    y_scale = custom_rescale) +
  theme_glyph() +
  labs(title = "Custom Rescale") +
  coord_sf(xlim = c(140,155))

custom_rescale <- grid.arrange(def_rescale, cus_rescale, ncol = 2)

ggsave("custom_rescale.png", custom_rescale,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)

# Comparison between global and local rescale -------------------------

# Global rescale
p1 <- aus_temp |>
  ggplot(aes(
    x_major = long,
    y_major = lat,
    x_minor = month,
    y_minor = tmin,
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(width = rel(3), height = rel(2)) +
  # Add reference lines to each glyph
  add_ref_lines(width = rel(3), height = rel(2)) +
  # Glyph segment plot with global rescale
  geom_glyph_segment(global_rescale = TRUE,
                     width = rel(3), height = rel(2)) +
  labs(title = "Global Rescale") +
  theme_glyph()

# Local Rescale
p2 <- aus_temp |>
  ggplot(aes(
    x_major = long,
    y_major = lat,
    x_minor = month,
    y_minor = tmin,
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(width = rel(3), height = rel(2)) +
  # Add reference lines to each glyph
  add_ref_lines(width = rel(3), height = rel(2)) +
  # Glyph segment plot with local rescale
  geom_glyph_segment(global_rescale = FALSE,
                     width = rel(3), height = rel(2)) +
  labs(title = "Local Rescale") +
  theme_glyph()

global_local <- grid.arrange(p1, p2, ncol = 2)

ggsave("global_rescale.png", global_local,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)


# Temperature variation --------------------------------------------------

temp_var <- aus_temp |>
  ggplot(aes(
    x_major = long,
    y_major = lat,
    x_minor = month,
    y_minor = tmin,
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(
    width = 4,
    height = 3) +
  # Add points for weather station
  geom_point(aes(x = long, y = lat,
                 color = "Weather Station")) +
  # Customize the size of each glyph box using the width and height parameters.
  geom_glyph_segment(
    width = 4, height = 3,
    aes(color = "Temperature")) +
  # Theme and aesthetic
  scale_color_manual(
    values = c("Weather Station" = "firebrick",
               "Temperature" = "black")) +
  labs(color = "Data")  +
  theme_glyph()

ggsave("temp_var.png", temp_var,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)

# Color coded glyph --------------------------------------------------

prcp_temp <- aus_temp |>
  group_by(id) |>
  mutate(prcp = mean(prcp, na.rm = TRUE)) |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = month, ymin_minor = tmin,
             ymax_minor = tmax,
             fill = prcp, color = prcp)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  # Add glyph box to each glyph
  add_glyph_boxes() +
  # Add ref line to each glyph
  add_ref_lines() +
  # Add glyph ribbon plots
  geom_glyph_ribbon() +
  coord_sf(xlim = c(112,155)) +
  # Theme and aesthetic
  theme_glyph() +
  scale_fill_gradientn(colors = c("#ADD8E6", "#2b5e82", "dodgerblue4")) +
  scale_color_gradientn(colors = c( "#ADD8E6", "#2b5e82", "dodgerblue4")) +
  labs(fill = "Percepitation", color = "Percepitation")

ggsave("prcp_temp.png", prcp_temp,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)


# Historical Temperature Trend ------------------------------------------
hist_temp <- historical_temp |>
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143")) |>
  ggplot(aes(color = factor(year), fill = factor(year),
             group = interaction(year,id),
             x_major = long, y_major = lat,
             x_minor = month, ymin_minor = tmin,
             ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
          fill = "antiquewhite", color = "white",
          inherit.aes = FALSE)  +
  # Customized the dimension of each glyph with `width` and `height` parameters
  add_glyph_boxes(width = rel(2),
                  height = rel(1.5)) +
  add_ref_lines(width = rel(2),
                height = rel(1.5)) +
  geom_glyph_ribbon(alpha = 0.5,
                    width = rel(2),
                    height = rel(1.5)) +
  labs(x = "Longitude", y = "Latitude",
       color = "year", fill = "year") +
  # Theme and aesthetic
  theme_glyph() +
  theme(legend.position.inside = c(.4,0)) +
  scale_colour_wsj("colors6") +
  scale_fill_wsj("colors6")

ggsave("hist_temp.png", hist_temp,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)


# Legend Glyph -------------------------------------------------------------

set.seed(28493)
legend_glyph <- aus_temp |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = month, ymin_minor = tmin,
             ymax_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  add_glyph_boxes(color = "#227B94") +
  add_ref_lines(color = "#227B94") +
  add_glyph_legend(color = "#227B94", fill = "#227B94") +
  # Add a ribbon legend
  geom_glyph_ribbon(color = "#227B94", fill = "#227B94") +
  theme_glyph()

ggsave("legend_glyph.png", legend_glyph,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)

# Applications -------------------------------------------------------------

# Interactive plot with girafe ---------------------------------------------

USmap <- us_map(regions = "state") |>
  filter(full != "Alaska")

# Specify tooltip for ggiraph
flights <- flights |>
  mutate(tooltip = paste("origin: ",origin,
                         "\nmonth: ", month,
                         "\nmin_flights: ", min_flights,
                         "\nmax_flights: ", max_flights))

fl <- flights |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = month, y_minor = min_flights,
             yend_minor = max_flights,
             tooltip = tooltip)) +
  geom_sf(data = USmap, color = "white",
          fill = "antiquewhite", inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326)) +
  add_glyph_boxes(color = "#CD5C08") +
  add_ref_lines(color = "#CD5C08") +
  geom_glyph_segment(color = "#CD5C08") +
  theme_glyph()

# Interactive plot using ggiraph
# girafe(ggobj = fl)

# West and South region --------------------------------------------------

south <- us_map(include = .south_region)
west <- us_map(include = .west_region, exclude = c("AK", "HI"))


southR <- flights |>
  filter(origin %in% c("ATL", "CLT", "MCO", "DFW")) |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = month, ymin_minor = min_flights,
             ymax_minor = max_flights)) +
  geom_sf(data = south, color = "white",
          fill = "antiquewhite", inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326)) +
  add_glyph_boxes(color = "#A6B37D") +
  add_ref_lines(color = "#A6B37D") +
  geom_glyph_ribbon(color = "#A6B37D", fill = "#A6B37D")  +
  labs(title = "South Region") +
  theme_glyph()

westR <- flights |>
  filter(origin %in% c("PHX", "LAS", "LAX", "SEA")) |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = month, ymin_minor = min_flights,
             ymax_minor = max_flights)) +
  geom_sf(data = west, color = "white",
          fill = "antiquewhite", inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326)) +
  add_glyph_boxes(color = "#CD5C08") +
  add_ref_lines(color = "#CD5C08") +
  geom_glyph_ribbon(color = "#CD5C08", fill = "#CD5C08")  +
  labs(title = "West Region") +
  theme_glyph()

west_south <- grid.arrange(westR, southR, ncol = 2)

ggsave("west_south.png", west_south,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)

# Weekend vs. Weekday Patronage -----------------------------------------------

# download.file("https://data.gov.au/data/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/38a8a499-928c-4de6-b6d2-2fceb74870a9/download/gda94.zip", destfile = "VICLGA.zip")
#
# unzip("VICLGA.zip")
# Due to the file size, this shapefile is not included in the repository.
# Please download and unzip the "VICLGA" folder before loading the sf object.
vic_lga <- read_sf("data/GDA94/vic_lga.shp")

melbourne_lga <- c("Maribyrnong City", "Moonee Valley City","Merri-Bek City",
                   "Melbourne City", "Stonnington City", "Yarra City",
                  "Boroondara City", "Glen Eira City", "Moreland City",
                  "Darebin City")

melbourne_city_lga <- c("Flagstaff", "Melbourne Central", "Kensington", "South Kensington",
                        "Jolimont", "Flinders Street", "Flemington Bridge",
                        "North Melbourne", "Parliament", "Royal Park", "Southern Cross" )

# All train station in Melbourne City LGA
station <- train |>
  mutate(hour = factor(hour, levels = sprintf("%02d:00", 5:23), ordered = TRUE)) |>
  filter(station_name %in% melbourne_city_lga)

weekday <- station |>
  group_by(station_name) |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = as.numeric(hour), y_minor = min_weekday,
             yend_minor = max_weekday,
             color = services, fill = services)) +
  geom_sf(data = vic_lga |>
            filter(LGA_NAME == "Melbourne City"), color = "white",
          fill = "antiquewhite", inherit.aes = FALSE) +
  add_glyph_boxes(width = rel(0.01), height = rel(0.005)) +
  add_ref_lines(width = rel(0.01), height = rel(0.005)) +
  geom_glyph_segment(width = rel(0.01), height = rel(0.005),
                     global_rescale = FALSE) +
  theme_glyph() +
  labs(title = "Weekday Patronage") +
  scale_fill_gradientn(colors = c("#FFD09B", "#FD8B51", "#A66E38")) +
  scale_color_gradientn(colors = c("#FFD09B", "#FD8B51", "#A66E38")) +
  theme_glyph() +
  theme(legend.position = "None")


weekend <- station |>
  group_by(station_name) |>
  ggplot(aes(x_major = long, y_major = lat,
             x_minor = as.numeric(hour), y_minor = min_weekend,
             yend_minor = max_weekend,
             fill = services, color = services)) +
  geom_sf(data = vic_lga |>
            filter(LGA_NAME == "Melbourne City")
          , color = "white",
          fill = "antiquewhite", inherit.aes = FALSE) +
  add_glyph_boxes(width = rel(0.01), height = rel(0.005)) +
  add_ref_lines(width = rel(0.01), height = rel(0.005)) +
  geom_glyph_segment(width = rel(0.01), height = rel(0.005),
                     global_rescale = FALSE) +
  labs(title = "Weekend Patronage") +
  scale_fill_gradientn(colors = c("#FFD09B", "#FD8B51", "#A66E38")) +
  scale_color_gradientn(colors = c("#FFD09B", "#FD8B51", "#A66E38")) +
  theme_glyph() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.justification = c(0.5, 0.5))


weekend_weekday <- cowplot::plot_grid(
  weekday, weekend + theme(legend.position = "None"),
  align = 'h', axis = 'tb', ncol = 2, rel_widths = c(1, 1)
)

# Extract legend and place it next to the combined plot
legend <- cowplot::get_legend(weekend)

# Final plot with shared legend
final_plot <- cowplot::plot_grid(weekend_weekday, legend, rel_widths = c(2, 0.3))

ggsave("weekend_weekday.png", final_plot,
       path = "figures",width = 12, height = 6, units = "in", dpi = 300)

# Vline ---------------------------------------------------

# sample <- train |>
#   filter(mode %in% c("vline", "both"))
#
# set.seed(35545)
# vline_holiday <- train |>
#   filter(station_name %in% sample(sample$station_name, 15)) |>
#   ggplot(aes(x_major = long, y_major = lat,
#              x_minor = as.numeric(hour), ymin_minor = min_holiday,
#              ymax_minor = max_holiday)) +
#   geom_sf(data = vic_lga, color = "white",
#           fill = "antiquewhite", inherit.aes = FALSE) +
#   add_glyph_boxes(width = rel(0.1), height = rel(0.05),
#                   color = "firebrick") +
#   add_ref_lines(width = rel(0.1), height = rel(0.05),
#                 color = "firebrick") +
#   geom_glyph_ribbon(width = rel(0.1), height = rel(0.05),
#                     global_rescale = FALSE,
#                     color = "darkviolet", fill = "darkviolet") +
#   coord_sf(xlim = c(144,146), ylim = c(-38.2,-37.4)) +
#   labs(title = "V/Line Traffic During Holidays Across 10 Stations") +
#   theme_glyph()
#
# ggsave("vline.png", vline_holiday,
#        path = "figures",width = 12, height = 6, units = "in", dpi = 300)

#  Leaflet ------------------------------------------------------------------

# Generate PNG of all the ribbon glyph
purrr::map(1:length(zone1), function(i) {
  dt <- train |> filter(station_name == zone1[i])
  p <- dt |>
    ggplot(aes(x_major = long, y_major = lat,
               x_minor = hour, ymin_minor = min_weekday,
               ymax_minor = max_weekday)) +
    add_glyph_boxes(color = "#ffc40d",
                    fill = "#eff4ff", alpha = 0.5,
                    linewidth = 1, width = 3, height  = 1.5) +
    add_ref_lines(color = "#ffc40d", alpha = 1,
                  linewidth = 1, width = 3, height  = 1.5) +
    geom_glyph_ribbon(color = "#2b5797", fill = "#2b5797",
                      width = 3, height  = 1.5) +
    theme_void()

  file_path <- paste0("figures/glyph_", zone1[i], ".png")
  ggsave(file_path, plot = p, width = 3, height = 1.5, units = "in", dpi = 300,
         bg = "transparent")
  return(file_path)

}) -> train_png

# Save image path as RDS object
saveRDS(train_png, file = "data/train_list")




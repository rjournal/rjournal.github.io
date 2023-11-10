# Create a plot with all the countries: NUTS3 both age/gender.

library(sf)
library(RColorBrewer)
library(leaflet)

# LEaflet tiles
leaflet_tiles <- "CartoDB.Positron"
leaflet_tiles <- "Stamen.Watercolor"
leaflet_tiles <- "OpenStreetMap"


countries <- c("England", "Greece", "Italy", "Spain", "Switzerland")
# Do not use England for now
#countries <- countries[-1]

# Load data
res <- lapply(countries, function(X) {
  load(paste0("data/", X, ".RData"))

  aux <- d$province$none[, "Median.cat"]
  #aux <- d$province$none[, "median.excess"]
  # Fix geometry
  aux <- st_make_valid(aux)
  # Simplify boundaries
  aux <- st_simplify(aux, dTolerance = 1000)

  return(aux)
})

mymap <- do.call(rbind, res)

plot(mymap)

colors <- rev(brewer.pal(n = 10, name = "RdBu")[1:6])
labels <- levels(mymap$Median.cat)
factpal <- colorFactor(colors, mymap$Median.cat)

# Highlight option
h_options <- highlightOptions(color = "white", weight = 2,
  bringToFront = TRUE)



    res <- leaflet(mymap) %>%
      addProviderTiles(leaflet_tiles,
          options = providerTileOptions(opacity = 0.99))  %>%
      addPolygons(fillColor = ~ factpal(Median.cat), weight = 1, opacity = 1, color = "black", fillOpacity = 0.75, highlightOptions = h_options) %>%
      leaflet::addLegend("topright", colors = colors, labels = labels,
         values = values)


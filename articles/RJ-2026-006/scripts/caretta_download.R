#=========================================================#
# GeoThinneR: An R Package for Efficient Spatial Thinning
# of Species Occurrences and Point Data
#
# Download and filter Caretta caretta occurrence data
#
# Jorge Mestre-Tomás, 2026
#
# Output: .rda saved to ../data/caretta.rda
#=========================================================#

library(rgbif)
# Function to retrieve taxon key from scientific name
get_taxon_key <- function(name) {
  result <- rgbif::occ_search(scientificName = name, hasCoordinate = TRUE)
  if (!is.null(result$data)) {
    return(as.character(result$data$taxonKey[1]))
  } else {
    warning(paste("No taxon key found for", name))
    return(NA)
  }
}

# Set your GBIF credentials as environment variables in .Renviron:
# GBIF_USER=yourusername
# GBIF_PWD=yourpassword
# GBIF_EMAIL=your@email.com
user  <- Sys.getenv("GBIF_USER")
pwd   <- Sys.getenv("GBIF_PWD")
email <- Sys.getenv("GBIF_EMAIL")
taxon_key <- get_taxon_key("Caretta caretta")

# Download GBIF occurrence data in Mediterranean Sea
request_id <- as.character(rgbif::occ_download(
  rgbif::pred("taxonKey", taxon_key),
  rgbif::pred("hasCoordinate", TRUE),
  rgbif::pred_within("POLYGON ((-10 28, 38 28, 38 51, -10 51, -10 28))"),
  rgbif::pred("occurrenceStatus", "PRESENT"),
  rgbif::pred_gte("YEAR", 1950),
  rgbif::pred_lte("YEAR", 2025),
  format = "SIMPLE_CSV",
  user = user,
  pwd = pwd,
  email = email
))

response <- rgbif::occ_download_wait(request_id)

if (response$status == "SUCCEEDED"){
  temp <- tempfile(fileext = ".csv")
  download.file(response$downloadLink, temp, mode = "wb")
  caretta <- read.csv(unz(temp, paste0(response$key, ".csv")),
                      header = TRUE, sep = "\t", dec = ".")
  caretta <- caretta[, c("decimalLongitude", "decimalLatitude", "year", "species", "coordinateUncertaintyInMeters")]
  # Get polygon from mediterranean sea
  medit_path <- system.file("extdata", "mediterranean_sea.gpkg", package = "GeoThinneR")
  medit <- sf::st_read(medit_path)
  # Convert dataframe to sf object and set CRS
  points_sf <- sf::st_as_sf(caretta, coords = c("decimalLongitude", "decimalLatitude"), crs = sf::st_crs(medit))
  # Ensure consistent handling of spatial attributes
  sf::st_agr(points_sf) <- "constant"
  # Filter points in land
  filtered_points <- points_sf[sf::st_intersects(points_sf, medit, sparse = FALSE), ]
  filtered_points_df <- cbind(sf::st_coordinates(filtered_points), as.data.frame(filtered_points))
  colnames(filtered_points_df)[1:2] <- c("decimalLongitude", "decimalLatitude")
  caretta <- filtered_points_df[, !colnames(filtered_points_df) %in% c("geometry")]
  caretta <- caretta[!duplicated(caretta[, 1:2]), ]
} else{
  caretta <- NULL
  stop("Error during GBIF download")
}

if (!is.null(caretta)){
  rgbif::gbif_citation(request_id)$download
  # GBIF Occurrence Download https://doi.org/10.15468/dl.9jcjrm
  # Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-02-07

  save(caretta, file = "../data/caretta.rda")
}

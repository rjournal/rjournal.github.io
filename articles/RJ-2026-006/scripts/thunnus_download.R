#=========================================================#
# GeoThinneR: An R Package for Efficient Spatial Thinning
# of Species Occurrences and Point Data
#
# Download and filter Thunnus albacares occurrence data
#
# Jorge Mestre-Tomás, 2026
#
# Output: .rda saved to ../data/thunnus.rda
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
taxon_key <- get_taxon_key("Thunnus albacares")

# Download GBIF occurrence data worldwide
request_id <- as.character(rgbif::occ_download(
  rgbif::pred("taxonKey", taxon_key),
  rgbif::pred("hasCoordinate", TRUE),
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
  thunnus <- read.csv(unz(temp, paste0(response$key, ".csv")),
                      header = TRUE, sep = "\t", dec = ".")
  thunnus <- thunnus[, c("decimalLongitude", "decimalLatitude", "year")]
  thunnus <- thunnus[!duplicated(thunnus[, 1:2]), ]
} else{
  thunnus <- NULL
  stop("Error during GBIF download")
}

if (!is.null(thunnus)){
  rgbif::gbif_citation(request_id)$download
  # GBIF Occurrence Download https://doi.org/10.15468/dl.xsyrkh
  # Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-02-07

  save(thunnus, file = "../data/thunnus.rda")
}

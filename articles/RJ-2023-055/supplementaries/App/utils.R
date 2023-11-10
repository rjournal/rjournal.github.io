# Set cuts and categories
mycut <- function(xx, breaks) {
  res <- cut(xx, breaks, include.lowest = TRUE)
  lvls <- levels(res)
  res <- as.character(res)
  res[is.na(res)] <- "No data"

  res <- factor(res, levels = c("No data", lvls))

  levels(res)[nlevels(res)] <- paste0("> ", breaks[nlevels(res) - 1])

  return(invisible(res))
}


# Set st_drop_geometry if it is not defined
library(sf)
if(!existsFunction("st_drop_geometry")) {

  st_drop_geoemtry <- function (x) 
{
    if (!inherits(x, "sf")) 
        stop("st_drop_geometry only works with objects of class sf")
    st_set_geometry(x, NULL)
}


}

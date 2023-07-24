#' Identify Zero Coordinates
#' 
#' Removes or flags records with either zero longitude or latitude and a radius
#' around the point at zero longitude and zero latitude. These problems are
#' often due to erroneous data-entry or geo-referencing and can lead to typical
#' patterns of high diversity around the equator.
#' 
#' 
#' @param buffer numerical. The buffer around the 0/0 point,
#' where records should be flagged as problematic, in decimal
#' degrees.  Default = 0.5.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' x <- data.frame(species = "A", 
#'                 decimallongitude = c(0,34.84, 0, 33.98), 
#'                 decimallatitude = c(23.08, 0, 0, 15.98))
#'                 
#' cc_zero(x)
#' cc_zero(x, value = "flagged")
#' 
#' @export
#' @importFrom terra extract buffer vect
cc_zero <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude",
                    buffer = 0.5,
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing zero coordinates")
  }

  # plain zero in coordinates
  t1 <- !(x[[lon]] == 0 | x[[lat]] == 0)

  # radius around point 0/0
  dat <- terra::vect(x[, c(lon, lat)], geom = c(lon, lat))
  buff <- terra::buffer(terra::vect(data.frame("lat" = 0, "lon" = 0)), 
                      width = buffer)
  t2 <- is.na(terra::extract(buff, dat)[, 2])

  # combine test results
  out <- Reduce("&", list(t1, t2))

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

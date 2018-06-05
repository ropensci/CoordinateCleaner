#' Flag Coordinates in Vicinity of Country Capitals.
#' 
#' Flags records within a certain radius around country capitals. Poorly
#' geo-referenced occurrence records in biological databases are often
#' erroneously geo-referenced to capitals.
#' 
#' Note: the buffer radius is in degrees, thus will differ slightly between
#' different latitudes.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param buffer The buffer around each capital coordinate (the centre of the
#' city), where records should be flagged as problematic, in decimal degrees.
#' Default = 0.1.
#' @param ref a SpatialPointsDataframe. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataframe, but the structure must be identical to
#' \code{\link{capitals}}.  Default = \code{\link{capitals}}
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic . Default = \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#' 
#' cc_cap(x)
#' cc_cap(x, value = "flagged")
#' 
#' @export
#' @importFrom sp SpatialPoints proj4string "proj4string<-" over
#' @importFrom raster extent crop
#' @importFrom rgeos gBuffer
cc_cap <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   buffer = 0.1,
                   ref = NULL, 
                   value = "clean", 
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing country capitals")
  }

  # select relevant columns
  dat <- sp::SpatialPoints(x[, c(lon, lat)])

  # check for reference data and adapt projection of custom reference data
  if (is.null(ref)) {
    ref <- CoordinateCleaner::capitals
  } else {
    sp::proj4string(ref) <- ""
    warning("assuming lat/lon WGS84 for ref")
  }

  # subset reference data to data window to spead up the test
  limits <- raster::extent(dat) + buffer
  ref <- raster::crop(SpatialPoints(ref[, c("longitude", "latitude")]), limits)

  # test if any points fall within the buffer incase no capitals are found in
  # the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    ref <- rgeos::gBuffer(ref, width = buffer, byid = TRUE)
    out <- is.na(sp::over(x = dat, y = ref))
  }

  # create output based on value argument
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

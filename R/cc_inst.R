#' Flag Records in the Vicinity of Biodiversity Institutions
#' 
#' Flag records assigned to the location of zoos, botanical gardens, herbaria,
#' universities and museums, based on a global database of ~10,000 such
#' biodiversity institutions. Coordinates from these locations can be related
#' to data-entry errors, false automated geo-reference or individuals in
#' captivity/horticulture.
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
#' @param buffer numerical. The buffer around each province or country
#' centroid, where records should be flagged as problematic, in decimal
#' degrees.  Default = 0.001 (= ~ 100m).
#' @param ref a SpatialPointsDataframe. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataframe, but the structure must be identical to
#' \code{\link{institutions}}.  Default = \code{\link{institutions}}
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'                 
#' cc_inst(x, value = "flagged", buffer = 5)#large buffer for demonstration
#' 
#' @export
#' @importFrom raster crop extent
#' @importFrom sp SpatialPoints
cc_inst <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    buffer = 0.001,
                    ref = NULL, 
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing biodiversity institutions")
  }

  dat <- sp::SpatialPoints(x[, c(lon, lat)])

  # prepare reference dataset
  if (is.null(ref)) {
    ref <- CoordinateCleaner::institutions
    ref <- ref[!is.na(ref$decimallongitude) & !is.na(ref$decimallatitude), ]
  }
  limits <- raster::extent(dat) + buffer

  # subset of testdatset according to limits
  ref <- raster::crop(
    sp::SpatialPoints(ref[, c("decimallongitude", "decimallatitude")]),
    limits
  )

  # test reference data after limiting and do test in case no bdinstitutions
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

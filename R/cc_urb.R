#' Flag Records Inside Urban Areas
#' 
#' Flags records from inside urban areas, based on a geographic gazetteer.
#' Often records from large databases span substantial time periods (centuries)
#' and old records might represent habitats which today are replaced by city
#' area.
#' 
#' No default reference is provided with the package due to the large file size
#' of such (global) gazetteers. You can download an example here:
#' \url{https://github.com/azizka/CoordinateCleaner/blob/master/extra_gazetteers/urbanareas.rda}.
#' Can be any \code{SpatialPolygonsDataframe}, but the structure must be
#' identical to \code{urbanareas}.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ref a SpatialPolygonsDataframe. Providing the geographic gazetteer
#' with the urban areas. See details.
#' @param value a character string. Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector, with TRUE = test passed and FALSE = test failed/potentially
#' problematic (\dQuote{flags}). Default = \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @examples
#' 
#' \dontrun{
#' # load reference 
#' #See details section on where to download the reference data
#' load("extra_gazetteers/urbanareas.rda")
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'                 
#' cc_urb(x, ref = urbanareas)
#' cc_urb(x, value = "flags", ref = urbanareas)
#' }
#' 
#' @export
#' @importFrom sp SpatialPoints "proj4string<-" over proj4string
#' @importFrom raster extent crop
cc_urb <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   ref = NULL,
                   value = "clean", 
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flags"))

  if (verbose) {
    message("Testing urban areas")
  }

  # check for reference data. FOr this function reference hast to be supplied,
  # availble e.g. from the packages GitHub repository
  if (is.null(ref)) {
    stop("No referencepolygons found. Set 'urban.ref'")
  } else {
    sp::proj4string(ref) <- ""
    warning("assumning lat/lon for ref")
  }

  # Prepare input points and extent
  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  limits <- raster::extent(dat) + 1
  ref <- raster::crop(ref, limits)

  # test if any points fall within the buffer incase no capitals are found in
  # the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    out <- is.na(sp::over(x = dat, y = ref)[, 1])
  }

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flags = return(out))
}

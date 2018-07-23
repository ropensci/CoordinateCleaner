#' Flag Coordinates in Vicinity of Country and Province Centroids
#' 
#' Flags records within a radius around the geographic centroids of political
#' countries and provinces. Poorly geo-referenced occurrence records in
#' biological databases are often erroneously geo-referenced to centroids.
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
#' degrees.  Default = 0.1.
#' @param test a character string. Specifying the details of the test. One of
#' c(\dQuote{both}, \dQuote{country}, \dQuote{provinces}).  If both tests for
#' country and province centroids.
#' @param ref a SpatialPointsDataframe. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataframe, but the structure must be identical to
#' \code{\link{countryref}}.  Default = \code{\link{countryref}}
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
#' cc_cen(x)
#' cc_cen(x, value = "flagged")
#' 
#' @export
#' @importFrom sp SpatialPoints "proj4string<-" over proj4string
#' @importFrom raster extent crop
#' @importFrom rgeos gBuffer
cc_cen <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   buffer = 0.1,
                   test = "both", 
                   ref = NULL, 
                   value = "clean", 
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))
  match.arg(test, choices = c("both", "country", "provinces"))

  if (verbose) {
    message("Testing country centroids")
  }

  # select relevant columns
  dat <- sp::SpatialPoints(x[, c(lon, lat)])

  if (is.null(ref)) {
    ref <- CoordinateCleaner::countryref

    switch(test, country = {
      ref <- ref[ref$type == "country", ]
    }, province = {
      ref <- ref[ref$type == "province", ]
    })
  } else {
    proj4string(ref) <- ""
    warning("assuming lat/lon for centroids.ref")
  }

  limits <- raster::extent(dat) + buffer

  # subset of testdatset according to speed up buffer
  ref <- raster::crop(
    sp::SpatialPoints(ref[, c("centroid.lon", "centroid.lat")]),
    limits
  )

  # run buffering incase no centroids are found in the study area
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

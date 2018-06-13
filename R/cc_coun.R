#' Flag Coordinates Outside their Reported Country
#' 
#' Identifies mismatches between geographic coordinates and additional country
#' information (usually this information is reliably reported with specimens).
#' Such a mismatch can occur for example, if latitude and longitude are
#' switched.
#' 
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names, and a country assignment.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param iso3 a character string. The column with the country assignment of
#' each record in three letter ISO code. Default = \dQuote{countrycode}.
#' @param ref a SpatialPolygonsDataframe. Providing the geographic gazetteer.
#' Can be any SpatialPolygonsDataframe, but the structure must be identical to
#' \code{rnaturalearth::ne_countries(scale = "medium")}.  Default = \code{rnaturalearth::ne_countries(scale = "medium")}
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note Non-terrestrial records are ignored. Use \code{\link{cc_sea}} to flag
#' these. See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @examples
#' 
#' \dontrun{
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -20, 30), 
#'                 decimallatitude = runif(100, 35,60),
#'                 countrycode = "RUS")
#' 
#' cc_coun(x, value = "flagged")#non-terrestrial records are not flagged! Use cc_sea for these
#' }
#' 
#' @export
#' @importFrom sp SpatialPoints "proj4string<-" over
#' @importFrom raster crop
cc_coun <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    iso3 = "countrycode",
                    value = "clean", 
                    ref = NULL, 
                    verbose = TRUE) {

  # check function arguments for validity
  match.arg(value, choices = c("clean", "flagged"))
  if (!iso3 %in% names(x)) {
    stop("iso3 argument missing, please specify")
  }

  if (verbose) {
    message("Testing country identity")
  }

  # set reference and check for dependency
  if (is.null(ref)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Install the 'rnaturalearth' package or provide a custom reference",
        call. = FALSE
      )
    }
    ref <- rnaturalearth::ne_countries(scale = "medium")
    sp::proj4string(ref) <- ""
  } else {
    sp::proj4string(ref) <- ""
    warning("assuming lat/lon for country.ref")
  }

  # prepare data
  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  sp::proj4string(ref) <- ""
  ref <- raster::crop(ref, raster::extent(dat) + 1)

  # get country from coordinates and compare with provided country
  country <- sp::over(x = dat, y = ref)[, "iso_a3"]
  out <- as.character(country) == as.character(unlist(x[, iso3]))
  out[is.na(out)] <- FALSE # marine records are ignored / not tested

  # return output
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

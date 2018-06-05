#' Flag Records Assigned to GBIF Headquarters
#' 
#' Flags records within 0.5 degree radius around the GBIF headquarters in
#' Copenhagen, DK.
#' 
#' Not recommended if working with records from Denmark or the Copenhagen area.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
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
#' x <- data.frame(species = "A", 
#'                 decimallongitude = c(12.58, 12.58), 
#'                 decimallatitude = c(55.67, 30.00))
#'                 
#' cc_gbif(x)
#' cc_gbif(x, value = "flagged")
#' 
#' @export
#' @importFrom sp over SpatialPoints
#' @importFrom rgeos gBuffer
cc_gbif <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    value = "clean",
                    verbose = TRUE) {

  # check function argument validity
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing GBIF headquarters")
  }

  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  ref <- rgeos::gBuffer(sp::SpatialPoints(cbind(12.58, 55.67)), width = 0.5)
  message("running GBIF test, flagging records around Copenhagen")

  out <- sp::over(x = dat, y = ref)
  out <- is.na(out)

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

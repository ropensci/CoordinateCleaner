#' Flag Records with Identical lat/lon
#' 
#' Flags records with equal latitude and longitude coordinates, either exact or
#' absolute. Equal coordinates can often indicate data entry errors.
#' 
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param test character string. Defines if coordinates are compared exactly
#' (\dQuote{identical}) or on the absolute scale (i.e. -1 = 1,
#' \dQuote{absolute}). Default is to \dQuote{absolute}.
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the value argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#' 
#' cc_equ(x)
#' cc_equ(x, value = "flagged")
#' 
#' @export
cc_equ <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   test = "absolute",
                   value = "clean", 
                   verbose = TRUE) {

  # check value and test arguments
  match.arg(test, choices = c("absolute", "identical"))
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing equal lat/lon")
  }

  switch(test, absolute = {
    out <- !(abs(x[[lon]]) == abs(x[[lat]]))
  }, identical = {
    out <- !(x[[lon]] == x[[lat]])
  })

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))

  return(out)
}

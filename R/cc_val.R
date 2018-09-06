#' Check Coordinate Validity in lat/lon
#' 
#' Checks if all coordinates in a data set are valid in a latitude/longitude
#' coordinate reference system. That is non-numeric, not available coordinates
#' and lat >90, la <-90, lon > 180 and lon < -180 are flagged.
#' 
#' This test is obligatory before running any further tests of
#' CoordinateCleaner, as additional tests only run with valid coordinates.
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
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = c(runif(106, -180, 180), NA, "13W33'", "67,09", 305), 
#'                 decimallatitude = runif(110, -90,90))
#'                 
#' cc_val(x)
#' cc_val(x, value = "flagged")
#' 
#' @export
cc_val <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   value = "clean",
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing coordinate validity")
  }

  x[[lon]] <- suppressWarnings(as.numeric(as.character(x[[lon]])))
  x[[lat]] <- suppressWarnings(as.numeric(as.character(x[[lat]])))
  
  out <- list(
    is.na(x[[lon]]), 
    is.na(x[[lat]]), 
    x[[lon]] < -180, 
    x[[lon]] > 180,
    x[[lat]] < -90, 
    x[[lat]] > 90
  )

  out <- !Reduce("|", out)

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))

  return(out)
}
80

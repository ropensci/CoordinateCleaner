#' Identify Records with Identical lat/lon
#' 
#' Removes or flags records with equal latitude and longitude coordinates,
#' either exact or absolute. Equal coordinates can often indicate data entry
#' errors.
#' 
#' 
#' @param test character string. Defines if coordinates are compared exactly
#' (\dQuote{identical}) or on the absolute scale (i.e. -1 = 1,
#' \dQuote{absolute}). Default is to \dQuote{absolute}.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
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
    if (value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

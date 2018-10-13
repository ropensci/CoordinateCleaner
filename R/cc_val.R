#' Identify Invalid lat/lon Coordinates
#' 
#' Removes or flags non-numeric and not available coordinates
#' as well as lat >90, la <-90, lon > 180 and lon < -180 are flagged.
#' 
#' This test is obligatory before running any further tests of
#' CoordinateCleaner, as additional tests only run with valid coordinates.
#' 
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
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))

  return(out)
}

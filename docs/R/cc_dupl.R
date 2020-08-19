#' Identify Duplicated Records
#' 
#' Removes or flags duplicated records based on species name and coordinates, as well as
#' user-defined additional columns. True (specimen) duplicates or duplicates
#' from the same species can make up the bulk of records in a biological
#' collection database, but are undesirable for many analyses. Both can be
#' flagged with this function, the former given enough additional information.
#' 
#' 
#' @param species a character string. The column with the species name. Default
#' = \dQuote{species}.
#' @param additions a vector of character strings. Additional columns to be
#' included in the test for duplication. For example as below, collector name
#' and collector number.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = sample(x = 0:10, size = 100, replace = TRUE), 
#'                 decimallatitude = sample(x = 0:10, size = 100, replace = TRUE),
#'                 collector = "Bonpl",
#'                 collector.number = c(1001, 354),
#'                 collection = rep(c("K", "WAG","FR", "P", "S"), 20))
#' 
#' cc_dupl(x, value = "flagged")
#' cc_dupl(x, additions = c("collector", "collector.number"))
#' 
#' @export
cc_dupl <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    species = "species",
                    additions = NULL, 
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing duplicates")
  }
  # test duplication
  out <- !duplicated(x[, c(lon, lat, species, additions)])

  # create output based on value argument
  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

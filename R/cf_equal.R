#' Identify Fossils with equal min and max age
#' 
#' Removes or flags records with equal minimum and maximum age.
#' 
#' @inheritParams cf_age
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Temporal cleaning Fossils
#' @family fossils
#' 
#' @examples
#' 
#' minages <- runif(n = 10, min = 0.1, max = 25)
#' x <- data.frame(species = letters[1:10], 
#'                 min_ma = minages, 
#'                 max_ma = minages + runif(n = 10, min = 0, max = 10))
#' x <- rbind(x, data.frame(species = "z", 
#'                 min_ma = 5, 
#'                 max_ma = 5))
#'                 
#' cf_equal(x, value = "flagged")
#' 
#' @export
cf_equal <- function(x, min_age = "min_ma", 
                     max_age = "max_ma", 
                     value = "clean",
                     verbose = TRUE) {
  match.arg(value, choices = c("clean", "flagged"))


  if (verbose) {
    message("Testing age validity")
  }

  # min_age == max_age
  t1 <- x[[max_age]] == x[[min_age]]

  # min_age > max_age

  t2 <- x[[min_age]] > x[[max_age]]

  flags <- t1 | t2

  # create output
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE

  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out, na.rm = TRUE)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out, na.rm = TRUE)))
    }
  }

  # value
  switch(value, clean = return(x[out, ]), flagged = return(out))
}

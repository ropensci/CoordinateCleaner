#' Flag Fossils with equal min and max age
#' 
#' Flags records of fossil with equal minimum and maximum age.
#' 
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param min.age a character string. The column with the minimum age. Default
#' = \dQuote{min_ma}.
#' @param max.age a character string. The column with the maximum age. Default
#' = \dQuote{max_ma}.
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector, with TRUE = test passed and FALSE = test failed/potentially
#' problematic (\dQuote{flags}). Default = \dQuote{clean}.
#' @keywords Temporal cleaning Fossils
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
#' tc_equal(x, value = "flags")
#' 
#' @export
tc_equal <- function(x, min.age = "min_ma", max.age = "max_ma", value = "clean",
                     verbose = TRUE) {
  match.arg(value, choices = c("clean", "flags"))


  if (verbose) {
    message("Testing age validity")
  }

  # min.age == max.age
  t1 <- x[[max.age]] == x[[min.age]]

  # min.age > max.age

  t2 <- x[[min.age]] > x[[max.age]]

  flags <- t1 | t2

  # create output
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  # value
  switch(value, clean = return(x[out, ]), flags = return(out))
}

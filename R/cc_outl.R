#' Flag Geographic Outliers in Species Distributions
#' 
#' Flags records that are outliers in geographic space according to the method
#' defined via the \code{method} argument. Geographic outliers often represent
#' erroneous coordinates, for example due to data entry errors, imprecise
#' geo-references, individuals in horticulture/captivity.
#' 
#' The method for outlier identification depends on the \code{method} argument.
#' If \dQuote{outlier}: a boxplot method is used and records are flagged as
#' outliers if their \emph{mean} distance to all other records of the same
#' species is larger than mltpl * the interquartile range of the mean distance
#' of all records of this species. If \dQuote{mad}: the median absolute
#' deviation is used. In this case a record is flagged as outlier, if the
#' \emph{mean} distance to all other records of the same species is larger than
#' the median of the mean distance of all points plus/minus the mad of the mean
#' distances of all records of the species * mltpl. If \dQuote{distance}:
#' records are flagged as outliers, if the \emph{minimum} distance to the next
#' record of the species is > \code{tdi}.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species a character string. The column with the species name. Default
#' = \dQuote{species}.
#' @param method a character string.  Defining the method for outlier
#' selection.  See details. One of \dQuote{distance}, \dQuote{quantile},
#' \dQuote{mad}.  Default = \dQuote{quantile}.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'})to identify outliers. See details.  Default = 3.
#' @param tdi numeric.  The minimum absolute distance (\code{method ==
#' 'distance'}) of a record to all other records of a species to be identified
#' as outlier, in km. See details. Default = 1000.
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
#' cc_outl(x)
#' cc_outl(x, method = "quantile", value = "flagged")
#' cc_outl(x, method = "distance", value = "flagged", tdi = 10000)
#' cc_outl(x, method = "distance", value = "flagged", tdi = 1000)
#' 
#' @export
#' @importFrom geosphere distm distHaversine
#' @importFrom stats mad IQR median quantile
cc_outl <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    species = "species",
                    method = "quantile", 
                    mltpl = 3, 
                    tdi = 1000, 
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("distance", "quantile", "mad"))

  if (verbose) {
    message("Testing geographic outliers")
  }

  # split up into species
  splist <- split(x, f = as.character(x[[species]]))

  # remove duplicate records and make sure that there are at least two records
  # left
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 7]

  # loop over species and run outlier test
  flags <- lapply(splist, function(k) {
    test <- nrow(k[!duplicated(k), ])

    if (test > 7) {
      # absolute distance test with mean interpoint distance
      if (method == "distance") {
        dist <- geosphere::distm(k[, c(lon, lat)], 
                                 fun = geosphere::distHaversine)
        dist[dist == 0] <- NA

        mins <- apply(dist, 1, min, na.rm = TRUE)
        out <- which(mins > tdi * 1000)
      }

      # Quantile based test, with mean interpoint distances
      if (method == "quantile") {
        dist <- geosphere::distm(k[, c(lon, lat)], 
                                 fun = geosphere::distHaversine)
        dist[dist == 0] <- NA

        mins <- apply(dist, 1, mean, na.rm = TRUE)
        quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
        out <- which(mins < quo[1] - stats::IQR(mins) * mltpl | mins > quo[2] +
          stats::IQR(mins) * mltpl)
      }

      # MAD (Median absolute deviation) based test, 
      # calculate the mean distance to
      # all other points for each point, and then take the mad of this
      if (method == "mad") {
        dist <- geosphere::distm(k[, c(lon, lat)], 
                                 fun = geosphere::distHaversine)
        dist[dist == 0] <- NA

        mins <- apply(dist, 1, mean, na.rm = TRUE)
        quo <- stats::median(mins)
        tester <- stats::mad(mins)
        out <- which(mins < quo - tester * mltpl | mins > quo + tester *
          mltpl)
      }
    }
    # create output object
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[out]
    }
    return(ret)
  })

  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]

  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE

  if (verbose) {
    if (value == "ids") {
      message(sprintf("Flagged %s records.", length(flags)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, 
         clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(flags))
}

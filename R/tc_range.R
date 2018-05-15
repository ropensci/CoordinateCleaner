#' Flag Fossils with Extreme Age Ranges
#' 
#' Flags record with an unexpectedly large temporal range, based on a quantile
#' outlier test.
#' 
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param min.age a character string. The column with the minimum age. Default
#' = \dQuote{min_ma}.
#' @param max.age a character string. The column with the maximum age. Default
#' = \dQuote{max_ma}.
#' @param taxon a character string. The column with the taxon name. If
#' \dQuote{}, searches for outliers over the entire dataset, otherwise per
#' specified taxon. Default = \dQuote{accepted_name}.
#' @param method a character string.  Defining the method for outlier
#' selection.  See details. Either \dQuote{quantile} \dQuote{mad}, or
#' \dQuote{time}.  Default = \dQuote{quantile}.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'}) to identify outliers. See details.  Default = 3.
#' @param size.thresh numeric.  The minimum number of records needed for a
#' dataset to be tested. Default = 10.
#' @param max.range numeric. A absolute maximum time interval between min age
#' and max age. Only relevant for \code{method} = \dQuote{time}.
#' @param uniq.loc logical.  If TRUE only single records per location and time
#' point (and taxon if \code{taxon} != "") are used for the outlier testing.
#' Default = T.
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector, with TRUE = test passed and FALSE = test failed/potentially
#' problematic (\dQuote{flags}). Default = \dQuote{clean}.
#' @keywords Fossil Temporal cleaning
#' @examples
#' 
#' minages <- runif(n = 11, min = 0.1, max = 25)
#' x <- data.frame(species = c(letters[1:10], "z"),
#'                 lng = c(runif(n = 9, min = 4, max = 16), 75, 7),
#'                 lat = c(runif(n = 11, min = -5, max = 5)),
#'                 min_ma = minages, 
#'                 max_ma = minages + c(runif(n = 10, min = 0, max = 5), 25))
#' 
#' tc_range(x, value = "flags", taxon = "")
#' 
#' @export
#' @importFrom stats median mad IQR quantile dist
tc_range <- function(x, 
                     lon = "lng", 
                     lat = "lat", 
                     min.age = "min_ma", 
                     max.age = "max_ma",
                     taxon = "accepted_name", 
                     method = "quantile", 
                     mltpl = 5, 
                     size.thresh = 7,
                     max.range = 500, 
                     uniq.loc = FALSE, 
                     value = "clean", 
                     verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flags", "ids"))
  match.arg(method, choices = c("quantile", "mad", "time"))

  # select relevant columns and calcualte age range
  x$range <- x[[max.age]] - x[[min.age]]
  x$idf <- rownames(x)

  # time and uniq loc do not work together
  if (method == "time") {
    unig.loc <- FALSE
    warning("Using method = 'time', set 'uniq.loc' to FALSE")
  }

  if (taxon == "") {
    if (verbose) {
      message("Testing temporal range outliers on dataset level")
    }

    # Get unique records
    if (uniq.loc) {
      # select relevant columns
      rang <- x[, c(lon, lat, min.age, max.age, "idf", "range")]

      # round coordinates to one decimal
      rang[, lon] <- round(rang[, lon], 1)
      rang[, lat] <- round(rang[, lat], 1)

      # get unique occurrences
      rang <- rang[!duplicated(rang[, c(lon, lat, min.age, max.age)]), ]
    } else {
      rang <- x[, c(lon, lat, min.age, max.age, "idf", "range")]
    }

    # Are there points with outlier min or max ages
    if (method == "time") {
      flags <- which(rang$range > max.range)
      flags <- rang[flags, "idf"]
    }

    # Quantile based test, with mean interpoint distances
    if (method == "quantile") {
      quo <- stats::quantile(rang$range, 0.75, na.rm = TRUE)
      flags <- which(rang$range > (quo + stats::IQR(rang$range, na.rm = TRUE) *
        mltpl))
      flags <- rang[flags, "idf"]
    }

    # MAD (Median absolute deviation) based test, calculate the mean distance to
    # all other points for each point, and then take the mad of this
    if (method == "mad") {
      quo <- stats::median(rang$range)
      tester <- mad(rang$range, na.rm = TRUE)
      flags <- which(rang$range > quo + tester * mltpl)
      flags <- rang[flags, "idf"]
    }
  } else {
    if (verbose) {
      message("Testing temporal range outliers on taxon level\n")
    }
    if (uniq.loc) {
      # select relevant columns
      splist <- x[, c(lon, lat, min.age, max.age, taxon, "idf", "range")]

      # round coordinates to one decimal
      splist[, lon] <- round(splist[, lon], 1)
      splist[, lat] <- round(splist[, lat], 1)

      # get unique occurrences
      splist <- splist[!duplicated(splist[, c(
        taxon, lon, lat, min.age,
        max.age
      )]), ]
    } else {
      splist <- x[, c(lon, lat, min.age, max.age, taxon, "idf", "range")]
    }

    # split up into taxon range <- 'range'
    splist <- split(splist, f = as.character(splist[[taxon]]))

    # only keep taxa with at least size.thresh taxa leftleft
    test <- as.vector(unlist(lapply(splist, "nrow")))
    splist <- splist[test >= size.thresh]

    # loop over taxon and run outlier test
    flags <- lapply(splist, function(k) {
      rang <- k[["range"]]

      # Are there points with outlier min or max ages
      if (method == "time") {
        out <- which(rang > max.range)
        out <- k[out, "idf"]
      }

      # Quantile based test, with mean interpoint distances
      if (method == "quantile") {
        quo <- stats::quantile(rang, 0.75, na.rm = TRUE)
        out <- which(rang > quo + stats::IQR(rang, na.rm = TRUE) * mltpl)
        out <- k[out, "idf"]
      }

      # MAD (Median absolute deviation) based test, 
      # calculate the mean distance to
      # all other points for each point, and then take the mad of this
      if (method == "mad") {
        quo <- stats::median(rang)
        tester <- stats::mad(rang, na.rm = TRUE)
        out <- which(rang > quo + tester * mltpl)
        out <- k[out, "idf"]
      }
      # create output object
      if (length(out) == 0) {
        ret <- NA
      } else {
        ret <- unlist(out)
      }
      return(ret)
    })
  }

  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]

  out <- rep(TRUE, nrow(x))
  out[rownames(x) %in% flags] <- FALSE

  # also mark records that might not have been flagged due to the duplicate
  # removal above
  if (uniq.loc & any(!out)) {
    sel <- x[rownames(x) %in% flags, c(min.age, max.age)]
    sel <- unique(sel[[max.age]] - sel[[min.age]])
    tar <- x[[max.age]] - x[[min.age]]
    out[as.numeric(x[tar %in% sel, ]$idf)] <- FALSE
  }

  # remove identifier column
  x <- x[, names(x) != "idf"]

  if (verbose) {
    if (value == "ids") {
      message(sprintf("Flagged %s records.", length(flags)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), 
         flags = return(out), 
         ids = return(flags))
}

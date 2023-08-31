#' Identify Fossils with Extreme Age Ranges
#' 
#' Removes or flags records with an unexpectedly large temporal range, based on a quantile
#' outlier test.
#' 
#' 
#' @param max_range numeric. A absolute maximum time interval between min age
#' and max age. Only relevant for \code{method} = \dQuote{time}.
#' @inheritParams cf_age
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Fossil Temporal cleaning
#' @family fossils
#' 
#' @examples
#' 
#' minages <- runif(n = 11, min = 0.1, max = 25)
#' x <- data.frame(species = c(letters[1:10], "z"),
#'                 lng = c(runif(n = 9, min = 4, max = 16), 75, 7),
#'                 lat = c(runif(n = 11, min = -5, max = 5)),
#'                 min_ma = minages, 
#'                 max_ma = minages + c(runif(n = 10, min = 0, max = 5), 25))
#' 
#' cf_range(x, value = "flagged", taxon = "")
#' 
#' @export
#' @importFrom stats median mad IQR quantile dist
cf_range <- function(x, 
                     lon = "decimalLongitude", 
                     lat = "decimalLatitude", 
                     min_age = "min_ma", 
                     max_age = "max_ma",
                     taxon = "accepted_name", 
                     method = "quantile", 
                     mltpl = 5, 
                     size_thresh  = 7,
                     max_range = 500, 
                     uniq_loc = FALSE, 
                     value = "clean", 
                     verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("quantile", "mad", "time"))
  
  #Also allow PBDB standard names
  if ("lat" %in% names(x) & !(lat %in% names(x))) {
    lat <- "lat"
    warning(paste(lat, "not found. Using lng instead."))
  }
  
  if ("lng" %in% names(x) & !(lon %in% names(x))) {
    lon <- "lng"
    warning(paste(lon, "not found. Using lng instead."))
  }

  # select relevant columns and calcualte age range
  x$range <- x[[max_age]] - x[[min_age]]
  x$idf <- rownames(x)

  # time and uniq loc do not work together
  if (method == "time" & uniq_loc) {
    uniq_loc <- FALSE
    warning("Using method = 'time', set 'uniq_loc' to FALSE")
  }

  if (taxon == "") {
    if (verbose) {
      message("Testing temporal range outliers on dataset level")
    }

    # Get unique records
    if (uniq_loc) {
      # select relevant columns
      rang <- x[, c(lon, lat, min_age, max_age, "idf", "range")]

      # round coordinates to one decimal
      rang[, lon] <- round(rang[, lon], 1)
      rang[, lat] <- round(rang[, lat], 1)

      # get unique occurrences
      rang <- rang[!duplicated(rang[, c(lon, lat, min_age, max_age)]), ]
    } else {
      rang <- x[, c(lon, lat, min_age, max_age, "idf", "range")]
    }

    # Are there points with outlier min or max ages
    if (method == "time") {
      flags <- which(rang$range > max_range)
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
  } 
  else {
    if (verbose) {
      message("Testing temporal range outliers on taxon level")
    }
    if (uniq_loc) {
      # select relevant columns
      splist <- x[, c(lon, lat, min_age, max_age, taxon, "idf", "range")]

      # round coordinates to one decimal
      splist[, lon] <- round(splist[, lon], 1)
      splist[, lat] <- round(splist[, lat], 1)

      # get unique occurrences
      splist <- splist[!duplicated(splist[, c(
        taxon, lon, lat, min_age,
        max_age
      )]), ]
    } else {
      splist <- x[, c(lon, lat, min_age, max_age, taxon, "idf", "range")]
    }

    # split up into taxon range <- 'range'
    splist <- split(splist, f = as.character(splist[[taxon]]))

    # only keep taxa with at least size_thresh  taxa leftleft
    test <- as.vector(unlist(lapply(splist, "nrow")))
    splist <- splist[test >= size_thresh ]

    # loop over taxon and run outlier test
    flags <- lapply(splist, function(k) {
      rang <- k[["range"]]

      # Are there points with outlier min or max ages
      if (method == "time") {
        out <- which(rang > max_range)
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
  if (uniq_loc & any(!out)) {
    sel <- x[rownames(x) %in% flags, c(min_age, max_age)]
    sel <- unique(sel[[max_age]] - sel[[min_age]])
    tar <- x[[max_age]] - x[[min_age]]
    out[as.numeric(x[tar %in% sel, ]$idf)] <- FALSE
  }

  # remove identifier column
  x <- x[, names(x) != "idf"]

  if (verbose) {
    if(value == "clean"){
      if (value == "ids") {
        message(sprintf("Removed %s records.", length(flags)))
      } else {
        message(sprintf("Removed %s records.", sum(!out)))
      }
    }else{
      if (value == "ids") {
        message(sprintf("Flagged %s records.", length(flags)))
      } else {
        message(sprintf("Flagged %s records.", sum(!out)))
      }
    }
    

  }

  switch(value, clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(flags))
}

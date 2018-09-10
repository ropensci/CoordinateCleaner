#' Flag Fossils with Outlier Age
#' 
#' Flags records of fossils that are temporal outliers based on
#' interquantile ranges. 
#' 
#' The outlier detection is based on an interquantile range test. A temporal
#' distance matrix among all records is calculated based on a single point selected by random
#' between the minimum and maximum age for each record. The mean distance for
#' each point to all neighbours is calculated and the sum of these distances
#' is then tested against the interquantile range and flagged as an outlier if
#' $x > IQR(x) + q_75 * mltpl$. The test is replicated \sQuote{replicates}
#' times, to account for dating uncertainty. Records are flagged as outliers
#' if they are flagged by a fraction of more than \sQuote{flag.thres}
#' replicates. Only datasets/taxa comprising more than \sQuote{size_thresh}
#' records are tested. Distance are calculated as EUclidean distance.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' To identify unique records if \code{uniq_loc  = TRUE}.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}. To identify unique records if \code{uniq_loc  = T}.
#' @param min_age a character string. The column with the minimum age. Default
#' = \dQuote{min_ma}.
#' @param max_age a character string. The column with the maximum age. Default
#' = \dQuote{max_ma}.
#' @param taxon a character string. The column with the taxon name. If
#' \dQuote{}, searches for outliers over the entire dataset, otherwise per
#' specified taxon. Default = \dQuote{accepted_name}.
#' @param method a character string.  Defining the method for outlier
#' selection.  See details. Either \dQuote{quantile} or \dQuote{mad}.  Default
#' = \dQuote{quantile}.
#' @param size_thresh numeric.  The minimum number of records needed for a
#' dataset to be tested. Default = 10.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'}) to identify outliers. See details.  Default = 5.
#' @param replicates numeric. The number of replications for the distance
#' matrix calculation. See details.  Default = 5.
#' @param flag_thresh numeric.  The fraction of replicates necessary to flag a
#' record. See details. Default = 0.5.
#' @param uniq_loc logical.  If TRUE only single records per location and time
#' point (and taxon if \code{taxon} != "") are used for the outlier testing.
#' Default = T.
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @keywords Fossil Coordinate cleaning Temporal cleaning
#' @family fossils
#' @examples
#' 
#' minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
#' x <- data.frame(species = c(letters[1:10], rep("z", 2)),
#'                 min_ma = minages,
#'                 max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))
#' 
#' cf_age(x, value = "flagged", taxon = "")
#' 
#' # unique locations only
#' x <- data.frame(species = c(letters[1:10], rep("z", 2)),
#'                 decimallongitude = c(runif(n = 10, min = 4, max = 16), 75, 7),
#'                 decimallatitude = c(runif(n = 12, min = -5, max = 5)),
#'                 min_ma = minages, 
#'                 max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))
#' 
#' cf_age(x, value = "flagged", taxon = "", uniq_loc = TRUE)
#' 
#' @export
#' @importFrom stats runif
#' @importFrom geosphere distm distHaversine
#' @importFrom stats median mad IQR quantile dist
cf_age <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    min_age = "min_ma", 
                    max_age = "max_ma",
                    taxon = "accepted_name", 
                    method = "quantile", 
                    size_thresh = 7, 
                    mltpl = 5,
                    replicates = 5, 
                    flag_thresh = 0.5, 
                    uniq_loc = FALSE, 
                    value = "clean", 
                    verbose = TRUE) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("quantile", "mad"))
  
  # report analyses step
  if (verbose) {
    if (taxon == "") {
      message("Testing temporal outliers on dataset level")
    } else {
      message("Testing temporal outliers on taxon level")
    }
  }
  
  #Also allow PBDB standard names
  if("lat" %in% names(x) & !("decimallatitude" %in% names(x))){
    lat <- "lat"
    warning("decimallatitude not found. Using lat instead.")
  }
  
  #Enable recent records with only one time stamp
  if(all(x[[min_age]] == x[[max_age]], na.rm = TRUE)){
    replicates <- 1
  }
  
  out <- replicate(replicates, expr = {
    
    # create testing data by simulating points within the age range of each
    # individal method fossil
    x$samplepoint <- apply(X = x, 1, FUN = function(k) {
      if(!is.na(k[[min_age]])){
        if(k[[min_age]] == k[[max_age]]){
          k[[min_age]]
        }else{
          round(stats::runif(
            n = 1, min = as.numeric(k[[min_age]], na.rm = TRUE),
            max = as.numeric(k[[max_age]], na.rm = TRUE)), 2)
        }
      }else{
        NA
      }
    })

    if (taxon == "") {
      # select relevant columns 
      if(uniq_loc){
        test <- x[, c(lon, lat, min_age, max_age, "samplepoint")]
        test <- test[!duplicated(test[, c(lon, lat, min_age, max_age)]), ]
      }else{
        test <- x[, c( min_age, max_age, "samplepoint")]
      }

      # calculate temporal distance
      dis <- as.matrix(dist(test[, c("samplepoint")]))
      dis[dis == 0] <- NA
      
      # quantile based method
      if (method == "quantile") {
        mins <- apply(dis, 1, mean, na.rm = TRUE)
        quo <- quantile(mins, 0.75, na.rm = TRUE)
        flags <- mins > quo + IQR(mins, na.rm = TRUE) * mltpl
      }
      
      # MAD (Median absolute deviation) based test, 
      # calculate the mean distance to
      # all other points for each point, and then take the mad of this
      if (method == "mad") {
        mins <- apply(dis, 1, mean, na.rm = TRUE)
        quo <- stats::median(mins, na.rm = TRUE)
        tester <- stats::mad(mins, na.rm = TRUE)
        flags <- mins > quo + tester * mltpl
      }
    } else {
      # create identifier
      x$idf <- seq_len(nrow(x))
      # select relevant columns
      splist <- x[, c(
        lon, lat, min_age, max_age, "samplepoint", taxon,
        "idf"
      )]

            # remove identifier column
      x <- x[, names(x) != "idf"]
      
      # round coordinates to one decimal
      splist[, lon] <- round(splist[, lon], 1)
      splist[, lat] <- round(splist[, lat], 1)
      
      # get unique occurrences
      if (uniq_loc) {
        splist <- splist[!duplicated(splist[, c(
          taxon, lon, lat, min_age,
          max_age
        )]), ]
      }
      
      # split up into taxon
      splist <- split(splist, f = as.character(splist[[taxon]]))
      
      # only test taxa with a minimum number of records
      test <- as.vector(unlist(lapply(splist, "nrow")))
      splist <- splist[test >= size_thresh]
      
      # loop over taxon and run outlier test
      test <- lapply(splist, function(k) {

        # calculate temporal distance
        dis <- as.matrix(dist(k[, c("samplepoint")]))
        dis[is.na(dis)] <- 0
        
        # test if there are distances other than 0
        if (sum(!is.na(dis)) > 0) {
          if (method == "quantile") {
            mins <- apply(dis, 1, mean, na.rm = TRUE)
            quo <- stats::quantile(mins, 0.75, na.rm = TRUE)
            flags <- mins > quo + stats::IQR(mins, na.rm = TRUE) * mltpl
          }
          
          # MAD (Median absolute deviation) based test, 
          # calculate the mean distance to
          # all other points for each point, and then take the mad of this
          if (method == "mad") {
            mins <- apply(dis, 1, mean, na.rm = TRUE)
            quo <- stats::median(mins, na.rm = TRUE)
            tester <- stats::mad(mins, na.rm = TRUE)
            flags <- mins > quo + tester * mltpl
          }

          # create output object
          if (length(flags) == 0) {
            ret <- NA
          } else {
            ret <- k[flags, "idf"]
          }
        } else {
          ret <- NA
        }
        return(ret)
        })
      
      # transform the identifiers into true/flas flags 
      # in the same order as x; TRUE
      # means flagged
      flags <- rep(FALSE, nrow(x))
      flags[unlist(test)] <- TRUE
    }
    out <- !flags
    
    return(out)
  })
  
  frac <- apply(out, 1, "mean")
  
  out <- frac >= flag_thresh
  
  # also mark records that might not have been flagged due to the duplicate
  # removal above
  if (taxon == "" & any(!out) & uniq_loc) {
    supp <- x[!out, c(lon, lat, min_age, max_age)]
    test <- apply(supp, 1, function(k) {
      outp <- which(k[[lon]] == x[[lon]] &
                      k[[lat]] == x[[lat]] &
                      k[[min_age]] == x[[min_age]] & 
                      k[[max_age]] == x[[max_age]])
    })
    test <- unlist(test)
    test <- as.numeric(x[test, ]$idf)
    out[test] <- FALSE
  } else {
    if (any(!out) & uniq_loc) {
      supp <- x[!out, c(taxon, lon, lat, min_age, max_age)]
      outp <- list()
      for (j in seq_len(nrow(supp))) {
        k <- supp[j, ]
        outp[[j]] <- which(k[[taxon]] == x[[taxon]] & 
                             k[[lon]] == x[[lon]] &
                             k[[lat]] == x[[lat]] & 
                             k[[min_age]] == x[[min_age]] & 
                             k[[max_age]] == x[[max_age]])
      }
      test <- unlist(outp)
      test <- as.numeric(x[test, ]$idf)
      out[test] <- FALSE
    }
  }
  
  # report to screen
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out, na.rm = TRUE)))
  }
  
  switch(value, 
         clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(which(!out)))
}
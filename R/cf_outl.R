#' Identify Outlier Records in Space and Time
#' 
#' Removes or flags records of fossils that are spatio-temporal outliers based on
#' interquantile ranges. Records are flagged if they are either extreme in time
#' or space, or both.
#' 
#' The outlier detection is based on an interquantile range test. In a first
#' step a distance matrix of geographic distances among all records is
#' calculate. Subsequently a similar distance matrix of temporal distances
#' among all records is calculated based on a single point selected by random
#' between the minimum and maximum age for each record. The mean distance for
#' each point to all neighbours is calculated for both matrices and spatial and
#' temporal distances are scaled to the same range. The sum of these distanced
#' is then tested against the interquantile range and flagged as an outlier if
#' \eqn{x > IQR(x) + q_75 * mltpl}. The test is replicated \sQuote{replicates}
#' times, to account for temporal uncertainty. Records are flagged as outliers
#' if they are flagged by a fraction of more than \sQuote{flag.thres}
#' replicates. Only datasets/taxa comprising more than \sQuote{size_thresh}
#' records are tested. Note that geographic distances are calculated as
#' geospheric distances for datasets (or taxa) with fewer than 10,000 records
#' and approximated as Euclidean distances for datasets/taxa with 10,000 to
#' 25,000 records. Datasets/taxa comprising more than 25,000 records are
#' skipped.
#' 
#' @inheritParams cf_age
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Fossil Coordinate cleaning Temporal cleaning
#' @family fossils
#' 
#' @examples
#' 
#' minages <- c(runif(n = 11, min = 10, max = 25), 62.5)
#' x <- data.frame(species = c(letters[1:10], rep("z", 2)),
#'                 lng = c(runif(n = 10, min = 4, max = 16), 75, 7),
#'                 lat = c(runif(n = 12, min = -5, max = 5)),
#'                 min_ma = minages, 
#'                 max_ma = c(minages[1:11] + runif(n = 11, min = 0, max = 5), 65))
#' 
#' cf_outl(x, value = "flagged", taxon = "")
#' 
#' @export
#' @importFrom stats runif
#' @importFrom geosphere distm distHaversine
#' @importFrom stats median mad IQR quantile dist
cf_outl <- function(x, 
                    lon = "decimalLongitude", 
                    lat = "decimalLatitude", 
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
  
  #reset the rownames
  rownames(x) <- NULL

  # report analyses step
  if (verbose) {
    if (taxon == "") {
      message("Testing spatio-temporal outliers on dataset level")
    } else {
      message("Testing spatio-temporal outliers on taxon level")
    }
  }
  
  #Also allow PBDB standard names
  if ("lat" %in% names(x) & !(lat %in% names(x))){
    warning(paste0(lat, " not found. Using lat instead."))
    lat <- "lat"
  }
  
  if ("lng" %in% names(x) & !(lon %in% names(x))){
    warning(paste0(lon, " not found. Using lat instead."))
    lon <- "lng"
  }
  
  out <- replicate(replicates, expr = {

    # create testing data by simulating points within the age range of each
    # individal method fossil
    x$samplepoint <- apply(X = x, 1, FUN = function(k) {
      stats::runif(
        n = 1, min = as.numeric(k[[min_age]], na.rm = TRUE),
        max = as.numeric(k[[max_age]], na.rm = TRUE)
      )
    })
    x$samplepoint <- round(x$samplepoint, 2)


    if (taxon == "") {
      # select relevant columns test <- x[, c(lon, lat, min_age, max_age,
      # 'samplepoint', 'idf')]
      test <- x[, c(lon, lat, min_age, max_age, "samplepoint")]
      # round coordinates to one decimal
      test[, lon] <- round(test[, lon], 1)
      test[, lat] <- round(test[, lat], 1)

      # remove duplicates
      if (uniq_loc) {
        test <- test[!duplicated(test[, c(lon, lat, min_age, max_age)]), ]
      }

      # calculate geographic distance
      if (nrow(test) < 10000) {
        dis_geo <- geosphere::distm(test[, c(lon, lat)], 
                                    fun = geosphere::distHaversine) / 1000
      } else {
        dis_geo <- as.matrix(dist(test[, c(lon, lat)]))
        warning(paste("Large dataset, geographic space", 
                "treated as euclidean for outlier test", sep = " "))
      }

      dis_geo[dis_geo == 0] <- NA

      # calculate temporal distance
      dis_tmp <- as.matrix(dist(test[, c("samplepoint")]))
      dis_tmp[dis_tmp == 0] <- NA

      # scale distance to be comparable
      dis_tmp <- dis_tmp * 
        max(dis_geo, na.rm = TRUE) / 
        max(dis_tmp, na.rm = TRUE)

      # sum time and space
      dis <- round(dis_tmp + dis_geo, 0)

      # quantile based method
      if (method == "quantile") {
        mins <- apply(dis, 1, mean, na.rm = TRUE)
        quo <- quantile(mins, 0.75, na.rm = TRUE)
        flags <- mins > quo + IQR(mins, na.rm = TRUE) * mltpl
        # out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl) 
        # flags <- test[out,'idf']
      }

      # MAD (Median absolute deviation) based test, 
      # calculate the mean distance to
      # all other points for each point, and then take the mad of this
      if (method == "mad") {
        mins <- apply(dis, 1, mean, na.rm = TRUE)
        quo <- stats::median(mins, na.rm = TRUE)
        tester <- stats::mad(mins, na.rm = TRUE)
        flags <- mins > quo + tester * mltpl
        # out <- which(mins > quo + tester * mltpl) flags <- test[out, 'idf']
      }
    } else {
      # create identifier
      x$idf <- seq_len(nrow(x))
      # select relevant columns
      splist <- x[, c(
        lon, lat, min_age, max_age, "samplepoint", taxon,
        "idf"
      )]
      # splist <- x[, c(lon, lat, min_age, max_age, 'samplepoint', taxon)]

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

        # calculate geographic distance
        if (nrow(k) < 10000) {
          dis_geo <- geosphere::distm(k[, c(lon, lat)], 
                                      fun = geosphere::distHaversine) / 1000
        } else {
          dis_geo <- as.matrix(dist(k[, c(lon, lat)]))
          warning("Large dataset, geographic treated as 
                  euclidean for outlier test")
        }

        # calculate temporal distance
        dis_tmp <- as.matrix(dist(k[, c("samplepoint")]))

        # scale distance to be comparable
        dis_tmp <- dis_tmp * max(dis_geo, na.rm = TRUE) / max(dis_tmp,
          na.rm = TRUE
        )
        dis_tmp[is.na(dis_tmp)] <- 0

        # sum time and space
        dis <- round(dis_tmp + dis_geo, 0)

        # test if there are distances other than 0
        if (sum(!is.na(dis)) > 0) {
          if (method == "quantile") {
            mins <- apply(dis, 1, mean, na.rm = TRUE)
            quo <- stats::quantile(mins, 0.75, na.rm = TRUE)
            flags <- mins > quo + stats::IQR(mins, na.rm = TRUE) * mltpl
            # out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl) 
            #out <- k[out,'idf']
          }

          # MAD (Median absolute deviation) based test, 
          # calculate the mean distance to
          # all other points for each point, and then take the mad of this
          if (method == "mad") {
            mins <- apply(dis, 1, mean, na.rm = TRUE)
            quo <- stats::median(mins, na.rm = TRUE)
            tester <- stats::mad(mins, na.rm = TRUE)
            flags <- mins > quo + tester * mltpl
            # out <- which(mins > quo + tester * mltpl) out <- k[out, 'idf']
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

    # create vector with flagged rownumbers flags <-
    # as.numeric(as.vector(unlist(flags))) flags <- flags[!is.na(flags)]

    # create vector of logical flags out <- rep(TRUE, nrow(x))
    # out[as.numeric(flags)] <- FALSE
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
    if (value == "clean"){
      message(sprintf("Removed %s records.", sum(!out, na.rm = TRUE)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out, na.rm = TRUE)))
    }
  }

  switch(value, 
         clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(which(!out)))
}

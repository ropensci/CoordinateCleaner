#' Geographic and Temporal Cleaning of Records from Fossil Collections
#' 
#' Cleaning records by multiple empirical tests to flag potentially erroneous
#' coordinates and time-spans, addressing issues common in fossil collection
#' databases. Individual tests can be activated via the tests argument:
#' 
#' * agesequal tests for equal minimum and maximum age.
#' * centroids tests a radius around country centroids.
#' The radius is \code{centroids_rad}.
#' * countries tests if coordinates are from the
#' country indicated in the country column.  *Switched off by default.*
#' * equal tests for equal absolute longitude and latitude.
#' * gbif tests a one-degree radius around the GBIF
#' headquarters in Copenhagen, Denmark.
#' * institutions tests a radius around known
#' biodiversity institutions from \code{instiutions}. The radius is
#' \code{inst_rad}.
#' * spatiotemp test for records which are outlier in time and space. See below for details.
#' * temprange tests for records with unexpectedly large temporal ranges, 
#' using a quantile-based outlier test.
#' * validity checks if coordinates correspond to a lat/lon coordinate reference system.
#' This test is always on, since all records need to pass for any other test to run.
#' * zeros tests for plain zeros, equal latitude and
#' longitude and a radius around the point 0/0. The radius is \code{zeros_rad}.

#' The outlier detection in \sQuote{spatiotemp} is based on an interquantile range test. In a first
#' step a distance matrix of geographic distances among all records is
#' calculate. Subsequently a similar distance matrix of temporal distances
#' among all records is calculated based on a single point selected by random
#' between the minimum and maximum age for each record. The mean distance for
#' each point to all neighbours is calculated for both matrices and spatial and
#' temporal distances are scaled to the same range. The sum of these distanced
#' is then tested against the interquantile range and flagged as an outlier if
#' \eqn{x > IQR(x) + q_75 * mltpl}. The test is replicated \sQuote{replicates}
#' times, to account for temporal uncertainty. Records are flagged as outliers
#' if they are flagged by a fraction of more than \sQuote{flag_thresh}
#' replicates. Only datasets/taxa comprising more than \sQuote{size.thresh}
#' records are tested. Note that geographic distances are calculated as
#' geospheric distances for datasets (or taxa) with fewer than 10,000 records
#' and approximated as Euclidean distances for datasets/taxa with 10,000 to
#' 25,000 records. Datasets/taxa comprising more than 25,000 records are
#' skipped.
#' 
#' @param x data.frame. Containing fossil records, containing taxon names, ages, 
#' and geographic coordinates..
#' @param min_age character string. The column with the minimum age. Default
#' = \dQuote{min_ma}.
#' @param max_age character string. The column with the maximum age. Default
#' = \dQuote{max_ma}.
#' @param taxon character string. The column with the taxon name. If
#' \dQuote{}, searches for outliers over the entire dataset, otherwise per
#' specified taxon. Default = \dQuote{accepted_name}.
#' @param tests vector of character strings, indicating which tests to run. 
#' See details for all tests available. Default = c("centroids", 
#' "equal", "gbif", "institutions", "temprange", "spatiotemp", "agesequal", "zeros")
#' @param outliers_threshold numerical.  The multiplier for the interquantile
#' range for outlier detection. The higher the number, the more conservative
#' the outlier tests.  See \code{\link{cf_outl}} for details. Default = 3.
#' @param outliers_replicates numeric. The number of replications for the
#' distance matrix calculation. See details.  Default = 5.
#' @inheritParams clean_coordinates
#' 
#' @inherit clean_coordinates return
#' 
#' @note Always tests for coordinate validity: non-numeric or missing
#' coordinates and coordinates exceeding the global extent (lon/lat, WGS84).
#' 
#' See \url{https://ropensci.github.io/CoordinateCleaner/} for more details
#' and tutorials.
#' 
#' @keywords Fossil Coordinate cleaning Temporal cleaning
#' @family Wrapper functions
#' 
#' @examples
#' 
#' minages <- runif(250, 0, 65)
#' exmpl <- data.frame(accepted_name = sample(letters, size = 250, replace = TRUE),
#'                     decimallongitude = runif(250, min = 42, max = 51),
#'                     decimallatitude = runif(250, min = -26, max = -11),
#'                     min_ma = minages,
#'                     max_ma = minages + runif(250, 0.1, 65))
#' 
#' test <- clean_fossils(x = exmpl)
#' 
#' summary(test)
#' 
#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
#' @md
clean_fossils <- function(x, 
                          lon = "decimallongitude", 
                          lat = "decimallatitude", 
                          min_age = "min_ma",
                          max_age = "max_ma", 
                          taxon = "accepted_name", 
                          tests = c("agesequal", "centroids", "equal",
                                    "gbif", "institutions", 
                                    "spatiotemp", "temprange", "validity",
                                    "zeros"), 
                          countries = NULL, 
                          centroids_rad = 0.05,
                          centroids_detail = "both", 
                          inst_rad = 0.001, 
                          outliers_method = "quantile",
                          outliers_threshold = 5, 
                          outliers_size = 7, 
                          outliers_replicates = 5, 
                          zeros_rad = 0.5,
                          centroids_ref = NULL, 
                          country_ref = NULL, 
                          inst_ref = NULL, 
                          value = "spatialvalid", 
                          verbose = TRUE,
                          report = FALSE) {


  # check function arguments
  match.arg(value, choices = c("spatialvalid", "flagged", "clean"))
  match.arg(centroids_detail, choices = c("both", "country", "provinces"))
  match.arg(outliers_method, choices = c("distance", "quantile", "mad"))

  # check column names
  nams <- c(lon, lat, taxon, min_age, max_age)
  if (!all(nams %in% names(x))) {
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  #test if countries column is are provided for countries test
  if (is.null(countries) & "countries" %in% tests) {
    stop("provide countries column or remove countries test")
  }
  
  #initiate output
  out <- data.frame(matrix(NA, nrow = nrow(x), ncol = 10))
  colnames(out) <- c("aeq", "cen",  "con","equ",
                     "gbf", "inst", "spt", "trg",
                     "val", "zer")
  
  # Run tests Validity, check if coordinates fit to lat/long system, this has
  # to be run all the time, as otherwise the other tests don't work
  val <- cc_val(x, lon = lon, lat = lat, verbose = verbose, value = "flagged")

  if (!all(val)) {
    stop("invalid coordinates found:\n", paste(which(!val), "\n"))
  }

  ## Equal coordinates
  if ("equal" %in% tests) {
    out$equ <- cc_equ(x,
      lon = lon, lat = lat, verbose = verbose, value = "flagged",
      test = "absolute"
    )
  }

  ## Zero coordinates
  if ("zeros" %in% tests) {
    out$zer <- cc_zero(x,
      lon = lon, lat = lat, buffer = zeros_rad, verbose = verbose,
      value = "flagged"
    )
  }
  ## Centroids
  if ("centroids" %in% tests) {
    out$cen <- cc_cen(x,
      lon = lon, lat = lat, buffer = centroids_rad, test = centroids_detail,
      ref = centroids_ref, value = "flagged", verbose = verbose
    )
  }

  # Country check
  if ("countries" %in% tests) {
    out$con <- cc_coun(x,
      lon = lon, lat = lat, iso3 = countries, ref = country_ref,
      verbose = verbose, value = "flagged"
    )
  }

  # Spatiotemporal
  if ("spatiotemp" %in% tests) {
    # if(nrow(x) < 10000){ 
    # otl.flag <- cf_outl(x, lon = lon, lat = lat, min_age
    # = min_age, max_age = max_age, taxon = '', 
    # method = outliers_method, mltpl
    # = outliers_threshold, 
    #replicates = outliers_replicates, value = 'ids',
    # verbose = verbose) 
    #otl <- rep(TRUE, nrow(x)) otl[rownames(otl) %in%
    # otl.flag] <- FALSE 
    #}else{ warning('Very large dataset skipped dataset
    # level outlier test') otl <- rep(TRUE, nrow(x)) }

    if (taxon != "") {
      # otl.test <- table(x[taxon]) 
      # otl.test <- otl.test[otl.test > outliers_size]
      # otl.test <- x[x[[taxon]] %in% names(otl.test),] otl.test <- otl.test[,
      # c(taxon, lon, lat, min_age, max_age)]

      otl.flag <- cf_outl(
        x = x, lon = lon, lat = lat, min_age = min_age,
        max_age = max_age, taxon = taxon, size_thresh = outliers_size,
        method = outliers_method, mltpl = outliers_threshold, 
        replicates = outliers_replicates,
        value = "ids", verbose = verbose
      )

      out$spt <- rep(TRUE, nrow(x))

      out$spt[otl.flag] <- FALSE
    }
  }

  # Temporal, range size outliers
  if ("temprange" %in% tests) {
    # always over entire dataset
    test <- x
    out$trg <- cf_range(
      x = test, taxon = "", min_age = min_age, max_age = max_age,
      lon = lon, lat = lat, method = outliers_method, 
      mltpl = outliers_threshold,
      value = "flagged", verbose = verbose
    )

    # per taxon
    if (taxon != "") {
      ran.test <- table(x[taxon])
      ran.test <- ran.test[ran.test > outliers_size]
      ran.test <- x[x[[taxon]] %in% names(ran.test), ]
      ran.test <- ran.test[, c(taxon, lon, lat, min_age, max_age)]

      # ran.otl <- rep(TRUE, nrow(x)) ran.otl[ran.otl.flag] <- FALSE

      trg.flag <- cf_range(ran.test,
        taxon = taxon, min_age = min_age,
        max_age = max_age, lon = lon, lat = lat, method = outliers_method,
        mltpl = outliers_threshold, value = "ids", verbose = verbose
      )

      out$trg[trg.flag] <- FALSE
    }
  }

  # Temporal, Equal ages
  if ("agesequal" %in% tests) {
    out$aeq <- cf_equal(x,
      min_age = min_age, max_age = max_age, value = "flagged",
      verbose = verbose
    )
  }

  # GBIF headquarters
  if ("gbif" %in% tests) {
    out$gbf <- cc_gbif(x, lon = lon, lat = lat, 
                       verbose = verbose, value = "flagged")
  }
  
  # Biodiversity institution
  if ("institutions" %in% tests) {
    out$inst <- cc_inst(x,
      lon = lon, lat = lat, ref = inst_ref, buffer = inst_rad,
      verbose = verbose, value = "flagged"
    )
  }

  # prepare output data
  out <- Filter(function(x) !all(is.na(x)), out)
  suma <- as.vector(Reduce("&", out))

  if (verbose) {
    if (!is.null(suma)) {
      message(sprintf(
        "Flagged %s of %s records, EQ = %s", sum(!suma, na.rm = TRUE),
        length(suma), round(sum(!suma, na.rm = TRUE) / length(suma), 2)
      ))
    } else {
      message("flagged 0 records, EQ = 0")
    }
  }
  if (value == "spatialvalid") {
    ret <- data.frame(x, out, summary = suma)
    names(ret) <- c(names(x),
                    paste(".", names(out), sep = ""),
                    ".summary")
    
    class(ret) <- c("spatialvalid", "data.frame", class(out))
    out <- ret
    
    if (report) {
      report <- "clean_fossils_report.txt"
    }
    if (is.character(report)) {
      suma <- data.frame(
        Test = as.character(names(out[-(1:3)])), 
        Flagged.records = colSums(!out[-(1:3)]),
        stringsAsFactors = FALSE
      )
      suma <- rbind(suma, c("Total number of records", length(out$summary)))
      suma <- rbind(suma, c("Error Quotient", round(sum(!out$summary,
        na.rm = TRUE
      ) / length(out$summary), 2)))

      write.table(suma, report, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  }
  if (value == "clean") {
    out <- x[suma, ]
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
  }
  if (value == "flagged") {
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
    out <- suma
  }

  return(out)
}

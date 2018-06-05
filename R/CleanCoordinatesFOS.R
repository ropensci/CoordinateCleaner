# A function to clean fossil data


#' Geographic and Temporal Cleaning of Records from Fossil Collections
#' 
#' Cleaning records by multiple empirical tests to flag potentially erroneous
#' coordinates and time-spans, addressing issues common in fossil collection
#' databases.
#' 
#' The outlier detection is based on an interquantile range test. In a first
#' step a distance matrix of geographic distances among all records is
#' calculate. Subsequently a similar distance matrix of temporal distances
#' among all records is calculated based on a single point selected by random
#' between the minimum and maximum age for each record. The mean distance for
#' each point to all neighbours is calculated for both matrices and spatial and
#' temporal distances are scaled to the same range. The sum of these distanced
#' is then tested against the interquantile range and flagged as an outlier if
#' $x > IQR(x) + q_75 * mltpl$. The test is replicated \sQuote{replicates}
#' times, to account for temporal uncertainty. Records are flagged as outliers
#' if they are flagged by a fraction of more than \sQuote{flag.thres}
#' replicates. Only datasets/taxa comprising more than \sQuote{size.thresh}
#' records are tested. Note that geographic distances are calculated as
#' geospheric distances for datasets (or taxa) with less than 10,000 records
#' and approximated as Euclidean distances for datasets/taxa with 10,000 to
#' 25,000 records. Datasets/taxa comprising more than 25,000 records are
#' skipped.
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
#' @param countries a character string. A vector of the same length as rows in
#' x, with country information for each record in ISO3 format.  If missing, the
#' countries test is skipped.
#' @param centroids logical. If TRUE, tests a radius around country centroids.
#' The radius is \code{centroids.rad}. Default = TRUE.
#' @param countrycheck logical.  If TRUE, tests if coordinates are from the
#' country indicated in the country column.  Default = FALSE.
#' @param equal logical.  If TRUE, tests for equal absolute longitude and
#' latitude.  Default = TRUE.
#' @param GBIF logical.  If TRUE, tests a one-degree radius around the GBIF
#' headquarters in Copenhagen, Denmark.  Default = TRUE.
#' @param institutions logical. If TRUE, tests a radius around known
#' biodiversity institutions from \code{instiutions}. The radius is
#' \code{inst.rad}. Default = TRUE.
#' @param temp.range.outliers logical. If TRUE, tests for records with
#' unexpectedly large temporal ranges, using a quantile-based outlier test.
#' Default = TRUE.
#' @param spatio.temp.outliers logical. IF TRUE, test for records which are
#' outlier in time and space. See \code{\link{dc_round}} for details.  Default
#' = TRUE.
#' @param temp.ages.equal logical. If TRUE, flags records with equal minimum
#' and maximum age. Default = TRUE.
#' @param zeros logical. If TRUE, tests for plain zeros, equal latitude and
#' longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.
#' Default = TRUE.
#' @param centroids.rad numeric. The side length of the rectangle around
#' country centroids in degrees. Default = 0.01.
#' @param centroids.detail a \code{character string}. If set to
#' \sQuote{country} only country (adm-0) centroids are tested, if set to
#' \sQuote{provinces} only province (adm-1) centroids are tested.  Default =
#' \sQuote{both}.
#' @param inst.rad numeric. The radius around biodiversity institutions
#' coordinates in degrees. Default = 0.001.
#' @param outliers.method The method used for outlier testing. See details.
#' @param outliers.threshold numerical.  The multiplier for the interquantile
#' range for outlier detection. The higher the number, the more conservative
#' the outlier tests.  See \code{\link{dc_round}} for details. Default = 3.
#' @param outliers.size numerical.  The minimum number of records in a dataset
#' to run the taxon-specific outlier test.  Default = 7.
#' @param outliers.replicates numeric. The number of replications for the
#' distance matrix calculation. See details.  Default = 5.
#' @param zeros.rad numeric. The radius around 0/0 in degrees. Default = 0.5.
#' @param centroids.ref a \code{data.frame} with alternative reference data for
#' the centroid test. If missing, the \code{centroids} dataset is used.
#' Alternatives must be identical in structure.
#' @param country.ref a \code{SpatialPolygonsDataFrame} as alternative
#' reference for the countrycheck test. If missing, the
#' \code{rnaturalearth:ne_countries('medium')} dataset is used.
#' @param inst.ref a \code{data.frame} with alternative reference data for the
#' biodiversity institution test. If missing, the \code{institutions} dataset
#' is used.  Alternatives must be identical in structure.
#' @param value a character string defining the output value. See the value
#' section for details. one of \sQuote{spatialvalid}, \sQuote{summary},
#' \sQuote{cleaned}. Default = \sQuote{\code{spatialvalid}}.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged
#' @param report logical or character.  If TRUE a report file is written to the
#' working directory, summarizing the cleaning results. If a character, the
#' path to which the file should be written.  Default = FALSE.
#' @return Depending on the output argument: \describe{
#' \item{list("spatialvalid")}{an object of class \code{spatialvalid} with one
#' column for each test. TRUE = clean coordinate, FALSE = potentially
#' problematic coordinates.  The summary column is FALSE if any test flagged
#' the respective coordinate.} \item{list("flagged")}{a logical vector with the
#' same order as the input data summarizing the results of all test. TRUE =
#' clean coordinate, FALSE = potentially problematic (= at least one test
#' failed).} \item{list("cleaned")}{a \code{data.frame} of cleaned coordinates
#' if \code{species = NULL} or a \code{data.frame} with cleaned coordinates and
#' species ID otherwise} }
#' @note Always tests for coordinate validity: non-numeric or missing
#' coordinates and coordinates exceeding the global extent (lon/lat, WGS84).
#' 
#' See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details
#' and tutorials.
#' @keywords Fossil Coordinate cleaning Temporal cleaning
#' @examples
#' 
#' minages <- runif(250, 0, 65)
#' exmpl <- data.frame(accepted_name = sample(letters, size = 250, replace = TRUE),
#'                     lng = runif(250, min = 42, max = 51),
#'                     lat = runif(250, min = -26, max = -11),
#'                     min_ma = minages,
#'                     max_ma = minages + runif(250, 0.1, 65))
#' 
#' test <- CleanCoordinatesFOS(x = exmpl)
#' 
#' summary(test)
#' 
#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
CleanCoordinatesFOS <- function(x, 
                                lon = "lng", 
                                lat = "lat", 
                                min.age = "min_ma",
                                max.age = "max_ma", 
                                taxon = "accepted_name", 
                                countries = "cc", 
                                centroids = TRUE,
                                countrycheck = TRUE, 
                                equal = TRUE, 
                                GBIF = TRUE, 
                                institutions = TRUE, 
                                temp.range.outliers = TRUE,
                                spatio.temp.outliers = TRUE, 
                                temp.ages.equal = TRUE, 
                                zeros = TRUE, 
                                centroids.rad = 0.05,
                                centroids.detail = "both", 
                                inst.rad = 0.001, 
                                outliers.method = "quantile",
                                outliers.threshold = 5, 
                                outliers.size = 7, 
                                outliers.replicates = 5, 
                                zeros.rad = 0.5,
                                centroids.ref, 
                                country.ref, 
                                inst.ref, 
                                value = "spatialvalid", 
                                verbose = TRUE,
                                report = FALSE) {


  # check function arguments
  match.arg(value, choices = c("spatialvalid", "flagged", "clean"))
  match.arg(centroids.detail, choices = c("both", "country", "provinces"))
  match.arg(outliers.method, choices = c("distance", "quantile", "mad"))

  # check column names
  nams <- c(lon, lat, taxon, min.age, max.age)
  if (!all(nams %in% names(x))) {
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  if (is.null(countries) | !countries %in% names(x)) {
    countries <- NULL
    if (countrycheck) {
      countrycheck <- FALSE
      warning("countries missing, countrycheck set to FALSE")
    }
  }
  # if(is.null(taxon)){ if (spatio.temp.outliers) { outliers <- FALSE
  # warning('is.null(taxon), outliers test skipped') } taxon <- NULL }
  if (missing(centroids.ref)) {
    centroids.ref <- NULL
  }
  if (missing(country.ref)) {
    country.ref <- NULL
  }
  if (missing(inst.ref)) {
    inst.ref <- NULL
  }

  # Run tests Validity, check if coordinates fit to lat/long system, this has
  # to be run all the time, as otherwise the other tests don't work
  val <- cc_val(x, lon = lon, lat = lat, verbose = verbose, value = "flagged")

  if (!all(val)) {
    stop("invalid coordinates found:\n", paste(which(!val), "\n"))
  }

  ## Equal coordinates
  if (equal) {
    equ <- cc_equ(x,
      lon = lon, lat = lat, verbose = verbose, value = "flagged",
      test = "absolute"
    )
  } else {
    equ <- rep(NA, dim(x)[1])
  }

  ## Zero coordinates
  if (zeros) {
    zer <- cc_zero(x,
      lon = lon, lat = lat, buffer = zeros.rad, verbose = verbose,
      value = "flagged"
    )
  } else {
    zer <- rep(NA, dim(x)[1])
  }

  ## Centroids
  if (centroids) {
    cen <- cc_cen(x,
      lon = lon, lat = lat, buffer = centroids.rad, test = centroids.detail,
      ref = centroids.ref, value = "flagged", verbose = verbose
    )
  } else {
    cen <- rep(NA, nrow(x))
  }

  # Country check
  if (countrycheck) {
    con <- cc_coun(x,
      lon = lon, lat = lat, iso3 = countries, ref = country.ref,
      verbose = verbose, value = "flagged"
    )
  } else {
    con <- rep(NA, dim(x)[1])
  }

  # Spatiotemporal
  if (spatio.temp.outliers) {
    # if(nrow(x) < 10000){ 
    # otl.flag <- tc_outl(x, lon = lon, lat = lat, min.age
    # = min.age, max.age = max.age, taxon = '', 
    # method = outliers.method, mltpl
    # = outliers.threshold, 
    #replicates = outliers.replicates, value = 'ids',
    # verbose = verbose) 
    #otl <- rep(TRUE, nrow(x)) otl[rownames(otl) %in%
    # otl.flag] <- FALSE 
    #}else{ warning('Very large dataset skipped dataset
    # level outlier test') otl <- rep(TRUE, nrow(x)) }

    if (taxon != "") {
      # otl.test <- table(x[taxon]) 
      # otl.test <- otl.test[otl.test > outliers.size]
      # otl.test <- x[x[[taxon]] %in% names(otl.test),] otl.test <- otl.test[,
      # c(taxon, lon, lat, min.age, max.age)]

      otl.flag <- tc_outl(
        x = x, lon = lon, lat = lat, min.age = min.age,
        max.age = max.age, taxon = taxon, size.thresh = outliers.size,
        method = outliers.method, mltpl = outliers.threshold, 
        replicates = outliers.replicates,
        value = "ids", verbose = verbose
      )

      otl <- rep(TRUE, nrow(x))

      otl[otl.flag] <- FALSE
    }
  } else {
    otl <- rep(NA, nrow(x))
  }

  # Temporal, range size outliers
  if (temp.range.outliers) {
    # always over entire dataset
    test <- x
    ran.otl <- tc_range(
      x = test, taxon = "", min.age = min.age, max.age = max.age,
      lon = lon, lat = lat, method = outliers.method, 
      mltpl = outliers.threshold,
      value = "flagged", verbose = verbose
    )

    # per taxon
    if (taxon != "") {
      ran.test <- table(x[taxon])
      ran.test <- ran.test[ran.test > outliers.size]
      ran.test <- x[x[[taxon]] %in% names(ran.test), ]
      ran.test <- ran.test[, c(taxon, lon, lat, min.age, max.age)]

      # ran.otl <- rep(TRUE, nrow(x)) ran.otl[ran.otl.flag] <- FALSE

      ran.otl.flag <- tc_range(ran.test,
        taxon = taxon, min.age = min.age,
        max.age = max.age, lon = lon, lat = lat, method = outliers.method,
        mltpl = outliers.threshold, value = "ids", verbose = verbose
      )

      ran.otl[ran.otl.flag] <- FALSE
    }
  } else {
    ran.otl <- rep(NA, nrow(x))
  }

  # Temporal, Equal ages
  if (temp.ages.equal) {
    age.equ <- tc_equal(x,
      min.age = min.age, max.age = max.age, value = "flagged",
      verbose = verbose
    )
  } else {
    age.equ <- rep(NA, dim(x)[1])
  }

  # GBIF headquarters
  if (GBIF) {
    gbf <- cc_gbif(x, lon = lon, lat = lat, verbose = verbose, value = "flagged")
  } else {
    gbf <- rep(NA, dim(x)[1])
  }

  # Biodiversity institution
  if (institutions) {
    inst <- cc_inst(x,
      lon = lon, lat = lat, ref = inst.ref, buffer = inst.rad,
      verbose = verbose, value = "flagged"
    )
  } else {
    inst <- rep(NA, dim(x)[1])
  }

  # prepare output data
  out <- list(val, zer, equ, cen, con, gbf, inst, otl, ran.otl, age.equ)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- as.vector(Reduce("&", out))

  if (verbose) {
    if (!is.null(out)) {
      message(sprintf(
        "Flagged %s of %s records, EQ = %s", sum(!out, na.rm = TRUE),
        length(out), round(sum(!out, na.rm = TRUE) / length(out), 2)
      ))
    } else {
      message("flagged 0 records, EQ = 0")
    }
  }
  if (value == "spatialvalid") {
    inp <- data.frame(taxon = x[, taxon], 
                      decimallongitude = x[, lon], 
                      decimallatitude = x[, lat])
    out <- data.frame(inp,
      validity = val, equal = equ, 
      zeros = zer, 
      centroids = cen,
      countrycheck = con, gbif = gbf, 
      institution = inst, 
      spatio.tmp.outl = otl,
      tmp.range.outl = ran.otl, 
      equal.min.max.age = age.equ, 
      summary = out
    )
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
    if (report) {
      report <- "CleanCoordinatesFOS_report.txt"
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
    out <- x[out, ]
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
  }
  if (value == "flagged") {
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
  }

  return(out)
}

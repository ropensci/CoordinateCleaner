#' Geographic Cleaning of Coordinates from Biologic Collections
#'
#' Cleaning geographic coordinates by multiple empirical tests to flag
#' potentially erroneous coordinates, addressing issues common in biological
#' collection databases.
#'
#' The function needs all coordinates to be formally valid according to WGS84.
#' If the data contains invalid coordinates, the function will stop and return a
#' vector flagging the invalid records. TRUE = non-problematic coordinate, FALSE
#' = potentially problematic coordinates.
#' * capitals tests a radius around adm-0 capitals. The
#' radius is \code{capitals_rad}.
#' * centroids tests a radius around country centroids.
#' The radius is \code{centroids_rad}.
#' * countries tests if coordinates are from the
#' country indicated in the country column.  *Switched off by default.*
#' * duplicates tests for duplicate records. This
#' checks for identical coordinates or if a species vector is provided for
#' identical coordinates within a species. All but the first records are flagged
#' as duplicates. *Switched off by default.*
#' * equal tests for equal absolute longitude and latitude.
#' * gbif tests a one-degree radius around the GBIF
#' headquarters in Copenhagen, Denmark.
#' * institutions tests a radius around known
#' biodiversity institutions from \code{instiutions}. The radius is
#' \code{inst_rad}.
#' * outliers tests each species for outlier records.
#' Depending on the \code{outliers_mtp} and \code{outliers.td} arguments either
#' flags records that are a minimum distance away from all other records of this
#' species (\code{outliers_td}) or records that are outside a multiple of the
#' interquartile range of minimum distances to the next neighbour of this
#' species (\code{outliers_mtp}). Three different methods are available for the
#' outlier test: "If \dQuote{outlier} a boxplot method is used and records are
#' flagged as outliers if their \emph{mean} distance to all other records of the
#' same species is larger than mltpl * the interquartile range of the mean
#' distance of all records of this species. If \dQuote{mad} the median absolute
#' deviation is used. In this case a record is flagged as outlier, if the
#' \emph{mean} distance to all other records of the same species is larger than
#' the median of the mean distance of all points plus/minus the mad of the mean
#' distances of all records of the species * mltpl. If \dQuote{distance} records
#' are flagged as outliers, if the \emph{minimum} distance to the next record of
#' the species is > \code{tdi}.
#' * ranges tests if records fall within provided natural range polygons on
#' a per species basis. See \code{\link{cc_iucn}} for details.
#' * seas tests if coordinates fall into the ocean.
#' * urban tests if coordinates are from urban areas.
#' *Switched off by default*
#' * validity checks if coordinates correspond to a lat/lon coordinate reference system.
#' This test is always on, since all records need to pass for any other test to
#' run.
#' * zeros tests for plain zeros, equal latitude and
#' longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.
#' 
#' @aliases summary.spatialvalid is.spatialvalid
#'
#' @param species a character string. A vector of the same length as rows in x,
#'   with the species identity for each record.  If NULL, \code{tests} must not
#'   include the "outliers" or "duplicates" tests.
#' @param countries a character string. The column with the country assignment
#'   of each record in three letter ISO code. Default = \dQuote{countrycode}. If
#'   missing, the countries test is skipped.
#' @param tests a vector of character strings, indicating which tests to run.
#'   See details for all tests available. Default = c("capitals", "centroids",
#'   "equal", "gbif", "institutions", "outliers", "seas", "zeros")
#' @param capitals_rad numeric. The radius around capital coordinates in meters.
#'   Default = 10000.
#' @param centroids_rad numeric. The radius around centroid coordinates in
#'   meters. Default = 1000.
#' @param centroids_detail a \code{character string}. If set to \sQuote{country}
#'   only country (adm-0) centroids are tested, if set to \sQuote{provinces}
#'   only province (adm-1) centroids are tested.  Default = \sQuote{both}.
#' @param inst_rad numeric. The radius around biodiversity institutions
#'   coordinates in metres. Default = 100.
#' @param outliers_method The method used for outlier testing. See details.
#' @param outliers_mtp numeric. The multiplier for the interquartile range of
#'   the outlier test.  If NULL \code{outliers.td} is used.  Default = 5.
#' @param outliers_td numeric.  The minimum distance of a record to all other
#'   records of a species to be identified as outlier, in km. Default = 1000.
#' @param outliers_size numerical.  The minimum number of records in a dataset
#'   to run the taxon-specific outlier test.  Default = 7.
#' @param range_rad buffer around natural ranges. Default = 0.
#' @param zeros_rad numeric. The radius around 0/0 in degrees. Default = 0.5.
#' @param capitals_ref a \code{data.frame} with alternative reference data for
#'   the country capitals test. If missing, the \code{countryref} dataset is
#'   used. Alternatives must be identical in structure.
#' @param centroids_ref a \code{data.frame} with alternative reference data for
#'   the centroid test. If NULL, the \code{countryref} dataset is used.
#'   Alternatives must be identical in structure.
#' @param country_ref a \code{SpatVector} as alternative reference
#'   for the countries test. If NULL, the
#'   \code{rnaturalearth:ne_countries('medium', returnclass = "sf")} dataset is used.
#' @param country_refcol the column name in the reference dataset, containing
#'   the relevant ISO codes for matching. Default is to "iso_a3_eh" which
#'   referes to the ISO-3 codes in the reference dataset. See notes.
#' @param country_buffer numeric. Units are in meters. If provided, a buffer is
#'   created around each country polygon.
#' @param inst_ref a \code{data.frame} with alternative reference data for the
#'   biodiversity institution test. If NULL, the \code{institutions} dataset is
#'   used.  Alternatives must be identical in structure.
#' @param range_ref a \code{SpatVector} of species natural ranges.
#'   Required to include the 'ranges' test. See \code{\link{cc_iucn}} for
#'   details.
#' @param seas_ref a \code{SpatVector} as alternative reference
#'   for the seas test. If NULL, the rnaturalearth::ne_download(scale = 110,
#'   type = 'land', category = 'physical', returnclass = "sf") dataset is used.
#' @param seas_scale The scale of the default landmass reference. Must be one of
#'   10, 50, 110. Higher numbers equal higher detail. Default = 50.
#' @param seas_buffer numeric. Units are in meters. If provided, a buffer is
#'   created around sea polygon.
#' @param urban_ref a \code{SpatVector} as alternative reference
#'   for the urban test. If NULL, the test is skipped. See details for a
#'   reference gazetteers.
#'  @param aohi_rad numeric. The radius around aohi coordinates in
#'   meters. Default = 1000.
#' @param value a character string defining the output value. See the value
#'   section for details. one of \sQuote{spatialvalid}, \sQuote{summary},
#'   \sQuote{clean}. Default = \sQuote{\code{spatialvalid}}.
#' @param report logical or character.  If TRUE a report file is written to the
#'   working directory, summarizing the cleaning results. If a character, the
#'   path to which the file should be written.  Default = FALSE.
#' @inheritParams cc_cap
#'
#' @return Depending on the output argument:
#' \describe{
#' \item{\dQuote{spatialvalid}}{an object of class \code{spatialvalid} similar to x
#' with one column added for each test. TRUE = clean coordinate entry, FALSE = potentially
#' problematic coordinate entries.  The .summary column is FALSE if any test flagged
#' the respective coordinate.}
#' \item{\dQuote{flagged}}{a logical vector with the
#' same order as the input data summarizing the results of all test. TRUE =
#' clean coordinate, FALSE = potentially problematic (= at least one test
#' failed).}
#' \item{\dQuote{clean}}{a \code{data.frame} similar to x
#' with potentially problematic records removed}
#' }
#'
#' @note Always tests for coordinate validity: non-numeric or missing
#'   coordinates and coordinates exceeding the global extent (lon/lat, WGS84).
#'   See \url{https://ropensci.github.io/CoordinateCleaner/} for more details
#'   and tutorials.
#'
#' @note The country_refcol argument allows to adapt the function to the
#'   structure of alternative reference datasets. For instance, for
#'   \code{rnaturalearth::ne_countries(scale = "small", returnclass = "sf")}, the default will fail,
#'   but country_refcol = "iso_a3" will work.
#'
#' @keywords Coordinate cleaning wrapper
#' @family Wrapper functions
#' 
#' @examples
#' 
#' 
#' exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
#'                     decimalLongitude = runif(250, min = 42, max = 51),
#'                     decimalLatitude = runif(250, min = -26, max = -11))
#' 
#' test <- clean_coordinates(x = exmpl, 
#'                           tests = c("equal"))
#'                                     
#'\dontrun{
#' #run more tests
#' test <- clean_coordinates(x = exmpl, 
#'                           tests = c("capitals", 
#'                           "centroids","equal", 
#'                           "gbif", "institutions", 
#'                           "outliers", "seas", 
#'                           "zeros"))
#'}
#'                                  
#'                                     
#' summary(test)
#' 
#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
#' @md
clean_coordinates <- function(x, 
                             lon = "decimalLongitude", 
                             lat = "decimalLatitude",
                             species = "species", 
                             countries = NULL, 
                             tests = c("capitals", "centroids",
                                       "equal", "gbif", 
                                       "institutions", 
                                       "outliers",
                                       "seas", "zeros"),
                             capitals_rad = 10000,
                             centroids_rad = 1000, 
                             centroids_detail = "both", 
                             inst_rad = 100, 
                             outliers_method = "quantile",
                             outliers_mtp = 5, 
                             outliers_td = 1000, 
                             outliers_size = 7, 
                             range_rad = 0,
                             zeros_rad = 0.5,
                             capitals_ref = NULL, 
                             centroids_ref = NULL, 
                             country_ref = NULL, 
                             country_refcol = "iso_a3",
                             country_buffer = NULL,
                             inst_ref = NULL, 
                             range_ref = NULL,
                             seas_ref = NULL, 
                             seas_scale = 50,
                             seas_buffer = NULL,
                             urban_ref = NULL,
                             aohi_rad = NULL,
                             value = "spatialvalid", 
                             verbose = TRUE, 
                             report = FALSE) {
  # check function arguments
  match.arg(value, choices = c("spatialvalid", "flagged", "clean"))
  match.arg(centroids_detail, choices = c("both", "country", "provinces"))
  match.arg(outliers_method, choices = c("distance", "quantile", "mad"))
  
  #reset the rownames
  #rownames(x) <- NULL

  # check column names
  nams <- c(lon, lat, species, countries)
  if (!all(nams %in% names(x))) {
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  if (is.null(countries) & "countries" %in% tests) {
    stop("provide countries column or remove countries test")
  }
  if (is.null(species)) {
    if ("outliers" %in% tests) {
      stop("provide species column or remove outliers test")
    }
    if ("duplicates" %in% tests) {
      stop("provide species column or remove duplicates test")
    }
  }

  # Initiate output 
  out <- data.frame(matrix(NA, nrow = nrow(x), ncol = 13))
  colnames(out) <- c("val", "equ", "zer", "cap", "cen", "sea", "urb", "con",
                    "otl", "gbf", "inst", "rang", "dpl")

  # Run tests Validity, check if coordinates fit to lat/long system, this has
  # to be run all the time, as otherwise the other tests don't work
  out$val <- cc_val(x, lon = lon, lat = lat, 
                    verbose = verbose, value = "flagged")

  if (!all(out$val)) {
    stop(
      "invalid coordinates found in rows, clean dataset before proceeding:\n",
      paste(which(!out$val), "\n")
    )
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

  ## Capitals
  if ("capitals" %in% tests) {
    out$cap <- cc_cap(x,
      lon = lon, lat = lat, buffer = capitals_rad, ref = capitals_ref,
      value = "flagged", verbose = verbose
    )
  }

  ## Centroids
  if ("centroids" %in% tests) {
    out$cen <- cc_cen(x,
      lon = lon, lat = lat, buffer = centroids_rad, test = centroids_detail,
      ref = centroids_ref, value = "flagged", verbose = verbose
    )
  }

  ## Seas
  if ("seas" %in% tests) {
    out$sea <- cc_sea(x,
      lon = lon, lat = lat, ref = seas_ref, 
      scale = seas_scale,
      verbose = verbose,
      value = "flagged",
      buffer = seas_buffer
    )
  }

  ## Urban Coordinates
  if ("urban" %in% tests) {
    out$urb <- cc_urb(x,
      lon = lon, lat = lat, ref = urban_ref, verbose = verbose,
      value = "flagged"
    )
  }

  ## Country check
  if ("countries" %in% tests) {
    out$con <- cc_coun(x,
      lon = lon, 
      lat = lat, 
      iso3 = countries, 
      ref = country_ref,
      ref_col = country_refcol,
      verbose = verbose, 
      value = "flagged",
      buffer = country_buffer
    )
  }

  ## Outliers
  if ("outliers" %in% tests) {
    # select species with more than threshold species
    otl_test <- table(x[species])
    otl_test <- otl_test[otl_test > outliers_size]
    otl_test <- x[x[[species]] %in% names(otl_test), ]
    otl_test <- otl_test[, c(species, lon, lat)]

    otl_flag <- cc_outl(otl_test,
      lon = lon, lat = lat, species = species,
      method = outliers_method, mltpl = outliers_mtp, tdi = outliers_td,
      value = "ids", verbose = verbose
    )
    otl <- rep(TRUE, nrow(x))
    names(otl) <- rownames(x)
    otl[otl_flag] <- FALSE
    out$otl <- otl
  }

  ## GBIF headquarters
  if ("gbif" %in% tests) {
    out$gbf <- cc_gbif(x, lon = lon, lat = lat, 
                       verbose = verbose, value = "flagged")
  }

  ## Biodiversity institution
  if ("institutions" %in% tests) {
    out$inst <- cc_inst(x,
      lon = lon, lat = lat, ref = inst_ref, buffer = inst_rad,
      verbose = verbose, value = "flagged"
    )
  }
  
  ## Natural ranges
  if ("range" %in% tests) {
    if (is.null(range_rad)) {
      stop("'range_rad' not found")
    } else {
      out$rang <- cc_iucn(x, range = range_ref,
                           lon = lon, lat = lat, species = species,
                           buffer = range_rad,
                           verbose = verbose, value = "flagged")
    }
  }

  ## exclude duplicates
  if ("duplicates" %in% tests) {
    out$dpl <- cc_dupl(x, lon = lon, lat = lat, species = species, 
                       value = "flagged")
  }

  if ("aohi" %in% tests) {
    out$aohi <- cc_aohi(x, lon = lon, lat = lat, species = species, 
                       value = "flagged", buffer = aohi_rad)
  }
  
  # prepare output data
  out <- Filter(function(x) !all(is.na(x)), out)
  suma <- as.vector(Reduce("&", out))

  if (verbose) {
    if (!is.null(suma)) {
      message(sprintf("Flagged %s of %s records, EQ = %s.", sum(!suma,
        na.rm = TRUE
      ), length(suma), round(
        sum(!suma, na.rm = TRUE) / length(suma), 2
      )))
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
    
    if (isTRUE(report)) {
      report <- "clean_coordinates_report.txt"
    }
    if (is.character(report)) {
      repo <- data.frame(
        Test = as.character(names(out[-(1:3)])), 
        Flagged.records = colSums(!out[-(1:3)]),
        stringsAsFactors = FALSE
      )
      repo <- rbind(repo, c("Total number of records", length(out$summary)))
      zeros <- ifelse(is.null(out$summary), 0, sum(!out$summary, na.rm = TRUE))
      repo <- rbind(repo, c("Error Quotient", 
                            round(zeros / length(out$summary), 2)))

      write.table(repo, report, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  }
  if (value == "clean") {
    out <- x[suma, ]
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
  }
  if (value == "flagged") {
    out <- suma
    if (report | is.character(report)) {
      warning("report only valid with value = 'spatialvalid'")
    }
  }
  return(out)
}

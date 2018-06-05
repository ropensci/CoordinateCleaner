#' Geographic Cleaning of Coordinates from Biologic Collections
#' 
#' Cleaning geographic coordinates by multiple empirical tests to flag
#' potentially erroneous coordinates, addressing issues common in biological
#' collection databases.
#' 
#' The function needs all coordinates to be formally valid according to WGS84.
#' If the data contains invalid coordinates, the function will stop and return
#' a vector flagging the invalid records. TRUE = non-problematic coordinate,
#' FALSE = potentially problematic coordinates. A reference gazetteer for the
#' urban test is available at at
#' \url{https://github.com/azizka/CoordinateCleaner/tree/master/extra_gazetteers}.
#' Three different methods are available for the outlier test: "If
#' \dQuote{outlier} a boxplot method is used and records are flagged as
#' outliers if their \emph{mean} distance to all other records of the same
#' species is larger than mltpl * the interquartile range of the mean distance
#' of all records of this species. If \dQuote{mad} the median absolute
#' deviation is used. In this case a record is flagged as outlier, if the
#' \emph{mean} distance to all other records of the same species is larger than
#' the median of the mean distance of all points plus/minus the mad of the mean
#' distances of all records of the species * mltpl. If \dQuote{distance}
#' records are flagged as outliers, if the \emph{minimum} distance to the next
#' record of the species is > \code{tdi}
#' 
#' @aliases CleanCoordinates summary.spatialvalid is.spatialvalid
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species a character string. A vector of the same length as rows in x,
#' with the species identity for each record.  If missing, the outliers test is
#' skipped.
#' @param countries a character string. A vector of the same length as rows in
#' x, with country information for each record in ISO3 format.  If missing, the
#' countries test is skipped.
#' @param capitals logical. If TRUE, tests a radius around adm-0 capitals. The
#' radius is \code{capitals.rad}. Default = TRUE.
#' @param centroids logical. If TRUE, tests a radius around country centroids.
#' The radius is \code{centroids.rad}. Default = TRUE.
#' @param countrycheck logical.  If TRUE, tests if coordinates are from the
#' country indicated in the country column.  Default = FALSE.
#' @param duplicates logical.  If TRUE, tests for duplicate records. This
#' checks for identical coordinates or if a species vector is provided for
#' identical coordinates within a species. All but the first records are
#' flagged as duplicates.  Default = FALSE.
#' @param equal logical.  If TRUE, tests for equal absolute longitude and
#' latitude.  Default = TRUE.
#' @param GBIF logical.  If TRUE, tests a one-degree radius around the GBIF
#' headquarters in Copenhagen, Denmark.  Default = TRUE.
#' @param institutions logical. If TRUE, tests a radius around known
#' biodiversity institutions from \code{instiutions}. The radius is
#' \code{inst.rad}. Default = TRUE.
#' @param outliers logical. If TRUE, tests each species for outlier records.
#' Depending on the \code{outliers.mtp} and \code{outliers.td} arguments either
#' flags records that are a minimum distance away from all other records of
#' this species (\code{outliers.td}) or records that are outside a multiple of
#' the interquartile range of minimum distances to the next neighbour of this
#' species (\code{outliers.mtp}).  Default = TRUE.
#' @param seas logical. If TRUE, tests if coordinates fall into the ocean.
#' Default = TRUE.
#' @param urban logical. If TRUE, tests if coordinates are from urban areas.
#' Default = FALSE.
#' @param zeros logical. If TRUE, tests for plain zeros, equal latitude and
#' longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.
#' Default = TRUE.
#' @param capitals.rad numeric. The radius around capital coordinates in
#' degrees. Default = 0.1.
#' @param centroids.rad numeric. The side length of the rectangle around
#' country centroids in degrees. Default = 0.01.
#' @param centroids.detail a \code{character string}. If set to
#' \sQuote{country} only country (adm-0) centroids are tested, if set to
#' \sQuote{provinces} only province (adm-1) centroids are tested.  Default =
#' \sQuote{both}.
#' @param inst.rad numeric. The radius around biodiversity institutions
#' coordinates in degrees. Default = 0.001.
#' @param outliers.method The method used for outlier testing. See details.
#' @param outliers.mtp numeric. The multiplier for the interquartile range of
#' the outlier test.  If NULL \code{outliers.td} is used.  Default = 3.
#' @param outliers.td numeric.  The minimum distance of a record to all other
#' records of a species to be identified as outlier, in km. Default = 1000.
#' @param outliers.size numerical.  THe minimum number of records in a dataset
#' to run the taxon-specific outlier test.  Default = 7.
#' @param zeros.rad numeric. The radius around 0/0 in degrees. Default = 0.5.
#' @param capitals.ref a \code{data.frame} with alternative reference data for
#' the country capitals test. If missing, the \code{capitals} dataset is used.
#' Alternatives must be identical in structure.
#' @param centroids.ref a \code{data.frame} with alternative reference data for
#' the centroid test. If missing, the \code{centroids} dataset is used.
#' Alternatives must be identical in structure.
#' @param country.ref a \code{SpatialPolygonsDataFrame} as alternative
#' reference for the countrycheck test. If missing, the
#' \code{rnaturalearth:ne_countries('medium')} dataset is used.
#' @param inst.ref a \code{data.frame} with alternative reference data for the
#' biodiversity institution test. If missing, the \code{institutions} dataset
#' is used.  Alternatives must be identical in structure.
#' @param seas.ref a \code{SpatialPolygonsDataFrame} as alternative reference
#' for the seas test. If missing, the \code{\link{landmass}} dataset is used.
#' @param urban.ref a \code{SpatialPolygonsDataFrame} as alternative reference
#' for the urban test. If missing, the test is skipped. See details for a
#' reference gazetteers.
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
#' @keywords Coordinate cleaning wrapper
#' @examples
#' 
#' 
#' exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
#'                     decimallongitude = runif(250, min = 42, max = 51),
#'                     decimallatitude = runif(250, min = -26, max = -11))
#' 
#' test <- CleanCoordinates(x = exmpl)
#' 
#' summary(test)
#' 
#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
#' 
CleanCoordinates <- function(x, 
                             lon = "decimallongitude", 
                             lat = "decimallatitude",
                             species = "species", 
                             countries = NULL, 
                             capitals = TRUE, 
                             centroids = TRUE,
                             countrycheck = FALSE, 
                             duplicates = FALSE, 
                             equal = TRUE, 
                             GBIF = TRUE, 
                             institutions = TRUE,
                             outliers = FALSE, 
                             seas = TRUE, 
                             urban = FALSE, 
                             zeros = TRUE, 
                             capitals.rad = 0.05,
                             centroids.rad = 0.01, 
                             centroids.detail = "both", 
                             inst.rad = 0.001, 
                             outliers.method = "quantile",
                             outliers.mtp = 3, 
                             outliers.td = 1000, 
                             outliers.size = 7, 
                             zeros.rad = 0.5,
                             capitals.ref, 
                             centroids.ref, 
                             country.ref, 
                             inst.ref, 
                             seas.ref, 
                             urban.ref,
                             value = "spatialvalid", 
                             verbose = TRUE, 
                             report = FALSE) {
  # check function arguments
  match.arg(value, choices = c("spatialvalid", "flagged", "clean"))
  match.arg(centroids.detail, choices = c("both", "country", "provinces"))
  match.arg(outliers.method, choices = c("distance", "quantile", "mad"))

  # check column names
  nams <- c(lon, lat, species, countries)
  if (!all(nams %in% names(x))) {
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  if (is.null(countries)) {
    countries <- NULL
    if (countrycheck) {
      countrycheck <- FALSE
      warning("countries missing, countrycheck set to FALSE")
    }
  }
  if (is.null(species)) {
    if (outliers) {
      outliers <- FALSE
      warning("is.null(species), outliers test skipped")
    }
    species <- NULL
  }
  if (missing(capitals.ref)) {
    capitals.ref <- NULL
  }
  if (missing(centroids.ref)) {
    centroids.ref <- NULL
  }
  if (missing(country.ref)) {
    country.ref <- NULL
  }
  if (missing(inst.ref)) {
    inst.ref <- NULL
  }
  if (missing(seas.ref)) {
    seas.ref <- NULL
  }
  if (missing(urban.ref)) {
    urban.ref <- NULL
  }

  # Run tests Validity, check if coordinates fit to lat/long system, this has
  # to be run all the time, as otherwise the other tests don't work
  val <- cc_val(x, lon = lon, lat = lat, verbose = verbose, value = "flagged")

  if (!all(val)) {
    stop(
      "invalid coordinates found in rows, clean dataset before proceeding:\n",
      paste(which(!val), "\n")
    )
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

  ## Capitals
  if (capitals) {
    cap <- cc_cap(x,
      lon = lon, lat = lat, buffer = capitals.rad, ref = capitals.ref,
      value = "flagged", verbose = verbose
    )
  } else {
    cap <- rep(NA, dim(x)[1])
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

  # Seas
  if (seas) {
    sea <- cc_sea(x,
      lon = lon, lat = lat, ref = seas.ref, verbose = verbose,
      value = "flagged"
    )
  } else {
    sea <- rep(NA, dim(x)[1])
  }

  # Urban Coordinates
  if (urban) {
    urb <- cc_urb(x,
      lon = lon, lat = lat, ref = urban.ref, verbose = verbose,
      value = "flagged"
    )
  } else {
    urb <- rep(NA, dim(x)[1])
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

  # Outliers
  if (outliers) {
    # select species with more than threshold species
    otl.test <- table(x[species])
    otl.test <- otl.test[otl.test > outliers.size]
    otl.test <- x[x[[species]] %in% names(otl.test), ]
    otl.test <- otl.test[, c(species, lon, lat)]

    otl.flag <- cc_outl(otl.test,
      lon = lon, lat = lat, species = species,
      method = outliers.method, mltpl = outliers.mtp, tdi = outliers.td,
      value = "ids", verbose = verbose
    )
    otl <- rep(TRUE, nrow(x))
    otl[otl.flag] <- FALSE
  } else {
    otl <- rep(NA, dim(x)[1])
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

  # exclude duplicates
  if (duplicates) {
    message("running duplicates test")
    if (is.null(species)) {
      dpl.test <- x
      warning("duplicates test without species id, 
              assuming single species dataset")
    } else {
      dpl.test <- data.frame(x, species)
    }
    dpl <- !duplicated(dpl.test)
    if (verbose) {
      message(sprintf("flagged %s records.", sum(!dpl)))
    }
  } else {
    dpl <- rep(NA, dim(x)[1])
  }

  # prepare output data

  out <- list(val, zer, equ, cap, cen, sea, urb, con, otl, gbf, inst, dpl)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- as.vector(Reduce("&", out))

  if (verbose) {
    if (!is.null(out)) {
      message(sprintf("Flagged %s of %s records, EQ = %s.", sum(!out,
        na.rm = TRUE
      ), length(out), round(
        sum(!out, na.rm = TRUE) / length(out), 2
      )))
    } else {
      message("flagged 0 records, EQ = 0")
    }
  }
  if (value == "spatialvalid") {
    inp <- data.frame(
      species = x[, species], 
      decimallongitude = x[, lon],
      decimallatitude = x[, lat]
    )
    out <- data.frame(inp,
      validity = val, equal = equ, zeros = zer, capitals = cap,
      centroids = cen, sea = sea, urban = urb, 
      countrycheck = con, outliers = otl,
      gbif = gbf, institution = inst, duplicates = dpl, summary = out
    )
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
    if (report) {
      report <- "CleanCoordinates_report.txt"
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

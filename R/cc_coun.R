#' Identify Coordinates Outside their Reported Country
#'
#' Removes or flags mismatches between geographic coordinates and additional
#' country information (usually this information is reliably reported with
#' specimens). Such a mismatch can occur for example, if latitude and longitude
#' are switched.
#'
#'
#' @param iso3 a character string. The column with the country assignment of
#'   each record in three letter ISO code. Default = \dQuote{countrycode}.
#' @param ref SpatVector (geometry: polygons). Providing the geographic
#'   gazetteer. Can be any SpatVector (geometry: polygons), but the structure
#'   must be identical to \code{rnaturalearth::ne_countries(scale = "medium",
#'   returnclass = "sf")}.
#'   Default = \code{rnaturalearth::ne_countries(scale = "medium", returnclass =
#'   "sf")}
#' @param ref_col the column name in the reference dataset, containing the
#'   relevant ISO codes for matching. Default is to "iso_a3_eh" which refers to
#'   the ISO-3 codes in the reference dataset. See notes.
#' @param buffer numeric. Units are in meters. If provided, a buffer is
#'   created around each country polygon.
#' @inheritParams cc_cen
#'
#' @inherit cc_cap return
#'
#' @note The ref_col argument allows to adapt the function to the structure of
#'   alternative reference datasets. For instance, for
#'   \code{rnaturalearth::ne_countries(scale = "small")}, the default will fail,
#'   but ref_col = "iso_a3" will work.
#'
#' @note With the default reference, records are flagged if they fall outside
#'   the terrestrial territory of countries, hence records in territorial waters
#'   might be flagged. See \url{https://ropensci.github.io/CoordinateCleaner/}
#'   for more details and tutorials.
#'
#' @keywords Coordinate cleaning
#' @family Coordinates
#'
#' @examples
#'
#' \dontrun{
#' x <- data.frame(species = letters[1:10],
#'                 decimalLongitude = runif(100, -20, 30),
#'                 decimalLatitude = runif(100, 35,60),
#'                 countrycode = "RUS")
#'
#' cc_coun(x, value = "flagged")#non-terrestrial records are flagged as wrong.
#' }
#'
#' @export
#' @importFrom terra vect geomtype extract
#' @importFrom stats na.omit

cc_coun <- function(x, 
                    lon = "decimalLongitude", 
                    lat = "decimalLatitude", 
                    iso3 = "countrycode",
                    value = "clean", 
                    ref = NULL, 
                    ref_col = "iso_a3",
                    verbose = TRUE,
                    buffer = NULL) {

  # check function arguments for validity
  match.arg(value, choices = c("clean", "flagged"))
  if (!iso3 %in% names(x)) {
    stop("iso3 argument missing, please specify")
  }

  if (verbose) {
    message("Testing country identity")
  }

  # set reference and check for dependency
  if (is.null(ref)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Install the 'rnaturalearth' package or provide a custom reference",
        call. = FALSE
      )
    }
    ref <- terra::vect(rnaturalearth::ne_countries(scale = "medium",
                                                   returnclass = "sf"))
  } else {
    #Enable sf formatted custom references
    if (any(is(ref) == "Spatial")  | inherits(ref, "sf")) {
      ref <- terra::vect(ref)
    }
    # Check if object is a SpatVector 
    if (!(inherits(ref, "SpatVector") & 
        terra::geomtype(ref) == "polygons")) {
      stop("ref must be a SpatVector with geomtype 'polygons'")
    }
    #Check projection of custom reference and reproject if necessary
    ref <- reproj(ref)
  }
  
  # prepare data
  dat <- terra::vect(x[, c(lon, lat)], 
                     geom = c(lon, lat),
                     crs = ref)
  
  # Buffer around countries
  if (is.numeric(buffer)) {
    buffer <- ifelse(buffer == 0, 0.00000000001, buffer)
    ref_buff <- terra::buffer(ref, buffer)
    # There is a weird bug in terra, so I did this work around
    ref <- terra::vect(stats::na.omit(terra::geom(ref_buff)), 
                       type = "polygon", crs = ref)
    terra::values(ref) <- terra::values(ref_buff)
  }
  
  # get country from coordinates and compare with provided country
  country <- terra::extract(ref, dat)
  count_dat <- as.character(unlist(x[, iso3]))
  
  if (is.numeric(buffer)) {
    out <- logical(length(dat))
    for (i in seq_along(dat)) {
      out[i] <- count_dat[i] %in% country[country[, 1] == i, ref_col]
    }
  } else {
    country <- country[, ref_col]
    out <- as.character(country) == count_dat
    out[is.na(out)] <- FALSE # marine records are set to False
  }
  # return output
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

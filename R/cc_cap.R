#' Identify Coordinates in Vicinity of Country Capitals.
#'
#' Removes or flags records within a certain radius around country capitals.
#' Poorly geo-referenced occurrence records in biological databases are often
#' erroneously geo-referenced to capitals.
#'
#' @param x data.frame. Containing geographical coordinates and species names.
#' @param lon character string. The column with the longitude coordinates.
#'   Default = \dQuote{decimalLongitude}.
#' @param lat character string. The column with the latitude coordinates.
#'   Default = \dQuote{decimalLatitude}.
#' @param species character string. The column with the species identity. Only
#'   required if verify = TRUE.
#' @param buffer The buffer around each capital coordinate (the centre of the
#'   city), where records should be flagged as problematic. Units depend on
#'   geod. Default = 10 kilometres.
#' @param geod logical. If TRUE the radius around each capital is calculated
#'   based on a sphere, buffer is in meters and independent of latitude. If
#'   FALSE the radius is calculated assuming planar coordinates and varies
#'   slightly with latitude. Default = TRUE.
#'   See https://seethedatablog.wordpress.com/ for detail and credits.
#' @param ref SpatVector (geometry: polygons). Providing the geographic
#'   gazetteer. Can be any SpatVector (geometry: polygons), but the structure
#'   must be identical to \code{\link{countryref}}.  Default =
#'   \code{\link{countryref}}.
#' @param verify logical. If TRUE records are only flagged if they are the only
#'   record in a given species flagged close to a given reference. If FALSE, the
#'   distance is the only criterion
#' @param value character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#'   of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#'   containing the records considered correct by the test (\dQuote{clean}) or a
#'   logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test
#'   failed/potentially problematic . Default = \dQuote{clean}.
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#'   details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' \dontrun{
#' x <- data.frame(species = letters[1:10],
#'                 decimalLongitude = c(runif(99, -180, 180), -47.882778),
#'                 decimalLatitude = c(runif(99, -90, 90), -15.793889))
#'
#' cc_cap(x)
#' cc_cap(x, value = "flagged")
#' }
#' @export
#' @importFrom geosphere destPoint
#' @importFrom terra vect ext crop geom union extract buffer

cc_cap <- function(x, 
                   lon = "decimalLongitude", 
                   lat = "decimalLatitude",
                   species = "species",
                   buffer = 10000,
                   geod = TRUE,
                   ref = NULL, 
                   verify = FALSE,
                   value = "clean", 
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing country capitals")
  }
  if (buffer > 10 & !geod) {
    warnings("Using large buffer check 'geod'")
  }
  if (buffer < 100 & geod) {
    warnings("Using small buffer check 'geod'")
  }
  
  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

  # select relevant columns
  dat <- terra::vect(x[, c(lon, lat)], geom = c(lon, lat),
                     crs = wgs84)

  # check for reference data and adapt projection of custom reference data
  if (is.null(ref)) {
    ref <- CoordinateCleaner::countryref
    ref <- ref[!is.na(ref$capital),]
  }
  # subset reference data to records extend to speed up the test
  buffer <- ifelse(buffer == 0, 0.00000000001, buffer)
  limits <- terra::ext(terra::buffer(dat, width = buffer))
  lat_lon <- c("capital.lon", "capital.lat")
  ref <- terra::crop(terra::vect(ref[, lat_lon], 
                                 geom = lat_lon, 
                                 crs = wgs84), 
                      limits)

  # test if any points fall within the buffer in case no capitals are found in
  # the study area
  if (is.null(ref)  | nrow(ref) == 0) {
    out <- rep(TRUE, nrow(x))
  } else {
    if (geod) {
      # credits to https://seethedatablog.wordpress.com/
      dg <- seq(from = 0, to = 360, by = 5)
      
      buff_XY <- geosphere::destPoint(p = terra::geom(ref)[, c("x", "y")], 
                                      b = rep(dg, each = length(ref)), 
                                      d = buffer)
      
      id <- rep(seq_along(ref), times = length(dg))
      
      
      lst <- split(data.frame(buff_XY), f = id)
      
      # Make SpatialPolygons out of the list of coordinates
      lst <- lapply(lst, as.matrix)
      ref <- lapply(lst, terra::vect, crs = wgs84, type = "polygons")
      ref <- terra::vect(ref)
      
      #point in polygon test
      ext_dat <- terra::extract(ref, dat)
      out <- is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])
    } else {
      ref <- terra::buffer(ref, width = buffer)
      ext_dat <- terra::extract(ref, dat)
      out <- is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])
    }
  }
  
  # implement the verification
  if (verify & sum(out) > 0) {
    # get flagged coordinates
    ver <- x[!out, ]
    
    # count the instances of all flagged records
    ver_count <- aggregate(ver[[species]] ~ ver[[lon]] + 
                             ver[[lat]] , FUN = "length")
    names(ver_count) <- c(lon, lat, "coord.count")
    
    ver_spec <- aggregate(ver[[lon]] ~ ver[[species]], FUN = "length")
    names(ver_spec) <- c(species, "species.count")
    
    # test which flagged x occur multiple times
    tester <- data.frame(x, ord = seq_len(nrow(x)))
    tester <- merge(tester, ver_count, by = c(lon,lat), all = TRUE)
    tester <- merge(tester, ver_spec, by = species, all = TRUE)
    
    tester <- tester[order(tester$ord),]
    tester[is.na(tester)] <- 0
    
    #only flag those records that occur with only one coordinate in the buffer
    out <-  tester$coord.count <= tester$species.count | out
  }
  
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

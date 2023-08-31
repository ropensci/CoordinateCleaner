#' Identify Coordinates in Vicinity of Country and Province Centroids
#' 
#' Removes or flags records within a radius around the geographic centroids of political
#' countries and provinces. Poorly geo-referenced occurrence records in
#' biological databases are often erroneously geo-referenced to centroids.
#' 
#' @param buffer numerical. The buffer around each province or country
#' centroid, where records should be flagged as problematic. Units depend on geod.  
#' Default = 1 kilometre.
#' @param test a character string. Specifying the details of the test. One of
#' c(\dQuote{both}, \dQuote{country}, \dQuote{provinces}).  If both tests for
#' country and province centroids.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimalLongitude = c(runif(99, -180, 180), -47.92), 
#'                 decimalLatitude = c(runif(99, -90,90), -15.78))
#' cc_cen(x, geod = FALSE)
#' 
#' \dontrun{
#' cc_inst(x, value = "flagged", buffer = 50000) #geod = T
#' }
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom terra vect ext crop buffer geom

cc_cen <- function(x, 
                   lon = "decimalLongitude", 
                   lat = "decimalLatitude", 
                   species = "species",
                   buffer = 1000,
                   geod = TRUE,
                   test = "both", 
                   ref = NULL,
                   verify = FALSE,
                   value = "clean", 
                   verbose = TRUE) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flagged"))
  match.arg(test, choices = c("both", "country", "provinces"))
  
  if (verbose) {
    message("Testing country centroids")
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
  dat <- terra::vect(x[, c(lon, lat), drop = FALSE], 
                     geom = c(lon, lat),
                     crs = wgs84)
  
  if (is.null(ref)) {
    ref <- CoordinateCleaner::countryref
    
    switch(test, country = {
      ref <- ref[ref$type == "country", ]
    }, province = {
      ref <- ref[ref$type == "province", ]
    })
  } else {
    #proj4string(ref) <- wgs84
    warning("assuming lat/lon for centroids.ref")
  }
  
  limits <- terra::ext(terra::buffer(dat, width = buffer))
  
  # subset of testdatset according to speed up buffer
  lon_lat <- c("centroid.lon", "centroid.lat")
  ref <- terra::crop(
    terra::vect(ref[, lon_lat],
                geom = lon_lat,
                crs = wgs84),
    limits)
  
  # run buffering incase no centroids are found in the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    if (geod) {
      # credits to https://seethedatablog.wordpress.com
      dg <- seq(from = 0, to = 360, by = 5)
      
      buff_XY <- geosphere::destPoint(p = terra::geom(ref)[, c("x", "y")], 
                                      b = rep(dg, each = length(ref)), 
                                      d = buffer)
      
      id <- rep(seq_along(ref), times = length(dg))
      
      
      lst <- split(data.frame(buff_XY), f = id)
      
      # Make SpatialPolygons out of the list of coordinates
      lst <- lapply(lst, as.matrix)
      ref <- sapply(lst, terra::vect, crs = wgs84, type = "polygons")
      ref <- Reduce(rbind, ref)
      
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
    ver <- x[!out,]
    
    #count the instances of all flagged records
    ver_count <- aggregate(ver[[species]] ~ ver[[lon]] + 
                             ver[[lat]] , FUN = "length")
    names(ver_count) <- c(lon, lat, "coord.count")
    
    ver_spec <- aggregate(ver[[lon]] ~ ver[[species]], FUN = "length")
    names(ver_spec) <- c(species, "species.count")
    
    #test which flagged x occur multiple times
    tester <- data.frame(x, ord = seq_len(nrow(x)))
    tester <- merge(tester, ver_count, by = c(lon,lat), all = TRUE)
    tester <- merge(tester, ver_spec, by = species, all = TRUE)
    
    tester <- tester[order(tester$ord),]
    tester[is.na(tester)] <- 0
    
    #only flag those records that occure with only one coordinate in the buffer
    out <-  tester$coord.count <= tester$species.count | out
  }
  # create output based on value argument
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }
  
  switch(value, clean = return(x[out, ]), flagged = return(out))
}

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
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'                 
#' cc_cen(x, geod = FALSE)
#' 
#' \dontrun{
#' #' cc_inst(x, value = "flagged", buffer = 50000) #geod = T
#' }
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom sp coordinates CRS disaggregate over Polygon Polygons proj4string "proj4string<-" SpatialPolygons SpatialPoints
#' @importFrom raster extent crop
#' @importFrom rgeos gBuffer
cc_cen <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
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
  if(buffer > 10 & !geod){
    warnings("Using large buffer check 'geod'")
  }
  if(buffer < 100 & geod){
    warnings("Using small buffer check 'geod'")
  }

  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # select relevant columns
  dat <- sp::SpatialPoints(x[, c(lon, lat)], 
                           proj4string = sp::CRS(wgs84))

  if (is.null(ref)) {
    ref <- CoordinateCleaner::countryref

    switch(test, country = {
      ref <- ref[ref$type == "country", ]
    }, province = {
      ref <- ref[ref$type == "province", ]
    })
  } else {
    proj4string(ref) <- wgs84
    warning("assuming lat/lon for centroids.ref")
  }

  limits <- raster::extent(dat) + buffer

  # subset of testdatset according to speed up buffer
  ref <- raster::crop(
    sp::SpatialPoints(ref[, c("centroid.lon", "centroid.lat")], 
                      proj4string = sp::CRS(wgs84)),
    limits
  )

  # run buffering incase no centroids are found in the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    if(geod){
      # credits to https://seethedatablog.wordpress.com
      dg <- seq(from = 0, to = 360, by = 5)
      
      buff_XY <- geosphere::destPoint(p = sp::coordinates(ref), 
                                      b = rep(dg, each = length(ref)), 
                                      d = buffer)
      
      id <- rep(seq_along(ref), times = length(dg))
      
      
      lst <- split(data.frame(buff_XY), f = id)
      
      # Make SpatialPolygons out of the list of coordinates
      poly   <- lapply(lst, sp::Polygon, hole = FALSE)
      polys  <- lapply(list(poly), sp::Polygons, ID = NA)
      spolys <- sp::SpatialPolygons(Srl = polys, proj4string = CRS(wgs84))
      ref <- sp::disaggregate(spolys)
      
      #point in polygon test
      out <- is.na(sp::over(x = dat, y = ref))
    }else{
      ref <- rgeos::gBuffer(ref, width = buffer, byid = TRUE)
      out <- is.na(sp::over(x = dat, y = ref))
      }
  }
  
  # implement the verification
  if(verify & sum(out) > 0){
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
    out <-  tester$coord.count <= tester$species.count| out
  }
  # create output based on value argument
  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

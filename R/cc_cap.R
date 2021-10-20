#' Identify Coordinates in Vicinity of Country Capitals.
#' 
#' Removes or flags records within a certain radius around country capitals. Poorly
#' geo-referenced occurrence records in biological databases are often
#' erroneously geo-referenced to capitals.
#' 
#' @param x data.frame. Containing geographical coordinates and species
#' names.
#' @param lon character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species character string. The column with the species identity. Only
#' required if verify = TRUE.
#' @param buffer The buffer around each capital coordinate (the centre of the
#' city), where records should be flagged as problematic. Units depend on geod.
#' Default = 10 kilometres.
#' @param geod logical. If TRUE the radius around each capital is calculated
#' based on a sphere, buffer is in meters and independent of latitude. If FALSE
#' the radius is calculated assuming planar coordinates and varies slightly with latitude,
#' in this case buffer is in degrees. Default = TRUE. See https://seethedatablog.wordpress.com/ 
#' for detail and credits.
#' @param ref SpatialPointsDataFrame. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataFrame, but the structure must be identical to
#' \code{\link{countryref}}.  Default = \code{\link{countryref}}.
#' @param verify logical. If TRUE records are only flagged if they are the
#' only record in a given species flagged close to a given reference.
#' If FALSE, the distance is the only criterion
#' @param value character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic . Default = \dQuote{clean}.
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#' 
#' cc_cap(x)
#' cc_cap(x, value = "flagged")
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom sp coordinates CRS disaggregate over Polygon Polygons proj4string "proj4string<-" SpatialPolygons SpatialPoints
#' @importFrom raster extent crop
#' @importFrom rgeos gBuffer
cc_cap <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude",
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
  if(buffer > 10 & !geod){
    warnings("Using large buffer check 'geod'")
  }
  if(buffer < 100 & geod){
    warnings("Using small buffer check 'geod'")
  }
  
  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

  # select relevant columns
  dat <- sp::SpatialPoints(x[, c(lon, lat)], 
                           proj4string = sp::CRS(wgs84))

  # check for reference data and adapt projection of custom reference data
  if (is.null(ref)) {
    ref <- CoordinateCleaner::countryref
    ref <- ref[!is.na(ref$capital),]
  }
  # subset reference data to data window to spead up the test
  limits <- raster::extent(dat) + buffer
  ref <- raster::crop(SpatialPoints(ref[, c("capital.lon", "capital.lat")], 
                                    proj4string = sp::CRS(wgs84)), 
                      limits)

  # test if any points fall within the buffer incase no capitals are found in
  # the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    if(geod){
      # credits to https://seethedatablog.wordpress.com/
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
  
  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
    
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

#' Flag Coordinates in Vicinity of Country Capitals.
#' 
#' Flags records within a certain radius around country capitals. Poorly
#' geo-referenced occurrence records in biological databases are often
#' erroneously geo-referenced to capitals.
#' 
#' Note: the buffer radius is in degrees, thus will differ slightly between
#' different latitudes.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param buffer The buffer around each capital coordinate (the centre of the
#' city), where records should be flagged as problematic. Units depend on geod.
#' Default = 10 kilometers.
#' @param geod logical. If TRUE the radius around each capital is calculated
#' based on a sphere, buffer is in meters and independent of latitude. If FALSE
#' the radius is calculated assuming planar coordinates and varies slightly with latitude,
#' in this case buffer is in degrees. Default = T.
#' @param ref a SpatialPointsDataFrame. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataFrame, but the structure must be identical to
#' \code{\link{countryref}}.  Default = \code{\link{countryref}}
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic . Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
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
                   buffer = 10000,
                   geod = TRUE,
                   ref = NULL, 
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
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

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

  # create output based on value argument
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

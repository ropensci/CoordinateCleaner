#' Flag Records in the Vicinity of Biodiversity Institutions
#' 
#' Flag records assigned to the location of zoos, botanical gardens, herbaria,
#' universities and museums, based on a global database of ~10,000 such
#' biodiversity institutions. Coordinates from these locations can be related
#' to data-entry errors, false automated geo-reference or individuals in
#' captivity/horticulture.
#' 
#' Note: the buffer radius is in degrees, thus will differ slightly between
#' different latitudes.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param buffer numerical. The buffer around each province or country
#' centroid, where records should be flagged as problematic, in decimal
#' degrees.  Default = 100m.
#' @param geod logical. If TRUE the radius around each institution is calculated
#' based on a sphere, buffer is in meters and independent of latitude. If FALSE
#' the radius is calculated assuming planar coordinates and varies slightly with latitude,
#' in this case buffer is in degrees. DEfault = T.
#' @param ref a SpatialPointsDataframe. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataframe, but the structure must be identical to
#' \code{\link{institutions}}.  Default = \code{\link{institutions}}
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'                 
#' cc_inst(x, value = "flagged", buffer = 500000)#large buffer for demonstration
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom raster crop extent
#' @importFrom sp coordinates CRS disaggregate over Polygon Polygons SpatialPolygons SpatialPoints
cc_inst <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    buffer = 100,
                    geod = TRUE,
                    ref = NULL, 
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing biodiversity institutions")
  }
  
  if(buffer > 10 & !geod){
    warnings("Using large buffer check 'geod'")
  }
  if(buffer < 100 & geod){
    warnings("Using small buffer check 'geod'")
  }
  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  dat <- sp::SpatialPoints(x[, c(lon, lat)], proj4string = sp::CRS(wgs84))

  # prepare reference dataset
  if (is.null(ref)) {
    ref <- CoordinateCleaner::institutions
    ref <- ref[!is.na(ref$decimallongitude) & !is.na(ref$decimallatitude), ]
  }
  limits <- raster::extent(dat) + buffer

  # subset of testdatset according to limits
  ref <- raster::crop(
    sp::SpatialPoints(ref[, c("decimallongitude", "decimallatitude")], 
                      proj4string = sp::CRS(wgs84)),
    limits
  )

  # test reference data after limiting and do test in case no bdinstitutions
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    if(geod){
      # credits to https://seethedatablog.wordpress.com/2017/08/03/euclidean-vs-geodesic-buffering-in-r/
      dg <- seq(from = 0, to = 360, by = 5)
      
      buff_XY <- geosphere::destPoint(p = sp::coordinates(ref), 
                                      b = rep(dg, each = length(ref)), 
                                      d = buffer)
      
      id <- rep(1:length(ref), times = length(dg))
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
  
  # if(geo_verify){
    #  #create second radius, x times the ratius of the other
    # if(geod){
    #   # credits to https://seethedatablog.wordpress.com/2017/08/03/euclidean-vs-geodesic-buffering-in-r/
    #   dg <- seq(from = 0, to = 360, by = 5)
    #   
    #   buff_XY <- geosphere::destPoint(p = sp::coordinates(ref), 
    #                                   b = rep(dg, each = length(ref)), 
    #                                   d = buffer * verify_mult)
    #   
    #   id <- rep(1:length(ref), times = length(dg))
    #   lst <- split(data.frame(buff_XY), f = id)
    #   
    #   # Make SpatialPolygons out of the list of coordinates
    #   poly   <- lapply(lst, sp::Polygon, hole = FALSE)
    #   polys  <- lapply(list(poly), sp::Polygons, ID = NA)
    #   spolys <- sp::SpatialPolygons(Srl = polys, proj4string = CRS(wgs84))
    #   ref <- sp::disaggregate(spolys)
    # }else{
    #   ref <- rgeos::gBuffer(ref, width = buffer * verify_mult, byid = TRUE)
    # }
    # 
    # #test species with flagged records, with larger radius
    # outl_sp <- x[!out,][[species]]
    # test <- x[as.character(x[,species]) %in% outl_sp,]
    # 
    # test_pts <- sp::SpatialPoints(test[, c(lon, lat)], proj4string = sp::CRS(wgs84))
    # test$class <- ref[sp::over(x = test_pts, y = ref),]
    # 
    # aggregate(class ~ species, data = test, FUN = sum)

     
  # }
  
  

  # create output based on value argument
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

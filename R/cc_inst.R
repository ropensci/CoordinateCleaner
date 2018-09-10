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
#' @param lon  character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat  character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species character string. The column with the species identity. Only
#' required if verify = TRUE.
#' @param buffer numerical. The buffer around each province or country
#' centroid, where records should be flagged as problematic, in decimal
#' degrees.  Default = 100m.
#' @param geod logical. If TRUE the radius around each institution is calculated
#' based on a sphere, buffer is in meters and independent of latitude. If FALSE
#' the radius is calculated assuming planar coordinates and varies slightly with latitude,
#' in this case buffer is in degrees. Default = TRUE.
#' @param ref  SpatialPointsDataFrame. Providing the geographic gazetteer. Can
#' be any SpatialPointsDataFrame, but the structure must be identical to
#' \code{\link{institutions}}.  Default = \code{\link{institutions}}
#' @param verify logical. If TRUE, records close to institutions are only flagged,
#'  if there are no other records of the same species in the greater vicinity 
#'  (a radius of buffer * verify_mltpl).
#' @param verify_mltpl numerical. indicates the factor by whihch the radius for verify
#' exceeds the radius of the initial test. Default = 10, which might be suitable if 
#' geod is TRUE, but might be too large otherwise.
#' @param value character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'
#'#large buffer for demonstration, using geod = FALSE for shorter runtime              
#' cc_inst(x, value = "flagged", buffer = 10, geod = FALSE) 
#' 
#' \dontrun{
#' #' cc_inst(x, value = "flagged", buffer = 50000) #geod = T
#' }
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom raster crop extent
#' @importFrom sp coordinates CRS disaggregate over Polygon Polygons SpatialPolygons SpatialPoints
cc_inst <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude",
                    species = "species",
                    buffer = 100,
                    geod = TRUE,
                    ref = NULL, 
                    verify = FALSE,
                    verify_mltpl = 10,
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
  
  #double check flagged records, for records from the same species in the greater surroundings
  if(verify){
    #identify flagged records
    ref_in <- x[!out,]
    
    #buffer with a larger buffer than for the intial test
    if(geod){
      # credits to https://seethedatablog.wordpress.com
        dg <- seq(from = 0, to = 360, by = 5)

        buff_XY <- geosphere::destPoint(p = ref_in[, c(lon, lat)],
                                        b = rep(dg, each = nrow(ref_in)),
                                        d = buffer * verify_mltpl)

        id <- rep(seq(from = 1, to = nrow(ref_in)), times = length(dg))
        lst <- split(data.frame(buff_XY), f = id)

        # Make SpatialPolygons out of the list of coordinates
        poly   <- lapply(lst, sp::Polygon, hole = FALSE)
        polys <- list()
        for(i in seq_along(poly)){
          polys[[i]] <- sp::Polygons(list(poly[[i]]), ID = rownames(ref_in)[i])
          }
        spolys <- sp::SpatialPolygons(Srl = polys, proj4string = CRS(wgs84))
        ref <- sp::SpatialPolygonsDataFrame(spolys, data = data.frame(ref_in))
    }else{
      ref <- rgeos::gBuffer(ref, width = buffer * verify_mltpl, byid = TRUE)
    }
    
    #identify all records from flagged sepcies in x
    f_spec <- x[x[, species] %in% ref@data[, species],]
    
    dbch_flag <- c()
    
    for(i in seq_len(nrow(ref_in))){
      dbch <- sp::over(sp::SpatialPoints(f_spec[unlist(f_spec[species]) == ref@data[i, "species"], 
                                                c(lon, lat)], proj4string = CRS(wgs84)), 
                       ref[i,])
      
      #check if there area other records of this species in the buffer
      dbch_flag[[i]] <- sum(!is.na(dbch[species])) > 
        nrow(ref_in[ref_in[[species]] == ref_in[[i, species]] &
                      ref_in[[lon]] ==  ref_in[[i, lon]] &
                      ref_in[[lat]] ==  ref_in[[i, lat]],])
    }
    
    #unflag those records with other records of the same species nearby
    out[rownames(ref_in)] <- dbch_flag
  }

    # create output based on value argument
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

#' Flag Non-terrestrial Coordinates
#' 
#' Flags coordinates outside the reference landmass. Can be used to restrict
#' datasets to terrestrial taxa, or exclude records from the open ocean, when
#' depending on the reference (see details). Often records of terrestrial taxa
#' can be found in the open ocean, mostly due to switched latitude and
#' longitude.
#' 
#' In some cases flagging records close of the coastline is not recommendable,
#' because of the low precision of the reference dataset, minor GPS imprecision
#' or because a dataset might include coast or marshland species. If you only
#' want to flag records in the open ocean, consider using a buffered landmass
#' reference, e.g.: \code{\link{buffland}}.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ref a SpatialPolygonsDataframe. Providing the geographic gazetteer.
#' Can be any SpatialPolygonsDataframe, but the structure must be identical to
#' \code{\link{landmass}}. See details.  Default = \code{\link{landmass}}
#' @param value a character string.  Defining the output value. See value.
#' @param speedup logical. USing heuristic to speed up the analysis for large datsets
#'  with many records per location.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged. Default = TRUE.
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
#'                 decimallongitude = runif(10, -30, 30), 
#'                 decimallatitude = runif(10, -30, 30))
#'                 
#' cc_sea(x, value = "flagged")
#' 
#' @export
#' @importFrom sp CRS SpatialPoints "proj4string<-" over proj4string coordinates
#' @importFrom raster crop
cc_sea <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   ref = NULL,
                   value = "clean",
                   speedup = TRUE, 
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing sea coordinates")
  }
  
  # run test
  if(speedup){
    ## heuristic to speedup - reduce to individual locations
    inp <- x[!duplicated(x[,c(lon, lat)]),]
    pts <- sp::SpatialPoints(inp[, c(lon, lat)])
    
    # select and prepare terrestrial surface reference
    if (is.null(ref)) {
      ref <- CoordinateCleaner::landmass
      ref <- raster::crop(ref, raster::extent(pts) + 1)
    } else {
      #Enable sf formatted custom references
      ref <- as(ref, "Spatial")
      
      #Check projection of custom reference and reproject if necessary
      wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      
      if(is.na(sp::proj4string(ref))){
        warning("no projection information for reference found, 
                assuming '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'")
      }else if(sp::proj4string(ref) == wgs84){
        sp::proj4string(ref) <- ""
      }else{
        ref <- sp::spTransform(ref, sp::CRS(wgs84))
        warning("reprojecting reference to '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'")
      }
    }
    
   
    ## point-in-polygon test
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
    out <- data.frame(sp::coordinates(pts), out)
    
    ## remerge with coordinates
    dum <- x
    dum$order <- 1:nrow(dum)
    out <- merge(dum, out, by = c(lat,lon), sort = FALSE)
    out <- out[order(out$order),]
    out <- out$out
  }else{
    pts <- sp::SpatialPoints(x[, c(lon, lat)])
    
    # select and prepare terrestrial surface reference
    if (is.null(ref)) {
      ref <- CoordinateCleaner::landmass
      ref <- raster::crop(ref, raster::extent(pts) + 1)
    } else {
      sp::proj4string(ref) <- ""
      warning("Assuming lat/lon for ref")
    }
    
    # select relevant columns
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
  }

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

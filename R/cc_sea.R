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
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ref a SpatialPolygonsDataFrame. Providing the geographic gazetteer.
#' Can be any SpatialPolygonsDataFrame, but the structure must be identical to
#' rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical').  
#' Default = rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical')
#' @param scale the scale of the default reference, as downloaded from natural earth. 
#' Must be one of 10, 50, 110. Higher numbers equal higher detail. Default = 50.
#' @param value a character string.  Defining the output value. See value.
#' @param speedup logical. Using heuristic to speed up the analysis for large data sets
#'  with many records per location.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged. Default = TRUE.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
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
#' @importFrom rgdal readOGR
#' @importFrom rnaturalearth ne_download
cc_sea <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   ref = NULL,
                   scale = 50,
                   value = "clean",
                   speedup = TRUE, 
                   verbose = TRUE){

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing sea coordinates")
  }
  
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # run test
  if(speedup){
    ## heuristic to speedup - reduce to individual locations
    inp <- x[!duplicated(x[,c(lon, lat)]),]
    pts <- sp::SpatialPoints(inp[, c(lon, lat)], proj4string = CRS(wgs84))
    
    # select and prepare terrestrial surface reference
    if (is.null(ref)) {
      if(!scale %in%  c(10, 50, 110)){
        stop("scale must be one of c(10,50,110)")
      }
      
      ref <- rnaturalearth::ne_download(scale = scale, 
                                        type = 'land', 
                                        category = 'physical')
      ref <- raster::crop(ref, raster::extent(pts) + 1)
    }else{
      #Check projection of custom reference and reproject if necessary
      if(is.na(sp::proj4string(ref))){
        warning("no projection information for reference found, 
                assuming '+proj=longlat +datum=WGS84 +no_defs 
                +ellps=WGS84 +towgs84=0,0,0'")
      }else if(sp::proj4string(ref) != wgs84){
        ref <- sp::spTransform(ref, sp::CRS(wgs84))
        warning("reprojecting reference to '+proj=longlat +datum=WGS84 
                +no_defs +ellps=WGS84 +towgs84=0,0,0'")
      }
    }

    ## point-in-polygon test
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
    out <- data.frame(sp::coordinates(pts), out)
    
    ## remerge with coordinates
    dum <- x
    dum$order <- seq_len(nrow(dum))
    out <- merge(dum, out, by = c(lat,lon), sort = FALSE)
    out <- out[order(out$order),]
    out <- out$out
  }else{
    
    pts <- sp::SpatialPoints(x[, c(lon, lat)], proj4string = wgs84)
    
    # select and prepare terrestrial surface reference
    if (is.null(ref)) {
      match.arg(scale, choices = c(10, 50, 110))
      
      ref <- rnaturalearth::ne_download(scale = scale, 
                                        type = 'land', 
                                        category = 'physical')
      ref <- raster::crop(ref, raster::extent(pts) + 1)
    } else {
      #Check projection of custom reference and reproject if necessary
      if(is.na(sp::proj4string(ref))){
        warning("no projection information for reference found, 
                assuming '+proj=longlat +datum=WGS84 +no_defs 
                +ellps=WGS84 +towgs84=0,0,0'")
      }else if(sp::proj4string(ref) != wgs84){
        ref <- sp::spTransform(ref, sp::CRS(wgs84))
        warning("reprojecting reference to '+proj=longlat +datum=WGS84 
                +no_defs +ellps=WGS84 +towgs84=0,0,0'")
      }
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

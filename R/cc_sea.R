#' Identify Non-terrestrial Coordinates
#' 
#' Removes or flags coordinates outside the reference landmass. Can be used to restrict
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
#' @param ref a SpatialPolygonsDataFrame. Providing the geographic gazetteer.
#' Can be any SpatialPolygonsDataFrame, but the structure must be identical to
#' rnaturalearth::ne_download(scale = 10, type = 'land', category = 'physical').  
#' Default = rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical')
#' @param scale the scale of the default reference, as downloaded from natural earth. 
#' Must be one of 10, 50, 110. Higher numbers equal higher detail. Default = 50.
#' @param speedup logical. Using heuristic to speed up the analysis for large data sets
#'  with many records per location.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(10, -30, 30), 
#'                 decimallatitude = runif(10, -30, 30))
#'                 
#' cc_sea(x, value = "flagged")
#' 
#' @export
#' @importFrom dplyr inner_join
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
  
  # heuristic to speedup - reduce to individual locations, 
  #overwritten later in cases speedup == FALSE
  inp <- x[!duplicated(x[,c(lon, lat)]),]
  pts <- sp::SpatialPoints(inp[, c(lon, lat)], proj4string = CRS(wgs84))
  
  # select and prepare terrestrial surface reference
  if (is.null(ref)) {
    if(!scale %in%  c(10, 50, 110)){
      stop("scale must be one of c(10,50,110)")
    }
    
    path <- file.path(system.file(package = "CoordinateCleaner"), "sea.EXT")
    file <- file.path(path, paste("ne_", scale, "m_land.shp", sep = ""))
    
    #Download if file does not exist yet
    if(!file.exists(file)) {
      ref <- rnaturalearth::ne_download(scale = scale, 
                                        type = 'land', 
                                        category = 'physical',
                                        destdir = path,
                                        load = FALSE)
    }
    
    #load reference
    ref <- rgdal::readOGR(path, 
                          paste("ne_", scale, "m_land", sep = ""), 
                          encoding = "UTF-8",
                          stringsAsFactors = FALSE, 
                          use_iconv = TRUE)
    ref@data[ref@data == "-99" | 
                     ref@data == "-099"] <- NA
    
    #Crop to the spatial extent of the points
    ref <- raster::crop(ref, raster::extent(pts) + 1)
  } else {
    ref <- reproj(ref)
  }
  
  # run test
  if(speedup){
    ## point-in-polygon test
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
    out <- data.frame(sp::coordinates(pts), out)
    
    ## remerge with coordinates
    dum <- x
    dum$order <- seq_len(nrow(dum))
    out <- dplyr::inner_join(dum,out, by = c(lat,lon))
    out <- out[order(out$order),]
    out <- out$out
  }else{
    pts <- sp::SpatialPoints(x[, c(lon, lat)], proj4string = CRS(wgs84))
    
    # select relevant columns
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
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

# Check projection of custom reference and reproject to wgs84 if necessary
reproj <- function(ref) {
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # if no projection information is given assume wgs84
  if (is.na(terra::crs(ref, proj = TRUE))) {
    warning("no projection information for reference found, 
            assuming '+proj=longlat +datum=WGS84 +no_defs'")
    ref <- terra::project(ref, wgs84)
  } else if (terra::crs(ref, proj = TRUE) != wgs84) {
    #otherwise reproject
    ref <- terra::project(ref, wgs84)
    warning("reprojecting reference to '+proj=longlat +datum=WGS84 +no_defs'")
  }
  return(ref)
}

# A function to create a raster from an input dataset, used in cc_outl for spatial thinning, 
# based on a point dataset and a raster resolution

ras_create <- function(x, lat, lon,  thinning_res){
  # get data extend
  ex <- terra::ext(terra::vect(x[, c(lon, lat)])) + thinning_res * 2
  
  # check for boundary conditions
  if(ex[1] < -180 | ex[2] > 180 | ex[3] < -90 | ex[4] >90){
    warning("fixing raster boundaries, assuming lat/lon projection")
    
    if(ex[1] < -180){ex[1] <- -180}
    
    if(ex[2] > 180){ex[2] <- 180}
    
    if(ex[3] < -90){ex[3] <- -90}
    
    if(ex[4] > 90){ex[4] <- 90}
  }
  
  # create raster
  ras <- terra::rast(x = ex, resolution = thinning_res)
  
  # set cell ids
  vals <- seq_len(terra::ncell(ras))
  ras <- terra::setValues(ras, vals)
  
  return(ras)
}


# A function to get the distance between raster midpoints and 
#output a data.frame with the distances and the cell IDs as row and column names for cc_outl

ras_dist <-  function(x, lat, lon, ras, weights) {
  # x = a data.frame of point coordinates, ras = a raster with cell IDs as layer,
  #weight = logical, shall the distance matrix be weightened by the number of points per cell?
  # assign each point to a raster cell
  pts <- terra::extract(x = ras, 
                        y = terra::vect(x[, c(lon, lat)],
                                        geom = c(lon, lat),
                                        crs = ras))
  
  # convert to data.frame
  midp <- data.frame(terra::as.points(ras))
  
  # retain only cells that contain points
  midp <- midp[midp$layer %in% unique(pts),]
  
  # order
  midp <- midp[match(unique(pts), midp$layer),]
  
  # calculate geospheric distance between raster cells with points
  dist <- geosphere::distm(midp[, c("x", "y")], 
                           fun = geosphere::distHaversine) / 1000
  
  # set rownames and colnames to cell IDs
  dist <- as.data.frame(dist, row.names = as.integer(midp$layer))
  names(dist) <- midp$layer
  
  if(weights){
    # approximate within cell distance as half 
    # the cell size, assumin 1 deg = 100km
    # this is crude, but doesn't really matter
    dist[dist == 0] <- 100 * mean(terra::res(ras)) / 2
    
    # weight matrix to account for the number of points per cell
    ## the number of points in each cell
    cou <- table(pts)
    
    ## order
    cou <- cou[match(unique(pts), names(cou))]
    
    # weight matrix, representing the number of distances between or within the cellse (points cell 1 * points cell 2)
    wm <- outer(cou, cou)
    
    # multiply matrix elements to get weightend sum
    dist <- round(dist * wm, 0)
    
    dist <- list(pts = pts, dist = dist, wm = wm)
  } else {
    # set diagonale to NA, so it does not influence the mean
    dist[dist == 0] <- NA
    
    dist <- list(pts = pts, dist = dist)
  }
  
  return(dist)
}







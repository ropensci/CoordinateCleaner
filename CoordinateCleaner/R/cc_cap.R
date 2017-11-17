cc_cap<- function(x, lon  = "decimallongitude", lat = "decimallatitude", 
                  buffer = 0.1, ref = NULL, value = "clean", verbose = TRUE) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags"))
  
  if(verbose){
    cat("Testing country capitals\n")
  }
  
  #select relevant columns
  dat <- sp::SpatialPoints(x[,c(lon,lat)])
  
  #check for reference data and adapt projection of custom reference data
  if (is.null(ref)) {
    ref <- CoordinateCleaner::capitals
  }else{
    sp::proj4string(ref) <- ""
    warning("assuming lat/lon WGS84 for ref")
  }
  
  #subset reference data to data window to spead up the test
  limits <- raster::extent(dat) + buffer
  ref <- raster::crop(SpatialPoints(ref[, c("longitude", "latitude")]), limits)
  
  #test if any points fall within the buffer
  if(is.null(ref)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    ref <- rgeos::gBuffer(ref, width = buffer, byid = T)
    out <- is.na(sp::over(x = dat, y = ref))
  }
  
  #create output based on value argument
  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }

  switch(value,
         clean = return(x[out,]),
         flags = return(out))
}

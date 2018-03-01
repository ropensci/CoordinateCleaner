cc_urb <- function(x, lon = "decimallongitude", lat = "decimallatitude", 
                   ref = NULL, value = "clean", verbose = TRUE) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags"))
  
  if(verbose){
    cat("Testing urban areas\n")
  }

  #check for reference data. FOr this function reference hast to be supplied, 
  #availble e.g. from the packages GitHub repository
  if (is.null(ref)) {
    stop("No referencepolygons found. Set 'urban.ref'")
  }else{
    sp::proj4string(ref) <- ""
    warning("assumning lat/lon for ref")
  }
  
  #Prepare input points and extent
  dat <- sp::SpatialPoints(x[,c(lon,lat)])
  limits <- raster::extent(dat) + 1
  ref <- raster::crop(ref, limits)
  
  #test if any points fall within the buffer
  if(is.null(ref)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    out <- is.na(sp::over(x = dat, y = ref)[, 1])
  }

  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
}
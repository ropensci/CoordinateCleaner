cc_coun <- function(x, lon = "decimallongitude", lat = "decimallatitude", iso3 = "countrycode",
                          value = "clean", ref = NULL, verbose = TRUE) {
  
  #check function arguments for validity
  match.arg(value, choices = c("clean", "flags"))
  if(!iso3 %in% names(x)){stop("iso3 argument missing, please specify")}
  
  if(verbose){
    cat("Testing country identity\n")
  }

  #set reference and check for dependency
  if (is.null(ref)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Please install the 'rnaturalearth' package for the country test or provide a custom reference via ref", 
           call. = FALSE)
    }
    ref <- rnaturalearth::ne_countries(scale = "medium")
    sp::proj4string(ref) <- ""
  }else{
    sp::proj4string(ref) <- ""
    warning("assuming lat/lon for country.ref")
  }
  
  #prepare data
  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  sp::proj4string(ref) <- ""
  ref <- raster::crop(ref, raster::extent(dat) + 1)
  
  #get country from coordinates and compare with provided country
  country <- sp::over(x = dat, y = ref)[, "iso_a3"]
  out <- as.character(country) == as.character(x[, iso3])
  out[is.na(out)] <- TRUE #marine records are ignored / not tested
  
  #return output
  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
}

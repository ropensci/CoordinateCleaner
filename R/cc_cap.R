cc_cap <- function(x, lon = "decimallongitude", lat = "decimallatitude", buffer = 0.1, 
    ref = NULL, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags"))
    
    if (verbose) {
        message("Testing country capitals")
    }
    
    # select relevant columns
    dat <- sp::SpatialPoints(x[, c(lon, lat)])
    
    # check for reference data and adapt projection of custom reference data
    if (is.null(ref)) {
        ref <- CoordinateCleaner::capitals
    } else {
        sp::proj4string(ref) <- ""
        warning("assuming lat/lon WGS84 for ref")
    }
    
    # subset reference data to data window to spead up the test
    limits <- raster::extent(dat) + buffer
    ref <- raster::crop(SpatialPoints(ref[, c("longitude", "latitude")]), limits)
    
    # test if any points fall within the buffer incase no capitals are found in
    # the study area
    if (is.null(ref)) {
        out <- rep(TRUE, nrow(x))
    } else {
        ref <- rgeos::gBuffer(ref, width = buffer, byid = TRUE)
        out <- is.na(sp::over(x = dat, y = ref))
    }
    
    # create output based on value argument
    if (verbose) {
        message(sprintf("Flagged %s records.", sum(!out)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out))
}
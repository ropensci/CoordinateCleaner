cc_sea <- function(x, lon = "decimallongitude", lat = "decimallatitude", ref = NULL, 
    value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags"))
    
    if (verbose) {
        message("Testing sea coordinates")
    }
    
    # select relevant columns
    pts <- sp::SpatialPoints(x[, c(lon, lat)])
    
    # select and prepare terrestrial surface reference
    if (is.null(ref)) {
        ref <- CoordinateCleaner::landmass
        ref <- raster::crop(ref, raster::extent(pts) + 1)
    } else {
        sp::proj4string(ref) <- ""
        warning("Assuming lat/lon for ref")
    }
    
    # run test
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
    
    if (verbose) {
        message(sprintf("Flagged %s records.", sum(!out)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out))
}
80

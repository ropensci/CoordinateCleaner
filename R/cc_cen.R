cc_cen <- function(x, lon = "decimallongitude", lat = "decimallatitude", buffer = 0.1, 
    test = "both", ref = NULL, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags"))
    match.arg(test, choices = c("both", "country", "provinces"))
    
    if (verbose) {
        cat("Testing country centroids\n")
    }
    
    # select relevant columns
    dat <- sp::SpatialPoints(x[, c(lon, lat)])
    
    if (is.null(ref)) {
        ref <- CoordinateCleaner::centroids
        
        switch(test, country = {
            ref <- ref[ref$type == "country", ]
        }, province = {
            ref <- ref[ref$type == "province", ]
        })
        
    } else {
        proj4string(ref) <- ""
        warning("assuming lat/lon for centroids.ref")
    }
    
    limits <- raster::extent(dat) + buffer
    
    # subset of testdatset according to speed up buffer
    ref <- raster::crop(sp::SpatialPoints(ref[, c("longitude", "latitude")]), 
        limits)
    
    # run buffering incase no capitals are found in the study area
    if (is.null(ref)) {
        out <- rep(TRUE, nrow(x))
    } else {
        ref <- rgeos::gBuffer(ref, width = buffer, byid = TRUE)
        out <- is.na(sp::over(x = dat, y = ref))
    }
    
    # create output based on value argument
    if (verbose) {
        cat(sprintf("Flagged %s records. \n", sum(!out)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out))
}
80

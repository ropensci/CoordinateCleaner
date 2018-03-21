cc_inst <- function(x, lon = "decimallongitude", lat = "decimallatitude", buffer = 0.001, 
    ref = NULL, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags"))
    
    if (verbose) {
        cat("Testing biodiversity institutions\n")
    }
    
    dat <- sp::SpatialPoints(x[, c(lon, lat)])
    
    # prepare reference dataset
    if (is.null(ref)) {
        ref <- CoordinateCleaner::institutions
        ref <- ref[!is.na(ref$decimallongitude) & !is.na(ref$decimallatitude), 
            ]
    }
    limits <- raster::extent(dat) + buffer
    
    # subset of testdatset according to limits
    ref <- raster::crop(sp::SpatialPoints(ref[, c("decimallongitude", "decimallatitude")]), 
        limits)
    
    # test reference data after limiting and do test in case no bdinstitutions
    if (is.null(ref)) {
        out <- rep(TRUE, nrow(x))
    } else {
        ref <- rgeos::gBuffer(ref, width = buffer, byid = TRUE)
        out <- is.na(sp::over(x = dat, y = ref))
    }
    
    # create output based on value argument
    if (verbose) {
        cat(sprintf("Flagged %s records\n", sum(!out)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out))
}
80

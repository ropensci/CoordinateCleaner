cc_outl <- function(x, lon = "decimallongitude", lat = "decimallatitude", species = "species", 
    method = "quantile", mltpl = 3, tdi = 1000, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags", "ids"))
    match.arg(method, choices = c("distance", "quantile", "mad"))
    
    if (verbose) {
        message("Testing geographic outliers")
    }
    
    # split up into species
    splist <- split(x, f = as.character(x[[species]]))
    
    # remove duplicate records and make sure that there are at least two records
    # left
    test <- lapply(splist, "duplicated")
    test <- lapply(test, "!")
    test <- as.vector(unlist(lapply(test, "sum")))
    splist <- splist[test > 7]
    
    # loop over species and run outlier test
    flags <- lapply(splist, function(k) {
        test <- nrow(k[!duplicated(k), ])
        
        if (test > 7) {
            # absolute distance test with mean interpoint distance
            if (method == "distance") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                
                mins <- apply(dist, 1, min, na.rm = TRUE)
                out <- which(mins > tdi * 1000)
            }
            
            # Quantile based test, with mean interpoint distances
            if (method == "quantile") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                
                mins <- apply(dist, 1, mean, na.rm = TRUE)
                quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
                out <- which(mins < quo[1] - IQR(mins) * mltpl | mins > quo[2] + 
                  IQR(mins) * mltpl)
            }
            
            # MAD (Median absolute deviation) based test, calculate the mean distance to
            # all other points for each point, and then take the mad of this
            if (method == "mad") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                
                mins <- apply(dist, 1, mean, na.rm = TRUE)
                quo <- median(mins)
                tester <- mad(mins)
                out <- which(mins < quo - tester * mltpl | mins > quo + tester * 
                  mltpl)
            }
        }
        # create output object
        if (length(out) == 0) {
            ret <- NA
        }
        if (length(out) > 0) {
            ret <- rownames(k)[out]
        }
        return(ret)
    })
    
    flags <- as.numeric(as.vector(unlist(flags)))
    flags <- flags[!is.na(flags)]
    
    out <- rep(TRUE, nrow(x))
    out[flags] <- FALSE
    
    if (verbose) {
        if (value == "ids") {
            message(sprintf("Flagged %s records.", length(flags)))
        } else {
            message(sprintf("Flagged %s records.", sum(!out)))
        }
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out), ids = return(flags))
}

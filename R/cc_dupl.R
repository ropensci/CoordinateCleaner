cc_dupl <- function(x, lon = "decimallongitude", lat = "decimallatitude", species = "species", 
    additions = NULL, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags"))
    
    if (verbose) {
        message("Testing duplicates")
    }
    # test duplication
    out <- !duplicated(x[, c(lon, lat, species, additions)])
    
    # create output based on value argument
    if (verbose) {
        message(sprintf("Flagged %s records.", sum(!out)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out))
    
}
tc_outl <- function(x, lon = "lng", lat = "lat", min.age = "min_ma", max.age = "max_ma", 
    taxon = "accepted_name", method = "quantile", size.thresh = 7, mltpl = 5, 
    replicates = 5, flag.thresh = 0.5, uniq.loc = FALSE, value = "clean", verbose = TRUE) {
    
    # check value argument
    match.arg(value, choices = c("clean", "flags", "ids"))
    match.arg(method, choices = c("quantile", "mad"))
    
    # report analyses step
    if (verbose) {
        if (taxon == "") {
            cat("Testing spatio-temporal outliers on dataset level\n")
        } else {
            cat("Testing spatio-temporal outliers on taxon level\n")
        }
    }
    
    out <- replicate(replicates, expr = {
        
        # create testing data by simulating points within the age range of each
        # individal method fossil
        x$samplepoint <- apply(X = x, 1, FUN = function(k) {
            stats::runif(n = 1, min = as.numeric(k[[min.age]], na.rm = TRUE), 
                max = as.numeric(k[[max.age]], na.rm = TRUE))
        })
        x$samplepoint <- round(x$samplepoint, 2)
        
        
        if (taxon == "") {
            # select relevant columns test <- x[, c(lon, lat, min.age, max.age,
            # 'samplepoint', 'idf')]
            test <- x[, c(lon, lat, min.age, max.age, "samplepoint")]
            # round coordinates to one decimal
            test[, lon] <- round(test[, lon], 1)
            test[, lat] <- round(test[, lat], 1)
            
            # remove duplicates
            if (uniq.loc) {
                test <- test[!duplicated(test[, c(lon, lat, min.age, max.age)]), 
                  ]
            }
            
            # calculate geographic distance
            if (nrow(test) < 10000) {
                dis.geo <- geosphere::distm(test[, c(lon, lat)], fun = geosphere::distHaversine)/1000
            } else {
                dis.geo <- as.matrix(dist(test[, c(lon, lat)]))
                warning("Large dataset, geographic space treated as euclidean for outlier test")
            }
            
            dis.geo[dis.geo == 0] <- NA
            
            # calculate temporal distance
            dis.tmp <- as.matrix(dist(test[, c("samplepoint")]))
            dis.tmp[dis.tmp == 0] <- NA
            
            # scale distance to be comparable
            dis.tmp <- dis.tmp * max(dis.geo, na.rm = TRUE)/max(dis.tmp, na.rm = TRUE)
            
            # sum time and space
            dis <- round(dis.tmp + dis.geo, 0)
            
            # quantile based method
            if (method == "quantile") {
                mins <- apply(dis, 1, mean, na.rm = TRUE)
                quo <- quantile(mins, 0.75, na.rm = TRUE)
                flags <- mins > quo + IQR(mins, na.rm = TRUE) * mltpl
                # out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl) flags <- test[out,
                # 'idf']
            }
            
            # MAD (Median absolute deviation) based test, calculate the mean distance to
            # all other points for each point, and then take the mad of this
            if (method == "mad") {
                mins <- apply(dis, 1, mean, na.rm = TRUE)
                quo <- median(mins, na.rm = TRUE)
                tester <- mad(mins, na.rm = TRUE)
                flags <- mins > quo + tester * mltpl
                # out <- which(mins > quo + tester * mltpl) flags <- test[out, 'idf']
            }
            
        } else {
            # create identifier
            x$idf <- seq_len(nrow(x))
            # select relevant columns
            splist <- x[, c(lon, lat, min.age, max.age, "samplepoint", taxon, 
                "idf")]
            # splist <- x[, c(lon, lat, min.age, max.age, 'samplepoint', taxon)]
            
            # remove identifier column
            x <- x[, names(x) != "idf"]
            
            # round coordinates to one decimal
            splist[, lon] <- round(splist[, lon], 1)
            splist[, lat] <- round(splist[, lat], 1)
            
            # get unique occurrences
            if (uniq.loc) {
                splist <- splist[!duplicated(splist[, c(taxon, lon, lat, min.age, 
                  max.age)]), ]
            }
            # split up into taxon
            splist <- split(splist, f = as.character(splist[[taxon]]))
            
            # only test taxa with a minimum number of records
            test <- as.vector(unlist(lapply(splist, "nrow")))
            splist <- splist[test >= size.thresh]
            
            # loop over taxon and run outlier test
            test <- lapply(splist, function(k) {
                
                # calculate geographic distance
                if (nrow(k) < 10000) {
                  dis.geo <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)/1000
                } else {
                  dis.geo <- as.matrix(dist(k[, c(lon, lat)]))
                  warning("Large dataset, geographic treated as euclidean for outlier test")
                }
                
                # calculate temporal distance
                dis.tmp <- as.matrix(dist(k[, c("samplepoint")]))
                
                # scale distance to be comparable
                dis.tmp <- dis.tmp * max(dis.geo, na.rm = TRUE)/max(dis.tmp, 
                  na.rm = TRUE)
                dis.tmp[is.na(dis.tmp)] <- 0
                
                # sum time and space
                dis <- round(dis.tmp + dis.geo, 0)
                
                # test if there are distances other than 0
                if (sum(!is.na(dis)) > 0) {
                  
                  if (method == "quantile") {
                    mins <- apply(dis, 1, mean, na.rm = TRUE)
                    quo <- quantile(mins, 0.75, na.rm = TRUE)
                    flags <- mins > quo + IQR(mins, na.rm = TRUE) * mltpl
                    # out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl) out <- k[out,
                    # 'idf']
                  }
                  
                  # MAD (Median absolute deviation) based test, calculate the mean distance to
                  # all other points for each point, and then take the mad of this
                  if (method == "mad") {
                    mins <- apply(dis, 1, mean, na.rm = TRUE)
                    quo <- median(mins, na.rm = TRUE)
                    tester <- mad(mins, na.rm = TRUE)
                    flags <- mins > quo + tester * mltpl
                    # out <- which(mins > quo + tester * mltpl) out <- k[out, 'idf']
                  }
                  
                  
                  # create output object
                  if (length(flags) == 0) {
                    ret <- NA
                  } else {
                    ret <- k[flags, "idf"]
                  }
                } else {
                  ret <- NA
                }
                return(ret)
            })
            
            # transform the identifiers into true/flas flags in the sam order as x; TRUE
            # means flagged
            flags <- rep(FALSE, nrow(x))
            flags[unlist(test)] <- TRUE
        }
        
        # create vector with flagged rownumbers flags <-
        # as.numeric(as.vector(unlist(flags))) flags <- flags[!is.na(flags)]
        
        # create vector of logical flags out <- rep(TRUE, nrow(x))
        # out[as.numeric(flags)] <- FALSE
        out <- !flags
        
        return(out)
    })
    
    frac <- apply(out, 1, "mean")
    
    out <- frac >= flag.thresh
    
    # also mark records that might not have been flagged due to the duplicate
    # removal above
    if (taxon == "" & any(!out) & uniq.loc) {
        supp <- x[!out, c(lon, lat, min.age, max.age)]
        test <- apply(supp, 1, function(k) {
            outp <- which(k[[lon]] == x[[lon]] & k[[lat]] == x[[lat]] & k[[min.age]] == 
                x[[min.age]] & k[[max.age]] == x[[max.age]])
        })
        test <- unlist(test)
        test <- as.numeric(x[test, ]$idf)
        out[test] <- FALSE
    } else {
        if (any(!out) & uniq.loc) {
            supp <- x[!out, c(taxon, lon, lat, min.age, max.age)]
            outp <- list()
            for (j in seq_len(nrow(supp))) {
                k <- supp[j, ]
                outp[[j]] <- which(k[[taxon]] == x[[taxon]] & k[[lon]] == x[[lon]] & 
                  k[[lat]] == x[[lat]] & k[[min.age]] == x[[min.age]] & k[[max.age]] == 
                  x[[max.age]])
            }
            test <- unlist(outp)
            test <- as.numeric(x[test, ]$idf)
            out[test] <- FALSE
        }
    }
    
    # report to screen
    if (verbose) {
        cat(sprintf("Flagged %s records. \n", sum(!out, na.rm = TRUE)))
    }
    
    switch(value, clean = return(x[out, ]), flags = return(out), ids = return(which(!out)))
}
80

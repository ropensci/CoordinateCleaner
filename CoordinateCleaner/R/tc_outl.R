# questions: How to deal with the fact, that there are many records with the same coordinates and or time range/midpoint 
#nrmalization correct? are mean values for the distances OK or any obvious problem? 
#excluded duplications for dataset test? - No duplications of coordinates and timrange?


tc_outl <- function(x, lon = "lng", lat = "lat", min.age = "min_ma", max.age = "max_ma",
                    taxon = "identified_name", method = "quantile", 
                    mltpl = 3, value = "clean", verbose = T, flag.prob = 0.1) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flags", "ids"))
  match.arg(method, choices = c("quantile", "mad"))

  # create testing data by simulating 100 points within the age range of each individal method fossil
  x$samplepoint <- runif(1, x[[min.age]], x[[max.age]])
  
  if (taxon == "") {
    
    if (verbose) {
      cat("Testing spatio-temporal outliers on dataset level\n")
    }
    
    # select relevant columns
    test <- x[, c(lon, lat, min.age, max.age, "samplepoint")]
    
    # remove duplicates
    #test <- test[!duplicated(test[, c(lon, lat, min.age, max.age)]), ]
    
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
    dis.tmp <- dis.tmp * max(dis.geo, na.rm = T)/max(dis.tmp, na.rm = T)
    
    # sum time and space
    dis <- round(dis.tmp + dis.geo, 0)
    
    # quantile based method
    if (method == "quantile") {
      mins <- apply(dis, 1, mean, na.rm = T)
      quo <- quantile(mins, 0.75, na.rm = T)
      out <- which(mins > quo + IQR(mins) * mltpl)
    }
    
    # MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
    if (method == "mad") {
      mins <- apply(dis, 1, mean, na.rm = T)
      quo <- median(mins)
      tester <- mad(mins)
      out <- which(mins > quo + tester * mltpl)
    }
    
    out <- rownames(test)[out]
  } else {
    if (verbose) {
      cat("Testing spatio-temporal outliers on taxon level\n")
    }
    
    # select relevant columns
    splist <- x[, c(lon, lat, min.age, max.age, "samplepoint", taxon)]
    
    # split up into taxon
    splist <- split(splist, f = as.character(x[[taxon]]))
    
    # only test taxa with a minimum number of records
    test <- as.vector(unlist(lapply(splist, "nrow")))
    splist <- splist[test > size.thresh]
    
    # loop over taxon and run outlier test
    flags <- lapply(splist, function(k) {

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
      dis.tmp <- dis.tmp * max(dis.geo, na.rm = T)/max(dis.tmp, na.rm = T)
      dis.tmp[is.na(dis.tmp)] <- 0
      
      # sum time and space
      dis <- round(dis.tmp + dis.geo, 0)
      
      #test if there are distances other than 0
      if(sum(!is.na(dis)) > 0){
        
        if (method == "quantile") {
          mins <- apply(dis, 1, mean, na.rm = T)
          quo <- quantile(mins, 0.75, na.rm = T)
          out <- which(mins > quo + IQR(mins) * mltpl)
        }
        
        # MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
        if (method == "mad") {
          mins <- apply(dis, 1, mean, na.rm = T)
          quo <- median(mins)
          tester <- mad(mins)
          out <- which(mins > quo + tester * mltpl)
        }
        
        # create output object
        if (length(out) == 0) {
          ret <- NA
        }
        if (length(out) > 0) {
          ret <- rownames(k)[out]
        }
      }else{
        ret <- NA
      }
      
     return(ret)
    })
  }
  
  # create vector with flagged rownumbers
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  # create vector of logical flags
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  # report to screen
  if (verbose) {
    if (value == "ids") {
      cat(sprintf("Flagged %s records. \n", length(flags)))
    } else {
      cat(sprintf("Flagged %s records. \n", sum(!out)))
    }
  }
  
  switch(value, clean = return(x[out, ]), 
         flags = return(out), 
         ids = return(flags))
}

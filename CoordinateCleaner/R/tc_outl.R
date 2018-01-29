# questions: How to deal with the fact, that there are many records with the same coordinates and or time range/midpoint 
#nrmalization correct? are mean values for the distances OK or any obvious problem? 
#excluded duplications for dataset test? - No duplications of coordinates and timrange?


tc_outl <- function(x, lon = "lng", lat = "lat", min.age = "min_ma", max.age = "max_ma",
                    taxon = "accepted_name", method = "quantile", size.thresh = 7,
                    mltpl = 3, replicates = 5, flag.thresh = 0.5,
                    value = "clean", verbose = T) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flags", "ids"))
  match.arg(method, choices = c("quantile", "mad"))
  
  # report analyses step
  if (verbose) {
    if (taxon == "") {
      cat("Testing spatio-temporal outliers on dataset level\n")
    }else{
      cat("Testing spatio-temporal outliers on taxon level\n")
    }
  }

  out <- replicate(replicates, expr = {
  
    
    
  # create testing data by simulating points within the age range of each individal method fossil
  x$samplepoint <- apply(X = x, 1, FUN = function(k){stats::runif(n = 1, 
                                                         min = as.numeric(k[[min.age]], na.rm = T),
                                                         max = as.numeric(k[[max.age]], na.rm = T))})
  x$samplepoint <- round(x$samplepoint, 2)
  
  if (taxon == "") {
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
      out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl)
    }
    
    # MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
    if (method == "mad") {
      mins <- apply(dis, 1, mean, na.rm = T)
      quo <- median(mins, na.rm = T)
      tester <- mad(mins, na.rm = T)
      out <- which(mins > quo + tester * mltpl)
    }
    
    flags <- rownames(test)[out]
  
    } else {
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
          out <- which(mins > quo + IQR(mins, na.rm = T) * mltpl)
        }
        
        # MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
        if (method == "mad") {
          mins <- apply(dis, 1, mean, na.rm = T)
          quo <- median(mins, na.rm = T)
          tester <- mad(mins, na.rm = T)
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
  out[rownames(x) %in% flags] <- FALSE
  

  return(out)
})

  frac <- apply(out, 1, "mean")
  out <- frac >= flag.thresh
  
  # report to screen
  if (verbose) {
      cat(sprintf("Flagged %s records. \n", sum(!out, na.rm = T)))
  }
  
  switch(value, clean = return(x[out, ]), 
         flags = return(out), 
         ids = return(which(!out)))
}

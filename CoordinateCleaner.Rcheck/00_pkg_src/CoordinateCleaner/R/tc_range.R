tc_range <- function(x, min.age = "min_ma", max.age = "max_ma", taxon = "identified_name", 
                     method = "quantile", mltpl = 5,
                     size.thresh = 7, max.range = 500,
                     value = "clean", verbose = T) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags", "ids"))
  match.arg(method, choices = c("quantile", "mad"))

  #select relevant columns and calcualte age range
  x$range <- x[[max.age]] - x[[min.age]]
  
  if(taxon == ""){
    if (verbose) {
      cat("Testing temporal range outliers on dataset level\n")
    }
    
    rang <- x$range
    #Are there points with outlier min or max ages
    if (method == "time") {
      out <- which(rang >  max.range)
    }
    
    #Quantile based test, with mean interpoint distances
    if (method == "quantile") {
      quo <- quantile(rang, 0.75, na.rm = T)
      flags <- which(rang > (quo + IQR(rang) * mltpl))
    }
    
    #MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
    if (method == "mad") {
      quo <- median(rang)
      tester <- mad(rang)
      flags <- which(rang > quo + tester * mltpl)
    }
  
    }else{
    if (verbose) {
      cat("Testing spatio-temporal outliers on taxon level\n")
    }
    #split up into taxon
    range <- "range"
    splist <- x[, c(taxon, range)]
    splist <- split(splist, f = as.character(x[[taxon]]))
    
    #remove duplicate records and make sure that there are at least two records left
    # test <- lapply(splist, "duplicated")
    # test <- lapply(test, "!")
    # test <- as.vector(unlist(lapply(test, "sum")))
    test <- as.vector(unlist(lapply(splist, "nrow")))
    splist <- splist[test > size.thresh]
    
    #loop over taxon and run outlier test
    flags <- lapply(splist, function(k) {
      #test <- nrow(k[!duplicated(k), ])
      rang <- k[[range]]
      
      #Are there points with outlier min or max ages
      if (method == "time") {
        out <- which(rang >  max.range)
      }
      
      #Quantile based test, with mean interpoint distances
      if (method == "quantile") {
        quo <- quantile(rang, 0.75, na.rm = T)
        out <- which(rang > quo + IQR(rang) * mltpl)
      }
      
      #MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
      if (method == "mad") {
        quo <- median(rang)
        tester <- mad(rang)
        out <- which(rang > quo + tester * mltpl)
      }
      #create output object
      if (length(out) == 0) {
        ret <- NA
      }
      if (length(out) > 0) {
        ret <- rownames(k)[out]
      }
      return(ret)
    })
  }
  
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  out <- rep(TRUE, nrow(x))
  out[rownames(x) %in% flags] <- FALSE
  
  if(verbose){
    if(value == "ids"){
      cat(sprintf("Flagged %s records. \n", length(flags)))
    }else{
      cat(sprintf("Flagged %s records. \n", sum(!out)))
    }
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out),
         ids = return(flags))
}


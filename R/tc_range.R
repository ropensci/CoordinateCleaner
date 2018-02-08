tc_range <- function(x, lon = "lng", lat = "lat", 
                     min.age = "min_ma", max.age = "max_ma", taxon = "accepted_name", 
                     method = "quantile", mltpl = 5,
                     size.thresh = 7, max.range = 500,
                     uniq.loc = F,
                     value = "clean", verbose = T) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags", "ids"))
  match.arg(method, choices = c("quantile", "mad", "time"))
  
  #time and uniq loc do not work together
  if(method == "time"){
    unig.loc <- F
    warning("Using method = 'time', set 'uniq.loc' to FALSE")}

  #select relevant columns and calcualte age range
  x$range <- x[[max.age]] - x[[min.age]]
  x$idf <- rownames(x)
  
  if(taxon == ""){
    if (verbose) {
      cat("Testing temporal range outliers on dataset level\n")
    }

    # Get unique records
    if(uniq.loc){
      # select relevant columns
      rang <- x[, c(lon, lat, min.age, max.age, "idf", "range")]
      
      #round coordinates to one decimal
      rang[, lon] <- round(rang[, lon], 1)
      rang[, lat] <- round(rang[, lat], 1)
      
      #get unique occurrences
      rang <- rang[!duplicated(rang[,c(lon, lat, min.age, max.age)]),]
    }else{
      rang <- x[, c(lon, lat, min.age, max.age, "idf", "range")]
    }
    
    #Are there points with outlier min or max ages
    if (method == "time") {
      flags <- which(rang$range > max.range)
      flags <- rang[flags, "idf"]
    }
    
    #Quantile based test, with mean interpoint distances
    if (method == "quantile") {
      quo <- quantile(rang$range, 0.75, na.rm = T)
      flags <- which(rang$range > (quo + IQR(rang$range, na.rm = T) * mltpl))
      flags <- rang[flags, "idf"]
    }
    
    #MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
    if (method == "mad") {
      quo <- median(rang$range)
      tester <- mad(rang$range, na.rm = T)
      flags <- which(rang$range > quo + tester * mltpl)
      flags <- rang[flags, "idf"]
    }
    }else{
    if (verbose) {
      cat("Testing temporal range outliers on taxon level\n")
    }
      if(uniq.loc){
        # select relevant columns
        splist <- x[, c(lon, lat, min.age, max.age, taxon, "idf", "range")]
        
        #round coordinates to one decimal
        splist[, lon] <- round(splist[, lon], 1)
        splist[, lat] <- round(splist[, lat], 1)
        
        #get unique occurrences
        splist <- splist[!duplicated(splist[,c(taxon, lon, lat, min.age, max.age)]),]
      }else{
        splist <- x[, c(lon, lat, min.age, max.age, taxon, "idf", "range")]
      }

    #split up into taxon
    #range <- "range"
    splist <- split(splist, f = as.character(splist[[taxon]]))
    
    #only keep taxa with at least size.thresh taxa leftleft
    test <- as.vector(unlist(lapply(splist, "nrow")))
    splist <- splist[test >= size.thresh]
    
    #loop over taxon and run outlier test
    flags <- lapply(splist, function(k) {
      rang <- k[["range"]]
      
      #Are there points with outlier min or max ages
      if (method == "time") {
        out <- which(rang >  max.range)
        out <- k[out, "idf"]
      }
      
      #Quantile based test, with mean interpoint distances
      if (method == "quantile") {
        quo <- quantile(rang, 0.75, na.rm = T)
        out <- which(rang > quo + IQR(rang, na.rm = T) * mltpl)
        out <- k[out, "idf"]
      }
      
      #MAD (Median absolute deviation) based test, calculate the mean distance to all other points for each point, and then take the mad of this
      if (method == "mad") {
        quo <- median(rang)
        tester <- mad(rang, na.rm = T)
        out <- which(rang > quo + tester * mltpl)
        out <- k[out, "idf"]
      }
      #create output object
      if (length(out) == 0) {
        ret <- NA
      }else{
        ret <- unlist(out)
      }
      return(ret)
    })
  }
  
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  out <- rep(TRUE, nrow(x))
  out[rownames(x) %in% flags] <- FALSE
  
  #also mark records that might not have been flagged due to the duplicate removal above
  if(uniq.loc & any(!out)){
    sel <- x[rownames(x) %in% flags, c(min.age, max.age)]
    sel <- unique(sel[[max.age]] - sel[[min.age]])
    tar <- x[[max.age]] - x[[min.age]]
    out[as.numeric(x[tar %in% sel,]$idf)] <- FALSE
  }

  #remove identifier column
  x <- x[,names(x) != "idf"]
  
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


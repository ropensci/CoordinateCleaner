dc_round <- function(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset",
                     target = "lon_lat", threshold.degree = 15, threshold.period = 3.5, 
                     subsampling = NULL, diagnostics = FALSE, 
                     value = "clean", verbose = T){
  
  #check value argument
  match.arg(value, choices = c("dataset", "clean", "flags"))
  
  if(verbose){
    cat("Testing for rounded coordinates\n")
  }
  
  if(!is.null(subsampling)){
    if(subsampling < 1000){
      warning("Subsampling <1000 not recommended") 
    }
  }

  #prepare dataset for analyses
  if (sum(!complete.cases(x)) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", sum(!complete.cases(x))))
  }
  #create working dataset
  dat <- x[complete.cases(x), ]
  
  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }
  
  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat$decimallongitude) - floor(abs(dat$decimallongitude))
  dat$lat.test <- abs(dat$decimallatitude) - floor(abs(dat$decimallatitude))
  
  # split into seperate datasets
  test <- split(dat, f = dat[[ds]])
  
  out<- lapply(test, function(k) {
    t2 <- k[, c("lon.test", "lat.test")]
    
    #Subsampling (for large datasets)
    if (!is.null(subsampling)) {
      if(subsampling > nrow(t2)){
        warning("Subsampling larger than rows in data, subsampling skipped")
      }else{
        warning(sprintf("Using subsampling for periodicity, n = %s", subsampling))
        t2 <- t2[sample(nrow(t2), subsampling), ]
      }
    }
    
    #if only longitude is tested
    if (target == "lon") {
      t2.res <- .AnalyzeBias(var = t2, nam = k[[ds]][1], var_latlong = 1, 
                             plot_bias = diagnostics, 
                             ratio_threshold_0 = threshold.degree,
                             ratio_threshold_12 = threshold.period)
      names(t2.res) <- c("mle", "rate.ratio", "pass.lon", "zero.mle", "zero.rate.ratio", 
                         "pass.zero.lon")
      t2.res$pass.zero.com <- t2.res$pass.zero.lon
      t2.res$pass.periodicity.com  <- t2.res$pass.lon
      
    }
    #if only latitude is tested
    if (target == "lat") {
      t2.res <- .AnalyzeBias(var = t2, nam = k[[ds]][1], var_latlong = 2, 
                             plot_bias = diagnostics, 
                             ratio_threshold_0 = threshold.degree,
                             ratio_threshold_12 = threshold.period)
      names(t2.res) <- c("mle", "rate.ratio", "pass.lat", "zero.mle", "zero.rate.ratio", 
                         "pass.zero.lat")
      t2.res$pass.zero.com <- t2.res$pass.zero.lat
      t2.res$pass.periodicity.com  <- t2.res$pass.lat
    }
    
    #if both are tested
    if (target == "lon_lat") {
      lon <- .AnalyzeBias(t2, nam = k[[ds]][1], var_latlong = 1, 
                          plot_bias = diagnostics, 
                          ratio_threshold_0 = threshold.degree,
                          ratio_threshold_12 = threshold.period)
      lat <- .AnalyzeBias(t2, nam = k[[ds]][1], var_latlong = 2, 
                          plot_bias = diagnostics, 
                          ratio_threshold_0 = threshold.degree,
                          ratio_threshold_12 = threshold.period)
      
      t2.res <- cbind(lon, lat)
      
      names(t2.res) <- c("mle.lon", "rate.ratio.lon", "pass.periodicity.lon", 
                         "zero.mle.lon", "zero.rate.ratio.lon", "pass.zero.lon", 
                         "mle.lat", "rate.ratio.lat", 
                         "pass.periodicity.lat", "zero.mle.lat", 
                         "zero.rate.ratio.lat", "pass.zero.lat")
      t2.res$pass.zero.com <- t2.res$pass.zero.lon & t2.res$pass.zero.lat
      t2.res$pass.periodicity.com <- t2.res$pass.periodicity.lon | t2.res$pass.periodicity.lat
    }
    return(t2.res)
  })
  
  out.ds <- do.call("rbind.data.frame", out)
  
  flags <- x[[ds]] %in% rownames(out.ds[out.ds$pass.zero.com & out.ds$pass.periodicity.com,])

  
  # return output dependent on value argument
  if(verbose){
    cat(sprintf("Flagged %s records\n", sum(!flags)))
  }
  
  switch(value,
         dataset = return(out.ds),
         clean = return(x[flags,]),
         flags = return(flags))

}

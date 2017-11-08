dc_ddmm <- function(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset",
                    pvalue = 0.025, diff = 0.1, value = "clean", verbose = TRUE){
  
  #check value argument
  match.arg(value, choices = c("clean", "flags", "dataset"))
  
  if(verbose){
    cat("Testing datasets for dd.mm to dd.dd conversion errors\n")
  }
  
  #prepare dataset for analyses
  if (sum(!complete.cases(x)) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", sum(!complete.cases(x))))
  }
  dat <- x[complete.cases(x), ]
  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }

  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat[[lon]]) - floor(abs(dat[[lon]]))
  dat$lat.test <- abs(dat[[lat]]) - floor(abs(dat[[lat]]))
  
  # split into seperate datasets
  test <- split(dat, f = dat[[ds]])
  
  # run ddmm to dd.dd conversion test error at 0.6
  out <- lapply(test, function(k) {
      ## create test datasets
      dat.unique <- k[!duplicated(k[, 1:2]), ]
      
      # create input data with a raster
      r <- raster::raster(xmn = 0, xmx = 1, ymn = 0, ymx = 1)
      raster::res(r) <- 0.01
      
      dat.t1 <- raster::rasterize(sp::SpatialPoints(dat.unique[, c("lon.test", "lat.test")]), 
                                  r, fun = "count")
      dat.t1 <- raster::as.matrix(dat.t1)
      dat.t1[is.na(dat.t1)] <- 0
      
      # Binomial test, to see if more values are below 0.6 than expected
      P_smaller_than_06 <- 59 * 59/10000  # 0.3481
      
      subt <- dat.t1[41:100, 1:59]  # subset tbl 
      p06 <- sum(subt >= 1)
      pAll <- sum(dat.t1 >= 1)
      
      B <- stats::binom.test(p06, pAll, p = P_smaller_than_06, alternative = c("greater"))
      
      # P-VALUE
      v1 <- B$p.value  
      # PERCENTAGE OF difference from expected
      v2 <- (B$estimate - P_smaller_than_06)/P_smaller_than_06  
      
      # These two thresholds could be changed
      if (v1 < pvalue & v2 > diff) {
        flag.t1 <- FALSE
      } else {
        flag.t1 <- TRUE
      }
      c(round(v1, 4), round(v2, 3), flag.t1)
    })
    
    # Create output objects  
    ## Reduce output to data.frame
    out.ds <- do.call("rbind.data.frame", out)
    names(out.ds) <- c("binomial.pvalue", "perc.difference", "pass")
    
    flags <- x[[ds]] %in% rownames(out.ds[out.ds$pass == 1,])
    
    
    # return output dependent on value argument
    if(verbose){
      cat(sprintf("Flagged %s records\n", sum(!flags)))
    }
    
    switch(value,
           dataset = return(out.ds),
           clean = return(x[flags,]),
           flags = return(flags))

}
  
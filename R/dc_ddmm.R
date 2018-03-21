dc_ddmm <- function(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset",
                    pvalue = 0.025, diff = 1, mat.size = 1000, min.span = 2,
                    value = "clean", verbose = TRUE, diagnostic = FALSE){
  
  #check value argument
  match.arg(value, choices = c("clean", "flags", "dataset"))
  
  if(verbose){
    cat("Testing datasets for dd.mm to dd.dd conversion errors\n")
  }
  
  #prepare dataset for analyses
  if (sum(!complete.cases(x[, c(lon, lat, ds)])) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", sum(!complete.cases(x))))
  }
  
  #get function data
  dat <- x[complete.cases(x[, c(lon, lat, ds)]), ]
  
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
      dat.unique <- k[!duplicated(k[, c(lon, lat, ds)]),]
      
      ##Test geographic span
      lon.span <- abs(max(dat.unique[,lon], na.rm = TRUE) - min(dat.unique[,lon], na.rm = TRUE))
      lat.span <- abs(max(dat.unique[,lat], na.rm = TRUE) - min(dat.unique[,lat], na.rm = TRUE))
      
      if(lon.span >= min.span & lat.span >= min.span){
        #Assign decimals to a 100x100 matrix for binomial test
        cl <- ceiling(dat.unique[, c("lon.test", "lat.test")] * mat.size)
        cl$lat.test <- mat.size - cl$lat.test
        
        mat <- matrix(ncol = mat.size, nrow = mat.size)
        mat[cbind(cl$lat.test, cl$lon.test)] <- 1
        mat[is.na(mat)] <- 0
        dat.t1 <- mat
        
        # Binomial test, to see if more values are below 0.6 than expected
        P_smaller_than_06 <- floor(0.599 * mat.size) * floor(0.599 * mat.size) / mat.size^2 # 0.3481
        
        x.ind <- (mat.size - floor(0.599 * mat.size)):mat.size
        y.ind <- 1:floor(0.599 * mat.size)
        
        subt <- dat.t1[x.ind, y.ind]  # subset tbl 
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
        
        outp <- c(round(v1, 4), round(v2, 3), flag.t1)
        
        #diagnostic plot of the decimal matrix
        if(diagnostic){
          plo <- raster(dat.t1)
          raster::plot(plo)
        } 
      }else{
        outp <- rep(NA, 3)
        warning("Geographic spann to small, check 'min.span'")
      }
      return(outp)
    })
  
    # Create output objects  
    ## Reduce output to data.frame
    out.ds <- do.call("rbind.data.frame", out)
    rownames(out.ds) <- names(out)
    names(out.ds) <- c("binomial.pvalue", "perc.difference", "pass")
    
    flags <- x[[ds]] %in% c(rownames(out.ds[out.ds$pass == 1,]), rownames(out.ds[is.na(out.ds$pass),]))

    # return output dependent on value argument
    if(verbose){
      cat(sprintf("Flagged %s records\n", sum(!flags)))
    }
    
    switch(value,
           dataset = return(out.ds),
           clean = return(x[flags,]),
           flags = return(flags))

}
  
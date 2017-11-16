CleanCoordinatesDS <- function(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset",
                               ddmm = TRUE, periodicity = TRUE, 
                               ddmm.pvalue = 0.025, ddmm.diff = 0.2, 
                               periodicity.target = "lon_lat", 
                               periodicity.thresh.deg = 15, 
                               periodicity.thresh.dec = 3.5, 
                               periodicity.diagnostics = FALSE, 
                               periodicity.subsampling = NULL, 
                               value = "dataset", verbose = TRUE) {
  
  # check input arguments
  match.arg(value, choices = c("dataset", "flags", "clean"))
  match.arg(periodicity.target, choices = c("lat", "lon", "lon_lat"))
  
  #check column names
  nams <- c(lon, lat, ds)
  if(!all(nams %in% names(x))){
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  # prepare input data
  dat <- x[,c(lon, lat, ds)]
  dat[[ds]] <- as.character(dat[[ds]])
  
  ## kick out NAs
  if (sum(!complete.cases(dat)) > 0) {
    warning(sprintf("Data contains incomplete cases. value set to 'dataset', ignoring %s cases with incomplete data", sum(!complete.cases(dat))))
    dat <- dat[complete.cases(dat), ]
    value <- "dataset"
  }
  
  #perpare dummy  value in case value == "clean"
  if(value == "clean"){
    value2  <- "flags"
  }else{
    value2 <- value
  }

  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }
  
  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat[[lon]]) - floor(abs(dat[[lon]]))
  dat$lat.test <- abs(dat[[lat]]) - floor(abs(dat[[lat]]))
  
  ## split into seperate datasets
  test <- split(dat, f = dat[[ds]])
  
  # run ddmm to dd.dd conversion error at 0.6 test
  if (ddmm) {
    out.t1 <- dc_ddmm(x = dat, lon = lon, lat = lat, ds = ds,
                      pvalue = ddmm.pvalue, diff = ddmm.diff,
                      value = value2, verbose = verbose)
    if(value == "dataset"){
      names(out.t1) <- c("binomial.pvalue", "perc.difference", "pass.ddmm")
    }
  } else {
    if(value == "dataset"){
      out.t1 <- data.frame(pass.ddmm = rep(NA, length(test)))
    }else{
      out.t1 <- rep(NA, nrow(dat))
    }
  }
  
  # Run periodicity test
  if (periodicity) {
    out.t2 <- dc_round(x = dat, lon = lon, lat = lat, ds = ds,
                       target = periodicity.target,
                       threshold.period = periodicity.thresh.dec, 
                       threshold.degree = periodicity.thresh.deg, 
                       subsampling = periodicity.subsampling, 
                       diagnostics = periodicity.diagnostics, 
                       value = value2, verbose = verbose)

  
    }else{
      if(value == "dataset"){
        out.t2 <- data.frame(pass.periodicity.com = rep(NA, length(test)),
                             pass.zero.com = rep(NA, length(test)))
      }else{
        out.t2 <- rep(NA, nrow(dat))
      }
    }
  
  # prepare output
  if (value == "dataset") {
    out <- data.frame(out.t1, out.t2)
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  if (value == "flags") {
    out <- data.frame(ddmm = out.t1, periodicity = out.t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    out$summary <-  Reduce("&", out)

  }
  if (value == "clean") {
    out <- data.frame(ddmm = out.t1, periodicity = out.t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    out$summary <-  Reduce("&", out)
    out <- dat[out$summary,]
  }
  
  #return output
  if(verbose){
    cat(sprintf("Flagged %s records\n", sum(!out)))
  }
 return(out)
}
.CapitalCoordinates <- function(x, testdist = 0.1, buffer = 1, referencedat = NULL) {
  dat <- sp::SpatialPoints(x)
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  return(out)
}

.CentroidCoordinates <- function(x, testdist = 0.1, buffer = 1, testtype = c("both", "country", "provinces"), referencedat = NULL) {
  dat <- sp::SpatialPoints(x)

  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  return(out)
}

.CountryCheck <- function(x, countries, poly = NULL) {
  pts <- SpatialPoints(x)
  
  testpolys <- crop(poly, extent(pts))
  
  country <- sp::over(x = pts, y = testpolys)[, "ISO2"]
  out <- as.character(country) == as.character(countries)
  out[is.na(out)] <- TRUE
  
  return(out)
}

.Institutions <- function(x, testdist = 0.001, buffer = 1, referencedat = NULL){
  dat <- sp::SpatialPoints(x)

  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("decimallongitude", "decimallatitude")]), limits)
  
  if(is.null(referencedat)){ # incase no bdinstitutions
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
}

.OutlierCoordinates <- function(x, species, mltpl, tdi, method = "Haversine") {
  
  splist <- split(x, f = as.character(species))
  
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 2]
  
  flags <- lapply(splist, function(k, td = tdi, mu = mltpl) {
    
    test <- nrow(k[!duplicated(k), ])
    dist <- geosphere::distm(k, fun = distHaversine)
    dist[dist == 0] <- NA
    
    if (!is.null(mu) & !is.null(td)) {
      stop("set outliers.td OR outliers.mtp, the other one to NULL")
    }
    
    if (!is.null(mu)) {
      mins <- apply(dist, 1, min, na.rm = T)
      quo <- quantile(mins, c(0.99), na.rm = T)
      out <- which(mins > (quo + mean(mins, na.rm = T) * mu))
    }
    
    if (!is.null(td)) {
      mins <- apply(dist, 1, min, na.rm = T)
      out <- which(mins > td * 1000)
    }
    
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[out]
    }
    return(ret)
  })
  
  
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  return(out)
}

.UrbanCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)
  limits <- extent(pts) + 1

  poly <- crop(poly, limits)
  
  urban <- over(x = pts, y = poly)[, 1]
  out <- is.na(urban)
  
  return(out)
}

.GBIF <- function(x) {
  pts <- sp::SpatialPoints(x)
  poly <- rgeos::gBuffer(SpatialPoints(cbind(12.58, 55.67)), width = 0.5)
  warning("running GBIF test, flagging records around Copenhagen")
  
  out <- sp::over(x = pts, y = poly)
  out <- is.na(out)
  
  return(out)
}

.ValidCoordinates <- function(x) {
  out <- list(is.na(x$decimallongitude), is.na(x$decimallatitude), suppressWarnings(is.na(as.numeric(as.character(x$decimallongitude)))), suppressWarnings(is.na(as.numeric(as.character(x$decimallatitude)))), 
              suppressWarnings(as.numeric(as.character(x$decimallongitude))) < -180, suppressWarnings(as.numeric(as.character(x$decimallongitude))) > 180, suppressWarnings(as.numeric(as.character(x$decimallatitude))) < 
                -90, suppressWarnings(as.numeric(as.character(x$decimallatitude))) > 90)
  
  out <- !Reduce("|", out)
  return(out)
}

.WaterCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)

    testpolys <- poly
    testpolys <- crop(testpolys, extent(pts) + 1)
  land <- over(x = pts, y = testpolys)[, 1]
  out <- !is.na(land)
  
  return(out)
}

.ZeroCoordinates <- function(x, pointlim = 0.5) {
  pts <- SpatialPoints(x)
  out <- rep(T, nrow(x))
  
  # plain zero in coordinates
  out[which(x$decimallongitude == 0 | x$decimallatitude == 0)] <- FALSE
  
  # radius around point 0/0
  test <- rgeos::gBuffer(sp::SpatialPoints(cbind(0, 0)), width = pointlim)
  out[which(!is.na(over(y = test, x = pts)))] <- FALSE
  
  # lat == long
  out[which(x$decimallongitude == x$decimallatitude)] <- FALSE
  
  return(out)
} 

plotter <- function(x, bgmap = NULL, clean = T, details = T, ...){
  
  #prepare background
  e <- raster::extent(SpatialPoints(x[, 1:2])) + 1
  
  if (is.null(bgmap)) {
    load("landmass.rda")
    bgmap <- landmass
    bgmap <- raster::crop(bgmap, e)
  }
  
  bgmap <- ggplot2::fortify(bgmap)
  
  #plot background
  plo <- ggplot2::ggplot()+
    geom_polygon(data = bgmap, aes(x = long, y = lat, group = group), fill = "grey80")+
    coord_fixed()+
    theme_bw()+
    scale_x_continuous( expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))
  
  #prepare occurence points
  inv <- x
  inv[,-c(1:2)] <- !inv[,-c(1:2)]
  occs <- names(inv[,-c(1:2)])[unlist(lapply(apply(inv[, -c(1:2)] == 1, 1, "which"), "[", 1))]
  
  if(length(occs) == 0){
    occs <- rep("AAAclean", nrow(x))
  }else{
    occs[is.na(occs)] <- "AAAclean"
  }
  
  occs <- cbind(x[,c("decimallongitude", "decimallatitude", "summary")], flag = occs)
  if(!"AAAclean" %in% occs$flag){
    clean <- FALSE
    warnings("All records were flagged, setting clean to FALSE")
  }
  
  #add points to background
  if(!clean & !details){
    pts <- occs[!occs$summary,]
    plo <- plo+
      ggplot2::geom_point(data = pts, aes(x = decimallongitude, y = decimallatitude), colour = "#F8766D") 
  }
  
  if(clean & !details){
    pts <- occs
    plo <- plo+
      ggplot2::geom_point(data = pts, aes(x = decimallongitude, y = decimallatitude, colour = summary))+
      ggplot2::scale_colour_manual(values = c("#F8766D", "#00BFC4"), labels = c("Flagged", "Clean"))+
      ggplot2::theme(legend.title=element_blank())
  }
  
  if(!clean & details){
    pts <- occs[!occs$summary,]
    plo <- plo+
      ggplot2::geom_point(data = pts, aes(x = decimallongitude, y = decimallatitude, shape = flag), colour = "#F8766D")+
      ggplot2::theme(legend.title=element_blank())
  }
  
  if(clean & details){
    pts <- occs
    plo <- plo+
      ggplot2::geom_point(data = pts, aes(x = decimallongitude, y = decimallatitude, shape = flag, colour = flag))+
      ggplot2::scale_colour_manual(values = c("#00BFC4", rep("#F8766D", length(unique(pts$flag)))),
                                   breaks = as.character(unique(pts$flag)),
                                   labels = c("clean", as.character(unique(pts$flag))[-1]))+
      ggplot2::scale_shape_manual(values = c(16, seq(15, 15+(length(unique(pts$flag))-1))),
                                  breaks = as.character(unique(pts$flag)),
                                  labels = c("clean", as.character(unique(pts$flag))[-1]))+
      ggplot2::theme(legend.title=element_blank())
  }
  plo
} 


rasPlotter <- function(x, y){
  e <- raster::extent(SpatialPoints(y[, 1:2])) + 1
  
  load("landmass.rda")
  bgmap <- landmass
  bgmap <- raster::crop(bgmap, e)
  bgmap <- ggplot2::fortify(bgmap)
  
    ggplot2::ggplot()+
    geom_polygon(data = bgmap, aes(x = long, y = lat, group = group), fill = "grey80")+
    geom_raster(data = x, aes(x = x, y = y, fill = layer))+
    scale_fill_viridis(option = "inferno", na.value = "transparent", name = "Flagged\nRecords", direction = 1)+
    coord_fixed()+
    theme_bw()+
    scale_x_continuous(expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))+
    theme(legend.position="right",
          legend.key.height = unit(0.7, "in"))
}


#dataset level cleaning
# Test 2 - Periodicity and zero inflation
.AnalyzeBias <- function(var, nam, var_latlong = 1, rounding_digits = 5, plot_bias = T, 
                         ratio_threshold = 2.5) {
  x <- unlist(var[, var_latlong])
  x <- round(x, rounding_digits)
  mle <- .RunGreedyOptim(x)  # run greedy ML optimization (rates, time frames)
  res <- .PoiRateMLE(x, mle$max_par, F)  # get ML rates
  
  rate12_ratio <- max(res$lambdas[2:3]) / min(res$lambdas[2:3])
  rate0_ratio <- res$lambdas[1] / max(unique(res$lambdas[2:3]))
  
  mlp <- log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])
  
  # condition for periodic bias
  PB <- min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])) > log(7) & rate12_ratio > 
    ratio_threshold
  
  # condition for 0-rounding bias
  PCP <- max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]) < 0.1 & rate0_ratio > ratio_threshold
  
  out <- data.frame(min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])), rate12_ratio, 
                    !PB, max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]), rate0_ratio, !PCP)
  
  if (plot_bias) {
    par(mfrow = c(2, 2))
    hist(x, nclass = 50)
    .PeriodicityRoundingLik(mle$max_par, x, T)
    
    plot(mle$lik_vec ~ log(mle$rate_changes), col = rgb(0,0,0, 0.1), pch = 16, 
         xlab = "Number of presumable sampling sites (log)", ylab = "log likelihood", 
         main = "Periodicity in the data (if any)")
    points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
             log(mle$rate_changes[mle$lik_vec >=(mle$max_lik - 2)]), col = "red", pch = 19)
    # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
    abline(v = log(c(5, 7, 11)))
    
    plot(mle$lik_vec ~ mle$s0_size, col = rgb(0,0,0, 0.02), pch = 16, xlab = "Size of s0", 
         ylab = "log likelihood", main = "Rounding bias (if any)")
    points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
             mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)], col = "red", pch = 19)
    # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
    abline(v = c(0.015))
    
    if (!PCP & !PB) {
      title(gsub("input/", "", nam), outer = TRUE, line = -1)
    }
    if (PB & PCP) {
      title(paste("Period + Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
            line = -1)
    } else {
      if (PB) {
        title(paste("Period", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
              line = -1)
      }
      if (PCP) {
        title(paste("Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
              line = -1)
      }
    }
    par(mfrow = c(1, 1))
  }
  return(out)
}

.PoissonProcessLik <- function(lambda = 1, x = data, S = 1) {
  # log PDF of Poisson distribution
  lambda <- lambda + 1e-10
  lik <- sum(x * log(lambda) - lambda * S)
  return(lik)
}

.PoiRateMLE <- function(xd, s, plt = F) {
  s0 <- s[1]
  s1 <- s[2]
  s2 <- s[3]
  S <- s1 + s2
  nS <- 1/S + 1
  bins <- cumsum(c(s0, rep(c(s1, s2), round(nS))))
  bins <- c(0, bins[bins < 1], 1)
  
  if (plt == T) {
    h <- hist(xd, breaks = bins, col = "gray")
  } else {
    h <- hist(xd, breaks = bins, plot = plt)
  }
  
  count <- h$counts
  
  l_indx <- rep(c(2, 3), nS)
  l_indx <- c(1, l_indx)
  l_indx <- l_indx[1:length(count)]
  
  dT <- diff(h$breaks) * 1000
  # rate MLE
  l0 <- mean(count[l_indx == 1]/dT[1])
  l1 <- mean(count[l_indx == 2]/dT[2])
  l2 <- mean(count[l_indx == 3]/dT[3])
  l_all <- c(l0, l1, l2)
  
  lambdas <- l_all[l_indx]
  mle <- NULL
  mle$lambdas <- lambdas
  mle$dT <- dT
  mle$count <- count
  return(mle)
}

.PeriodicityRoundingLik <- function(args, x, plt = F) {
  s0 <- abs(args[1])
  s1 <- abs(args[2])
  s2 <- abs(args[3])
  
  if ((s0 + s1 + s2) > 1) {
    return(-1e+05)
  }
  
  mle <- .PoiRateMLE(x, c(s0, s1, s2), plt)
  lambdas <- mle$lambdas
  count <- mle$count
  dT <- mle$dT
  lik <- .PoissonProcessLik(lambdas, count, dT)
  # print(c(l_all, -lik))
  return(lik)
}

.RunGreedyOptim <- function(xd) {
  # Greedy optimization possible_set_0 = (1:9)*10^(-2)
  possible_set <- (1:33) * 10^(-2)
  max_lik <- -1e+05
  max_par <- NULL
  lik_vec <- c()
  rate_changes <- c()
  s0_size <- c()
  for (s0 in possible_set) {
    print(c(s0, max_lik))
    for (s1 in possible_set) {
      for (s2 in possible_set) {
        args <- c(s0, s1, s2)
        lik <- .PeriodicityRoundingLik(args, x = xd)
        lik_vec <- c(lik_vec, lik)
        # rate_changes is the number of bins that are being used
        rate_changes <- c(rate_changes, (1 + (1 - args[1])/(args[2] + args[3]) * 
                                           2))
        s0_size <- c(s0_size, s0)
        if (lik > max_lik) {
          max_lik <- lik
          max_par <- args
        }
      }
    }
  }
  res <- NULL
  res$max_lik <- max_lik
  res$max_par <- max_par
  res$lik_vec <- lik_vec
  res$rate_changes <- rate_changes
  print(res$max_lik)
  print(res$max_par)
  res$s0_size <- s0_size
  return(res)
}


CleanCoordinatesDS <- function(x, ddmm = TRUE, periodicity = TRUE, output = "flags", 
                               ddmm.pvalue = 0.025, ddmm.diff = 0.2, 
                               periodicity.target = "both", periodicity.thresh = 2.5, 
                               periodicity.diagnostics = FALSE, 
                               subsampling = FALSE) {
  
  # check input arguments
  match.arg(output, choices = c("detail", "flags", "minimum"))
  match.arg(periodicity.target, choices = c("lat", "lon", "both"))
  
  # prepare input data
  if (!is.data.frame(x)) {
    stop("only defined for class 'data.frame'")
  }
  if (ncol(x) > 3) {
    if ("decimallongitude" %in% names(x) & "decimallatitude" %in% names(x) & "dataset" %in% 
        names(x)) {
      dat <- x[, c("decimallongitude", "decimallatitude", "dataset")]
    } else {
      stop("inputcolumns not found. Check input format")
    }
  } else {
    if (ncol(x) == 3) {
      if ("decimallongitude" %in% names(x) & "decimallatitude" %in% names(x) & "dataset" %in% 
          names(x)) {
        dat <- x[, c("decimallongitude", "decimallatitude", "dataset")]
      } else {
        dat <- x
      }
    }
    if (ncol(x) == 2) {
      dat <- x
      dat$dataset <- "dataset"
      warning("column 'dataset' not found, assuming lon, lat input from single dataset")
    }
  }
  
  names(dat) <- c("decimallongitude", "decimallatitude", "dataset")
  
  dat$dataset <- as.character(dat$dataset)
  
  ## kick out NAs
  if (sum(!complete.cases(dat)) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", sum(!complete.cases(dat))))
  }
  dat <- dat[complete.cases(dat), ]
  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }
  
  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat$decimallongitude) - floor(abs(dat$decimallongitude))
  dat$lat.test <- abs(dat$decimallatitude) - floor(abs(dat$decimallatitude))
  
  # split into seperate datasets
  test <- split(dat, f = dat$dataset)
  
  # run Test 1 per dataset - ddmm to dd.dd conversion error at 0.6
  if (ddmm) {
    out.t1 <- lapply(test, function(k) {
      ## create test datasets
      dat.unique <- k[!duplicated(k[, 1:2]), ]
      
      # create input data with a raster
      r <- raster::raster(xmn = 0, xmx = 1, ymn = 0, ymx = 1)
      raster::res(r) <- 0.01
      
      dat.t1 <- raster::rasterize(SpatialPoints(dat.unique[, c("lon.test", "lat.test")]), 
                                  r, fun = "count")
      dat.t1 <- raster::as.matrix(dat.t1)
      dat.t1[is.na(dat.t1)] <- 0
      
      # Binomial test, to see if more values are below 0.6 than expected
      P_smaller_than_06 <- 59 * 59/10000  # 0.3481
      
      subt <- dat.t1[1:59, 1:59]  # subset tbl 
      p06 <- sum(subt >= 1)
      pAll <- sum(dat.t1 >= 1)
      
      B <- stats::binom.test(p06, pAll, p = P_smaller_than_06, alternative = c("greater"))
      
      # P-VALUE
      v1 <- B$p.value  
      # PERCENTAGE OF difference from expected
      v2 <- (B$estimate - P_smaller_than_06)/P_smaller_than_06  
      
      # These to thresholds could be changed
      if (v1 < ddmm.pvalue & v2 > ddmm.diff) {
        flag.t1 <- FALSE
      } else {
        flag.t1 <- TRUE
      }
      
      c(round(v1, 4), round(v2, 3), flag.t1)
    })
    
    # Reduce output to data.frame
    
    out.t1 <- do.call("rbind.data.frame", out.t1)
    names(out.t1) <- c("binomial.pvalue", "perc.difference", "pass.ddmm")
    out.t1$pass.ddmm <- as.logical(out.t1$pass.ddmm)
  } else {
    out.t1 <- data.frame(pass.ddmm = rep(NA, length(test)))
  }
  
  # Run Test 2 and 3 per dataset
  if (periodicity) {
    out.t2 <- lapply(test, function(k) {
      t2 <- k[, c("lon.test", "lat.test")]
      if (nrow(t2) > 1000 & subsampling) {
        warning("large dataset. Using subsampling for periodicity")
        t2 <- t2[sample(nrow(t2), 1000), ]
      }
      if (periodicity.target == "lon") {
        t2.res <- .AnalyzeBias(var = t2, nam = k$dataset[1], var_latlong = 1, 
                               plot_bias = periodicity.diagnostics, 
                               ratio_threshold = periodicity.thresh)
        names(t2.res) <- c("mle", "rate.ratio", "pass.lon", "zero.mle", "zero.rate.ratio", 
                           "pass.zero.lon")
        
      }
      if (periodicity.target == "lat") {
        t2.res <- .AnalyzeBias(var = t2, nam = k$dataset[1], var_latlong = 2, 
                               plot_bias = periodicity.diagnostics, 
                               ratio_threshold = periodicity.thresh)
        names(t2.res) <- c("mle", "rate.ratio", "pass.lat", "zero.mle", "zero.rate.ratio", 
                           "pass.zero.lat")
      }
      
      if (periodicity.target == "both") {
        lon <- .AnalyzeBias(t2, nam = k$dataset[1], var_latlong = 1, 
                            plot_bias = periodicity.diagnostics, 
                            ratio_threshold = periodicity.thresh)
        lat <- .AnalyzeBias(t2, nam = k$dataset[1], var_latlong = 2, 
                            plot_bias = periodicity.diagnostics, 
                            ratio_threshold = periodicity.thresh)
        
        t2.res <- cbind(lon, lat)
        
        names(t2.res) <- c("mle.lon", "rate.ratio.lon", "pass.periodicity.lon", 
                           "zero.mle.lon", "zero.rate.ratio.lon", "pass.zero.lon", 
                           "mle.lat", "rate.ratio.lat", 
                           "pass.periodicity.lat", "zero.mle.lat", 
                           "zero.rate.ratio.lat", "pass.zero.lat")
        t2.res$pass.zero.com <- t2.res$pass.zero.lon & t2.res$pass.zero.lat
        t2.res$pass.periodicity.com <- t2.res$pass.periodicity.lon & t2.res$pass.periodicity.lat
      }
      return(t2.res)
    })
    out.t2 <- do.call("rbind.data.frame", out.t2)
    
  } else {
    filler <- rep(NA, length(test))
    out.t2 <- data.frame(pass.zero.lon = filler, 
                         pass.zero.lat = filler, 
                         pass.zero.com = filler, 
                         pass.periodicity.lon = filler, 
                         pass.periodicity.lat = filler, 
                         pass.periodicity.com = filler)
  }
  # prepare output
  if (output == "detail") {
    out <- data.frame(out.t1, out.t2)
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  if (output == "flags") {
    out <- data.frame(pass.ddmm = out.t1$pass.ddmm, 
                      pass.zero.lon = out.t2$pass.zero.lon, 
                      pass.zero.lat = out.t2$pass.zero.lat, 
                      pass.zero.com = out.t2$pass.zero.com, 
                      pass.periodicity.lon = out.t2$pass.periodicity.lon, 
                      pass.periodicity.lat = out.t2$pass.periodicity.lat, 
                      pass.periodicity.com = out.t2$pass.periodicity.com, 
                      row.names = names(test))
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  if (output == "minimum") {
    out <- data.frame(pass.ddmm = out.t1$pass.ddmm, 
                      pass.zero.com = out.t2$pass.zero.com, 
                      pass.periodicity.com = out.t2$pass.periodicity.com, 
                      row.names = names(test))
    out$summary <- out$pass.ddmm & out$pass.zero.com & out$pass.periodicity.com
    out <- Filter(function(x) !all(is.na(x)), out)
  }
  return(out)
}




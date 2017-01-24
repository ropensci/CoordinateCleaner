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
  if (is.null(referencedat)) {
    referencedat <- speciesgeocodeR::institutions
  }
  
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
    theme(legend.position="bottom",
          legend.key.width = unit(0.7, "in"))
}
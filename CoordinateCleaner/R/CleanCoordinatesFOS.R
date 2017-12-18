#A function to clean fossil data
CleanCoordinatesFOS <- function(x, lon = "lng", lat = "lat",
                                min.age = "min_ma", max.age = "max_ma",
                                species = "accepted_name", countries = "cc",
                                centroids = T, countrycheck = T, 
                                equal = T,  GBIF = T, 
                                institutions = T, 
                                temp.range.outliers = T, spatio.temp.outliers = T, temp.ages.equal = T, 
                                zeros = T, 
                                centroids.rad = 0.01,
                                centroids.detail = "both", inst.rad = 0.001, 
                                outliers.method = "quantile", outliers.mtp = 3, outliers.td = 1000, 
                                outliers.threshold = 7, 
                                zeros.rad = 0.5, centroids.ref, country.ref,
                                inst.ref, value = "spatialvalid", verbose = T, report = F){
  
  
  #check function arguments
  match.arg(value, choices = c("spatialvalid", "flags", "clean"))
  match.arg(centroids.detail, choices = c("both", "country", "provinces"))
  match.arg(outliers.method, choices = c("distance", "quantile", "mad"))
  
  #check column names
  nams <- c(lon, lat, species, min.age, max.age)
  if(!all(nams %in% names(x))){
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }
  
  if(is.null(countries) | !countries %in% names(x)){
    countries <- NULL
    if (countrycheck) {
      countrycheck <- FALSE
      warning("countries missing, countrycheck set to FALSE")
    }
  }
  if(is.null(species)){
    if (spatio.temp.outliers) {
      outliers <- FALSE
      warning("is.null(species), outliers test skipped")
    }
    species <- NULL
  }
  if(missing(centroids.ref)){
    centroids.ref <- NULL
  }
  if(missing(country.ref)){
    country.ref <- NULL
  }
  if(missing(inst.ref)){
    inst.ref <- NULL
  }

  # Run tests Validity, check if coordinates fit to lat/long system, this has to be run all the time, as otherwise the other tests don't work
  val <- cc_val(x, lon = lon, lat = lat,
                verbose = verbose, value = "flags")
  
  if (!all(val)) {
    stop("invalid coordinates found in rows, clean dataset before proceeding:\n", 
         paste(which(!val), "\n"))
  }
  
  ##Equal coordinates
  if (equal) {
    equ <- cc_equ(x, lon = lon, lat = lat,
                  verbose = verbose, value = "flags", test = "absolute")
    
  } else {
    equ <- rep(NA, dim(x)[1])
  }
  
  ## Zero coordinates
  if (zeros) {
    zer <- cc_zero(x, lon = lon, lat = lat,
                   buffer = zeros.rad, verbose = verbose, value = "flags")
    
  } else {
    zer <- rep(NA, dim(x)[1])
  }
  
  ## Centroids
  if (centroids) {
    cen <- cc_cen(x, lon = lon, lat = lat, 
                  buffer = centroids.rad, test = centroids.detail, ref = centroids.ref,
                  value = "flags", verbose = verbose)
  } else {
    cen <- rep(NA, nrow(x))
  }

  # Country check
  if (countrycheck) {
    con <- cc_coun(x, lon = lon, lat = lat, 
                   iso3 = countries, ref = country.ref, 
                   verbose = verbose, value = "flags")
  } else {
    con <- rep(NA, dim(x)[1])
  }
  
  # Spatiotemporal
  if (spatio.temp.outliers) {
    
    if(nrow(x) < 25000){
      otl.flag <- tc_outl(x, lon = lon, lat = lat,  min.age = min.age, max.age = max.age,
                          taxon = "",
                          method = outliers.method, mltpl = outliers.mtp, 
                          value = "ids", verbose = verbose)
      
      otl <- rep(TRUE, nrow(x))
      otl[otl.flag] <- FALSE
    }else{
      warning("Very large dataset skipped dataset level outlier test")
      otl <- rep(TRUE, nrow(x))
    }

    if(taxon != ""){
      otl.test <- table(x[species])
      otl.test <- otl.test[otl.test > outliers.threshold]
      otl.test <- x[x[[species]] %in% names(otl.test),]
      otl.test <- otl.test[, c(species, lon, lat, min.age, max.age)]
      
      otl.flag <- tc_outl(x = otl.test, lon = lon, lat = lat,  min.age = min.age, max.age = max.age,
                          taxon = species,
                          method = outliers.method, mltpl = outliers.mtp, 
                          value = "ids", verbose = verbose)
      
      otl[otl.flag] <- FALSE

    }

  } else {
    otl <- rep(NA, nrow(x))
  }
  
  #Temporal, range size outliers
  if (temp.range.outliers) {
    
    #over entire dataser
    ran.otl.flag <- tc_range(x, taxon = "",  min.age = min.age, max.age = max.age,
                             method = outliers.method, mltpl = outliers.mtp,
                             value = "ids", verbose = verbose)
    
    
    if(taxon != ""){
      #per taxon
      ran.test <- table(x[species])
      ran.test <- ran.test[ran.test > outliers.threshold]
      ran.test <- x[x[[species]] %in% names(ran.test),]
      ran.test <- ran.test[, c(species, min.age, max.age)]
      
      ran.otl  <- rep(TRUE, nrow(x))
      ran.otl[ran.otl.flag] <- FALSE
      
      ran.otl.flag <- tc_range(otl.test, taxon = species,  min.age = min.age, max.age = max.age,
                               method = outliers.method, mltpl = outliers.mtp,
                               value = "ids", verbose = verbose)
      
      ran.otl[ran.otl.flag] <- FALSE
    }
  } else {
    ran.otl <- rep(NA, nrow(x))
  }

  # Temporal, Equal ages
  if (temp.ages.equal) {
    age.equ <- tc_equal(x, min.age = min.age, max.age = max.age,
                       value = "flags", verbose = verbose)
  } else {
    age.equ <- rep(NA, dim(x)[1])
  }
  
  # GBIF headquarters
  if (GBIF) {
    gbf <- cc_gbif(x, lon = lon, lat = lat,
                   verbose = verbose, value = "flags")
  } else {
    gbf <- rep(NA, dim(x)[1])
  }
  
  #Biodiversity institution
  if (institutions) {
    inst <- cc_inst(x, lon = lon, lat = lat,
                    ref = inst.ref, buffer = inst.rad,
                    verbose = verbose, value = "flags")
  } else {
    inst <- rep(NA, dim(x)[1])
  }

  #prepare output data
  out <- list(val, zer, equ, cen, con, gbf, inst, otl, ran.otl, age.equ)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- as.vector(Reduce("&", out))
  
  if (verbose) {
    if (!is.null(out)) {
      cat(sprintf("Flagged %s of %s records, EQ = %s \n", sum(!out, na.rm = T), 
                  length(out), round(sum(!out, na.rm = T)/length(out), 2)))
    } else {
      cat("flagged 0 records, EQ = 0 \n")
    }
  }
  if (value == "spatialvalid") {
    inp <- data.frame(species = x[, species], decimallongitude = x[, lon], decimallatitude = x[, lat])
    out <- data.frame(inp, geo.validity = val, geo.equal = equ, geo.zeros = zer, geo.centroids = cen, 
                      geo.countrycheck = con,
                      geo.gbif = gbf, geo.institution = inst, 
                      spatio.tmp.outl = otl, tmp.range.outl = ran.otl, equal.min.max.age = age.equ, 
                      summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
    if (report) {
      report <- "CleanCoordinatesFOS_report.txt"
    }
    if (is.character(report)) {
      suma <- data.frame(Test = as.character(names(out[-(1:3)])), 
                         Flagged.records = colSums(!out[-(1:3)]), stringsAsFactors = F)
      suma <- rbind(suma, c("Total number of records", length(out$summary)))
      suma <- rbind(suma, c("Error Quotient", 
                            round(sum(!out$summary, na.rm = T)/length(out$summary), 2)))
      
      write.table(suma, report, sep = "\t", row.names = F, quote = F)
    }
    
  }
  if (value == "clean") {
    out <- x[out, ]
    if(report | is.character(report)){
      warning("report only valid with value = 'spatialvalid'")
    }
  }
  if(value == "flags"){
    if(report | is.character(report)){
      warning("report only valid with value = 'spatialvalid'")
    }
  } 

  return(out)
}

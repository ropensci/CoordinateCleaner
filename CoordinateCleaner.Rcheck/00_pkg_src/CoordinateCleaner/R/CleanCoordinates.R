# include warning message in case the standard landmass is used

CleanCoordinates <- function(x, lon = "decimallongitude", lat = "decimallatitude",
                             species = "species", countries = NULL,
                             capitals = T, centroids = T, countrycheck = F, 
                             duplicates = F, equal = T,
                             GBIF = T, institutions = T, outliers = F, seas = T, 
                             urban = F, zeros = T, 
                             capitals.rad = 0.05, centroids.rad = 0.01, 
                             centroids.detail = "both", inst.rad = 0.001, 
                             outliers.method = "quantile", outliers.mtp = 3, outliers.td = 1000, 
                             zeros.rad = 0.5, capitals.ref, centroids.ref, country.ref,
                             inst.ref, seas.ref, urban.ref,
                             value = "spatialvalid", verbose = T,
                             report = F){
  #check function arguments
  match.arg(value, choices = c("spatialvalid", "flags", "clean"))
  match.arg(centroids.detail, choices = c("both", "country", "provinces"))
  match.arg(outliers.method, choices = c("distance", "quantile", "mad"))
  
  #check column names
  nams <- c(lon, lat, species, countries)
  if(!all(nams %in% names(x))){
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }
  
  if(is.null(countries)){
    countries <- NULL
    if (countrycheck) {
      countrycheck <- FALSE
      warning("countries missing, countrycheck set to FALSE")
    }
  }
  if(is.null(species)){
    if (outliers) {
      outliers <- FALSE
      warning("is.null(species), outliers test skipped")
    }
    species <- NULL
  }
  if(missing(capitals.ref)){
    capitals.ref <- NULL
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
  if(missing(seas.ref)){
    seas.ref <- NULL
  }
  if(missing(urban.ref)){
    urban.ref <- NULL
  }

    # Run tests Validity, check if coordinates fit to lat/long system, this has to be run all the time, as otherwise the other tests don't work
    val <- cc_val(x, lon = lon, lat = lat,
                  verbose = verbose, value = "flags")
    
    if (!all(val)) {
      stop("invalid coordinates found in rows, clean dataset before proceeding:\n", 
           paste(which(!val), "\n"))
    }
    
    ##Equal coordinates
    if (zeros) {
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
    
    ## Capitals
    if (capitals) {
      cap <- cc_cap(x, lon = lon, lat = lat, 
                    buffer = capitals.rad, ref = capitals.ref, value = "flags",
                    verbose = verbose)

    } else {
      cap <- rep(NA, dim(x)[1])
    }
    
    ## Centroids
    if (centroids) {
      cen <- cc_cen(x, lon = lon, lat = lat, 
                    buffer = centroids.rad, test = centroids.detail, ref = centroids.ref,
                    value = "flags", verbose = verbose)
    } else {
      cen <- rep(NA, nrow(x))
    }
    
    # Seas
    if (seas) {
      sea <- cc_sea(x, lon = lon, lat = lat, 
                    ref = seas.ref, verbose = verbose, value = "flags")

    } else {
      sea <- rep(NA, dim(x)[1])
    }
    
    # Urban Coordinates
    if (urban) {
      urb <- cc_urb(x, lon = lon, lat = lat, 
                    ref = urban.ref, verbose = verbose, value = "flags")
    } else {
      urb <- rep(NA, dim(x)[1])
    }
    
    # Country check
    if (countrycheck) {
      con <- cc_coun(x, lon = lon, lat = lat, 
                     iso3 = countries, ref = country.ref, 
                     verbose = verbose, value = "flags")
    } else {
      con <- rep(NA, dim(x)[1])
    }
    
    # Outliers
    if (outliers) {
      otl <- cc_outl(x, lon = lon, lat = lat, species = species,
                     method = outliers.method, mltpl = outliers.mtp, tdi = outliers.td, 
                     value = "flags", verbose = verbose)
    } else {
      otl <- rep(NA, dim(x)[1])
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
    
    # exclude duplicates
    if (duplicates) {
      cat("running duplicates test\n")
      if (is.null(species)) {
        dpl.test <- x
        warning("running duplicates test without species id, assuming single species dataset")
      } else {
        dpl.test <- data.frame(x, species)
      }
      dpl <- !duplicated(dpl.test)
      if (verbose) {
        cat(sprintf("flagged %s records \n", sum(!dpl)))
      }
    } else {
      dpl <- rep(NA, dim(x)[1])
    }
  
  #prepare output data
  
  out <- list(val, zer, equ, cap, cen, sea, urb, con, otl, gbf, inst, dpl)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- as.vector(Reduce("&", out))
  
  if (verbose) {
    if (!is.null(out)) {
      cat(sprintf("flagged %s of %s records, EQ = %s \n", sum(!out, na.rm = T), 
                  length(out), round(sum(!out, na.rm = T)/length(out), 2)))
    } else {
      cat("flagged 0 records, EQ = 0 \n")
    }
  }
  if (value == "spatialvalid") {
    out <- data.frame(x, validity = val, equal = equ, zeros = zer, capitals = cap, centroids = cen, 
                      sea = sea, urban = urb, countrycheck = con, outliers = otl, 
                      gbif = gbf, institution = inst, duplicates = dpl, summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
    if (report) {
      report <- "CleanCoordinates_report.txt"
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
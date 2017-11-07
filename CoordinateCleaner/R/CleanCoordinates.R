# include warning message in case the standard landmass is used

CleanCoordinates <- function(x, countries, species,
                             output = "spatialvalid", report = F, capitals = T, 
                             centroids = T, countrycheck = F, duplicates = F, equal = T,
                             GBIF = T, institutions = T, outliers = F, seas = T, 
                             urban = F, validity = T, zeros = T, verbose = T,
                             capitals.rad = 0.05, centroids.rad = 0.01, 
                             centroids.detail = "both", inst.rad = 0.001, 
                             outliers.method = "quantile", outliers.mtp = 3, outliers.td = 1000, 
                             zeros.rad = 0.5, capitals.ref, centroids.ref, country.ref,
                             inst.ref, seas.ref, urban.ref) {
  #check function arguments
  match.arg(output, choices = c("spatialvalid", "summary", "cleaned"))
  match.arg(centroids.detail, choices = c("both", "country", "provinces"))
  match.arg(outliers.method, choices = c("distance", "quantile", "mad"))
  
  
  if(missing(countries) | is.null(countries)){
    countries <- NULL
    if (countrycheck) {
      countrycheck <- FALSE
      warning("countries missing, countrycheck set to FALSE")
    }
  }
  if(missing(species) | is.null(species)){
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

  #Check function input
  if (is.matrix(x) | is.data.frame(x)) {
    if (ncol(x) != 2) {
      x <- as.data.frame(x)
      if("decimallongitude" %in% names(x) & "decimallatitude" %in% names(x)) {
        x <- x[, c("decimallongitude", "decimallatitude")]
        warning("more than two columns, input guessed from column names")
      }
      if("decimalLongitude" %in% names(x) & "decimalLatitude" %in% names(x)) {
        x <- x[, c("decimalLongitude", "decimalLatitude")]
        warning("more than two columns, input guessed from column names")
        }
      if("longitude" %in% names(x) & "latitude" %in% names(x)) {
        x <- x[, c("longitude", "latitude")]
          warning("more than two columns, coordinates guessed from column names")
      }
    }

    names(x) <- c("decimallongitude", "decimallatitude")
    
    # Run tests Validity, check if coordinates fit to lat/long system, this has to be run all the time, as otherwise the other tests don't work
    val <- cc_val(x, lon = "decimallongitude", lat = "decimallatitude",
                  verbose = T, value = "flags")
    
    if (!all(val)) {
      stop("invalid coordinates found in rows, clean dataset before proceeding:\n", 
           paste(which(!val), "\n"))
    }
    
    ##Equal coordinates
    if (zeros) {
      equ <- cc_equ(x, lon = "decimallongitude", lat = "decimallatitude",
                    verbose = T, value = "flags", test = "absolute")

    } else {
      equ <- rep(NA, dim(x)[1])
    }
    
    ## Zero coordinates
    if (zeros) {
      zer <- cc_zer(x, lon = "decimallongitude", lat = "decimallatitude",
                    buffer = 0.5, verbose = T, value = "flags")

    } else {
      zer <- rep(NA, dim(x)[1])
    }
    
    ## Capitals
    if (capitals) {
      cap <- cc_cap(x, lon = "decimallongitude", lat = "decimallatitude", 
                    buffer = capitals.rad, ref = capitals.ref, value = "flags",
                    verbose = T)

    } else {
      cap <- rep(NA, dim(x)[1])
    }
    
    ## Centroids
    if (centroids) {
      cen <- cc_cen(x, lon = "decimallongitude", lat = "decimallatitude", 
                    buffer = 0.5, test = centroids.detail, ref = centroids.ref,
                    value = "flags", verbose = T)
    } else {
      cen <- rep(NA, nrow(x))
    }
    
    # Seas
    if (seas) {
      sea <- cc_sea(x, lon = "decimallongitude", lat = "decimallatitude", 
                    ref = seas.ref, verbose = T, value = "flags")

    } else {
      sea <- rep(NA, dim(x)[1])
    }
    
    # Urban Coordinates
    if (urban) {
      urb <- cc_urb(x, lon = "decimallongitude", lat = "decimallatitude", 
                    ref = urban.ref, verbose = T, value = "flags")
    } else {
      urb <- rep(NA, dim(x)[1])
    }
    
    # Country check
    if (countrycheck) {
      con <- cc_coun(x, lon = "decimallongitude", lat = "decimallatitude", 
                     iso3 = countries, ref = country.ref, 
                     verbose = T, value = "flags")
    } else {
      con <- rep(NA, dim(x)[1])
    }
    
    # Outliers
    if (outliers) {
      otl <- cc_outl(x, lon = "decimallongitude", lat = "decimallatitude", species = "species",
                     method = outliers.method, mltpl = outliers.mtp, tdi = outliers.td, 
                     value = "flags", verbose = T)
    } else {
      otl <- rep(NA, dim(x)[1])
    }
    
    # GBIF headquarters
    if (GBIF) {
      gbf <- cc_gbif(x, lon = "decimallongitude", lat = "decimallatitude",
                     verbose = T, value = "flags")
    } else {
      gbf <- rep(NA, dim(x)[1])
    }
    
    #Biodiversity institution
    if (institutions) {
      inst <- cc_inst(x, lon = "decimallongitude", lat = "decimallatitude",
                      ref = inst.ref,
                      verbose = T, value = "flags")
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
  }else{
    stop("wrong input format, x needs to be a data.frame or matrix with two columns")
  }
  
  #prepare output data
  
  out <- list(val, zer, equ, cap, cen, sea, urb, con, otl, gbf, inst, dpl)
  out <- Filter(function(x) !all(is.na(x)), out)
  out <- Reduce("&", out)
  
  if (verbose) {
    if (!is.null(out)) {
      cat(sprintf("flagged %s of %s records, EQ = %s \n", sum(!out, na.rm = T), 
                  length(out), round(sum(!out, na.rm = T)/length(out), 2)))
    } else {
      cat("flagged 0 records, EQ = 0 \n")
    }
  }
  if (output[1] == "spatialvalid") {
    out <- data.frame(x, validity = val, equal = equ, zeros = zer, capitals = cap, centroids = cen, 
                      sea = sea, urban = urb, countrycheck = con, outliers = otl, 
                      gbif = gbf, institution = inst, duplicates = dpl, summary = out)
    out <- Filter(function(x) !all(is.na(x)), out)
    class(out) <- c("spatialvalid", "data.frame", class(out))
  }
  if (output[1] == "cleaned") {
    if (is.null(species)) {
      out <- data.frame(x, validity = val, equal = equ, zeros = zer, capitals = cap, centroids = cen, 
                        sea = sea, urban = urb, countrycheck = con, outliers = otl, 
                        gbif = gbf, institution = inst, duplicates = dpl, 
                        summary = out)
      out <- Filter(function(x) !all(is.na(x)), out)
      out <- out[, 1:2]
    } else {
      out <- data.frame(x, species = species, validity = val, equal = equ, zeros = zer, capitals = cap, 
                        centroids = cen, sea = sea, urban = urb, countrycheck = con, 
                        outliers = otl, gbif = gbf, institution = inst, 
                        duplicates = dpl, summary = out)
      out <- Filter(function(x) !all(is.na(x)), out)
      out <- out[, 1:3]
    }
  }
  
  if (report) {
    report <- "CleanCoordinates_report.txt"
  }
  if (is.character(report)) {
    suma <- data.frame(test = as.character(names(out[-c(1:2)])), 
                       flagged.records = colSums(!out[-c(1:2)]))
    suma <- rbind(suma, c("Error Quotient", 
                          round(sum(out$summary, na.rm = T)/length(out$summary), 2)))
    write.table(suma, report, sep = "\t", row.names = F)
  }
  
  return(out)
} 
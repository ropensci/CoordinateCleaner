#' Identify Geographic Outliers in Species Distributions
#'
#' Removes out or flags records that are outliers in geographic space according
#' to the method defined via the \code{method} argument. Geographic outliers
#' often represent erroneous coordinates, for example due to data entry errors,
#' imprecise geo-references, individuals in horticulture/captivity.
#'
#' The method for outlier identification depends on the \code{method} argument.
#' If \dQuote{quantile}: a boxplot method is used and records are flagged as
#' outliers if their \emph{mean} distance to all other records of the same
#' species is larger than mltpl * the interquartile range of the mean distance
#' of all records of this species. If \dQuote{mad}: the median absolute
#' deviation is used. In this case a record is flagged as outlier, if the
#' \emph{mean} distance to all other records of the same species is larger than
#' the median of the mean distance of all points plus/minus the mad of the mean
#' distances of all records of the species * mltpl. If \dQuote{distance}:
#' records are flagged as outliers, if the \emph{minimum} distance to the next
#' record of the species is > \code{tdi}. For species with records from > 10000
#' unique locations a random sample of 1000 records is used for the distance
#' matrix calculation. The test skips species with fewer than \code{min_occs},
#' geographically unique records.
#'
#' The likelihood of occurrence records being erroneous outliers is linked to
#' the sampling effort in any given location. To account for this, the
#' sampling_cor option fetches the number of occurrence records available from
#' www.gbif.org, per country as a proxy of sampling effort. The outlier test
#' (the mean distance) for each records is than weighted by the log transformed
#' number of records per square kilometre in this country. See for
#' \url{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152}
#' an example and further explanation of the outlier test.
#'
#' @param species character string. The column with the species name. Default =
#'   \dQuote{species}.
#' @param method character string.  Defining the method for outlier selection.
#'   See details. One of \dQuote{distance}, \dQuote{quantile}, \dQuote{mad}.
#'   Default = \dQuote{quantile}.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'})to identify outliers. See details.  Default = 5.
#' @param tdi numeric.  The minimum absolute distance (\code{method ==
#'   'distance'}) of a record to all other records of a species to be identified
#'   as outlier, in km. See details. Default = 1000.
#' @param sampling_thresh numeric. Cut off threshold for the sampling
#'   correction. Indicates the quantile of sampling in which outliers should be
#'   ignored. For instance, if \code{sampling_thresh} == 0.25, records in the
#'   25% worst sampled countries will not be flagged as outliers. Default = 0
#'   (no sampling correction).
#' @param min_occs Minimum number of geographically unique datapoints needed for
#'   a species to be tested. This is necessary for reliable outlier estimation.
#'   Species with fewer than min_occs records will not be tested and the output
#'   value will be 'TRUE'. Default is to 7. If \code{method == 'distance'},
#'   consider a lower threshold.
#' @param thinning forces a raster approximation for the distance calculation.
#'   This is routinely used for species with more than 10,000 records for
#'   computational reasons, but can be enforced for smaller datasets, which is
#'   recommended when sampling is very uneven.
#' @param thinning_res The resolution for the spatial thinning in decimal
#'   degrees. Default = 0.5.
#' @inheritParams cc_cap
#'
#' @inherit cc_cap return
#'
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#'   details and tutorials.
#'
#' @keywords Coordinate cleaning
#' @family Coordinates
#'
#' @examples
#'
#' x <- data.frame(species = letters[1:10],
#'                 decimalLongitude = runif(100, -180, 180),
#'                 decimalLatitude = runif(100, -90,90))
#'
#' cc_outl(x)
#' cc_outl(x, method = "quantile", value = "flagged")
#' cc_outl(x, method = "distance", value = "flagged", tdi = 10000)
#' cc_outl(x, method = "distance", value = "flagged", tdi = 1000)
#'
#' @export
#' @importFrom geosphere distm distHaversine
#' @importFrom stats mad IQR median quantile
#' @importFrom dplyr left_join
#' @importFrom rgbif occ_count
#' @importFrom terra vect expanse 


cc_outl <- function(x, 
                    lon = "decimalLongitude", 
                    lat = "decimalLatitude", 
                    species = "species",
                    method = "quantile", 
                    mltpl = 5, 
                    tdi = 1000, 
                    value = "clean",
                    sampling_thresh = 0,
                    verbose = TRUE,
                    min_occs = 7,
                    thinning = FALSE,
                    thinning_res = 0.5) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("distance", "quantile", "mad"))
  
  # standardizing rownames to account for dataframe with 
  # unsorted rownames
  init_ids <- rownames(x)
  rownames(x) <- NULL
  
  
  if (verbose) {
    message("Testing geographic outliers")
  }
  
  # split input up into species
  splist <- split(x, f = as.character(x[[species]]))
  
  # remove duplicate records and make sure that there are at least two records
  test <- lapply(splist, function(k){duplicated(k[, c(species, lon, lat)])})
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  
  # only select species with more than min_occs records
  splist <- splist[test >= min_occs]
  
  # issue warning if species are omitted due to few records
  if (any(test < min_occs)) {
    warning(sprintf(
      "Species with fewer than %o unique records will not be tested.", 
      min_occs))
  }
 
  #Return values in case all species have less than the minimum number of occurrences
  if (all(test < min_occs)) {
    switch(value, 
           clean = return(x), 
           flagged = return(rep(TRUE, nrow(x))), 
           ids = return(init_ids))
  } else {
    
    # create a flag for raster approximation, in case 
    # thinning=TRUE or there are species with many records
    record_numbers <- unlist(lapply(splist, nrow))
    
    if (any(record_numbers >= 10000) | thinning) { #Thanks to Barnaby Walker & Shawn Laffan
      warning("Using raster approximation.")
      
      # create a raster with extent similar to all points, 
      # and unique IDs as cell values
      ras <- ras_create(x = x, # CHANGE HERE
                        lat = lat, 
                        lon = lon, 
                        thinning_res = thinning_res)
    }
    
    
    # identify points for flagging
    flags <- lapply(splist, 
                    .flagging, 
                    thinning = thinning, 
                    lon = lon,
                    lat = lat,
                    method = method,
                    mltpl = mltpl,
                    ras = ras,
                    sampling_thresh = sampling_thresh, 
                    tdi = tdi)
    
    # turn to numeric and remove NAs
    flags <- as.numeric(as.vector(unlist(flags)))
    flags <- flags[!is.na(flags)]
    
    # sampling correction for cross country datasets
    if (sampling_thresh > 0) {
      # identify countries in the dataset, with point polygon test
      pts <- terra::vect(x[flags, c(lon, lat)],
                         geom = c(lon, lat))
      
      if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
        stop("package 'rnaturalearth' not found. Needed for sampling_cor = TRUE",
             call. = FALSE
        )
      }
      if (!requireNamespace("rgbif", quietly = TRUE)) {
        stop("package 'rgbif' not found. Needed for sampling_cor = TRUE",
             call. = FALSE
        )
      }
      
      if (inherits(try(rgbif::occ_count(country = "DEU"), silent = TRUE), 
                   "try-error")) {
        warnings("Could not retrive records number from GBIF, skipping sampling correction")
      }else{
        # get country area from naturalearth
        ref <- terra::vect(rnaturalearth::ne_countries(scale = "medium", 
                                                       returnclass = "sf"))
        area <- data.frame(country = ref$iso_a2, 
                           area = terra::expanse(ref))
        area <- area[!is.na(area$area), ]
        area <- area[!is.na(area$country), ]

        ## naturalearth uses -99 for missing values?
        area <- area[area$country != "-99", ]

        ## CN-TW (Taiwan) doesn't map to a country in the GBIF database:
        ## naturalearth dataset uses CN-TW for Taiwan, GBIF uses TW.
        area[area$country == "CN-TW", "country"] <- "TW"
        
        # get number of records in GBIF per country as proxy for sampling intensity
        nrec <- vapply(area$country, 
                       FUN = function(k){rgbif::occ_count(country = k)},
                       FUN.VALUE = 1)##get record count
        nrec <- data.frame(country = area$country, 
                           recs = unlist(nrec), 
                           row.names = NULL)
        
        # normalize by area
        nrec_norm <- dplyr::left_join(nrec, area, by = "country")
        nrec_norm$norm <- log(nrec_norm$recs /  (nrec_norm$area  / 1000000 / 100))
        #ref <- raster::crop(ref, raster::extent(pts) + 1)
        
        # get country from coordinates and compare with provided country
        # country <- terra::over(x = pts, y = ref)[, "iso_a2"]
        ext_over <- terra::extract(ref, pts)
        out <- !is.na(ext_over[!duplicated(ext_over[, 1]), 2])
        country <- ext_over[out, "iso_a2"]
        # get the sampling for the flagged countries
        thresh <- stats::quantile(nrec_norm$norm, probs = sampling_thresh)
        s_flagged <- nrec_norm$norm[match(country, nrec_norm$country)]
        s_flagged <- s_flagged > thresh
        
        #treat countries with no country information as FALSE
        s_flagged[is.na(s_flagged)] <- FALSE
        
        # Only retain those flags with sampling higher than threshold
        flags <- flags[s_flagged]
      }
      
      
    }
    
    out <- rep(TRUE, nrow(x))
    out[flags] <- FALSE
    
    
    if (verbose) {
      if (value == "ids") {
          message(sprintf("Flagged %s records.", length(flags)))
        } else {
        if (value == "clean") {
          message(sprintf("Removed %s records.", sum(!out)))
        } else {
          message(sprintf("Flagged %s records.", sum(!out)))
        }
      }
    }
    
    switch(value, 
           clean = return(x[out, ]), 
           flagged = return(out), 
           ids = return(init_ids[flags]))
  }
}


####------

.flagging <- function(k, thinning, lon, lat, method, mltpl, ras,
                      sampling_thresh, tdi) {
  
  # Set raster flag inc ase thinning= TRUE,
  # or this particualr species has 100000 or more records
  if (nrow(k) >= 10000 | thinning) {
    raster_flag <- TRUE
  } else {
    raster_flag <- FALSE
  }
  
  # calculate the distance matrix between all points for the outlier tests
  ## for small datasets and without thinning, 
  ## simply a distance matrix using geospheric distance
  if (raster_flag) { 
    # raster approximation for large datasets and thinning
    # get a distance matrix of raster midpoints, with the row 
    # and colnames giving the cell IDs
    
    # if the raster distance is used due to large dataset and 
    # not for thinning, account for the number of points per gridcell
    if (thinning) {
      dist_obj <- ras_dist(x = k, 
                           lat = lat, 
                           lon = lon,
                           ras = ras, 
                           weights = FALSE)
      
      # the point IDS
      pts <-  dist_obj$pts
      
      # the distance matrix 
      dist <- dist_obj$dist
      
    } else {
      dist_obj <-  ras_dist(x = k, 
                            lat = lat, 
                            lon = lon,
                            ras = ras, 
                            weights = TRUE)
      
      # the point IDS
      pts <-  dist_obj$pts
      
      # the distance matrix 
      dist <- dist_obj$dist
      
      # a weight matrix to weight each distance by the number of points in it
      wm <- dist_obj$wm
    }
  } else { 
    #distance calculation
    dist <- geosphere::distm(k[, c(lon, lat)], 
                             fun = geosphere::distHaversine) / 1000
    
    # set diagonale to NA, so it does not influence the mean
    dist[dist == 0] <- NA
  }
  
  # calculate the outliers for the different methods
  ##  distance method useing absolute distance
  if (method == "distance") {
    # Drop an error if the sampling correction is activated
    if (sampling_thresh > 0) {
      stop("Sampling correction impossible for method 'distance'" )
    }
    
    # calculate the minimum distance to any next point
    mins <- apply(dist, 1, min, na.rm = TRUE)
    
    # outliers based on absolute distance threshold
    out <- which(mins > tdi)
  } else { # for the other methods the mean must be claculated 
    # depending on if the raster method is used
    # get row means
    if (raster_flag & !thinning) {
      # get mean distance to all other points
      mins <- apply(dist, 1, sum, na.rm = TRUE) / rowSums(wm, na.rm = TRUE)
    } else {
      # get mean distance to all other points
      mins <-  apply(dist, 1, mean, na.rm = TRUE)
    }
  }
  
  ## the quantile method
  if (method == "quantile") {
    # identify the quaniles
    quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
    
    # flag all upper outliers
    out <- which(mins > (quo[2] + stats::IQR(mins) * mltpl))
  }
  
  ## the mad method
  if (method == "mad") {
    # get the median
    quo <- stats::median(mins, na.rm = TRUE)
    
    # get the mad stat
    tester <- stats::mad(mins, na.rm = TRUE)
    
    # Identify outliers
    out <- which(mins > (quo + tester * mltpl))
  }
  
  # Identify the outlier points depending on 
  # if the raster approximation was used
  if (raster_flag) {
    # create output object
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[which(pts %in% gsub("X", "", names(out)))]
    }
  }else{
    # create output object
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[out]
    }
  }
  return(ret)
}
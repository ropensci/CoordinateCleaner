#' Identify Geographic Outliers in Species Distributions
#' 
#' Removes out or flags records that are outliers in geographic space according to the method
#' defined via the \code{method} argument. Geographic outliers often represent
#' erroneous coordinates, for example due to data entry errors, imprecise
#' geo-references, individuals in horticulture/captivity.
#' 
#' The method for outlier identification depends on the \code{method} argument.
#' If \dQuote{outlier}: a boxplot method is used and records are flagged as
#' outliers if their \emph{mean} distance to all other records of the same
#' species is larger than mltpl * the interquartile range of the mean distance
#' of all records of this species. If \dQuote{mad}: the median absolute
#' deviation is used. In this case a record is flagged as outlier, if the
#' \emph{mean} distance to all other records of the same species is larger than
#' the median of the mean distance of all points plus/minus the mad of the mean
#' distances of all records of the species * mltpl. If \dQuote{distance}:
#' records are flagged as outliers, if the \emph{minimum} distance to the next
#' record of the species is > \code{tdi}. For species with records from > 10000
#' unique locations a random sample of 1000 records is used for 
#' the distance matrix calculation. The test skipps species with less than \code{min_occs},
#'  geographically unique records.
#' 
#' The likelihood of occurrence records being erroneous outliers is linked to the sampling effort
#' in any given location. To account for this, the sampling_cor option fetches 
#' the number of occurrence records available 
#' from www.gbif.org, per country as a proxy of sampling effort. The outlier test 
#' (the mean distance) for each records is than weighted by the log transformed 
#' number of records per square kilometre in this country. 
#' See for \url{https://ropensci.github.io/CoordinateCleaner/articles/Tutorial_geographic_outliers.html} 
#' an example and further explanation of the outlier test.
#' 
#' @param species character string. The column with the species name. Default
#' = \dQuote{species}.
#' @param method character string.  Defining the method for outlier
#' selection.  See details. One of \dQuote{distance}, \dQuote{quantile},
#' \dQuote{mad}.  Default = \dQuote{quantile}.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'})to identify outliers. See details.  Default = 5.
#' @param tdi numeric.  The minimum absolute distance (\code{method ==
#' 'distance'}) of a record to all other records of a species to be identified
#' as outlier, in km. See details. Default = 1000.
#' @param sampling_thresh numeric. Cut off threshold for the sampling correction.
#' Indicates the quantile of sampling in which outliers should be ignored. For instance, 
#' if \code{sampling_thresh} == 0.25, records in the 25% worst sampled countries will 
#' not be flagged as outliers. Default = 0 (no sampling correction).
#' @param min_occs Minimum number of geographically unique datapoints needed for a species to be tested. 
#' This is necessary for reliable outlier estimation.
#' Species wit less than min_occs records will not be tested and the output value will be 'TRUE'.
#' Default is to 7. If \code{method == 'distance'}, consider a lower threshold.
#' @param thinning forces a raster approximation for the distance calculation. 
#' This is routinely used for species with more than 10,000 records for computational reasons, 
#' but can be enforced for smaller datasets, which is reommended when sampling is very uneven. Default = T.
#' @param thinning_res The resolution for the spatial thinning in decimal degrees. Default = 0.5.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90))
#'                 
#' cc_outl(x)
#' cc_outl(x, method = "quantile", value = "flagged")
#' cc_outl(x, method = "distance", value = "flagged", tdi = 10000)
#' cc_outl(x, method = "distance", value = "flagged", tdi = 1000)
#' 
#' @export
#' @importFrom geosphere distm distHaversine
#' @importFrom stats mad IQR median quantile
#' @importFrom sp over SpatialPoints
#' @importFrom dplyr left_join
#' @importFrom raster crop extent ncell res setValues
#' @importFrom rgbif occ_count
#' 
cc_outl <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
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
  if(any(test < min_occs)){
    warning(sprintf(
      "Species with less than %o unique records will not be tested.", 
      min_occs))
  }
  
  # create raster for raster approximation if there are large 
  # datasets or spatial thinning is activated
  if(any(test >= 10000) | thinning){
    warning("Using raster approximation.")
    # create a raster with extent similar to all points, 
    # and unique IDs as cell values
    ras <- ras_create(x = x, 
                      lat = lat, 
                      lon = lon, 
                      thinning_res = thinning_res)
  }
  
  # identify points for flagging
  flags <- lapply(splist, function(k) {
    
    # calculate the distance matrix between all points for the outlier tests
    ## for small datasets and without thinning, 
    ## simply a distance matrix using geospheric distance
    if(nrow(k) <= 10000 & !thinning){ 
      
      #distance calculation
      dist <- geosphere::distm(k[, c(lon, lat)], 
                               fun = geosphere::distHaversine) / 1000
      
      # set diagonale to NA, so it does not influence the mean
      dist[dist == 0] <- NA
      
    }else{ 
      # raster approximation for large datasets and thinning
      # get a distance matrix of raster midpoints, with the row 
      # and colnames giving the cell IDs
      
      # if the raster distance is used due to large dataset and 
      # not for thinning, account for the number of points per gridcell
      if(thinning){
        dist_obj <- ras_dist(x = k, 
                             lat = lat, 
                             lon = lon,
                             ras = ras, 
                             weights = FALSE)
        
        # the point IDS
        pts <-  dist_obj$pts
        
        # the distance matrix 
        dist <- dist_obj$dist
        
      }else{
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
    }
    
    # calculate the outliers for the different methods
    ##  distance method useing absolute distance
    if(method == "distance"){
      
      # Drop an error if the sampling correction is activated
      if(sampling_thresh > 0){
        stop("Sampling correction impossible for method 'distance'" )
      }
      
      # calculate the minimum distance to any next point
      mins <- apply(dist, 1, min, na.rm = TRUE)
      
      # outliers based on absolute distance threshold
      out <- which(mins > tdi)
    }else{ # for the other methods the mean must be claculated 
      # depending on if the raster method is used
      # get row means
      if(nrow(k) >= 10000 & !thinning){
        # get mean distance to all other points
        mins <- apply(dist, 1, sum, na.rm = TRUE) / rowSums(wm, na.rm = TRUE)
      }else{
        # get mean distance to all other points
        mins <-  apply(dist, 1, mean, na.rm = TRUE)
      }
    }
    
    ## the quantile method
    if(method == "quantile"){
      # identify the quaniles
      quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
      
      # flag all upper outliers
      out <- which(mins > quo[2] + stats::IQR(mins) * mltpl)
    }
    
    ## the mad method
    if (method == "mad") {
      # get the median
      quo <- stats::median(mins, na.rm = TRUE)
      
      # get the mad stat
      tester <- stats::mad(mins, na.rm = TRUE)
      
      # Identify outliers
      out <- which(mins > quo + tester * mltpl)
    }
    
    # Identify the outlier points depending on 
    # if the raster approximation was used
    if(nrow(k) > 10000 | thinning){
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
  })
  
  # turn to numeric and remove NAs
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  # sampling correction for cross country datasets
  if(sampling_thresh > 0){
    # identify countries in the dataset, with point polygon test
    pts <- sp::SpatialPoints(x[flags, c(lon, lat)])
    
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
    
    # get country area from naturalearth
    ref <- rnaturalearth::ne_countries(scale = "medium")
    sp::proj4string(ref) <- ""
    area <- data.frame(country = ref@data$iso_a3, 
                       area = geosphere::areaPolygon(ref))
    area <- area[!is.na(area$area),]
    area <- area[!is.na(area$country),]
    
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
    ref <- raster::crop(ref, raster::extent(pts) + 1)
    
    # get country from coordinates and compare with provided country
    country <- sp::over(x = pts, y = ref)[, "iso_a3"]
    
    # get the sampling for the flagged countries
    thresh <- stats::quantile(nrec_norm$norm, probs = sampling_thresh)
    s_flagged <- nrec_norm$norm[match(country, nrec_norm$country)]
    s_flagged <- s_flagged > thresh
    
    #treat countries with no country information as FALSE
    s_flagged[is.na(s_flagged)] <- FALSE
    
    # Only retain those flags with sampling higher than threshold
    flags <- flags[s_flagged]
  }
  
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  
  if (verbose) {
    if (value == "ids") {
      if(value == "clean"){
        message(sprintf("Removed %s records.", length(flags)))
      }else{
        message(sprintf("Flagged %s records.", length(flags)))
      }
    } else {
      if(value == "clean"){
        message(sprintf("Removed %s records.", sum(!out)))
      }else{
        message(sprintf("Flagged %s records.", sum(!out)))
      }
    }
  }
  
  switch(value, 
         clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(flags))
}

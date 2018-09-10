#' Flag Geographic Outliers in Species Distributions
#' 
#' Flags records that are outliers in geographic space according to the method
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
#' the distance matrix calculation.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species a character string. The column with the species name. Default
#' = \dQuote{species}.
#' @param method a character string.  Defining the method for outlier
#' selection.  See details. One of \dQuote{distance}, \dQuote{quantile},
#' \dQuote{mad}.  Default = \dQuote{quantile}.
#' @param mltpl numeric. The multiplier of the interquartile range
#' (\code{method == 'quantile'}) or median absolute deviation (\code{method ==
#' 'mad'})to identify outliers. See details.  Default = 5.
#' @param tdi numeric.  The minimum absolute distance (\code{method ==
#' 'distance'}) of a record to all other records of a species to be identified
#' as outlier, in km. See details. Default = 1000.
#' @param value a character string.  Defining the output value. See value.
#' @param sampling_thresh a numeric. Cut off threshold fo
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @details The likelihood of occurrence records being erroneous outliers is linked to the sampling effort
#' in any given location. To account for this, the sampling_cor option fetches the number of occurrence records available 
#' from www.gbif.org, per country as a proxy of sampling effort. The outlier test 
#' (the mean distance) for each records is than weighted by the log transformed 
#' number of records per square kilometre in this country. See for an example and further
#'  explanation of the outlier test.
#' @keywords Coordinate cleaning
#' @family Coordinates
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
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("distance", "quantile", "mad"))

  if (verbose) {
    message("Testing geographic outliers")
  }
  
  # split up into species
  splist <- split(x, f = as.character(x[[species]]))
  
  # remove duplicate records and make sure that there are at least two records
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 7]

  # create raster for raster approximation  of large datasets
  if(any(test >= 10000)){
    warning("large dataset. Using raster approximation.")
    
    # get data extend
    ex <- raster::extent(sp::SpatialPoints(x[, c(lon, lat)]))
    # create raster
    # using a raster that is 1deg by 1 deg globally
    ras <- raster::raster(x = ex, nrow = 180, ncol = 360) 
    vals <- seq_len(raster::ncell(ras))
    ras <- raster::setValues(ras, vals)
  }

  # loop over species and run outlier test
  flags <- lapply(splist, function(k) {
    test <- nrow(k[!duplicated(k), ])

    if (test > 7) {
      if(nrow(k) <= 10000){
        # Calculate distance between individual points
        dist <- geosphere::distm(k[, c(lon, lat)], 
                                 fun = geosphere::distHaversine) / 1000
        dist[dist == 0] <- NA
        
        if(method == "distance"){
          # get minimum distance to all other points
          mins <- apply(dist, 1, min, na.rm = TRUE)
        }else{
          # get mean distance to all other points
          mins <- apply(dist, 1, mean, na.rm = TRUE)
        }
        
      }else{
        # assign points to raster cells 
        pts <- raster::extract(x = ras, y = sp::SpatialPoints(k[, c(lon, lat)]))
        midp <- data.frame(raster::rasterToPoints(ras))
        midp <- midp[midp$layer %in% unique(pts),]
        midp <- midp[match(unique(pts), midp$layer),]
        
        # calculate geospheric distance between raster cells with points
        dist <- geosphere::distm(midp[, c("x", "y")], 
                                 fun = geosphere::distHaversine) / 1000
        
        # approximate within cell distance as half 
        # the cell size, assumin 1 deg = 100km
        # this is crude, but doesn't really matter
        dist[dist == 0] <- 100 * mean(raster::res(ras)) / 2
        
        dist <- as.data.frame(dist, row.names = as.integer(midp$layer))
        names(dist) <- midp$layer
        
        # weight matrix to account for the number of points per cell
        cou <- table(pts)
        cou <- cou[match(unique(pts), names(cou))]
        wm <- outer(cou, cou)
        
        # multiply matrix elements to get weightend sum
        dist <- round(dist * wm, 0)
        
        if(method == "distance"){
          mins <- apply(dist, 1, min, na.rm = TRUE)
        }else{
          # get row means
          mins <- apply(dist, 1, sum) / rowSums(wm)
        }
      }
    }
    
    ## Absolute distance test with mean interpoint distance
    if (method == "distance") {
      out <- which(mins > tdi * 1000)
      if(sampling_thresh > 0){
        stop("Sampling correction impossible for method 'distance'" )
      }
    }
    
    ## Quantile based test, with mean interpoint distances
    if (method == "quantile") {
      quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
      out <- which(mins < quo[1] - stats::IQR(mins) * mltpl | mins > quo[2] +
                     stats::IQR(mins) * mltpl)
    }
    
    ## MAD (Median absolute deviation) based test, 
    if (method == "mad") {
      quo <- stats::median(mins)
      tester <- stats::mad(mins)
      out <- which(mins < quo - tester * mltpl | mins > quo + tester *
                     mltpl)
    }
    
    if(nrow(k) > 10000){
      # create output object
      if (length(out) == 0) {
        ret <- NA
      }
      if (length(out) > 0) {
        ret <- which(pts %in% names(out))
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

  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
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
      message(sprintf("Flagged %s records.", length(flags)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, 
         clean = return(x[out, ]), 
         flagged = return(out), 
         ids = return(flags))
}

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
#' record of the species is > \code{tdi}. For speces with records from > 10000
#' unique locations a random sample of 1000 records is used for 
#' the distance matrix calculation.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
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
#' @param sampling_cor logical. If TRUE, outlier detection is weightend by sampling intensity. 
#' See details. Defeulat = F.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @details The likelihood of occurrence records being erroroneous outliers is linked to the sampling effort
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
#' 
cc_outl <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    species = "species",
                    method = "quantile", 
                    mltpl = 5, 
                    tdi = 1000, 
                    value = "clean", 
                    sampling_cor = FALSE,
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged", "ids"))
  match.arg(method, choices = c("distance", "quantile", "mad"))

  if (verbose) {
    message("Testing geographic outliers")
  }
  
  if(sampling_cor){
    # identify countries in the dataset, with point polygon test
    pts <- sp::SpatialPoints(x[, c(lon, lat)])
    
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Install the 'rnaturalearth' package or provide a custom reference",
           call. = FALSE
      )
    }
    if (!requireNamespace("rgbif", quietly = TRUE)) {
      stop("'rgbif' is needed for sampling_cor = TRUE.",
           call. = FALSE
      )
    }
    
    # load country reference and get area
    ref <- rnaturalearth::ne_countries(scale = "medium")
    sp::proj4string(ref) <- ""
    area <- data.frame(country = ref@data$iso_a3, 
                       area = geosphere::areaPolygon(ref))
    area <- area[!is.na(area$area),]
    area <- area[!is.na(area$country),]
    # 
    # nrec_norm <- data.frame(country = names(nrec), nrec)
    # nrec_norm <- dplyr::left_join(nrec_norm, area, by = "country")
    # nrec_norm$norm <- nrec_norm$nrec /  (nrec_norm$area  / 1000000 / 100)
    # nrec_norm <- nrec_norm[!is.na(nrec_norm$country),]
    # 
    # hist(log(nrec_norm$norm))
    # abline(v = log(nrec_norm[nrec_norm$country == "USA", "norm"]))
    # abline(v = log(nrec_norm[nrec_norm$country == "CHN", "norm"]), col = "yellow")
    # abline(v = log(nrec_norm[nrec_norm$country == "RUS", "norm"]), col = "red")
    # abline(v = log(nrec_norm[nrec_norm$country == "BOL", "norm"]), col = "green")
    # 
    # abline(v = log(nrec_norm[nrec_norm$country == "MUS", "norm"]), col = "darkgreen")
    
    ref <- raster::crop(ref, raster::extent(pts) + 1)
    
    # get country from coordinates and compare with provided country
    country <- sp::over(x = pts, y = ref)[, "iso_a3"]
    
    dat <- data.frame(x[,c(species, lon, lat)], country)
    
    # get number of records in GBIF per country as proxy for sampling intensity
    country <- unique(country)
    country <- country[!is.na(country)]
    
    nrec <- sapply(country, FUN = function(k){rgbif::occ_count(country = k)})##get record count
    nrec <- data.frame(country = country, weight = unlist(nrec), row.names = NULL)
    
    # log transform and prepare as weight for each plot
    dat <- dplyr::left_join(dat, nrec, by = "country")
    dat <- dplyr::left_join(dat, area, by = "country")
    
    dat$weight <- dat$weight / (dat$area / 1000000 / 100) #normalize number of records to 100 sqkm
    dat$weight <- log(dat$weight)
    dat$weight[is.na(dat$weight)] <- median(dat$weight, na.rm = T) # records in the sea get the mean weight
    #rescale weight between 0.5 and 2
    dat$weight <- (dat$weight - 0) / (11 - 2) * 
      (2 - 0.5) + 0.5 
    dat$weight[dat$weight < 0.5] <- 0.5 # records with less than 20 records per 100 sqkm get all the same minimum value
    dat$weight[dat$weight > 2] <- 2 #records with more than 59874 records per 100 sqkm get the amximum value
    
    dat <- data.frame(x, weight = dat$weight)
    
  }else{
    dat <- x
  }

  # split up into species
  splist <- split(dat, f = as.character(dat[[species]]))

  # remove duplicate records and make sure that there are at least two records
  # left
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 7]
  
  # loop over species and run outlier test
  flags <- lapply(splist, function(k) {
    test <- nrow(k[!duplicated(k), ])

    if (test > 7) {
      #calculate distance matrix
      if(nrow(k) > 10000){ #subsampling for large datasets
        dum <- k[sample(1:nrow(k), size = 1000),]
        dist <- geosphere::distm(k[, c(lon, lat)], dum[, c(lon,lat)])
        warning("large dataset. Using subsampling for outlier detection.")
      }else{
        dist <- geosphere::distm(k[, c(lon, lat)], 
                                 fun = geosphere::distHaversine)
      }
      
      dist[dist == 0] <- NA
      
      # absolute distance test with mean interpoint distance
      if (method == "distance") {
        mins <- apply(dist, 1, min, na.rm = TRUE)
        out <- which(mins > tdi * 1000)
        
        if(sampling_cor){
          stop("Sampling correction impossible for method 'distance'" )
        }
      }

      # Quantile based test, with mean interpoint distances
      if (method == "quantile") {
        mins <- apply(dist, 1, mean, na.rm = TRUE)
        quo <- quantile(mins, c(0.25, 0.75), na.rm = TRUE)
        
        if(sampling_cor){#weight for sampling intensity in country
          mins <- mins * k$weight
        }
        
        out <- which(mins < quo[1] - stats::IQR(mins) * mltpl | mins > quo[2] +
          stats::IQR(mins) * mltpl)
      }

      # MAD (Median absolute deviation) based test, 
      # calculate the mean distance to
      # all other points for each point, and then take the mad of this
      if (method == "mad") {
        mins <- apply(dist, 1, mean, na.rm = TRUE)
       
         if(sampling_cor){ #weight for sampling intensity in country
          mins <- mins * k$weight
        }
        
        quo <- stats::median(mins)
        tester <- stats::mad(mins)
        out <- which(mins < quo - tester * mltpl | mins > quo + tester *
          mltpl)
      }
    }
    # create output object
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

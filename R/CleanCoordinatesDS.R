#' Geographic Coordinate Cleaning based on Dataset Properties
#' 
#' Identifies potentially problematic coordinates based on dataset properties.
#' Includes test to flag potential errors with converting ddmm to dd.dd, and
#' periodicity in the data decimals indicating rounding or a raster basis
#' linked to low coordinate precision.
#' 
#' This function checks the statistical distribution of decimals within
#' datasets of geographic distribution records to identify datasets with
#' potential errors/biases. Three potential error sources can be identified.
#' The ddmm flag tests for the particular pattern that emerges if geographical
#' coordinates in a degree minute annotation are transferred into decimal
#' degrees, simply replacing the degree symbol with the decimal point. This
#' kind of problem has been observed by in older datasets first recorded on
#' paper using typewriters, where e.g. a floating point was used as symbol for
#' degrees. The function uses a binomial test to check if more records then
#' expected have decimals blow 0.6 (which is the maximum that can be obtained
#' in minutes, as one degree has 60 minutes) and if the number of these records
#' is higher than those above 0.59 by a certain proportion. The periodicity
#' test uses rate estimation in a poison process to estimate if there is
#' periodicity in the decimals of a dataset (as would be expected by for
#' example rounding or data that was collected in a raster format) and if there
#' is an over proportional number of records with the decimal 0 (full degrees)
#' which indicates rounding and thus low precision. The default values are
#' empirically optimized by with GBIF data, but should probably be adapted.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ds a character string. The column with the dataset of each record. In
#' case \code{x} should be treated as a single dataset, identical for all
#' records. Default = \dQuote{dataset}.
#' @param ddmm logical. If TRUE, testing for erroneous conversion from a degree
#' minute format (ddmm) to a decimal degree (dd.dd) format. See details.
#' @param periodicity logical. If TRUE, testing for periodicity in the data,
#' which can indicate imprecise coordinates, due to rounding or rasterization.
#' @param ddmm.pvalue numeric. The p-value for the one-sided t-test to flag the
#' ddmm test as passed or not. Both ddmm.pvalue and ddmm.diff must be met.
#' Default = 0.025.
#' @param ddmm.diff numeric. The threshold difference for the ddmm test.
#' Indicates by which fraction the records with decimals below 0.6 must
#' outnumber the records with decimals above 0.025. Default = 0.2
#' @param periodicity.T1 numeric.  The threshold for outlier detection in a in
#' an interquantile range based test. This is the major parameter to specify
#' the sensitivity of the test: lower values, equal higher detection rate.
#' Values between 7-11 are recommended. Default = 7.
#' @param periodicity.reg.thresh numeric. Threshold on the number of equal
#' distances between outlier points.  See details.  Default = 2.
#' @param periodicity.dist.min numeric.  The minimum detection distance between
#' outliers in degrees (the minimum resolution of grids that will be flagged).
#' Default = 0.1.
#' @param periodicity.dist.max numeric.  The maximum detection distance between
#' outliers in degrees (the maximum resolution of grids that will be flagged).
#' Default = 2.
#' @param periodicity.min.size numeric.  The minimum number of unique locations
#' (values in the tested column) for datasets to be included in the test.
#' Default = 4.
#' @param periodicity.target character string.  Indicates which column to test.
#' Either \dQuote{lat] for latitude, \dQuote{lon} for longitude, or
#' \dQuote{both} for both.  In the latter case datasets are only flagged if
#' both test are failed.  Default = \dQuote{both}}
#' @param periodicity.diagnostics logical. If TRUE, diagnostic plots are
#' produced.  Default = TRUE.
#' @param value a character string.  Defining the output value. See value.
#' Default = \dQuote{dataset}.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a summary per
#' dataset \code{dataset}, a data.frame (\dQuote{clean}) containing the records considered
#' correct by the test or a logical vector (\dQuote{flagged}), with TRUE = test
#' passed and FALSE = test failed/potentially problematic .
#' Default = \dQuote{clean}. If \dQuote{dataset}: \code{data.frame} with one
#' row for each dataset in \code{x}.
#' @seealso \code{\link{CleanCoordinates}}
#' @keywords Coordinate cleaning wrapper
#' @examples
#' 
#' #Create test dataset
#' clean <- data.frame(dataset = rep("clean", 1000),
#'                     decimallongitude = runif(min = -42, max = -40, n = 1000),
#'                     decimallatitude = runif(min = -12, max = -10, n = 1000))
#'                     
#' bias.long <- c(round(runif(min = -42, max = -40, n = 500), 1),
#'                round(runif(min = -42, max = -40, n = 300), 0),
#'                runif(min = -42, max = -40, n = 200))
#' bias.lat <- c(round(runif(min = -12, max = -10, n = 500), 1),
#'               round(runif(min = -12, max = -10, n = 300), 0),
#'               runif(min = -12, max = -10, n = 200))
#' bias <- data.frame(dataset = rep("biased", 1000),
#'                    decimallongitude = bias.long,
#'                    decimallatitude = bias.lat)
#' test <- rbind(clean, bias)
#' 
#' \dontrun{                  
#' #run CleanCoordinatesDS
#' flags <- CleanCoordinatesDS(test)
#' 
#' #check problems
#' #clean
#' hist(test[test$dataset == rownames(flags[flags$summary,]), "decimallongitude"])
#' #biased
#' hist(test[test$dataset == rownames(flags[!flags$summary,]), "decimallongitude"])
#' 
#' }
#' @export
#' @importFrom stats complete.cases
CleanCoordinatesDS <- function(x, 
                               lon = "decimallongitude", 
                               lat = "decimallatitude",
                               ds = "dataset", 
                               ddmm = TRUE, 
                               periodicity = TRUE, 
                               ddmm.pvalue = 0.025, 
                               ddmm.diff = 0.2,
                               periodicity.T1 = 7, 
                               periodicity.reg.thresh = 2, 
                               periodicity.dist.min = 0.1,
                               periodicity.dist.max = 2, 
                               periodicity.min.size = 4, 
                               periodicity.target = "both",
                               periodicity.diagnostics = TRUE, 
                               value = "dataset",
                               verbose = TRUE) {

  # check input arguments
  match.arg(value, choices = c("dataset", "flagged", "clean"))
  match.arg(periodicity.target, choices = c("lat", "lon", "both"))

  # check column names
  nams <- c(lon, lat, ds)
  if (!all(nams %in% names(x))) {
    stop(sprintf("%s column not found\n", nams[which(!nams %in% names(x))]))
  }

  # prepare input data
  dat <- x[, c(lon, lat, ds)]
  dat[[ds]] <- as.character(dat[[ds]])

  ## kick out NAs
  if (sum(!complete.cases(dat)) > 0) {
    warning(sprintf(
      "Incomplete cases found. value set to 'dataset', 
                    ignoring %s cases with incomplete data",
      sum(!complete.cases(dat))
    ))
    dat <- dat[complete.cases(dat), ]
    value <- "dataset"
  }

  # perpare dummy value in case value == 'clean'
  if (value == "clean") {
    value2 <- "flagged"
  } else {
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
    out.t1 <- dc_ddmm(
      x = dat, lon = lon, lat = lat, ds = ds, pvalue = ddmm.pvalue,
      diff = ddmm.diff, value = value2, verbose = verbose
    )
    if (value == "dataset") {
      names(out.t1) <- c("binomial.pvalue", "perc.difference", "pass.ddmm")
    }
  } else {
    if (value == "dataset") {
      out.t1 <- data.frame(pass.ddmm = rep(NA, length(test)))
    } else {
      out.t1 <- rep(NA, nrow(dat))
    }
  }

  # Run periodicity test
  if (periodicity) {
    out.t2 <- dc_round(x,
      lon = lon, 
      lat = lat, 
      ds = ds, 
      T1 = periodicity.T1,
      reg.out.thresh = periodicity.reg.thresh, 
      reg.dist.min = periodicity.dist.min,
      reg.dist.max = periodicity.dist.max, 
      min.unique.ds.size = periodicity.min.size,
      graphs = periodicity.diagnostics, 
      test = periodicity.target, 
      value = value2
    )
  } else {
    if (value == "dataset") {
      out.t2 <- data.frame(
        pass.periodicity.com = rep(NA, length(test)),
        pass.zero.com = rep(NA, length(test)),
        summary = NA
      )
    } else {
      out.t2 <- rep(NA, nrow(dat))
    }
  }

  # prepare output
  if (value == "dataset") {
    out <- data.frame(out.t1, out.t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    if(periodicity & ddmm){
      out$summary <- as.logical(out.t1$pass.ddmm) & as.logical(out.t2$summary)
    }else if(periodicity & !ddmm){
      out$summary <- out.t2$summary
    }else if(!periodicity & ddmm){
      out$summary <- as.logical(out.t1$pass.ddmm)
    }

    if (verbose) {
      message(sprintf("Flagged %s datasets.", sum(!out$summary)))
    }
  }
  if (value == "flagged") {
    out <- data.frame(ddmm = out.t1, periodicity = out.t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    out$summary <- Reduce("&", out)

    if (verbose) {
      message(sprintf("Flagged %s records.", sum(!out$summary)))
    }
  }
  if (value == "clean") {
    out <- data.frame(ddmm = out.t1, periodicity = out.t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    out$summary <- Reduce("&", out)
    if (verbose) {
      message(sprintf("Flagged %s records.", sum(!out$summary)))
    }
    out <- dat[out$summary, ]
  }
  # return output
  return(out)
}

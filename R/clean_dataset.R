#' Coordinate Cleaning using Dataset Properties
#' 
#' Tests for problems associated with coordinate conversions and rounding,
#' based on dataset properties. Includes test to identify contributing datasets with
#' potential errors with converting ddmm to dd.dd, and
#' periodicity in the data decimals indicating rounding or a raster basis
#' linked to low coordinate precision. Specifically:
#' * ddmm  tests for erroneous conversion from a degree
#' minute format (ddmm) to a decimal degree (dd.dd) format
#' * periodicity test for periodicity in the data,
#' which can indicate imprecise coordinates, due to rounding or rasterization.
#'
#' These tests are based on the statistical distribution of coordinates and
#'  their decimals within
#' datasets of geographic distribution records to identify datasets with
#' potential errors/biases. Three potential error sources can be identified.
#' The ddmm flag tests for the particular pattern that emerges if geographical
#' coordinates in a degree minute annotation are transferred into decimal
#' degrees, simply replacing the degree symbol with the decimal point. This
#' kind of problem has been observed by in older datasets first recorded on
#' paper using typewriters, where e.g. a floating point was used as symbol for
#' degrees. The function uses a binomial test to check if more records than
#' expected have decimals below 0.6 (which is the maximum that can be obtained
#' in minutes, as one degree has 60 minutes) and if the number of these records
#' is higher than those above 0.59 by a certain proportion. The periodicity
#' test uses rate estimation in a Poisson process to estimate if there is
#' periodicity in the decimals of a dataset (as would be expected by for
#' example rounding or data that was collected in a raster format) and if there
#' is an over proportional number of records with the decimal 0 (full degrees)
#' which indicates rounding and thus low precision. The default values are
#' empirically optimized by with GBIF data, but should probably be adapted.
#' 
#' @aliases CleanCoordinatesDS
#' 
#' @param ds a character string. The column with the dataset of each record. In
#' case \code{x} should be treated as a single dataset, identical for all
#' records. Default = \dQuote{dataset}.
#' @param tests a vector of character strings, indicating which tests to run. 
#' See details for all tests available. Default = c("ddmm", "periodicity")
#' @param ... additional arguments to be passed to \code{\link{cd_ddmm}} and 
#' \code{\link{cd_round}} to customize test sensitivity.
#' @param value a character string.  Defining the output value. See value.
#' Default = \dQuote{dataset}.
#' @inheritParams cc_cap
#' 
#' @return Depending on the \sQuote{value} argument: 
#' \describe{
#'   \item{\dQuote{dataset}}{a \code{data.frame} with the
#'   the test summary statistics for each dataset in \code{x}}
#'   \item{\dQuote{clean}}{a \code{data.frame} containing only 
#'   records from datatsets in \code{x} that passed the tests}
#'   \item{\dQuote{flagged}}{a logical vector of the same length as
#'   rows in \code{x}, with TRUE = test passed and 
#'   FALSE = test failed/potentially problematic.}
#' }
#' 
#' @seealso \code{\link{cd_ddmm}} \code{\link{cd_round}}
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more details
#' and tutorials.
#' 
#' @keywords Coordinate cleaning wrapper
#' @family Wrapper functions
#' 
#' @examples
#' #Create test dataset
#' clean <- data.frame(dataset = rep("clean", 1000),
#'                     decimallongitude = runif(min = -43, max = -40, n = 1000),
#'                     decimallatitude = runif(min = -13, max = -10, n = 1000))
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
#' #run clean_dataset
#' flags <- clean_dataset(test)
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
#' @md
clean_dataset <- function(x,
                          lon = "decimallongitude", 
                          lat = "decimallatitude",
                          ds = "dataset",
                          tests = c("ddmm", "periodicity"),
                          value = "dataset",
                          verbose = TRUE,
                          ...) {

  # check input arguments
  match.arg(value, choices = c("dataset", "flagged", "clean"))

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
  if ("ddmm" %in% tests) {
    out_t1 <- cd_ddmm(
      x = dat, lon = lon, lat = lat, ds = ds, value = value2, 
      verbose = verbose, ...)
    if (value == "dataset") {
      names(out_t1) <- c("binomial.pvalue", "perc.difference", "pass.ddmm")
    }
  } else {
    if (value == "dataset") {
      out_t1 <- data.frame(pass.ddmm = rep(NA, length(test)))
    } else {
      out_t1 <- rep(NA, nrow(dat))
    }
  }

  # Run periodicity test
  if ("periodicity" %in% tests) {
    out_t2 <- cd_round(x,
      lon = lon, 
      lat = lat, 
      ds = ds, 
      value = value2,
      ...
    )
  } else {
    if (value == "dataset") {
      out_t2 <- data.frame(
        pass.periodicity.com = rep(NA, length(test)),
        pass.zero.com = rep(NA, length(test)),
        summary = NA
      )
    } else {
      out_t2 <- rep(NA, nrow(dat))
    }
  }

  # prepare output
  if (value == "dataset") {
    out <- data.frame(out_t1, out_t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    if("periodicity" %in% tests & "ddmm" %in% tests ){
      out$summary <- as.logical(out_t1$pass.ddmm) & as.logical(out_t2$summary)
    }else if("periodicity" %in% tests & !("ddmm" %in% tests)){
      out$summary <- out_t2$summary
    }else if(!("periodicity" %in% tests) & "ddmm" %in% tests ){
      out$summary <- as.logical(out_t1$pass.ddmm)
    }

    if (verbose) {
      message(sprintf("Flagged %s datasets.", sum(!out$summary)))
    }
  }
  if (value == "flagged") {
    out <- data.frame(ddmm = out_t1, periodicity = out_t2)
    out <- Filter(function(x) !all(is.na(x)), out)
    out$summary <- Reduce("&", out)

    if (verbose) {
      message(sprintf("Flagged %s records.", sum(!out$summary)))
    }
  }
  if (value == "clean") {
    out <- data.frame(ddmm = out_t1, periodicity = out_t2)
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

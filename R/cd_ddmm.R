#' Flag Datasets with a Degree Conversion Error
#' 
#' This test identifies datasets where a significant fraction of records has
#' been subject to a common degree minute to decimal degree conversion error,
#' where the degree sign is recognized as decimal delimiter.
#' 
#' If the degree sign is recognized as decimal delimiter during coordinate
#' conversion, no coordinate decimals above 0.59 (59') are possible. The test
#' here uses a binomial test to test if a significant proportion of records in
#' a dataset have been subject to this problem. The test is best adjusted via
#' the diff argument. The lower \code{diff}, the stricter the test. Also scales
#' with dataset size. Empirically, for datasets with < 5,000 unique coordinate
#' records \code{diff = 0.1} has proven reasonable flagging most datasets with
#' >25\% problematic records and all dataset with >50\% problematic records.
#' For datasets between 5,000 and 100,000 geographic unique records \code{diff
#' = 0.01} is recommended, for datasets between 100,000 and 1 M records diff =
#' 0.001, and so on.  See
#' \url{https://github.com/azizka/CoordinateCleaner/wiki/3.-Identifying-problematic-data-sets:-clean_dataset}
#' for explanation and simulation results.
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ds a character string. The column with the dataset of each record. In
#' case \code{x} should be treated as a single dataset, identical for all
#' records.  Default = \dQuote{dataset}.
#' @param pvalue numeric. The p-value for the one-sided t-test to flag the test
#' as passed or not. Both ddmm.pvalue and diff must be met. Default = 0.025.
#' @param diff numeric. The threshold difference for the ddmm test. Indicates
#' by which fraction the records with decimals below 0.6 must outnumber the
#' records with decimals above 0.6. Default = 1
#' @param min_span numeric. The minimum geographic extent of datasets to be
#' tested. Default = 2.
#' @param mat_size numeric. The size of the matrix for the binomial test. Must
#' be changed in decimals (e.g. 100, 1000, 10000). Adapt to dataset size,
#' generally 100 is better for datasets < 10000 records, 1000 is better for
#' datasets with 10000 - 1M records. Higher values also work reasonably well
#' for smaller datasets, therefore, default = 1000. For large datasets try
#' 10000.
#' @param value a character string.  Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @param diagnostic logical. If TRUE plots the analyses matrix for each
#' dataset.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' with summary statistics and flags for each dataset (\dQuote{dataset}) or a
#' \code{data.frame} containing the records considered correct by the test
#' (\dQuote{clean}) or a logical vector (\dQuote{flags}), with TRUE = test passed and FALSE =
#' test failed/potentially problematic. Default =
#' \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords "Coordinate cleaning" "Dataset level cleaning"
#' @examples
#' 
#' clean <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -180, 180), 
#'                 decimallatitude = runif(100, -90,90),
#'                 dataset = "FR")
#'                 
#' cd_ddmm(x = clean, value = "flagged")
#' 
#' #problematic dataset
#' lon <- sample(0:180, size = 100, replace = TRUE) + runif(100, 0,0.59)
#' lat <- sample(0:90, size = 100, replace = TRUE) + runif(100, 0,0.59)
#' 
#' prob <-  data.frame(species = letters[1:10], 
#'                 decimallongitude = lon, 
#'                 decimallatitude = lat,
#'                 dataset = "FR")
#'                 
#' cd_ddmm(x = prob, value = "flagged")
#' 
#' @export
#' @importFrom stats complete.cases binom.test
#' @importFrom raster plot raster
cd_ddmm <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    ds = "dataset",
                    pvalue = 0.025, 
                    diff = 1, 
                    mat_size = 1000, 
                    min_span = 2, 
                    value = "clean",
                    verbose = TRUE, 
                    diagnostic = FALSE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged", "dataset"))

  if (verbose) {
    message("Testing for dd.mm to dd.dd conversion errors")
  }

  # prepare dataset for analyses
  if (sum(!complete.cases(x[, c(lon, lat, ds)])) > 0) {
    warning(sprintf("ignored %s cases with incomplete data", 
                    sum(!complete.cases(x))))
  }

  # get function data
  dat <- x[complete.cases(x[, c(lon, lat, ds)]), ]

  if (nrow(dat) == 0) {
    stop("no complete cases found")
  }

  ## create test columns: decimal degrees long and lat
  dat$lon.test <- abs(dat[[lon]]) - floor(abs(dat[[lon]]))
  dat$lat.test <- abs(dat[[lat]]) - floor(abs(dat[[lat]]))

  # split into seperate datasets
  test <- split(dat, f = dat[[ds]])

  # run ddmm to dd.dd conversion test error at 0.6
  out <- lapply(test, function(k) {
    ## create test datasets
    dat_unique <- k[!duplicated(k[, c(lon, lat, ds)]), ]

    ## Test geographic span
    lon_span <- abs(max(dat_unique[, lon], na.rm = TRUE) - min(dat_unique[
      ,
      lon
    ], na.rm = TRUE))
    lat_span <- abs(max(dat_unique[, lat], na.rm = TRUE) - min(dat_unique[
      ,
      lat
    ], na.rm = TRUE))

    if (lon_span >= min_span & lat_span >= min_span) {
      # Assign decimals to a 100x100 matrix for binomial test
      cl <- ceiling(dat_unique[, c("lon.test", "lat.test")] * mat_size)
      cl$lat.test <- mat_size - cl$lat.test

      mat <- matrix(ncol = mat_size, nrow = mat_size)
      mat[cbind(cl$lat.test, cl$lon.test)] <- 1
      mat[is.na(mat)] <- 0
      dat_t1 <- mat

      # Binomial test, to see if more values are below 0.6 than expected
      P_smaller_than_06 <- 
        floor(0.599 * mat_size) * 
        floor(0.599 * mat_size) /
        mat_size^2 # 0.3481

      x_ind <- (mat_size - floor(0.599 * mat_size)):mat_size
      y_ind <- 1:floor(0.599 * mat_size)

      subt <- dat_t1[x_ind, y_ind] # subset tbl
      p06 <- sum(subt >= 1)
      pAll <- sum(dat_t1 >= 1)

      B <- stats::binom.test(p06, 
                             pAll, 
                             p = P_smaller_than_06, 
                             alternative = c("greater"))

      # P-VALUE
      v1 <- B$p.value
      # PERCENTAGE OF difference from expected
      v2 <- (B$estimate - P_smaller_than_06) / P_smaller_than_06

      # These two thresholds could be changed
      if (v1 < pvalue & v2 > diff) {
        flag_t1 <- FALSE
      } else {
        flag_t1 <- TRUE
      }

      outp <- c(round(v1, 4), round(v2, 3), flag_t1)

      # diagnostic plot of the decimal matrix
      if (diagnostic) {
        plo <- raster(dat_t1)
        raster::plot(plo)
      }
    } else {
      outp <- rep(NA, 3)
      warning("Geographic spann to small, check 'min_span'")
    }
    return(outp)
  })

  # Create output objects Reduce output to data.frame
  out_ds <- do.call("rbind.data.frame", out)
  rownames(out_ds) <- names(out)
  names(out_ds) <- c("binomial.pvalue", "perc.difference", "pass")

  flags <- x[[ds]] %in% c(rownames(out_ds[out_ds$pass == 1, ]), 
                          rownames(out_ds[is.na(out_ds$pass), ]))

  # return output dependent on value argument
  if (verbose) {
    message(sprintf("Flagged %s records", sum(!flags)))
  }

  switch(value, 
         dataset = return(out_ds), 
         clean = return(x[flags, ]), 
         flagged = return(flags))
}

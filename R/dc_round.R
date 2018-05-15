#' Flags Datasets with Rasterized Coordinates
#' 
#' Flags datasets with periodicity patterns indicative of a rasterized
#' (lattice) collection scheme, as often obtain from e.g. atlas data. Using a
#' combination of autocorrelation and sliding-window outlier detection to
#' identify periodicity patterns in the data.
#' 
#' see supplement %Copy later from supplementary material
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
#' @param T1 numeric.  The threshold for outlier detection in a in an
#' interquantile range based test. This is the major parameter to specify the
#' sensitivity of the test: lower values, equal higher detection rate. Values
#' between 7-11 are recommended. Default = 7.
#' @param reg.out.thresh numeric. Threshold on the number of equal distances
#' between outlier points.  See details.  Default = 2.
#' @param reg.dist.min numeric.  The minimum detection distance between
#' outliers in degrees (the minimum resolution of grids that will be flagged).
#' Default = 0.1.
#' @param reg.dist.max numeric.  The maximum detection distance between
#' outliers in degrees (the maximum resolution of grids that will be flagged).
#' Default = 2.
#' @param min.unique.ds.size numeric.  The minimum number of unique locations
#' (values in the tested column) for datasets to be included in the test.
#' Default = 4.
#' @param graphs logical. If TRUE, diagnostic plots are produced.  Default =
#' TRUE.
#' @param test character string.  Indicates which column to test. Either
#' \dQuote{lat} for latitude, \dQuote{lon} for longitude, or \dQuote{both} for
#' both.  In the latter case datasets are only flagged if both test are failed.
#' Default = \dQuote{both}
#' @param value a character string.  Defining the output value. See value.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' with summary statistics and flags for each dataset (\dQuote{dataset}) or a
#' \code{data.frame} containing the records considered correct by the test
#' (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE =
#' test failed/potentially problematic (\dQuote{flags}). Default =
#' \dQuote{clean}.
#' @note See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more
#' details and tutorials.
#' @keywords "Coordinate cleaning" "Dataset level cleaning"
#' @examples
#' 
#' #simulate bias grid, one degree resolution, 10% error on a 1000 records dataset
#'   ##simulate biased fraction of the data, grid resolution = 1 degree
#'   
#'   
#'   
#'   #simulate non-biased fraction of the data
#'   bi <- sample(3 + 0:5, size = 100, replace = TRUE)
#'   mu <- runif(3, 0, 15)
#'   sig <- runif(3, 0.1, 5)
#'   cl <- rnorm(n = 900, mean = mu, sd = sig)
#'   lon <- c(cl, bi)
#'   
#'   bi <- sample(9:13, size = 100, replace = TRUE)
#'   mu <- runif(3, 0, 15)
#'   sig <- runif(3, 0.1, 5)
#'   cl <- rnorm(n = 900, mean = mu, sd = sig)
#'   lat <- c(cl, bi)
#'   
#'   #add biased data
#'   
#'   inp <- data.frame(decimallongitude = lon,
#'                     decimallatitude = lat,
#'                     dataset = "test")
#'             
#'   #plot overview
#'   suma <- inp
#'   suma[,1:2] <- round(suma[,1:2], 0)
#'   suma <- aggregate(dataset ~ decimallongitude + decimallatitude, FUN = "length", data = suma)
#'   colo <- rev(heat.colors(max(suma$dataset)))
#'   plot(suma$decimallatitude ~ suma$decimallongitude, col = colo[suma$dataset])
#'           
#'   #run test
#'   
#'   dc_round(inp, value = "dataset")
#' 
#' @export
#' @importFrom stats complete.cases
#' @importFrom graphics title
dc_round <- function(x, 
                     lon = "decimallongitude", 
                     lat = "decimallatitude", 
                     ds = "dataset",
                     T1 = 7, 
                     reg.out.thresh = 2, 
                     reg.dist.min = 0.1, 
                     reg.dist.max = 2, 
                     min.unique.ds.size = 4,
                     graphs = TRUE, 
                     test = "both", 
                     value = "clean") {

  # fix operators which are tuned and people most likely do not want to change
  window.size <- 10
  detection.rounding <- 2
  detection.threshold <- 6
  digit.round <- 0
  nc <- 3000
  rarefy <- FALSE

  match.arg(value, choices = c("flags", "clean", "dataset"))

  if (length(unique(x[[ds]])) > 1) {
    dat <- split(x, f = x[[ds]])

    out <- lapply(dat, function(k) {
      tester <- k[complete.cases(k[, c(lon, lat)]), ]
      if (nrow(tester[!duplicated(tester[, c(lon, lat)]), ]) < 
          min.unique.ds.size) {
        warning("Dataset smaller than minimum test size")
        out <- data.frame(
          dataset = unique(x[[ds]]), n.outliers = NA,
          n.regular.outliers = NA, regular.distance = NA, summary = NA
        )
      } else {
        if (test == "lon") {
          # calculate autocorrelation
          gvec <- .CalcACT(
            data = k[[lon]], digit.round = digit.round,
            nc = nc, graphs = graphs, graph.title = unique(k[[ds]])
          )
          # run the sliding window outlier detection
          n.outl <- .OutDetect(gvec,
            T1 = T1, window.size = window.size,
            detection.rounding = detection.rounding, 
            detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl$flag <- !all(n.outl$n.outliers > 0, 
                              n.outl$regular.distance >= reg.dist.min, 
                              n.outl$regular.distance <= reg.dist.max, 
                              n.outl$n.regular.outliers >= reg.out.thresh)

          if (graphs) {
            title(paste(unique(k[[ds]]), n.outl$flag, sep = " - "))
          }
        }

        if (test == "lat") {
          gvec <- .CalcACT(
            data = k[[lat]], 
            digit.round = digit.round,
            nc = nc, graphs = graphs, 
            graph.title = unique(k[[ds]])
          )
          # run the sliding window outlier detection
          n.outl <- .OutDetect(gvec,
            T1 = T1, window.size = window.size,
            detection.rounding = detection.rounding, 
            detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl$flag <- !all(n.outl$n.outliers > 0, 
                              n.outl$regular.distance >= reg.dist.min, 
                              n.outl$regular.distance <= reg.dist.max, 
                              n.outl$n.regular.outliers >= reg.out.thresh)

          if (graphs) {
            title(paste(unique(k[[ds]]), n.outl$flag, sep = " - "))
          }
        }

        if (test == "both") {
          gvec1 <- .CalcACT(
            data = k[[lon]], digit.round = digit.round,
            nc = nc, graphs = graphs, graph.title = unique(k[[ds]])
          )
          n.outl.lon <- .OutDetect(gvec1,
            T1 = T1, window.size = window.size,
            detection.rounding = detection.rounding, 
            detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl.lon$flag <- !all(
            n.outl.lon$n.outliers > 0, n.outl.lon$regular.distance >=
              reg.dist.min, n.outl.lon$regular.distance <= reg.dist.max,
            n.outl.lon$n.regular.outliers >= reg.out.thresh
          )

          if (graphs) {
            title(paste(unique(k[[ds]]), n.outl.lon$flag, sep = " - "))
          }

          gvec2 <- .CalcACT(
            data = k[[lat]], digit.round = digit.round,
            nc = nc, graphs = graphs, graph.title = unique(k[[ds]])
          )
          n.outl.lat <- .OutDetect(gvec2,
            T1 = T1, window.size = window.size,
            detection.rounding = detection.rounding, 
            detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl.lat$flag <- !all(
            n.outl.lat$n.outliers > 0, n.outl.lat$regular.distance >=
              reg.dist.min, n.outl.lat$regular.distance <= reg.dist.max,
            n.outl.lat$n.regular.outliers >= reg.out.thresh
          )

          if (graphs) {
            title(paste(unique(k[[ds]]), n.outl.lat$flag, sep = " - "))
          }

          n.outl <- cbind(unique(k[[ds]]), n.outl.lon, n.outl.lat)
          names(n.outl) <- c(
            "dataset", 
            "lon.n.outliers", 
            "lon.n.regular.outliers",
            "lon.regular.distance", 
            "lon.flag", 
            "lat.n.outliers", 
            "lat.n.regular.outliers",
            "lat.regular.distance", "lat.flag"
          )

          n.outl$summary <- n.outl$lon.flag | 
            n.outl$lat.flag # only flag if both are flagged
        }

        return(n.outl)
      }
    })

    out <- do.call("rbind.data.frame", out)
  } else {
    if (nrow(x[!duplicated(x[, c(lon, lat)]), ]) < min.unique.ds.size) {
      warning("Dataset smaller than minimum test size")
      out <- data.frame(
        dataset = unique(x[[ds]]), 
        n.outliers = NA, 
        n.regular.outliers = NA,
        regular.distance = NA, summary = NA
      )
    } else {
      if (test == "lon") {
        # calculate autocorrelation
        gvec <- .CalcACT(
          data = x[[lon]], 
          digit.round = digit.round,
          nc = nc, 
          graphs = graphs, 
          graph.title = unique(x[[ds]])
        )
        # run the sliding window outlier detection
        n.outl <- .OutDetect(gvec,
          T1 = T1, window.size = window.size,
          detection.rounding = detection.rounding, 
          detection.threshold = detection.threshold,
          graphs = graphs
        )

        n.outl$flag <- !all(n.outl$n.outliers > 0, 
                            n.outl$regular.distance >= reg.dist.min, 
                            n.outl$regular.distance <= reg.dist.max, 
                            n.outl$n.regular.outliers >= reg.out.thresh)

        if (graphs) {
          title(paste(unique(x[[ds]]), n.outl$flag, sep = " - "))
        }

        n.outl <- data.frame(unique(x[[ds]]), n.outl)
        names(n.outl) <- c(
          "dataset", "lon.n.outliers", "lon.n.regular.distance",
          "lon.regular.distance", "summary"
        )
      }

      if (test == "lat") {
        gvec <- .CalcACT(
          data = x[[lat]], digit.round = digit.round,
          nc = nc, graphs = graphs, graph.title = unique(x[[ds]])
        )
        # run the sliding window outlier detection
        n.outl <- .OutDetect(gvec,
          T1 = T1, window.size = window.size,
          detection.rounding = detection.rounding, 
          detection.threshold = detection.threshold,
          graphs = graphs
        )

        n.outl$flag <- !all(n.outl$n.outliers > 0, 
                            n.outl$regular.distance >= reg.dist.min, 
                            n.outl$regular.distance <= reg.dist.max, 
          n.outl$n.regular.outliers >= reg.out.thresh)

        if (graphs) {
          title(paste(unique(x[[ds]]), n.outl$flag, sep = " - "))
        }

        n.outl <- data.frame(unique(x[[ds]]), n.outl)
        names(n.outl) <- c(
          "dataset", "lat.n.outliers", "lat.n.regular.distance",
          "lat.regular.distance", "summary"
        )
      }

      if (test == "both") {
        gvec1 <- .CalcACT(
          data = x[[lon]], digit.round = digit.round,
          nc = nc, graphs = graphs, graph.title = unique(x[[ds]])
        )

        n.outl.lon <- .OutDetect(gvec1,
          T1 = T1, window.size = window.size,
          detection.rounding = detection.rounding, 
          detection.threshold = detection.threshold,
          graphs = graphs
        )

        n.outl.lon$flag <- !all(
          n.outl.lon$n.outliers > 0, n.outl.lon$regular.distance >=
            reg.dist.min, n.outl.lon$regular.distance <= reg.dist.max,
          n.outl.lon$n.regular.outliers >= reg.out.thresh
        )

        if (graphs) {
          title(paste(unique(x[[ds]]), n.outl.lon$flag, sep = " - "))
        }


        gvec2 <- .CalcACT(
          data = x[[lat]], digit.round = digit.round,
          nc = nc, graphs = graphs, graph.title = unique(x[[ds]])
        )

        n.outl.lat <- .OutDetect(gvec2,
          T1 = T1, window.size = window.size,
          detection.rounding = detection.rounding, 
          detection.threshold = detection.threshold,
          graphs = graphs
        )


        n.outl.lat$flag <- !all(
          n.outl.lat$n.outliers > 0, n.outl.lat$regular.distance >=
            reg.dist.min, n.outl.lat$regular.distance <= reg.dist.max,
          n.outl.lat$n.regular.outliers >= reg.out.thresh
        )

        if (graphs) {
          title(paste(unique(x[[ds]]), n.outl.lat$flag, sep = " - "))
        }

        n.outl <- data.frame(unique(x[[ds]]), n.outl.lon, n.outl.lat)
        names(n.outl) <- c(
          "dataset", "lon.n.outliers", 
          "lon.n.regular.distance",
          "lon.regular.distance", 
          "lon.flag", 
          "lat.n.outliers", 
          "lat.n.regular.distance",
          "lat.regular.distance", 
          "lat.flag"
        )

        n.outl$summary <- n.outl$lon.flag | 
          n.outl$lat.flag # only flag if both are flagged
      }
      out <- n.outl
    }
  }

  switch(value, dataset = return(out), clean = return({
    test <- x[x[[ds]] %in% out[out$summary, "dataset"], ]
    if (length(test) < 0) {
      test
    } else {
      NULL
    }
  }), flags = return(x[[ds]] %in% out[out$summary, "dataset"]))
}

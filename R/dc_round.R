dc_round <- function(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset",
                     T1 = 7, reg.out.thresh = 2, reg.dist.min = 0.1, reg.dist.max = 2, min.unique.ds.size = 4,
                     graphs = TRUE, test = "both", value = "clean") {

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
      if (nrow(tester[!duplicated(tester[, c(lon, lat)]), ]) < min.unique.ds.size) {
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
            detection.rounding = detection.rounding, detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl$flag <- !all(n.outl$n.outliers > 0, n.outl$regular.distance >=
            reg.dist.min, n.outl$regular.distance <= reg.dist.max, n.outl$n.regular.outliers >=
            reg.out.thresh)

          if (graphs) {
            title(paste(unique(k[[ds]]), n.outl$flag, sep = " - "))
          }
        }

        if (test == "lat") {
          gvec <- .CalcACT(
            data = k[[lat]], digit.round = digit.round,
            nc = nc, graphs = graphs, graph.title = unique(k[[ds]])
          )
          # run the sliding window outlier detection
          n.outl <- .OutDetect(gvec,
            T1 = T1, window.size = window.size,
            detection.rounding = detection.rounding, detection.threshold = detection.threshold,
            graphs = graphs
          )

          n.outl$flag <- !all(n.outl$n.outliers > 0, n.outl$regular.distance >=
            reg.dist.min, n.outl$regular.distance <= reg.dist.max, n.outl$n.regular.outliers >=
            reg.out.thresh)

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
            detection.rounding = detection.rounding, detection.threshold = detection.threshold,
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
            detection.rounding = detection.rounding, detection.threshold = detection.threshold,
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
            "dataset", "lon.n.outliers", "lon.n.regular.outliers",
            "lon.regular.distance", "lon.flag", "lat.n.outliers", "lat.n.regular.outliers",
            "lat.regular.distance", "lat.flag"
          )

          n.outl$summary <- n.outl$lon.flag | n.outl$lat.flag # only flag if both are flagged
        }

        return(n.outl)
      }
    })

    out <- do.call("rbind.data.frame", out)
  } else {
    if (nrow(x[!duplicated(x[, c(lon, lat)]), ]) < min.unique.ds.size) {
      warning("Dataset smaller than minimum test size")
      out <- data.frame(
        dataset = unique(x[[ds]]), n.outliers = NA, n.regular.outliers = NA,
        regular.distance = NA, summary = NA
      )
    } else {
      if (test == "lon") {
        # calculate autocorrelation
        gvec <- .CalcACT(
          data = x[[lon]], digit.round = digit.round,
          nc = nc, graphs = graphs, graph.title = unique(x[[ds]])
        )
        # run the sliding window outlier detection
        n.outl <- .OutDetect(gvec,
          T1 = T1, window.size = window.size,
          detection.rounding = detection.rounding, detection.threshold = detection.threshold,
          graphs = graphs
        )

        n.outl$flag <- !all(n.outl$n.outliers > 0, n.outl$regular.distance >=
          reg.dist.min, n.outl$regular.distance <= reg.dist.max, n.outl$n.regular.outliers >=
          reg.out.thresh)

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
          detection.rounding = detection.rounding, detection.threshold = detection.threshold,
          graphs = graphs
        )

        n.outl$flag <- !all(n.outl$n.outliers > 0, n.outl$regular.distance >=
          reg.dist.min, n.outl$regular.distance <= reg.dist.max, n.outl$n.regular.outliers >=
          reg.out.thresh)

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
          detection.rounding = detection.rounding, detection.threshold = detection.threshold,
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
          detection.rounding = detection.rounding, detection.threshold = detection.threshold,
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
          "dataset", "lon.n.outliers", "lon.n.regular.distance",
          "lon.regular.distance", "lon.flag", "lat.n.outliers", "lat.n.regular.distance",
          "lat.regular.distance", "lat.flag"
        )

        n.outl$summary <- n.outl$lon.flag | n.outl$lat.flag # only flag if both are flagged
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
80

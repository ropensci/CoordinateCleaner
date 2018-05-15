is.spatialvalid <- function(x) {
  inherits(x, "spatialvalid")
}

plot.spatialvalid <- function(x, 
                              bgmap = NULL, 
                              clean = TRUE, 
                              details = TRUE,
                              pts.size = 1, 
                              font.size = 10, 
                              ...) {
  x <- data.frame(x)

  # prepare background
  e <- raster::extent(sp::SpatialPoints(
    x[, c("decimallongitude", "decimallatitude")])) + 1

  if (is.null(bgmap)) {
    bgmap <- CoordinateCleaner::landmass
    bgmap <- raster::crop(bgmap, e)
  }

  bgmap <- suppressWarnings(ggplot2::fortify(bgmap))

  # plot background
  plo <- ggplot2::ggplot() + 
    ggplot2::geom_polygon(data = bgmap, 
                          ggplot2::aes_string(
    x = "long",
    y = "lat", group = "group"), 
    fill = "grey60") + 
    ggplot2::coord_fixed() +
    ggplot2::theme_bw()

  # prepare occurence points
  inv <- x
  inv[, -c(1:2)] <- !inv[, -c(1:2)]
  occs <- names(inv)[unlist(lapply(apply(inv == 1, 1, "which"), "[", 1))]

  if (length(occs) == 0) {
    occs <- rep("AAAclean", nrow(x))
  } else {
    occs[is.na(occs)] <- "AAAclean"
  }

  occs <- cbind(x[, c("decimallongitude", "decimallatitude", "summary")],
    flag = occs
  )

  if (!"AAAclean" %in% occs$flag) {
    clean <- FALSE
    warnings("All records were flagged, setting clean to FALSE")
  }

  # add points to background
  if (!clean & !details) {
    pts <- occs[!occs$summary, ]
    plo <- plo + 
      ggplot2::geom_point(data = pts, ggplot2::aes_string(
        x = "decimallongitude",
        y = "decimallatitude"), 
        colour = "#F8766D", 
        size = pts.size) + 
      ggplot2::theme(axis.title = ggplot2::element_text(size = font.size))
  }

  if (clean & !details) {
    pts <- occs
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", colour = "summary"), 
      size = pts.size) + 
      ggplot2::scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                                   labels = c("Flagged", "Clean")) + 
      ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font.size), 
      legend.text = ggplot2::element_text(size = font.size)
    )
  }

  if (!clean & details) {
    pts <- occs[!occs$summary, ]
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", colour = "flag"
    ), size = pts.size) + ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font.size), 
      legend.text = ggplot2::element_text(size = font.size)
    )
  }

  if (clean & details) {
    pts <- occs
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", shape = "flag", colour = "flag"
    ), size = pts.size) +
      ggplot2::scale_colour_manual(
        values = c("#00BFC4", rep(
          "#F8766D",
          length(unique(pts$flag))
        )), breaks = sort(as.character(unique(pts$flag))),
        labels = c("clean", sort(as.character(unique(pts$flag)))[-1])
      ) +
      ggplot2::scale_shape_manual(
        values = c(16, seq(15, 15 + (length(unique(pts$flag)) - 1))), 
        breaks = sort(as.character(unique(pts$flag))), 
        labels = c("clean", sort(as.character(unique(pts$flag)))[-1])) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(size = font.size), 
        legend.text = ggplot2::element_text(size = font.size)
      )
  }
  plo
}

summary.spatialvalid <- function(object, ...) {
  out <- apply(object[, -c(1, 2)], 2, "!")
  out <- apply(out, 2, "sum")
  return(out)
}

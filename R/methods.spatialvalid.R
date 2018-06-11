#' @method is spatialvalid
#' @export
is.spatialvalid <- function(x) {
  inherits(x, "spatialvalid")
}


#' Plot Method for Class Spatialvalid
#' 
#' A set of plots to explore objects of the class \code{spatialvalid}. A plot
#' to visualize the flags from clean_coordinates
#' 
#' 
#' @param x an object of the class \code{spatialvalid} as from
#' \code{\link{clean_coordinates}}.
#' @param bgmap an object of the class \code{SpatialPolygonsDataFrame} used as
#' background map. Default = \code{\link{landmass}}
#' @param clean logical.  If TRUE, non-flagged coordinates are included in the
#' map.
#' @param details logical. If TRUE, occurrences are colour-coded by the type of
#' flag.
#' @param pts_size numeric. The point size for the plot.
#' @param font_size numeric. The font size for the legend and axes
#' @param \dots additional arguments passed to other methods
#' @return A plot of the records flagged as potentially erroneous by
#' \code{\link{clean_coordinates}}.
#' @seealso \code{\link{clean_coordinates}}
#' @keywords Visualisation
#' @examples
#' 
#' 
#' exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
#'                     decimallongitude = runif(250, min = 42, max = 51),
#'                     decimallatitude = runif(250, min = -26, max = -11))
#' 
#' test <- clean_coordinates(exmpl, species = "species", verbose = FALSE)
#' 
#' summary(test)
#' plot(test)
#' @export
#' @method plot spatialvalid
#' @importFrom raster crop
#' @importFrom ggplot2 fortify aes_string geom_polygon coord_fixed theme_bw theme element_text geom_point scale_colour_manual scale_shape_manual element_blank
plot.spatialvalid <- function(x, 
                              bgmap = NULL, 
                              clean = TRUE, 
                              details = FALSE,
                              pts_size = 1, 
                              font_size = 10, 
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
        size = pts_size) + 
      ggplot2::theme(axis.title = ggplot2::element_text(size = font_size))
  }

  if (clean & !details) {
    pts <- occs
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", colour = "summary"), 
      size = pts_size) + 
      ggplot2::scale_colour_manual(values = c("#F8766D", "#00BFC4"), 
                                   labels = c("Flagged", "Clean")) + 
      ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font_size), 
      legend.text = ggplot2::element_text(size = font_size)
    )
  }

  if (!clean & details) {
    pts <- occs[!occs$summary, ]
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", colour = "flag"
    ), size = pts_size) + ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font_size), 
      legend.text = ggplot2::element_text(size = font_size)
    )
  }

  if (clean & details) {
    pts <- occs
    plo <- plo + ggplot2::geom_point(data = pts, ggplot2::aes_string(
      x = "decimallongitude",
      y = "decimallatitude", shape = "flag", colour = "flag"
    ), size = pts_size) +
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
        axis.title = ggplot2::element_text(size = font_size), 
        legend.text = ggplot2::element_text(size = font_size)
      )
  }
  plo
}

#' @export
#' @method summary spatialvalid
summary.spatialvalid <- function(object, ...) {
  out <- apply(object[, -c(1, 2)], 2, "!")
  out <- apply(out, 2, "sum")
  return(out)
}

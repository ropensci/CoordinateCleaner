#' Identify Non-terrestrial Coordinates
#' 
#' Removes or flags coordinates outside the reference landmass. Can be used to
#' restrict datasets to terrestrial taxa, or exclude records from the open
#' ocean, when depending on the reference (see details). Often records of
#' terrestrial taxa can be found in the open ocean, mostly due to switched
#' latitude and longitude.
#'
#' In some cases flagging records close of the coastline is not recommendable,
#' because of the low precision of the reference dataset, minor GPS imprecision
#' or because a dataset might include coast or marshland species. If you only
#' want to flag records in the open ocean, consider using a buffered landmass
#' reference, e.g.: \code{\link{buffland}}.
#'
#' @param ref SpatVector (geometry: polygons). Providing the geographic
#'   gazetteer. Can be any SpatVector (geometry: polygons), but the structure
#'   must be identical to \code{\link{countryref}}. but the structure must be identical to
#'   rnaturalearth::ne_download(scale = 110, type = 'land', category =
#'   'physical', returnclass = 'sf'). Default = rnaturalearth::ne_download(scale = 110, type =
#'   'land', category = 'physical', returnclass = 'sf').
#' @param scale the scale of the default reference, as downloaded from natural
#'   earth. Must be one of 10, 50, 110. Higher numbers equal higher detail.
#'   Default = 110.
#' @param speedup logical. Using heuristic to speed up the analysis for large
#'   data sets with many records per location.
#' @inheritParams cc_cap
#'
#' @inherit cc_cap return
#'
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#'   details and tutorials.
#'
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(10, -30, 30), 
#'                 decimallatitude = runif(10, -30, 30))
#'                 
#' cc_sea(x, value = "flagged")
#' 
#' @export
#' @importFrom dplyr inner_join
#' @importFrom rnaturalearth ne_download
#'@importFrom terra extract vect geom geomtype

cc_sea <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   ref = NULL,
                   scale = 110,
                   value = "clean",
                   speedup = TRUE, 
                   verbose = TRUE){
  
  # check value argument
  match.arg(value, choices = c("clean", "flagged"))
  
  if (verbose) {
    message("Testing sea coordinates")
  }
  
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # heuristic to speedup - reduce to individual locations, 
  #overwritten later in cases speedup == FALSE
  inp <- x[!duplicated(x[, c(lon, lat)]), ]
  pts <- terra::vect(inp[, c(lon, lat)], 
                     geom = c(lon, lat),
                     crs = wgs84)
  
  # select and prepare terrestrial surface reference
  if (is.null(ref)) {
    if (!scale %in%  c(10, 50, 110)) {
      stop("scale must be one of c(10,50,110)")
    }
    ref <-
      try(suppressWarnings(terra::vect(
        rnaturalearth::ne_download(
          scale = scale,
          type = 'land',
          category = 'physical',
          load = TRUE,
          returnclass = "sf"
        )
      )),
      silent = TRUE)
    
    if (inherits(ref, "try-error")) {
      warning(sprintf("Gazetteer for land mass not found at\n%s",
                      rnaturalearth::ne_file_name(scale = scale,
                                                  type = 'land',
                                                  category = 'physical',
                                                  full_url = TRUE)))
      warning("Skipping sea test")
      switch(value, clean = return(x), flagged = return(rep(NA, nrow(x))))
    }
  } else {
    #Enable sf formatted custom references
    if (any(is(ref) == "Spatial") | inherits(ref, "sf")) {
      ref <- terra::vect(ref)
    }
    # Check if object is a SpatVector 
    if (!(inherits(ref, "SpatVector") & 
        terra::geomtype(ref) == "polygons")) {
      stop("ref must be a SpatVector with geomtype 'polygons'")
    }
    ref <- reproj(ref)
  }
  
  # run test
  if (speedup) {
    ## -----
    ## MDSumner@gmail.com 2020-05-06
    ## over() uses identicalCRS() and doesn't know that these are trivially 
    ## the same
    # +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs 
    # +proj=longlat +datum=WGS84 +no_defs 
    ## so if there's continuing trouble do this because it's always assumed
    ## the same as rnaturalearth anyway:
    ## #
    ## #suppressWarnings(sp::proj4string(pts) <- sp::CRS(sp::proj4string(ref)))
    ## #
    ## alternatively, do it above - get the proj4string(ref) and pass it into 
    ## SpatialPoints (the sf and sp won't transform for you bizarrely so you 
    ## have to make sure they are the same and in this case they already are
    ## -----
    ## point-in-polygon test
    out <- !is.na(terra::extract(ref, pts)[, 2])
    out <- data.frame(terra::geom(pts)[, c("x", "y")], out)
    colnames(out)[1:2] <- c(lon, lat)
    ## remerge with coordinates
    dum <- x
    dum$order <- seq_len(nrow(dum))
    out <- dplyr::inner_join(dum, out, by = c(lat,lon))
    out <- out[order(out$order), ]
    out <- out$out
  } else {
    pts <- terra::vect(x[, c(lon, lat)], 
                       geom = c(lon, lat),
                       crs = wgs84)
    
    # select relevant columns
    out <- !is.na(terra::extract(ref, pts)[, 2])
  }
  
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }
  
  switch(value, clean = return(x[out, ]), flagged = return(out))
}

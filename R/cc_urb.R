#' Identify Records Inside Urban Areas
#'
#' Removes or flags records from inside urban areas, based on a geographic
#' gazetteer. Often records from large databases span substantial time periods
#' (centuries) and old records might represent habitats which today are replaced
#' by city area.
#'
#'
#' @param ref a SpatVector. Providing the geographic gazetteer
#'   with the urban areas. See details. By default
#'   rnaturalearth::ne_download(scale = 'medium', type = 'urban_areas',
#'   returnclass = "sf"). Can be any \code{SpatVector}, but the
#'   structure must be identical to \code{rnaturalearth::ne_download()}.
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
#'
#' \dontrun{
#' x <- data.frame(species = letters[1:10],
#'                 decimalLongitude = runif(100, -180, 180),
#'                 decimalLatitude = runif(100, -90,90))
#'
#' cc_urb(x)
#' cc_urb(x, value = "flagged")
#' }
#'
#' @export
#' @importFrom terra vect crop project extract
#' @importFrom rnaturalearth ne_download ne_file_name

cc_urb <- function(x,
                   lon = "decimalLongitude",
                   lat = "decimalLatitude",
                   ref = NULL,
                   value = "clean",
                   verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing urban areas")
  }

  # check for reference data. 
  if (is.null(ref)) {
    message("Downloading urban areas via rnaturalearth")
    ref <-
      try(suppressWarnings(terra::vect(
        rnaturalearth::ne_download(scale = 'medium',
                                   type = 'urban_areas',
                                   returnclass = "sf")
      )),
      silent = TRUE)
    
    if (inherits(ref, "try-error")) {
      warning(sprintf("Gazetteer for urban areas not found at\n%s",
                      rnaturalearth::ne_file_name(scale = 'medium',
                                                  type = 'urban_areas',
                                                  full_url = TRUE)))
      warning("Skipping urban test")
      switch(value, clean = return(x), flagged = return(rep(NA, nrow(x))))
    }

  } else {
    # Enable sf formatted custom references
    if (any(is(ref) == c("Spatial"))  | inherits(ref, "sf")) {
      ref <- terra::vect(ref)
    }
    # Check if object is a SpatVector 
    if (!(inherits(ref, "SpatVector") & 
        terra::geomtype(ref) == "polygons")) {
      stop("ref must be a SpatVector with geomtype 'polygons'")
    }
    ref <- reproj(ref)
  }

  # Prepare input points and extent
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

  dat <- terra::vect(x[, c(lon, lat)],
                     geom = c(lon, lat),
                     crs = wgs84)
  limits <- terra::ext(dat) + 1
  ref <- terra::crop(ref, limits)
  ref <- terra::project(ref, wgs84)
  
  # test if any points fall within the buffer in case no urban areas are found
  # in the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    #point in polygon test
    ext_dat <- terra::extract(ref, dat)
    out <- is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])
  }

  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

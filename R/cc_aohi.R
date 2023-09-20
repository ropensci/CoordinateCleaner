#' Identify Coordinates in Artificial Hotspot Occurrence Inventory
#' 
#' Removes or flags records within Artificial Hotspot Occurrence Inventory.
#' Poorly geo-referenced occurrence records in biological databases are often
#' erroneously geo-referenced to highly recurring coordinates that were assessed
#' by Park et al 2022. See the reference for more details.
#' 
#' 
#' @inheritParams cc_cap
#' @param taxa Artificial Hotspot Occurrence Inventory (AHOI) were created based
#'   on four different taxa, birds, insecta, mammalia, and plantae. Users can
#'   choose to keep all, or any specific taxa subset to define the AHOI locations.
#'   Default is to keep all: c("Aves", "Insecta", "Mammalia", "Plantae").
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @references Park, D. S., Xie, Y., Thammavong, H. T., Tulaiha, R., & Feng, X.
#'   (2023). Artificial Hotspot Occurrence Inventory (AHOI). Journal of
#'   Biogeography, 50, 441–449. https://doi.org/10.1111/jbi.14543
#' 
#' @examples
#' 
#' x <- data.frame(species = letters[1:10], 
#'                 decimalLongitude = c(runif(99, -180, 180), -47.92), 
#'                 decimalLatitude = c(runif(99, -90,90), -15.78))
#' 
#' @export
#' @importFrom geosphere destPoint
#' @importFrom terra vect ext crop buffer geom
#' @importFrom utils data

cc_aohi <- function(x, 
                   lon = "decimalLongitude", 
                   lat = "decimalLatitude", 
                   species = "species",
                   taxa = c("Aves", "Insecta", "Mammalia", "Plantae"),
                   buffer = 10000,
                   geod = TRUE,
                   value = "clean", 
                   verbose = TRUE) {
  
  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing Artificial Hotspot Occurrence Inventory")
  }
  if (buffer > 10 & !geod) {
    warnings("Using large buffer check 'geod'")
  }
  if (buffer < 100 & geod) {
    warnings("Using small buffer check 'geod'")
  }
  
  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # select relevant columns
  dat <- terra::vect(x[, c(lon, lat), drop = FALSE], 
                     geom = c(lon, lat),
                     crs = wgs84)
  
  # Load ref
  aohi <- base::get(data("aohi"))
  aohi <- aohi[aohi$taxa %in% taxa, ]
  lon_lat <- c("decimalLongitude", "decimalLatitude")
  ref <- terra::vect(aohi[, lon_lat],
                    geom = lon_lat,
                    crs = wgs84)
  
  if (geod) {
    # credits to https://seethedatablog.wordpress.com
    dg <- seq(from = 0, to = 360, by = 5)
    
    buff_XY <-
      geosphere::destPoint(
        p = terra::geom(ref)[, c("x", "y")],
        b = rep(dg, each = length(ref)),
        d = buffer
      )
    
    id <- rep(seq_along(ref), times = length(dg))
    
    
    lst <- split(data.frame(buff_XY), f = id)
    
    # Make SpatialPolygons out of the list of coordinates
    lst <- lapply(lst, as.matrix)
    ref <-
      sapply(lst, terra::vect, crs = wgs84, type = "polygons")
    ref <- Reduce(rbind, ref)
    
    #point in polygon test
    ext_dat <- terra::extract(ref, dat)
    out <- is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])
  } else {
    ref_buff <- terra::buffer(ref, buffer)
    # There is a weird bug in terra, so I did this work around
    ref <- terra::vect(stats::na.omit(terra::geom(ref_buff)), 
                       type = "polygon", crs = ref)
    terra::values(ref) <- terra::values(ref_buff)
    
    ext_dat <- terra::extract(ref, dat)
    out <- is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])
  }
  
  # create output based on value argument
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }
  
  switch(value, clean = return(x[out, ]), flagged = return(out))
}

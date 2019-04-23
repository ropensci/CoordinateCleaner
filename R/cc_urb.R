#' Identify Records Inside Urban Areas
#'
#' Removes or flags records from inside urban areas, based on a geographic gazetteer.
#' Often records from large databases span substantial time periods (centuries)
#' and old records might represent habitats which today are replaced by city
#' area.
#'
#'
#' @param ref a SpatialPolygonsDataFrame. Providing the geographic gazetteer
#' with the urban areas. See details. By default 
#' rnaturalearth::ne_download(scale = 'medium', type = 'urban_areas').
#' Can be any \code{SpatialPolygonsDataframe}, but the structure must be
#' identical to \code{rnaturalearth::ne_download()}.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#'
#' \dontrun{
#' x <- data.frame(species = letters[1:10],
#'                 decimallongitude = runif(100, -180, 180),
#'                 decimallatitude = runif(100, -90,90))
#'
#' cc_urb(x)
#' cc_urb(x, value = "flagged")
#' }
#'
#' @export
#' @importFrom sp CRS SpatialPoints "proj4string<-" over proj4string
#' @importFrom raster extent crop
#' @importFrom rnaturalearth ne_download ne_file_name
cc_urb <- function(x,
                   lon = "decimallongitude",
                   lat = "decimallatitude",
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
    ref <- try(suppressWarnings(rnaturalearth::ne_download(scale = 'medium', 
                                                           type = 'urban_areas')), 
               silent = TRUE)
    
    if(class(ref) == "try-error"){
      warning(sprintf("Gazetteer for urban areas not found at\n%s",
                      rnaturalearth::ne_file_name(scale = 'medium',
                                                  type = 'urban_areas',
                                                  full_url = TRUE)))
      warning("Skipping urban test")
      switch(value, clean = return(x), flagged = return(rep(NA, nrow(x))))
    }

    sp::proj4string(ref) <- ""
  } else {
    #Enable sf formatted custom references
    if(!any(is(ref) == "Spatial")){
      ref <- as(ref, "Spatial")
    }
    ref <- reproj(ref)
  }

  # Prepare input points and extent
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  dat <- sp::SpatialPoints(x[, c(lon, lat)], proj4string = CRS(wgs84))
  limits <- raster::extent(dat) + 1
  ref <- raster::crop(ref, limits)
  proj4string(ref) <- wgs84
  
  # test if any points fall within the buffer incase no urban areas are found in
  # the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    out <- is.na(sp::over(x = dat, y = ref)[, 1])
  }

  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

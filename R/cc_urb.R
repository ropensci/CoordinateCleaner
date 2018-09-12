#' Flag Records Inside Urban Areas
#'
#' Flags records from inside urban areas, based on a geographic gazetteer.
#' Often records from large databases span substantial time periods (centuries)
#' and old records might represent habitats which today are replaced by city
#' area.
#'
#' By default rnaturalearth::ne_download will be used to download reference data, if no reference is provided by the user.
#' User-provided references can be any \code{SpatialPolygonsDataframe}, but the structure must be
#' identical to \code{rnaturalearth::ne_download(scale = 'medium', type = 'urban_areas')}.
#'
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param ref a SpatialPolygonsDataFrame. Providing the geographic gazetteer
#' with the urban areas. See details.
#' @param value a character string. Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' @keywords Coordinate cleaning
#' @family Coordinates
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
    message("No reference for urban areas found. 
            Using rnaturalearth to download.")
    ref <- rnaturalearth::ne_download(scale = 'medium', type = 'urban_areas')
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
  
  dat <- sp::SpatialPoints(x[, c(lon, lat)], proj4string = CRS(egs84))
  limits <- raster::extent(dat) + 1
  ref <- raster::crop(ref, limits)

  # test if any points fall within the buffer incase no urban areas are found in
  # the study area
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    out <- is.na(sp::over(x = dat, y = ref)[, 1])
  }

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

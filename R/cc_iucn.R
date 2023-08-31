#' Identify Records Outside Natural Ranges
#' 
#' Removes or flags records outside of the provided natural range polygon, on a per species basis. 
#' Expects one entry per species. See the example or 
#' \url{https://www.iucnredlist.org/resources/spatial-data-download} for 
#' the required polygon structure. 
#' 
#' Download natural range maps in suitable format for amphibians, birds,
#' mammals and reptiles
#' from \url{https://www.iucnredlist.org/resources/spatial-data-download}.
#' Note: the buffer radius is in degrees, thus will differ slightly between
#' different latitudes. 
#' 
#' @param range a SpatVector of natural ranges for species in x. 
#' Must contain a column named as indicated by \code{species}. See details.  
#' @param species a character string. The column with the species name. 
#' Default = \dQuote{species}.
#' @param buffer numerical. The buffer around each species' range,
#' from where records should be flagged as problematic, in meters. Default = 0.
#' @inheritParams cc_cap
#' 
#' @inherit cc_cap return
#' 
#' @note See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' library(terra)
#' 
#' x <- data.frame(species = c("A", "B"),
#' decimalLongitude = runif(100, -170, 170),
#' decimalLatitude = runif(100, -80,80))
#'
#' range_species_A <- cbind(c(-45,-45,-60,-60,-45), c(-10,-25,-25,-10,-10))
#' rangeA <- terra::vect(range_species_A, "polygons")
#' range_species_B <- cbind(c(15,15,32,32,15), c(10,-10,-10,10,10))
#' rangeB <- terra::vect(range_species_B, "polygons")
#' range <- terra::vect(list(rangeA, rangeB))
#' range$binomial <- c("A", "B")
#'
#' cc_iucn(x = x, range = range, buffer = 0)
#' 
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom terra vect buffer extract geomtype subset crs

cc_iucn <- function(x,
                     range,
                     lon = "decimalLongitude", 
                     lat = "decimalLatitude",
                     species = "species",
                     buffer = 0,
                     value = "clean",
                     verbose = TRUE){
  
  # Check value argument
  match.arg(value, choices = c("clean", "flagged"))
  
  if (verbose) {
    message("Testing natural ranges")
  }
  
  if (any(is(range) == "Spatial")  | inherits(range, "sf")) {
    range <- terra::vect(range)
  }
  # Check if object is a SpatVector 
  if (!(inherits(range, "SpatVector") & 
      terra::geomtype(range) == "polygons")) {
    stop("ref must be a SpatVector with geomtype 'polygons'")
  }
  
  # Prepare shape file
  ## Adapt to iucn polygons
  if("binomial" %in% names(range) &
     !species %in% names(range) &
     species %in% names(x)) {
    names(range)[names(range) == "binomial"] <- species
  }
  
  ## Reduce to species in dataset
  test_range <- range[[species]][, 1] %in% unique(unlist(x[, species]))
  range <- terra::subset(range, test_range)
  # Split by species
  dat <- data.frame(x, order = rownames(x))
  dat <- split(dat, f = dat[, species])
  
  # Apply buffer to ranges
  if (buffer != 0) {
    range <- terra::buffer(range, width = buffer)
  } 
  
  # Check projection of ranges
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
  
  if (terra::crs(range) == "") {
    warning("no projection information for reference found, 
              assuming '+proj=longlat +datum=WGS84 +no_defs'")
    terra::crs(range) <- wgs84
  }else if(terra::crs(range) != terra::crs(wgs84)) {
    range <- terra::project(range, wgs84)
    warning("reprojecting reference to '+proj=longlat +datum=WGS84 +no_defs'")
  }
  
  # Point-in-polygon-test
  out <- lapply(dat, function(k){
    if (unique(k[, species]) %in% range[[species]][, 1]) {
      sub <- terra::vect(k[, c(lon, lat)], 
                         crs = wgs84, 
                         geom = c(lon, lat))
      test_range_sub <- range[[species]][, 1] == unique(k[, species])
      range_sub <- terra::subset(range, test_range_sub)
      #point in polygon test
      ext_dat <- terra::extract(range_sub, sub)
      flag <- !is.na(ext_dat[!duplicated(ext_dat[, 1]), 2])

      data.frame(order = k$order,
                 flag = flag)
    }else{
      data.frame(order = k$order,
                 flag = TRUE)
    }
  })

  out <- dplyr::bind_rows(out)
  out <- out[order(as.numeric(as.character(out$order))), ]

  # Warning for species not in range
  tester <- unique(unlist(x[, species]))
  if(sum(!tester %in% range[[species]][, 1]) > 0){
    miss <- tester[!tester %in% range[[species]][, 1]]
    warning(sprintf("species not found in range and not tested %s\n", miss))
  }

  # Generate output
  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out$flag)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out$flag)))
    }
  }
  
  switch(value, clean = return(x[out$flag, ]), 
         flagged = return(out$flag))
}


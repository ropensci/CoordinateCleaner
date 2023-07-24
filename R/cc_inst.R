#'Identify Records in the Vicinity of Biodiversity Institutions
#'
#'Removes or flags records assigned to the location of zoos, botanical gardens,
#'herbaria, universities and museums, based on a global database of ~10,000 such
#'biodiversity institutions. Coordinates from these locations can be related to
#'data-entry errors, false automated geo-reference or individuals in
#'captivity/horticulture.
#'
#'Note: the buffer radius is in degrees, thus will differ slightly between
#'different latitudes.
#'
#'@param buffer numerical. The buffer around each institution, where records
#'  should be flagged as problematic, in decimal degrees.  Default = 100m.
#'@param ref  SpatVector (geometry: polygons). Providing the geographic
#'  gazetteer. Can be any SpatVector (geometry: polygons), but the structure
#'  must be identical to \code{\link{institutions}}.  Default =
#'  \code{\link{institutions}}
#'@param verify logical. If TRUE, records close to institutions are only
#'  flagged, if there are no other records of the same species in the greater
#'  vicinity (a radius of buffer * verify_mltpl).
#'@param verify_mltpl numerical. indicates the factor by which the radius for
#'  verify exceeds the radius of the initial test. Default = 10, which might be
#'  suitable if geod is TRUE, but might be too large otherwise.
#'@inheritParams cc_cap
#'
#'@inherit cc_cap return
#'
#'@keywords Coordinate cleaning
#'@family Coordinates
#' @examples
#'
#' x <- data.frame(species = letters[1:10],
#'                 decimallongitude = c(runif(99, -180, 180), 37.577800),
#'                 decimallatitude = c(runif(99, -90,90), 55.710800))
#'
#'#large buffer for demonstration, using geod = FALSE for shorter runtime
#' cc_inst(x, value = "flagged", buffer = 10, geod = FALSE)
#'
#' \dontrun{
#' #' cc_inst(x, value = "flagged", buffer = 50000) #geod = T
#' }
#'
#'@export
#'@importFrom geosphere destPoint
#'@importFrom terra extract vect geom buffer

cc_inst <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude",
                    species = "species",
                    buffer = 100,
                    geod = TRUE,
                    ref = NULL, 
                    verify = FALSE,
                    verify_mltpl = 10,
                    value = "clean", 
                    verbose = TRUE) {

  # check value argument
  match.arg(value, choices = c("clean", "flagged"))

  if (verbose) {
    message("Testing biodiversity institutions")
  }
  
  if (buffer > 10 & !geod) {
    warnings("Using large buffer check 'geod'")
  }
  if (buffer < 100 & geod) {
    warnings("Using small buffer check 'geod'")
  }
  
  # set default projection
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

  dat <- terra::vect(x[, c(lon, lat)],
                     geom = c(lon, lat),
                     crs = wgs84)
  
  # prepare reference dataset
  if (is.null(ref)) {
    ref <- CoordinateCleaner::institutions
    ref <- ref[!is.na(ref$decimallongitude) & !is.na(ref$decimallatitude), ]
  }
  limits <- terra::ext(dat) + buffer

  # subset of test datset according to limits
  ref <- terra::crop(terra::vect(
    ref[, c("decimallongitude", "decimallatitude")],
    geom = c("decimallongitude", "decimallatitude"),
    crs = wgs84), limits)
  
  # test reference data after limiting and do test in case no bdinstitutions
  if (is.null(ref)) {
    out <- rep(TRUE, nrow(x))
  } else {
    if (geod) {
      # credits to https://seethedatablog.wordpress.com
      dg <- seq(from = 0, to = 360, by = 5)
      
      buff_XY <- geosphere::destPoint(p = terra::geom(ref)[, c("x", "y")], 
                                      b = rep(dg, each = length(ref)), 
                                      d = buffer)
      
      
      id <- rep(seq_along(ref), times = length(dg))
      lst <- split(data.frame(buff_XY), f = id)
      
      # Make SpatialPolygons out of the list of coordinates
      lst <- lapply(lst, as.matrix)
      ref <- lapply(lst, terra::vect, crs = wgs84, type = "polygons")
      ref <- Reduce(rbind, ref)
      
      # Point in polygon test
      out <- terra::extract(ref, dat)
      out <- out[!duplicated(out[, 1]), ]
      out <- is.na(out[, 2])
    } else {
      ref <- terra::buffer(ref, width = buffer)
      out <- terra::extract(ref, dat)
      out <- out[!duplicated(out[, 1]), ]
      out <- is.na(out[, 2])
    }
  }
  
  # double check flagged records, for records from the 
  # same species in the greater surroundings
  if (verify) {
    #identify flagged records
    ref_in <- x[!out, ]
    
    if (nrow(ref_in) > 0) {
      #buffer with a larger buffer than for the initial test
      if (geod) {
        # credits to https://seethedatablog.wordpress.com
        dg <- seq(from = 0, to = 360, by = 5)
        
        buff_XY <- geosphere::destPoint(p = ref_in[, c(lon, lat)],
                                        b = rep(dg, each = nrow(ref_in)),
                                        d = buffer * verify_mltpl)
        
        id <- rep(seq(from = 1, to = nrow(ref_in)), times = length(dg))
        lst <- split(data.frame(buff_XY), f = id)
        
        # Make SpatialPolygons out of the list of coordinates
        lst <- lapply(lst, as.matrix)
        
        poly <- lapply(lst, terra::vect, crs = wgs84, type = "polygons")
        ref <- Reduce(rbind, poly)
        ref$species <- ref_in[, species]
      } else {
        ref <- terra::vect(ref_in, geom = c(lon, lat))
        ref <- terra::buffer(ref, 
                              width = buffer * verify_mltpl)
        ref$species <- ref_in[, species]
      }
      
      #identify all records from flagged species in x
      f_spec <- x[x[, species] %in% ref$species,]
      
      dbch_flag <- c()
      
      for (i in seq_len(nrow(ref_in))) {
        dbch <- terra::extract(terra::vect(
          f_spec[unlist(f_spec[species]) == ref$species[i], ],
          geom = c(lon, lat),
          crs = wgs84), 
          terra::subset(ref, seq_len(length(ref)) == i))
        
        #check if there area other records of this species in the buffer
        dbch_flag[[i]] <- sum(!is.na(dbch[species])) > 
          nrow(ref_in[ref_in[[species]] == ref_in[[i, species]] &
                        ref_in[[lon]] ==  ref_in[[i, lon]] &
                        ref_in[[lat]] ==  ref_in[[i, lat]],])
      }
      
      #unflag those records with other records of the same species nearby
      out[rownames(ref_in)] <- unlist(dbch_flag)
    }
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

#' Flag Records Outside Natural Ranges
#' 
#' Flags records outside of the provided natural range polygon, on a per species basis. 
#' Expects one entry per species. See the example or 
#' \url{http://www.iucnredlist.org/technical-documents/spatial-data} for 
#' the required polygon structure. 
#' 
#' Download natural range maps in suitable format for amphibians, birds,
#' mammals and reptiles
#' from \url{http://www.iucnredlist.org/technical-documents/spatial-data}.
#' Note: the buffer radius is in degrees, thus will differ slightly between
#' different latitudes. 
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param range a SpatialPolygonsDataFrame of natural ranges for species in x. 
#' Must contain a column named as indicated by \code{species}. See details.  
#' @param lon a character string. The column with the longitude coordinates.
#' Default = \dQuote{decimallongitude}.
#' @param lat a character string. The column with the latitude coordinates.
#' Default = \dQuote{decimallatitude}.
#' @param species a character string. The column with the species name. 
#' Default = \dQuote{species}.
#' @param buffer numerical. The buffer around each species' range,
#' from where records should be flagged as problematic, in decimal
#' degrees. Default = 0.
#' @param value a character string. Defining the output value. See value.
#' @param verbose logical. If TRUE reports the name of the test and the number
#' of records flagged.
#' @return Depending on the \sQuote{value} argument, either a \code{data.frame}
#' containing the records considered correct by the test (\dQuote{clean}) or a
#' logical vector (\dQuote{flagged}), with TRUE = test passed and FALSE = test failed/potentially
#' problematic. Default = \dQuote{clean}.
#' @note See \url{https://azizka.github.io/CoordinateCleaner} for more
#' @keywords Coordinate cleaning
#' @family Coordinates
#' @examples
#' require(sp)
#' 
#' x <- data.frame(species = c("A", "B"),
#' decimallongitude = runif(100, -170, 170),
#' decimallatitude = runif(100, -80,80))
#'
#' range_species_A <- Polygon(cbind(c(-45,-45,-60,-60,-45),c(-10,-25,-25,-10,-10)))
#' range_species_B <- Polygon(cbind(c(15,15,32,32,15),c(10,-10,-10,10,10)))
#' range_A <- Polygons(list(range_species_A), ID = c("A"))
#' range_B <- Polygons(list(range_species_B), ID = c("B"))
#' range <- SpatialPolygons(list(range_A, range_B))
#' df <- data.frame(species = c("A", "B"), row.names = c("A", "B"))
#' range <- SpatialPolygonsDataFrame(range, data = as.data.frame(df))
#'
#' cc_iucn(x = x, range = range, buffer = 10)
#' 
#' @export
#' @importFrom rgeos gBuffer
#' @importFrom dplyr bind_rows
#' @importFrom sp CRS over Polygon Polygons proj4string SpatialPoints SpatialPolygons spTransform
cc_iucn <- function(x,
                     range,
                     lon = "decimallongitude", 
                     lat = "decimallatitude",
                     species = "species",
                     buffer = 0,
                     value = "clean",
                     verbose = TRUE){
  
  # Check value argument
  match.arg(value, choices = c("clean", "flagged"))
  
  if (verbose) {
    message("Testing natural ranges")
  }
  
  # Prepare shape file
  ## Adapt to iucn polygons
  if("binomial" %in% names(range@data) &
     !species %in% names(range@data) &
     species %in% names(x)){
    names(range@data)[names(range@data) == "binomial"] <- species
  }
  
  ## Reduce to species in dataset
  range <- range[range@data[, species] %in% unique(unlist(x[, species])),]
  
  # Split by species
  dat <- data.frame(x, order = rownames(x))
  dat <- split(dat, f = dat[, species])
  
  # Apply buffer to ranges
  if(buffer != 0){
    range <-  rgeos::gBuffer(range, byid = TRUE, width = buffer)
  } 
  
  # Check projection of ranges
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  if(is.na(sp::proj4string(range))){
    warning("no projection information for reference found, 
              assuming '+proj=longlat +datum=WGS84 
              +no_defs +ellps=WGS84 +towgs84=0,0,0'")
    sp::proj4string(range) <- sp::CRS(wgs84)
  }else if(sp::proj4string(range) != wgs84){
    range <- sp::spTransform(range, sp::CRS(wgs84))
    warning("reprojecting reference to '+proj=longlat 
              +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'")
  }
  
  # Point-in-polygon-test
  out <- lapply(dat, function(k){
    if(unique(k[, species]) %in% range@data[, species]){
      sub <- sp::SpatialPoints(k[, c(lon, lat)], proj4string = sp::CRS(wgs84))
      range_sub <- range[range@data[, species] == unique(k[, species]),]
      
      data.frame(order = k$order,
                 flag = !is.na(sp::over(x = sub, y = range_sub)[, species]))
    }else{
      data.frame(order = k$order,
                 flag = TRUE)
    }
  })

  out <- dplyr::bind_rows(out)
  out <- out[order(as.numeric(as.character(out$order))), ]

  # Warning for species not in range
  tester <- unique(unlist(x[, species]))
  if(sum(!tester %in% range@data[, species]) > 0){
    miss <- tester[!tester %in% range@data[, species]]
    warning(sprintf("species not found in range and not tested %s\n", miss))
  }

  # Generate output
  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out$flag)))
  }
  
  switch(value, clean = return(x[out$flag, ]), 
         flagged = return(out$flag))
}

#' Identify Coordinates Outside their Reported Country
#' 
#' Removes or flags mismatches between geographic coordinates and additional country
#' information (usually this information is reliably reported with specimens).
#' Such a mismatch can occur for example, if latitude and longitude are
#' switched.
#' 
#' 
#' @param iso3 a character string. The column with the country assignment of
#' each record in three letter ISO code. Default = \dQuote{countrycode}.
#' @param ref a SpatialPolygonsDataFrame. Providing the geographic gazetteer.
#' Can be any SpatialPolygonsDataFrame, but the structure must be identical to
#' \code{rnaturalearth::ne_countries(scale = "medium")}.  
#' Default = \code{rnaturalearth::ne_countries(scale = "medium")}
#' @param ref_col the column name in the reference dataset, containing the relevant
#' ISO codes for matching. Default is to "iso_a3_eh" which referes to the ISO-3
#' codes in the reference dataset. See notes.
#' @inheritParams cc_cen
#' 
#' @inherit cc_cap return
#' 
#' @note The ref_col argument allows to adapt the function to the structure of
#' alternative reference datasets. For instance, for 
#' \code{rnaturalearth::ne_countries(scale = "small")}, the default will fail, 
#' but ref_col = "iso_a3" will work.
#' 
#' @note With the default reference, records are flagged if they fall 
#' outside the terrestrial territory of countries, hence records in territorial waters might be flagged. 
#' See \url{https://ropensci.github.io/CoordinateCleaner/} for more
#' details and tutorials.
#' 
#' @keywords Coordinate cleaning
#' @family Coordinates
#' 
#' @examples
#' 
#' \dontrun{
#' x <- data.frame(species = letters[1:10], 
#'                 decimallongitude = runif(100, -20, 30), 
#'                 decimallatitude = runif(100, 35,60),
#'                 countrycode = "RUS")
#' 
#' cc_coun(x, value = "flagged")#non-terrestrial records are flagged as wrong. 
#' }
#' 
#' @export
#' @importFrom sp CRS SpatialPoints "proj4string<-" over
#' @importFrom raster crop
cc_coun <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    iso3 = "countrycode",
                    value = "clean", 
                    ref = NULL, 
                    ref_col = "iso_a3_eh",
                    verbose = TRUE) {

  # check function arguments for validity
  match.arg(value, choices = c("clean", "flagged"))
  if (!iso3 %in% names(x)) {
    stop("iso3 argument missing, please specify")
  }

  if (verbose) {
    message("Testing country identity")
  }

  # set reference and check for dependency
  if (is.null(ref)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("Install the 'rnaturalearth' package or provide a custom reference",
        call. = FALSE
      )
    }
    ref <- rnaturalearth::ne_countries(scale = "medium")
    sp::proj4string(ref) <- ""
  } else {
    #Check projection of custom reference and reproject if necessary
    ref <- reproj(ref)
  }
  
  # prepare data
  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  sp::proj4string(ref) <- ""
  ref <- raster::crop(ref, raster::extent(dat) + 1)

  # get country from coordinates and compare with provided country
  country <- sp::over(x = dat, y = ref)[, ref_col]
  out <- as.character(country) == as.character(unlist(x[, iso3]))
  out[is.na(out)] <- FALSE # marine records are set to False

  # return output
  if (verbose) {
    if(value == "clean"){
      message(sprintf("Removed %s records.", sum(!out)))
    }else{
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }

  switch(value, clean = return(x[out, ]), flagged = return(out))
}

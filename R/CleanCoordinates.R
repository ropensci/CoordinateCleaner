#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
#' 
CleanCoordinates <- function(x, 
                             lon = "decimallongitude", 
                             lat = "decimallatitude",
                             species = "species", 
                             countries = NULL, 
                             capitals = TRUE, 
                             centroids = TRUE,
                             countrycheck = FALSE, 
                             duplicates = FALSE, 
                             equal = TRUE, 
                             GBIF = TRUE, 
                             institutions = TRUE,
                             outliers = FALSE, 
                             seas = TRUE, 
                             urban = FALSE, 
                             zeros = TRUE, 
                             capitals.rad = 0.05,
                             centroids.rad = 0.01, 
                             centroids.detail = "both", 
                             inst.rad = 0.001, 
                             outliers.method = "quantile",
                             outliers.mtp = 3, 
                             outliers.td = 1000, 
                             outliers.size = 7, 
                             zeros.rad = 0.5,
                             capitals.ref, 
                             centroids.ref, 
                             country.ref, 
                             inst.ref, 
                             seas.ref, 
                             urban.ref,
                             value = "spatialvalid", 
                             verbose = TRUE, 
                             report = FALSE) {
 
  .Deprecated("clean_coordinates")
  
  if(missing(capitals.ref)){capitals.ref <- NULL} 
  if(missing(centroids.ref)){centroids.ref <- NULL} 
  if(missing(country.ref)){country.ref <- NULL}
  if(missing(inst.ref)){inst.ref <- NULL}
  if(missing(seas.ref)){seas.ref <- NULL} 
  if(missing(urban.ref)){urban.ref <- NULL}
  
  
  tests <- "val"
  if(capitals){tests <- c(tests, "capitals")}
  if(centroids){tests <- c(tests, "centroids")}
  if(countrycheck){tests <- c(tests, "countries")}
  if(duplicates){tests <- c(tests, "duplicates")}
  if(equal){tests <- c(tests, "equal")}
  if(GBIF){tests <- c(tests, "gbif")}
  if(institutions){tests <- c(tests, "institutions")}
  if(outliers){tests <- c(tests, "outliers")}
  if(seas){tests <- c(tests, "seas")}
  if(urban){tests <- c(tests, "urban")}
  if(zeros){tests <- c(tests, "zeros")}
  
  out <- clean_coordinates(x = x, lat = lat, lon = lon, countries = countries, tests = tests,
                           capitals_rad = capitals.rad, centroids_rad = centroids.rad,
                           centroids_detail = centroids.detail, inst_rad = inst.rad,
                           outliers_method = outliers.method, outliers_mtp = outliers.mtp,
                           outliers_td = outliers.td, outliers_size = outliers.size,
                           zeros_rad = zeros.rad, capitals_ref = capitals.ref, centroids_ref = centroids.ref,
                           country_ref = country.ref, inst_ref = inst.ref, seas_ref = seas.ref,
                           urban_ref = urban.ref,
                           value = value, verbose = verbose, report = report)
  return(out)
}

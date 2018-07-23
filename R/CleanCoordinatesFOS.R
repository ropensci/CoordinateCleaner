#' @export
#' @importFrom methods as is
#' @importFrom utils write.table
CleanCoordinatesFOS <- function(x, 
                                lon = "lng", 
                                lat = "lat", 
                                min.age = "min_ma",
                                max.age = "max_ma", 
                                taxon = "accepted_name", 
                                countries = "cc", 
                                centroids = TRUE,
                                countrycheck = TRUE, 
                                equal = TRUE, 
                                GBIF = TRUE, 
                                institutions = TRUE, 
                                temp.range.outliers = TRUE,
                                spatio.temp.outliers = TRUE, 
                                temp.ages.equal = TRUE, 
                                zeros = TRUE, 
                                centroids.rad = 0.05,
                                centroids.detail = "both", 
                                inst.rad = 0.001, 
                                outliers.method = "quantile",
                                outliers.threshold = 5, 
                                outliers.size = 7, 
                                outliers.replicates = 5, 
                                zeros.rad = 0.5,
                                centroids.ref, 
                                country.ref, 
                                inst.ref, 
                                value = "spatialvalid", 
                                verbose = TRUE,
                                report = FALSE) {
  
  .Deprecated(new = "clean_fossils")
  
  if(missing(centroids.ref)){centroids.ref <- NULL}
  if(missing(country.ref)){country.ref <- NULL}
  if(missing(inst.ref)){inst.ref <- NULL}
  
  tests <- "val"
  if(centroids){tests <- c(tests, "centroids")}
  if(countrycheck){tests <- c(tests, "countries")}
  if(equal){tests <- c(tests, "equal")}
  if(GBIF){tests <- c(tests, "gbif")}
  if(institutions){tests <- c(tests, "institutions")}
  if(temp.range.outliers){tests <- c(tests, "temprange")}
  if(spatio.temp.outliers ){tests <- c(tests, "spatiotemp")}
  if(temp.ages.equal){tests <- c(tests, "agesequal")}
  if(zeros){tests <- c(tests, "zeros")}
  
  out <- clean_fossils(x = x, lon = lon, lat = lat, min_age = min.age, 
                       max_age = max.age,
                       taxon = taxon, countries = countries,
                       centroids_rad = centroids.rad,
                       centroids_detail = centroids.detail, 
                       inst_rad = inst.rad, 
                       outliers_method = outliers.method,
                       outliers_threshold = outliers.threshold, 
                       outliers_size = outliers.size, 
                       outliers_replicates = outliers.replicates, 
                       zeros_rad = zeros.rad,
                       centroids_ref = centroids.ref, 
                       country_ref = country.ref, 
                       inst_ref = inst.ref, 
                       value = value, 
                       verbose = verbose,
                       report = report)
  
  return(out)
  
}

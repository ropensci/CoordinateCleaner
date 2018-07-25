#' @export
#' @importFrom stats complete.cases
CleanCoordinatesDS <- function(x, 
                               lon = "decimallongitude", 
                               lat = "decimallatitude",
                               ds = "dataset", 
                               ddmm = TRUE, 
                               periodicity = TRUE, 
                               ddmm.pvalue = 0.025, 
                               ddmm.diff = 1,
                               periodicity.T1 = 7, 
                               periodicity.reg.thresh = 2, 
                               periodicity.dist.min = 0.1,
                               periodicity.dist.max = 2, 
                               periodicity.min.size = 4, 
                               periodicity.target = "both",
                               periodicity.diagnostics = TRUE, 
                               value = "dataset",
                               verbose = TRUE) {
 .Deprecated(new = "clean_dataset") 
  
  tests <- vector()
  if(ddmm){tests <- c(tests, "ddmm")}
  if(periodicity){tests <- c(tests, "periodicity")}
  
  out <- clean_dataset(x = x, lat = lat, lon = lon, ds = ds, tests = tests,
                pvalue = ddmm.pvalue, diff = ddmm.diff,
                T1 = periodicity.T1, 
                reg_out_thresh = periodicity.reg.thresh,
                reg_dist_min = periodicity.dist.min, 
                reg_dist_max = periodicity.dist.max,
                min_unique_ds_size = periodicity.min.size, 
                test = periodicity.target,
                graphs = periodicity.diagnostics,
                value = value, verbose = verbose)
  return(out)
}

cc_gbif <- function(x, 
                    lon = "decimallongitude", 
                    lat = "decimallatitude", 
                    value = "clean",
                    verbose = TRUE) {

  # check function argument validity
  match.arg(value, choices = c("clean", "flags"))

  if (verbose) {
    message("Testing GBIF headquarters")
  }

  dat <- sp::SpatialPoints(x[, c(lon, lat)])
  ref <- rgeos::gBuffer(sp::SpatialPoints(cbind(12.58, 55.67)), width = 0.5)
  warning("running GBIF test, flagging records around Copenhagen")

  out <- sp::over(x = dat, y = ref)
  out <- is.na(out)

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flags = return(out))
}
80

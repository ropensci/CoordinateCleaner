cc_equ <- function(x, 
                   lon = "decimallongitude", 
                   lat = "decimallatitude", 
                   test = "absolute",
                   value = "clean", 
                   verbose = TRUE) {

  # check value and test arguments
  match.arg(test, choices = c("absolute", "identical"))
  match.arg(value, choices = c("clean", "flags"))

  if (verbose) {
    message("Testing equal lat/lon")
  }

  switch(test, absolute = {
    out <- !(abs(x[[lon]]) == abs(x[[lat]]))
  }, identical = {
    out <- !(x[[lon]] == x[[lat]])
  })

  if (verbose) {
    message(sprintf("Flagged %s records.", sum(!out)))
  }

  switch(value, clean = return(x[out, ]), flags = return(out))

  return(out)
}
80

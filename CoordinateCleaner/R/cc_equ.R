cc_equ <- function(x, lon = "decimallongitude", lat = "decimallatitude",
                   test = "absolute", value = "clean", verbose = T){
  
  #check value and test arguments
  match.arg(test, choices = c("absolute", "identical"))
  match.arg(value, choices = c("clean", "flags"))
  
  if(verbose){
    cat("Testing equal lat/lon\n")
  }
  
  switch(test,
         absolute = {out <- !(abs(x[[lon]]) == abs(x[[lat]]))},
         identical = {out <- !(x[[lon]] == x[[lat]])})

  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
  
  return(out)
  
}
cc_val <- function(x, lon = "decimallongitude", lat = "decimallatitude", 
                   value = "clean", verbose = T) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags"))
  
  if(verbose){
    cat("Testing coordinate validity\n")
  }

  
  out <- list(is.na(x[[lon]]), is.na(x[[lat]]), 
              suppressWarnings(is.na(as.numeric(as.character(x[[lon]])))), 
              suppressWarnings(is.na(as.numeric(as.character(x[[lat]])))), 
              suppressWarnings(as.numeric(as.character(x[[lon]]))) < -180, 
              suppressWarnings(as.numeric(as.character(x[[lon]]))) > 180, 
              suppressWarnings(as.numeric(as.character(x[[lat]]))) < -90, 
              suppressWarnings(as.numeric(as.character(x[[lat]]))) > 90)
  
  out <- !Reduce("|", out)
  
  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
  
  return(out)
}
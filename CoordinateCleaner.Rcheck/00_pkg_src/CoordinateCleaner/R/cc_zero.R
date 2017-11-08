cc_zero <- function(x, lon = "decimallongitude", lat = "decimallatitude", 
                   buffer = 0.5, value = "clean", verbose = T) {
  
  #check value argument
  match.arg(value, choices = c("clean", "flags"))

  if(verbose){
    cat("Testing zero coordinates\n")
  }
  
  # plain zero in coordinates
  t1 <- !(x[[lon]] == 0 | x[[lat]] == 0)

  # radius around point 0/0
  dat <- sp::SpatialPoints(x[, c(lon,lat)])
  t2 <- rgeos::gBuffer(sp::SpatialPoints(cbind(0, 0)), width = buffer)
  t2 <- is.na(sp::over(x = dat, y = t2))

  #combine test results
  
  out <- Reduce("&", list(t1, t2))
  
  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
} 
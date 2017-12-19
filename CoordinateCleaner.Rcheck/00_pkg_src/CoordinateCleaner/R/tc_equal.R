tc_equal <- function(x, min.age = "min_ma", max.age = "max_ma", 
                    value = "clean", verbose = T){
  match.arg(value, choices = c("clean", "flags"))
  
  
  if(verbose){
    cat("Testing ages\n")
  }
  
  #min.age == max.age
  
  t1 <- x[[max.age]] == x[[min.age]]
  
  flags <- t1
  
  #create output
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  if(verbose){
    cat(sprintf("Flagged %s records. \n", sum(!out)))
  }
  
  #value
  switch(value,
         clean = return(x[out,]),
         flags = return(out))
}
tc_equal <- function(x, min.age = "min_ma", max.age = "max_ma", 
                    value = "clean", verbose = T){
  match.arg(value, choices = c("clean", "flags"))
  
  
  if(verbose){
    cat("Testing age validity\n")
  }
  
  #min.age == max.age
  t1 <- x[[max.age]] == x[[min.age]]
  
  #min.age > max.age
  
  t2 <- x[[min.age]] > x[[max.age]]
  
  flags <- t1 | t2
  
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
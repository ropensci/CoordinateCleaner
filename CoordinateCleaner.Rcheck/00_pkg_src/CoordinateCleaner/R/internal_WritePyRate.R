.NoExtension <- function(filename) { 
  if (substr(filename, nchar(filename), nchar(filename))==".") { 
    return(substr(filename, 1, nchar(filename)-1)) 
  } else { 
    .NoExtension(substr(filename, 1, nchar(filename)-1)) 
  } 
}
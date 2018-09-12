# Check projection of custom reference and reproject to wgs84 if necessary
reproj <- function(ref){
  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # if no projection information is given assume wgs84
  if(is.na(sp::proj4string(ref))){
    warning("no projection information for reference found, 
            assuming '+proj=longlat +datum=WGS84 +no_defs 
            +ellps=WGS84 +towgs84=0,0,0'")
    proj4string(ref) <- wgs84
  }else if(sp::proj4string(ref) != wgs84){
    #otherwise reproject
    ref <- sp::spTransform(ref, sp::CRS(wgs84))
    warning("reprojecting reference to '+proj=longlat +datum=WGS84 
            +no_defs +ellps=WGS84 +towgs84=0,0,0'")
  }
  return(ref)
}
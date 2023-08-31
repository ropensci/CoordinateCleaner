## ----options, echo = FALSE----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  library(CoordinateCleaner)
#  library(dplyr)
#  library(ggplot2)
#  library(rgbif)
#  library(viridis)
#  library(terra)
#  
#  #download data from GBIF
#  dat <- rgbif::occ_search(scientificName = "Avicennia", limit = 1000,
#           hasCoordinate = T)
#  
#  dat <- dat$data
#  
#  dat <-  dat %>%
#    dplyr::select(species = name, decimalLongitude = decimalLongitude,
#           decimalLatitude = decimalLatitude, countryCode)
#  
#  # run with default gazetteer
#  outl <- cc_sea(dat, value = "flagged")
#  ## OGR data source with driver: ESRI Shapefile
#  ## Source: "C:\Users\az64mycy\AppData\Local\Temp\Rtmp4SRhHV", layer: "ne_110m_land"
#  ## with 127 features
#  ## It has 3 fields
#  
#  plo <- data.frame(dat, outlier =  as.factor(!outl))
#  
#  #plot results
#  ggplot() +
#    borders(fill = "grey60") +
#    geom_point(data = plo,
#               aes(x = decimalLongitude, y = decimalLatitude, col = outlier)) +
#    scale_color_viridis(discrete = T, name = "Flagged outlier") +
#    coord_fixed() +
#    theme_bw() +
#    theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
#  # The buffered custom gazetteer
#  data("buffland")
#  buffland <- terra::vect(buffland)
#  plot(buffland)

## -----------------------------------------------------------------------------
#  
#  # run with custom gazetteer
#  outl <- cc_sea(dat, value = "flagged", ref = buffland)
#  
#  plo <- data.frame(dat, outlier =  as.factor(!outl))
#  
#  #plot results
#  ggplot()+
#    borders(fill = "grey60")+
#    geom_point(data = plo,
#               aes(x = decimalLongitude, y = decimalLatitude, col = outlier))+
#    scale_color_viridis(discrete = T, name = "Flagged outlier")+
#    coord_fixed()+
#    theme_bw()+
#    theme(legend.position = "bottom")


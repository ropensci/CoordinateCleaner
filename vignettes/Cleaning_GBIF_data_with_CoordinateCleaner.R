## ----options, echo = FALSE----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----intall_github, eval=FALSE------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("ropensci/CoordinateCleaner")

## ----libraries----------------------------------------------------------------
#  library(countrycode)
#  library(CoordinateCleaner)
#  library(dplyr)
#  library(ggplot2)
#  library(rgbif)
#  library(sf)

## ----obtain data--------------------------------------------------------------
#  #obtain data from GBIF via rgbif
#  dat <- occ_search(scientificName = "Panthera leo",
#                    limit = 5000,
#                    hasCoordinate = TRUE)
#  
#  dat <- dat$data
#  
#  # names(dat) # a lot of columns
#  
#  # select columns of interest
#  dat <- dat %>%
#    dplyr::select(species, decimalLongitude,
#                  decimalLatitude, countryCode, individualCount,
#                  gbifID, family, taxonRank, coordinateUncertaintyInMeters,
#                  year, basisOfRecord, institutionCode, datasetName)
#  
#  # remove records without coordinates
#  dat <- dat %>%
#    filter(!is.na(decimalLongitude)) %>%
#    filter(!is.na(decimalLatitude))

## ----map1---------------------------------------------------------------------
#  #plot data to get an overview
#  wm <- borders("world", colour = "gray50", fill = "gray50")
#  ggplot() +
#    coord_fixed() +
#    wm +
#    geom_point(data = dat,
#               aes(x = decimalLongitude, y = decimalLatitude),
#               colour = "darkred",
#               size = 0.5) +
#    theme_bw()

## ---- echo=FALSE, eval = TRUE, out.width="100%", fig.cap="Occurrence records for Panthera leo obtained from GBIF."----
knitr::include_graphics("gbif-clgbif5-1.png")

## -----------------------------------------------------------------------------
#  #convert country code from ISO2c to ISO3c
#  dat$countryCode <-  countrycode(dat$countryCode,
#                                  origin =  'iso2c',
#                                  destination = 'iso3c')
#  
#  #flag problems
#  dat <- data.frame(dat)
#  flags <- clean_coordinates(x = dat,
#                             lon = "decimalLongitude",
#                             lat = "decimalLatitude",
#                             countries = "countryCode",
#                             species = "species",
#                             tests = c("capitals", "centroids",
#                                      "equal", "zeros", "countries")) # most test are on by default

## -----------------------------------------------------------------------------
#  summary(flags)
#  plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

## -----------------------------------------------------------------------------
#  #Exclude problematic records
#  dat_cl <- dat[flags$.summary,]
#  
#  #The flagged records
#  dat_fl <- dat[!flags$.summary,]

## -----------------------------------------------------------------------------
#  # To avoid specifying it in each function
#  names(dat)[2:3] <- c("decimalLongitude", "decimalLatitude")
#  
#  clean <- dat %>%
#    cc_val() %>%
#    cc_equ() %>%
#    cc_cap() %>%
#    cc_cen() %>%
#    cc_coun(iso3 = "countryCode") %>%
#    cc_sea() %>%
#    cc_zero() %>%
#    cc_outl() %>%
#    cc_dupl()

## -----------------------------------------------------------------------------
#  dat %>%
#      as_tibble() %>%
#      mutate(val = cc_val(., value = "flagged"),
#             sea = cc_sea(., value = "flagged"))

## -----------------------------------------------------------------------------
#  flags <- cf_age(x = dat_cl,
#                  lon = "decimalLongitude",
#                  lat = "decimalLatitude",
#                  taxon = "species",
#                  min_age = "year",
#                  max_age = "year",
#                  value = "flagged")
#  # Testing temporal outliers on taxon level
#  # Flagged 0 records.
#  
#  dat_cl <- dat_cl[flags, ]

## -----------------------------------------------------------------------------
#  #Remove records with low coordinate precision
#  dat_cl %>%
#    mutate(Uncertainty = coordinateUncertaintyInMeters / 1000) %>%
#    ggplot(aes(x = Uncertainty)) +
#    geom_histogram() +
#    xlab("Coordinate uncertainty in meters") +
#    theme_bw()
#  

## -----------------------------------------------------------------------------
#  dat_cl <- dat_cl %>%
#    filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))
#  
#  # Remove unsuitable data sources, especially fossils
#  # which are responsible for the majority of problems in this case
#  table(dat$basisOfRecord)
#  
#  ## HUMAN_OBSERVATION    MATERIAL_SAMPLE PRESERVED_SPECIMEN
#  ##              4979                  2                 19
#  
#  dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" |
#                           basisOfRecord == "OBSERVATION" |
#                           basisOfRecord == "PRESERVED_SPECIMEN")

## -----------------------------------------------------------------------------
#  #Individual count
#  table(dat_cl$individualCount)

## -----------------------------------------------------------------------------
#  dat_cl <- dat_cl %>%
#    filter(individualCount > 0 | is.na(individualCount)) %>%
#    filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

## -----------------------------------------------------------------------------
#  #Age of records
#  table(dat_cl$year)

## -----------------------------------------------------------------------------
#  dat_cl <- dat_cl %>%
#    filter(year > 1945) # remove records from before second world war

## -----------------------------------------------------------------------------
#  table(dat_cl$family) #that looks good
#  ##
#  ## Felidae
#  ##    4505
#  dat_cl <- dat_cl %>%
#    filter(family == 'Felidae')
#  
#  table(dat_cl$taxonRank) # this is also good
#  ##
#  ##   SPECIES SUBSPECIES
#  ##       520       3985

## -----------------------------------------------------------------------------
#  #exclude based on study area
#  dat_fin <- filter(dat_cl, decimalLatitude < 40)

## -----------------------------------------------------------------------------
#  #create simple natural range for lions
#  coords_range <- cbind(cbind(c(-23, -7, 31, 71, 83, 42, 41, 24, -23), c(14, 37, 32, 27, 18, 0, -16, -38, 14)))
#  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#  
#  nat_range <- terra::vect(coords_range, "polygons",
#                       crs = wgs84)
#  nat_range$species <- "Panthera leo"
#  
#  # Visualize range
#  plo <- sf::st_as_sf(nat_range)
#  
#  ## Regions defined for each Polygons
#  ggplot() +
#    borders("world", colour = "gray50", fill = "gray50") +
#    geom_sf(data = plo, aes(fill = species), alpha = 0.5) +
#    theme_bw() +
#    theme(legend.position = "none",
#          axis.title = element_blank())

## -----------------------------------------------------------------------------
#  
#  # run cc_iucn()
#  range_flags <- cc_iucn(x = dat_cl,
#                         range = nat_range,
#                         lon = "decimalLongitude",
#                         lat = "decimalLatitude",
#                         value = "flagged")

## -----------------------------------------------------------------------------
#  dat_fin <- dat_cl[range_flags, ]

## ---- echo = FALSE------------------------------------------------------------
#  dat <- dat %>%
#    as_tibble() %>%
#    mutate(phase = "Raw data")
#  dat_cl <- dat_cl %>%
#    mutate(phase = "Automatic cleaning")
#  dat_fin <- dat_fin %>%
#    mutate(phase = "Manual polishing")
#  
#  dat %>%
#    bind_rows(dat_cl, dat_fin) %>%
#    mutate(phase = factor(phase, c("Raw data", "Automatic cleaning", "Manual polishing"))) %>%
#    ggplot(aes(x = decimalLongitude, y = decimalLatitude, color = phase)) +
#    borders("world", colour = "gray50", fill = "gray50") +
#    geom_point() +
#    theme_bw() +
#    theme(legend.position = "none",
#          axis.title = element_blank()) +
#    facet_wrap(. ~ phase, ncol = 1)
#  

## ---- echo=FALSE, eval = TRUE, out.width="100%", fig.cap="\\label{fig:final}The dataset of occurrence of lions after different cleaning phases."----
knitr::include_graphics("gbif-clgbif17-1.png")

## -----------------------------------------------------------------------------
#  out.ddmm <- cd_ddmm(dat_cl, lon = "decimalLongitude", lat = "decimalLatitude",
#                      ds = "species", diagnostic = T, diff = 1,
#                      value = "dataset")

## -----------------------------------------------------------------------------
#  par(mfrow = c(2,2), mar = rep(2, 4))
#  out.round <- cd_round(dat_fin, lon = "decimalLongitude",
#                        lat = "decimalLatitude",
#                        ds = "species",
#                        value = "dataset",
#                        T1 = 7,
#                        graphs = T)
#  ## Testing for rasterized collection


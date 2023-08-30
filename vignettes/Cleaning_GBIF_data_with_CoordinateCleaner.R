## ----intall_github, eval=FALSE------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("ropensci/CoordinateCleaner")

## ----libraries----------------------------------------------------------------
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)

## ----obtain data--------------------------------------------------------------
#obtain data from GBIF via rgbif
dat <- occ_search(scientificName = "Panthera leo", 
                  limit = 5000, 
                  hasCoordinate = TRUE)

dat <- dat$data

# names(dat) # a lot of columns

# select columns of interest
dat <- dat %>%
  dplyr::select(species, decimalLongitude, 
                decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters,
                year, basisOfRecord, institutionCode, datasetName)

# remove records without coordinates
dat <- dat %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))

## ----map1---------------------------------------------------------------------
#plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = dat,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()

## -----------------------------------------------------------------------------
#convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, 
                                origin =  'iso2c',
                                destination = 'iso3c')

#flag problems
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids",
                                    "equal", "zeros", "countries")) # most test are on by default
# Testing coordinate validity
# Flagged 0 records.
# Testing equal lat/lon
# Flagged 0 records.
# Testing zero coordinates
# Flagged 0 records.
# Testing country capitals
# Flagged 35 records.
# Testing country centroids
# Flagged 1 records.
# Testing country identity
# Flagged 316 records.
# Testing GBIF headquarters, flagging records around Copenhagen
# Flagged 0 records.
# Testing biodiversity institutions
# Flagged 0 records.
# Flagged 351 of 5000 records, EQ = 0.07.

## -----------------------------------------------------------------------------
summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

## -----------------------------------------------------------------------------
#Exclude problematic records
dat_cl <- dat[flags$.summary,]

#The flagged records
dat_fl <- dat[!flags$.summary,]

## -----------------------------------------------------------------------------
 #to avoid specifying it in each function
names(dat)[2:3] <- c("decimallongitude", "decimallatitude")

clean <- dat %>%
  cc_val() %>%
  cc_equ() %>%
  cc_cap() %>%
  cc_cen() %>%
  cc_coun(iso3 = "countryCode") %>%
  cc_sea() %>%
  cc_zero() %>%
  cc_outl() %>%
  cc_dupl()

## -----------------------------------------------------------------------------
dat %>%
    as_tibble() %>% 
    mutate(val = cc_val(., value = "flagged"),
           sea = cc_sea(., value = "flagged"))

## -----------------------------------------------------------------------------
flags <- cf_age(x = dat_cl,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                taxon = "species", 
                min_age = "year", 
                max_age = "year", 
                value = "flagged")
# Testing temporal outliers on taxon level
# Flagged 0 records.

dat_cl <- dat_cl[flags, ]

## -----------------------------------------------------------------------------
#Remove records with low coordinate precision
hist(dat_cl$coordinateUncertaintyInMeters / 1000, breaks = 30)

## -----------------------------------------------------------------------------

dat_cl <- dat_cl %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

# Remove unsuitable data sources, especially fossils 
# which are responsible for the majority of problems in this case
table(dat$basisOfRecord)

## HUMAN_OBSERVATION    MATERIAL_SAMPLE PRESERVED_SPECIMEN 
##              4979                  2                 19 

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | 
                         basisOfRecord == "OBSERVATION" |
                         basisOfRecord == "PRESERVED_SPECIMEN")

## -----------------------------------------------------------------------------
#Individual count
table(dat_cl$individualCount)

## -----------------------------------------------------------------------------
dat_cl <- dat_cl %>%
  filter(individualCount > 0 | is.na(individualCount)) %>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

## -----------------------------------------------------------------------------
#Age of records
table(dat_cl$year)

## -----------------------------------------------------------------------------
dat_cl <- dat_cl %>%
  filter(year > 1945) # remove records from before second world war

## -----------------------------------------------------------------------------
table(dat_cl$family) #that looks good
## 
## Felidae 
##    4505
dat_cl <- dat_cl %>%
  filter(family == 'Felidae')

table(dat_cl$taxonRank) # this is also good
## 
##   SPECIES SUBSPECIES 
##       546       3959 

## -----------------------------------------------------------------------------
#exclude based on study area
dat_fin <- filter(dat_cl, decimalLatitude < 40)

## -----------------------------------------------------------------------------

#create simple natural range for lions
coords_range <- cbind(cbind(c(-23, -7, 31, 71, 83, 42, 41, 24, -23), c(14, 37, 32, 27, 18, 0, -16, -38, 14)))
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

nat_range <- terra::vect(coords_range, "polygons",
                     crs = wgs84)
nat_range$species <- "Panthera leo"
 
# Visualize range
plo <- sf::st_as_sf(nat_range)

## Regions defined for each Polygons
ggplot() +
  borders("world", colour = "gray50", fill = "gray50") +
  geom_sf(data = plo, aes(fill = species), alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

## -----------------------------------------------------------------------------

# run cc_iucn()
range_flags <- cc_iucn(x = dat_cl,
                       range = nat_range,
                       lon = "decimalLongitude",
                       lat = "decimalLatitude",
                       value = "flagged")
## Testing natural ranges
## Flagged 141 records.
## Warning message:
## In cc_iucn(x = dat_cl, range = nat_range, lon = "decimalLongitude",  :
##   reprojecting reference to '+proj=longlat +datum=WGS84 +no_defs'

dat_fin <- dat_cl[range_flags, ]

## -----------------------------------------------------------------------------
out.ddmm <- cd_ddmm(dat_cl, lon = "decimalLongitude", lat = "decimalLatitude", 
                    ds = "species", diagnostic = T, diff = 1,
                    value = "dataset")

## -----------------------------------------------------------------------------
par(mfrow = c(2,2), mar = rep(2, 4))
out.round <- cd_round(dat_fin, lon = "decimalLongitude", 
                      lat = "decimalLatitude", 
                      ds = "species",
                      value = "dataset",
                      T1 = 7,
                      graphs = T)
## Testing for rasterized collection


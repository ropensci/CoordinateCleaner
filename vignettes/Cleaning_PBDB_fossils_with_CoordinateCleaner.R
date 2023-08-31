## ----options, echo = FALSE----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  
#  install_github("ropensci/CoordinateCleaner")

## -----------------------------------------------------------------------------
#  library(dplyr)
#  library(ggplot2)
#  library(CoordinateCleaner)
#  library(countrycode)
#  library(paleobioDB)

## -----------------------------------------------------------------------------
#  #load data
#  dat <- paleobioDB::pbdb_occurrences(base_name = "Magnoliopsida",
#                                      vocab = "pbdb", limit = 5000,
#                          show = c("coords", "phylo", "attr", "loc", "time", "rem"))
#  dat <- dat %>% mutate(lng = as.numeric(lng),
#                 lat = as.numeric(lat),
#                 early_age = as.numeric(early_age),
#                 late_age = as.numeric(late_age))
#  rownames(dat) <- NULL

## -----------------------------------------------------------------------------
#  #plot data to get an overview
#  wm <- borders("world", colour = "gray50", fill = "gray50")
#  dat %>%
#    ggplot(aes(x = lng, y = lat)) +
#    coord_fixed() +
#    wm +
#    geom_point(
#               colour = "darkred",
#               size = 0.5) +
#    theme_bw()

## -----------------------------------------------------------------------------
#  cl <- cc_val(dat, lat = "lat", lon = "lng")

## -----------------------------------------------------------------------------
#  cl <- cc_equ(cl, lat = "lat", lon = "lng")

## -----------------------------------------------------------------------------
#  fl <- cc_equ(dat, value = "flagged", lat = "lat", lon = "lng")
#  ## Testing equal lat/lon
#  ## Flagged 0 records.
#  
#  # extract and check the flagged records
#  fl_rec <- dat[!fl,]
#  head(fl_rec)
#  ##  [1] occurrence_no  record_type    collection_no  taxon_name     taxon_rank     taxon_no       matched_name   matched_rank
#  ##  [9] matched_no     early_interval late_interval  early_age      late_age       reference_no   lng            lat
#  ## [17] class          class_no       phylum         phylum_no      cc             state          geogscale      early_age.1
#  ## [25] late_age.1     cx_int_no      early_int_no   late_int_no    genus          genus_no       family         family_no
#  ## [33] order          order_no       county         reid_no
#  ## <0 rows> (or 0-length row.names)

## -----------------------------------------------------------------------------
#  fl <- cc_cen(cl, lat = "lat", lon = "lng", value = "flagged")
#  ## Testing country centroids
#  ## Flagged 6 records.
#  fl_rec <- cl[!fl, ]
#  unique(fl_rec$cc)
#  ## [1] JP
#  ## Levels: NZ US CN IR KP AU UK FR JP DE CA RU KE ZM EG CD ZA TZ UG ET MX IT
#  cl <- cl[fl, ]

## -----------------------------------------------------------------------------
#  #adapt country code to ISO3, for country test
#  cs_ma <- "GBR"
#  names(cs_ma) <- "UK"
#  cl$cc_iso3 <- countrycode(cl$cc, origin = "iso2c", destination = "iso3c", custom_match = cs_ma)
#  
#  cl <- cc_coun(cl, lat = "lat", lon = "lng", iso3 = "cc_iso3")
#  ## Testing country identity
#  ## Removed 234 records.

## -----------------------------------------------------------------------------
#  cl <- cc_inst(cl, lat = "lat", lon = "lng")
#  ## Testing biodiversity institutions
#  ## Removed 0 records.
#  cl <- cc_gbif(cl, lat = "lat", lon = "lng")
#  ## Testing GBIF headquarters, flagging records around Copenhagen
#  ## Removed 0 records.

## -----------------------------------------------------------------------------
#  cl <- cc_zero(cl, lat = "lat", lon = "lng")
#  ## Testing zero coordinates
#  ## Removed 0 records.

## -----------------------------------------------------------------------------
#  cl <- cl[!is.na(cl$late_age),]
#  cl <- cl[!is.na(cl$early_age),]
#  cl <- cf_equal(cl, min_age = "late_age", max_age = "early_age")
#  ## Testing age validity
#  ## Removed 0 records.

## -----------------------------------------------------------------------------
#  rang <- cl$early_age - cl$late_age
#  hist(rang, breaks = 40, xlab = "Date range [max age - min age]", main = "")

## -----------------------------------------------------------------------------
#  # Outlier dataset
#  cl <- cf_range(cl, taxon = "", min_age = "late_age", max_age = "early_age")
#  ## Testing temporal range outliers on dataset level
#  ## Removed 57 records.
#  
#  # Outlier per taxon
#  cl <- cf_range(cl, taxon = "taxon_name", min_age = "late_age", max_age = "early_age")
#  ## Testing temporal range outliers on taxon level
#  ## Removed 86 records.
#  
#  # Absolute age limit
#  cl <- cf_range(cl, taxon = "taxon_name", min_age = "late_age",
#                 max_age = "early_age", method = "time", max_range = 35)
#  ## Testing temporal range outliers on taxon level
#  ## Removed 1 records.
#  
#  rang <- cl$early_age - cl$late_age
#  hist(rang, breaks = 40, xlab = "Date range [max age - min age]", main = "")

## -----------------------------------------------------------------------------
#  # Outlier dataset
#  cl <- cf_outl(cl, taxon = "", lat = "lat", lon = "lng",
#                min_age = "late_age", max_age = "early_age")
#  ## Testing spatio-temporal outliers on dataset level
#  ## Removed 256 records.
#  
#  # Outlier taxon
#  cl <- cf_outl(cl, taxon = "taxon_name", lat = "lat", lon = "lng",
#                min_age = "late_age", max_age = "early_age")
#  ## Testing spatio-temporal outliers on taxon level
#  ## Removed 30 records.

## -----------------------------------------------------------------------------
#  nrow(dat) - nrow(cl)

## -----------------------------------------------------------------------------
#  #adapt country code to ISO3, for country test
#  cs_ma <- "GBR"
#  names(cs_ma) <- "UK"
#  dat$cc <- countrycode(dat$cc, origin = "iso2c", destination = "iso3c", custom_match = cs_ma)
#  
#  cl <- dat %>%
#    cc_val(lat = "lat", lon = "lng") %>%
#    cc_equ(lat = "lat", lon = "lng") %>%
#    cc_cen(lat = "lat", lon = "lng") %>%
#    cc_coun(lat = "lat", lon = "lng", iso3 = "cc") %>%
#    cc_gbif(lat = "lat", lon = "lng") %>%
#    cc_inst(lat = "lat", lon = "lng") %>%
#    cc_zero(lat = "lat", lon = "lng") %>%
#    cf_equal(min_age = "late_age", max_age = "early_age") %>%
#    cf_range(
#      taxon = "taxon_name",
#      lat = "lat",
#      lon = "lng",
#      min_age = "late_age",
#      max_age = "early_age"
#    ) %>%
#    cf_outl(
#      taxon = "taxon_name",
#      lat = "lat",
#      lon = "lng",
#      min_age = "late_age",
#      max_age = "early_age"
#    )

## -----------------------------------------------------------------------------
#  #run automated testing
#  flags <- clean_fossils(x = dat,
#                         lat = "lat",
#                         lon = "lng",
#                         taxon = "taxon_name",
#                         min_age = "late_age", max_age = "early_age",
#                         value = "spatialvalid")
#  
#  head(flags)
#  cl <- dat[flags$.summary,] #the cleaned records
#  fl_rec <- dat[!flags$.summary,] # the flagged records for verification

## -----------------------------------------------------------------------------
#  #1. This looks OK
#  table(cl$phylum)
#  ##
#  ## Spermatophyta
#  ##          2220
#  
#  #2. Taxonomic level of identification
#  table(cl$taxon_rank)
#  ##
#  ##  class   genus species
#  ##    371     502    1347
#  

## -----------------------------------------------------------------------------
#  cl <- cl %>%
#    filter(taxon_rank %in% c("species", "genus"))

## -----------------------------------------------------------------------------
#  table(cl$geogscale)

## -----------------------------------------------------------------------------
#  #minimum ages
#  tail(table(cl$late_age))
#  ##
#  ##  63.3    66  70.6  93.5  93.9 100.5
#  ##    53   138     8     3     3    21
#  
#  ggplot(cl)+
#    geom_histogram(aes(x = late_age))
#  ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

## -----------------------------------------------------------------------------
#  
#  #maximum ages
#  tail(table(cl$early_age))
#  ##
#  ##  70.6  83.5  99.6 100.5 105.3   113
#  ##   126     9     3    11     3    21
#  
#  ggplot(cl) +
#    geom_histogram(aes(x = early_age))
#  ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

## -----------------------------------------------------------------------------
#  # replace  blanks in taxon names
#  cl$taxon_name <- gsub("[[:blank:]]{1,}","_", cl$taxon_name)
#  
#  #simulated current status, soley for demonstration purposes, replace with your own data
#  mock_status <- data.frame(taxon_name = unique(cl$taxon_name),
#                            status = sample(c("extinct", "extant"),
#                                            size = length(unique(cl$taxon_name)),
#                                            replace = TRUE))
#  
#  #add current status to fossils
#  cl2 <- inner_join(cl, mock_status, by = "taxon_name")
#  
#  #Write PyRate input to disk
#  write_pyrate(cl, fname = "paleobioDB_angiosperms", status = cl2$status,
#              taxon = "taxon_name", min_age = "late_age", max_age = "early_age")


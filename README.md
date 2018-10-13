# CoordinateCleaner v2.0-2
[![Build Status](https://travis-ci.com/ropensci/CoordinateCleaner.svg?branch=master)](https://travis-ci.com/ropensci/CoordinateCleaner)
[![codecov](https://codecov.io/gh/azizka/CoordinateCleaner/branch/master/graph/badge.svg)](https://codecov.io/gh/azizka/CoordinateCleaner)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cran.r-project.org/package=CoordinateCleaner)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/CoordinateCleaner)](http://cranlogs.r-pkg.org/badges/grand-total/CoordinateCleaner)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/CoordinateCleaner)](http://cranlogs.r-pkg.org/badges/CoordinateCleaner)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

***Note: the documentation of CoordinateCleaner has moved to https://ropensci.github.io/CoordinateCleaner/.***


Automated flagging of common spatial and temporal errors in biological and palaeontological collection data, for the use in conservation, ecology and palaeontology. Specifically includes tests for

* General coordinate validity
* Country and province centroids
* Capital coordinates
* Coordinates of biodiversity institutions
* Spatial outliers
* Temporal outliers
* Coordinate-country discordance
* Duplicated coordinates per species
* Assignment to the location of the GBIF headquarters
* Urban areas
* Seas
* Plain zeros
* Equal longitude and latitude
* Rounded coordinates
* DDMM to DD.DD coordinate conversion errors
* Large temporal uncertainty (fossils)
* Equal minimum and maximum ages (fossils)
* Spatio-temporal outliers (fossils)

CoordinateCleaner can be particularly useful to improve data quality when using data from GBIF (e.g. obtained with [rgbif]( https://github.com/ropensci/rgbif)) or the Paleobiology database (e.g. obtained with [paleobioDB](https://github.com/ropensci/paleobioDB)) for historical biogeography (e.g. with [BioGeoBEARS](https://cran.r-project.org/web/packages/BioGeoBEARS/index.html) or [phytools](https://cran.r-project.org/web/packages/phytools/index.html)), automated conservation assessment (e.g. with [speciesgeocodeR](https://github.com/azizka/speciesgeocodeR/wiki) or [conR](https://cran.r-project.org/web/packages/ConR/index.html)) or species distribution modelling (e.g. with [dismo](https://cran.r-project.org/web/packages/dismo/index.html) or [sdm](https://cran.r-project.org/web/packages/sdm/index.html)). See [scrubr](https://github.com/ropensci/scrubr) and [taxize](https://github.com/ropensci/taxize) for complementary taxonomic cleaning or [biogeo](https://github.com/cran/biogeo) for correcting spatial coordinate errors. You can find a detailed comaprison of the functionality of `CoordinateCleaner`, `scrubr`, and `biogeo` [here](https://ropensci.github.io/CoordinateCleaner/articles/comparison_other_software.html).

See [News](https://github.com/ropensci/CoordinateCleaner/blob/master/NEWS.md) for update information.

# Installation
## Stable from CRAN

```{r}
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
```

## Developmental using devtools
```{r}
devtools::install_github("ropensci/CoordinateCleaner")
library(CoordinateCleaner)
```

# Usage
A simple example:

```{r}
# Simulate example data
minages <- runif(250, 0, 65)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65),
                    dataset = "clean")

# Run record-level tests
rl <- clean_coordinates(x = exmpl)
summary(rl)
plot(rl)

# Dataset level 
dsl <- clean_dataset(exmpl)

# For fossils
fl <- clean_fossils(x = exmpl,
                          taxon = "species",
                          lon = "decimallongitude", 
                          lat = "decimallatitude")
summary(fl)

# Alternative example using the pipe
library(tidyverse)

cl <- exmpl %>%
  cc_val()%>%
  cc_cap()%>%
  cd_ddmm()%>%
  cf_range(lon = "decimallongitude", 
           lat = "decimallatitude", 
           taxon  ="species")
```

# Documentation
Pipelines for cleaning data from the Global Biodiversity Information Facility (GBIF) and the Paleobiology Database (PaleobioDB) are available in [here](https://ropensci.github.io/CoordinateCleaner/articles/).


# Contributing
See the [CONTRIBUTING](https://github.com/ropensci/CoordinateCleaner/blob/master/CONTRIBUTING.md) document.

# Citation
Zizka A, Silvestro D, Andermann T, Azevedo J, Duarte RItter C, Edler D, Farooq H, Herdean A, Ariza M, Scharn R, Svanteson S, Wengtrom N, Zizka V & Antonelli A (2018) CoordinateCleaner: standardized cleaning of occurrence records from biological collection databases. https://github.com/ropensci/CoordinateCleaner

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)


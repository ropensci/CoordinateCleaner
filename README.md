# CoordinateCleaner v2.0-1
[![Build Status](https://travis-ci.org/azizka/CoordinateCleaner.svg?branch=master)](https://travis-ci.org/azizka/CoordinateCleaner)
[![codecov](https://codecov.io/gh/azizka/CoordinateCleaner/branch/master/graph/badge.svg)](https://codecov.io/gh/azizka/CoordinateCleaner)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cran.r-project.org/package=CoordinateCleaner)
![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/CoordinateCleaner)

***Note: the latest release of CoordinateCleaner (2.0), might cause compatibility issues with scripts using older versions (1.x), since some function and argument names changed. Please see the news file for details.***


Automated flagging of common spatial and temporal errors in biological and palaeontological collection data, for the use in conservation, ecology and palaeontology. Specifically includes tests for

* General coordinate validity
* Country and province centroids
* Capital coordinates
* Coordinates of biodiversity institutions
* Spatial outliers
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

CoordinateCleaner can be particularly useful to ensure geographic data quality when using data from GBIF (e.g. obtained with [rgbif]( https://github.com/ropensci/rgbif)) for historical biogeography (e.g. with [BioGeoBEARS](https://cran.r-project.org/web/packages/BioGeoBEARS/index.html) or [phytools](https://cran.r-project.org/web/packages/phytools/index.html)), automated conservation assessment (e.g. with [speciesgeocodeR](https://github.com/azizka/speciesgeocodeR/wiki) or [conR](https://cran.r-project.org/web/packages/ConR/index.html)) or species distribution modelling (e.g. with [dismo](https://cran.r-project.org/web/packages/dismo/index.html) or [sdm](https://cran.r-project.org/web/packages/sdm/index.html). See [scrubr](https://github.com/ropensci/scrubr) and [taxize](https://github.com/ropensci/taxize) for complementary taxonomic cleaning or [biogeo](https://github.com/cran/biogeo) for correcting spatial coordinate errors.

See [News](https://github.com/azizka/CoordinateCleaner/blob/master/NEWS.md) for update information.

# Installation
## Stable from CRAN

```{r, evaluate = F}
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
```

## Developmental using devtools
```{r, evaluate = F}
devtools::install_github("azizka/CoordinateCleaner")
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
Pipelines for cleaning data from the Global Biodiversity Information Facility and the Paleobiology Database are available in [vignettes](https://github.com/azizka/CoordinateCleaner/tree/master/vignettes).


# Contributing
See the [CONTRIBUTING](https://github.com/azizka/CoordinateCleaner/blob/master/CONTRIBUTING.md) document.

# Citation
Zizka, A, Silvestro, D & Antonelli A (2018) CoordinateCleaner: standardized cleaning of occurrence records from biological collection databases. https://github.com/azizka/CoordinateCleaner



[![Build Status](https://travis-ci.org/azizka/CoordinateCleaner.svg?branch=master)](https://travis-ci.org/azizka/CoordianteCleaner)
[![Coverage Status](https://coveralls.io/repos/github/azizka/CoordinateCleaner/badge.svg?branch=master)](https://coveralls.io/github/azizka/CoordinateCleaner?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cran.r-project.org/package=CoordinateCleaner)

# CoordinateCleaner v1.0-7
An R-package to flag of common spatial and temporal errors in biological and palaentological collection data, for the use in conservation, ecology and palentology. Specifically includes tests for

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

# Installation
## Stable from CRAN

```
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
```

## Developmental using devtools
```
devtools::install_github("azizka/CoordinateCleaner")
library(CoordinateCleaner)
````

# Usage
A simple example:

```
# Simulate example data
minages <- runif(250, 0, 65)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65),
                    dataset = "clean")

# Run record-level tests
rl <- CleanCoordinates(x = exmpl)
summary(rl)
plot(rl)

# Dataset level 
dsl <- CleanCoordinatesDS(exmpl)

# For fossils
fl <- CleanCoordinatesFOS(x = exmpl,
                          taxon = "species",
                          lon = "decimallongitude", 
                          lat = "decimallatitude")
summary(fl)

# Alternative example using the pipe
library(tidyverse)

cl <- exmpl %>%
  cc_val()%>%
  cc_cap()%>%
  dc_ddmm()%>%
  tc_range(lon = "decimallongitude", 
           lat = "decimallatitude", 
           taxon  ="species")
```

# Documentation
Pipelines for cleaning data from teh Global Biodiversity Information Facility and the Paleobiology Database are available in \Tutorials.

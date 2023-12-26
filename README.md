# CoordinateCleaner v3.0
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cranlogs.r-pkg.org:443/badges/CoordinateCleaner)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/CoordinateCleaner)](https://cranlogs.r-pkg.org:443/badges/grand-total/CoordinateCleaner)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/CoordinateCleaner)](https://cranlogs.r-pkg.org:443/badges/CoordinateCleaner)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2539408.svg)](https://doi.org/10.5281/zenodo.2539408)
[![rOpenSci peer-review](https://badges.ropensci.org/210_status.svg)](https://github.com/ropensci/software-review/issues/210)

**CoordinateCleaner has been updated to version 3.0 on github and will shortly be updated on CRAN to adapt to the retirement of sp and raster. The update may not be compatible with analysis-pipelines build with version 2.x***

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

CoordinateCleaner can be particularly useful to improve data quality when using data from GBIF (e.g. obtained with [rgbif]( https://github.com/ropensci/rgbif)) or the Paleobiology database (e.g. obtained with [paleobioDB](https://github.com/ropensci/paleobioDB)) for historical biogeography (e.g. with [BioGeoBEARS](https://CRAN.R-project.org/package=BioGeoBEARS) or [phytools](https://CRAN.R-project.org/package=phytools)), automated conservation assessment (e.g. with [speciesgeocodeR](https://github.com/azizka/speciesgeocodeR/wiki) or [conR](https://CRAN.R-project.org/package=ConR)) or species distribution modelling (e.g. with [dismo](https://CRAN.R-project.org/package=dismo) or [sdm](https://CRAN.R-project.org/package=sdm)). See [scrubr](https://github.com/ropensci-archive/scrubr) and [taxize](https://github.com/ropensci/taxize) for complementary taxonomic cleaning or [biogeo](https://github.com/cran/biogeo) for correcting spatial coordinate errors.

See [News](https://github.com/ropensci/CoordinateCleaner/blob/master/NEWS.md) for update information.

# Installation
## Stable from CRAN

```r
install.packages("CoordinateCleaner")
library(CoordinateCleaner)
```

## Developmental from GitHub
```r
devtools::install_github("ropensci/CoordinateCleaner")
library(CoordinateCleaner)
```

# Usage
A simple example:

```r
# Simulate example data
minages <- runif(250, 0, 65)
exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimalLongitude = runif(250, min = 42, max = 51),
                    decimalLatitude = runif(250, min = -26, max = -11),
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
                          lon = "decimalLongitude", 
                          lat = "decimalLatitude")
summary(fl)

# Alternative example using the pipe
library(tidyverse)

cl <- exmpl %>%
  cc_val()%>%
  cc_cap()%>%
  cd_ddmm()%>%
  cf_range(lon = "decimalLongitude", 
           lat = "decimalLatitude", 
           taxon  ="species")
```

# Documentation
Pipelines for cleaning data from the Global Biodiversity Information Facility (GBIF) and the Paleobiology Database (PaleobioDB) are available in [here](https://ropensci.github.io/CoordinateCleaner/articles/).


# Contributing
See the [CONTRIBUTING](https://github.com/ropensci/CoordinateCleaner/blob/master/CONTRIBUTING.md) document.

# Citation
Zizka A, Silvestro D, Andermann T, Azevedo J, Duarte Ritter C, Edler D, Farooq H, Herdean A, Ariza M, Scharn R, Svanteson S, Wengtrom N, Zizka V & Antonelli A (2019) CoordinateCleaner: standardized cleaning of occurrence records from biological collection databases. Methods in Ecology and Evolution, 10(5):744-751, doi:10.1111/2041-210X.13152, https://github.com/ropensci/CoordinateCleaner

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)


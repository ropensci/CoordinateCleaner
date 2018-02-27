[![Build Status](https://travis-ci.org/azizka/CoordinateCleaner.svg?branch=master)](https://travis-ci.org/azizka/CoordianteCleaner)
[![Coverage Status](https://coveralls.io/repos/github/azizka/CoordinateCleaner/badge.svg?branch=master)](https://coveralls.io/github/azizka/CoordinateCleaner?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cran.r-project.org/package=CoordinateCleaner)

# CoordinateCleaner v1.0-6
An R-package to flag of potential errors in geographic coordinates common to biological and palaentological collection data, including tests for

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


The results can be downloaded as a data.frame with separate flags for each test, and visualized using plotting methods. 

# Input data

A tab-delimited text file, including the column headers 'decimallongitude' and 'decimallatitude' and optionally 'species' (for the outlier and duplicate test) and 'countrycode' (for the countrycheck test). Simple csv files downloaded from www.gbif.org can directly be used as input.


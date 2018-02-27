[![Build Status](https://travis-ci.org/azizka/CoordinateCleaner.svg?branch=master)](https://travis-ci.org/azizka/CoordianteCleaner)
[![Coverage Status](https://coveralls.io/repos/github/azizka/CoordinateCleaner/badge.svg?branch=master)](https://coveralls.io/github/azizka/CoordinateCleaner?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CoordinateCleaner)](https://cran.r-project.org/package=CoordinateCleaner)

# CoordinateCleaner v1.0-6
An R-package and shiny app for easy automated flagging of potential errors in geographic coordinates common to biological collection data, including tests for

* General coordinate validity
* Country and province centroids
* Capital coordinates
* Coordinate-country discordance
* Duplicated coordinates per species
* Assignment to the location of the GBIF headquarters
* Outliers
* Urban areas
* Seas
* Plain zeros
* Equal longitude and latitude
* Rounded coordinates
' DDMM to DD.DD coordinate conversion errors

The results can be downloaded as a data.frame with separate flags for each test. All tests are also included in the CleanCoordinate function. 

#Input data
A tab-delimited text file, including the column headers 'decimallongitude' and 'decimallatitude' and optionally 'species' (for the outlier and duplicate test) and 'countrycode' (for the countrycheck test). Simple csv files downloaded from www.gbif.org can directly be used as input.


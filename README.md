[![Coverage Status](https://coveralls.io/repos/github/azizka/CoordinateCleaner/badge.svg?branch=master)](https://coveralls.io/github/azizka/CoordinateCleaner?branch=master)

# CoordinateCleaner v1.0-3
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

The results can be downloaded as a data.frame with separate flags for each test. All tests are also included in the CleanCoordinate function of [speciesgeocodeR](https://github.com/azizka/speciesgeocodeR/tree/master/speciesgeocodeR). 

#Input data
A tab-delimited text file, including the column headers 'decimallongitude' and 'decimallatitude' and optionally 'species' (for the outlier and duplicate test) and 'countrycode' (for the countrycheck test). Simple csv files downloaded from www.gbif.org can directly be used as input.

#Run shiny app online
The app is available at https://azizka.shinyapps.io/CoordinateCleaner/. The online version might currently crash due to memory limitations, running the app locally solves this problem.

#Run shiny app locally
To run the shiny app locally, just copy the following code into your R console.

```{r}
require(DT)
require(geosphere)
require(ggplot2)
require(raster)
require(rgeos)
require(sp)
require(shiny)
require(viridis)
shiny::runGitHub('CoordinateCleaner', 'azizka')
```

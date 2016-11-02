# CoordinateCleaner
A shiny app for easy automated flagging of potential errors in geographic coordinates common to biological collection data, including tests for

* general coordinate validity
* country and province centroids
* capital coordinates
* check if coordinates agree with country information
* Duplicated coordinates per species
* assignment to the location of the GBIF headquaters
* Outliers
* Urban areas
* Seas
* PLain zeros
* equal longitude and latitude

The results can be downloaded as a data.frame with seperate flags for each test. All tests are also included in the CleanCoordinate function of [speciesgeocodeR](https://github.com/azizka/speciesgeocodeR/tree/master/speciesgeocodeR). 

#Input data
A tab-delimited text file, including the column headers 'decimallongitude' and 'decimallatitude' and optionally 'species' (for the outlier and duplicate test) and 'countrycode' (for the countrycheck test). Simple csv files downloaded from www.gbif.org can directly be used as imput.

#Run online
The app is available at https://azizka.shinyapps.io/CoordinateCleaner/. In case of possible memory limitations, the app can be run locally.

#Run locally
Just copy the follwoing code into your R console.

```{r}
reauire(DT)
reauire(geosphere)
require(ggplot2)
require(raster)
require(rgeos)
require(sp)
require(shiny)
require(viridis)
shiny::runGitHub('CoordinateCleaner', 'azizka')
```

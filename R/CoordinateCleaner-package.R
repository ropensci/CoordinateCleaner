

#' Global Capital Locations
#' 
#' A gazetteer of global capital coordinates.
#' 
#' 
#' @name capitals
#' @docType data
#' @format A data frame with 225 observations on the following 4 variables.
#' \describe{\item{ISO3}{a factor, ISO-3 country code.}
#' \item{capital}{a factor, capital names.} \item{longitude}{a
#' numeric vector.} \item{latitude}{a numeric vector.} }
#' @source CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook},
#' Washington, DC.
#' 
#' \url{https://www.cia.gov/library/publications/the-world-factbook/}
#' @keywords gazetteers
#' @examples
#' 
#' data(capitals)
#' str(capitals)
#' 
NULL





#' Global Country and Province Centroids
#' 
#' A gazetteer of country and province centroids. 
#' Centroids are derived from publically available datasets (see sources below).
#' 
#' 
#' @name centroids
#' @docType data
#' @format A data frame with 5142 observations on the following 6 variables. 
#' \describe{ \item{adm1_code}{a factor; province code.}
#' \item{iso3}{a factor; country ISO-3 code.} \item{name}{a
#' factor; name of the country or province.} \item{type}{a character
#' vector; country or province.} \item{longitude}{a numeric vector}
#' \item{latitude}{a numeric vector} }
#' @source 
#' \url{http://www.naturalearthdata.com/}
#' \url{http://thematicmapping.org/downloads/world_borders.php}
#' 
#' CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook}, Washington,
#' DC.
#' \url{https://www.cia.gov/library/publications/the-world-factbook/fields/2011.html}
#' 
#' 
#' @keywords gazetteers
#' @examples
#' 
#' data(centroids)
#' str(centroids)
#' 
NULL





#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_title("CoordinateCleaner")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_description("CoordinateCleaner")}
#' 
#' The DESCRIPTION file:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_DESCRIPTION("CoordinateCleaner")}
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_indices("CoordinateCleaner")}
#' 
#' @name CoordinateCleaner-package
#' @aliases CoordinateCleaner-package CoordinateCleaner
#' @docType package
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_author("CoordinateCleaner")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "CoordinateCleaner")\Sexpr{tools:::Rd_package_maintainer("CoordinateCleaner")}
NULL





#' Country Centroids and Country Capitals
#' 
#' A \code{data.frame} with coordinates of country centroids and country
#' capitals as reference for the \code{\link{clean_coordinates}} function.
#' Coordinates are based on the Central Intelligence Agency World Factbook as
#' provided at \url{http://opengeocode.org/download/cow.php}.
#' 
#' 
#' @name countryref
#' @docType data
#' @format A data frame with 249 observations on 7 variables.
#' @source CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook},
#' Washington, DC.
#' 
#' \url{http://opengeocode.org/download/cow.php}
#' @keywords gazetteers
#' @examples
#' 
#' data(countryref)
#' 
NULL





#' Global Locations of Biodiversity Institutions
#' 
#' A global gazetteer for biodiversity institutions from various sources,
#' including zoos, museums, botanical gardens, GBIF contributors, herbaria,
#' university collections.
#' 
#' 
#' @name institutions
#' @docType data
#' @format A data frame with 12170 observations on 12 variables.
#' @source Compiled from various sources: \itemize{ \item Global Biodiversity
#' Information Facility \url{www.gbif.org} \item Wikipedia
#' \url{www.wikipedia.org} \item Geonames \url{www.geonames.org} \item The Global
#' Registry of Biodiversity Repositories \url{www.grbio.org} \item Index
#' Herbariorum \url{http://sciweb.nybg.org/Science2/IndexHerbariorum.asp}
#' \item Botanic Gardens Conservation International \url{https://www.bgci.org/}
#' }
#' @keywords gazetteers
#' @examples
#' 
#' data(institutions)
#' str(institutions)
#' 
NULL





#' Global Coastlines
#' 
#' A \code{SpatialPolygonsDataFrame} with global coastlines.
#' 
#' 
#' @name landmass
#' @docType data
#' @note Most of the times it might be desirable to only flag records far away
#' from the coast as problematic rather than those close to the coastline
#' (which might be due to disagreements in coastlines, or low GPS uncertainty).
#' For these cases, there is a alternative coastline reference buffered by one
#' degree available at
#' \url{https://github.com/azizka/CoordinateCleaner/tree/master/extra_gazetteers}.
#' @source
#' \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
#' @keywords gazetteers
#' @examples
#' 
#' data("landmass")
#' 
NULL




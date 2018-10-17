
#'Global Coastlines buffered by 1 degree
#'
#'  A \code{SpatialPolygonsDataFrame} with global coastlines, with a 1 degree buffer to extent coastlines as alternative reference for \code{\link{cc_sea}}. Can be useful to identify species in the sea, without flagging records in mangroves, marshes, etc.
#'
#' @name buffland
#' @docType data
#' @source 
#' \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
#' @keywords gazetteers
#' @examples
#' 
#' data("buffland")
NULL


#' CoordinateCleaner
#' 
#' Automated Cleaning of Occurrence Records from Biological Collections
#' 
#' Automated flagging of common spatial and temporal errors in biological and paleontological collection data, for the use in conservation, ecology and paleontology. Includes automated tests to easily flag (and exclude) records assigned to country or province centroid, the open ocean, the headquarters of the Global Biodiversity Information Facility, urban areas or the location of biodiversity institutions (museums, zoos, botanical gardens, universities). Furthermore identifies per species outlier coordinates, zero coordinates, identical latitude/longitude and invalid coordinates. Also implements an algorithm to identify data sets with a significant proportion of rounded coordinates. Especially suited for large data sets. See <https://ropensci.github.io/CoordinateCleaner/> for more details and tutorials.
#' 
#' @name CoordinateCleaner-package
#' @aliases CoordinateCleaner-package CoordinateCleaner
#' @docType package
#' @author Alexander Zizka, Daniele Silvestro, Tobias Andermann, Josue Azevedo, 
#' Camila Duarte Ritter, Daniel Edler, Harith Farooq, Andrei Herdean, Maria Ariza, 
#' Ruud Scharn, Sten Svantesson, Niklas Wengstrom, Vera Zizka 
#' 
NULL



#' Country Centroids and Country Capitals
#' 
#' A \code{data.frame} with coordinates of country and province centroids and country
#' capitals as reference for the \code{\link{clean_coordinates}}, \code{\link{cc_cen}} and \code{\link{cc_cap}} functions.
#' Coordinates are based on the Central Intelligence Agency World Factbook as
#' provided at \url{http://opengeocode.org/download/cow.php} and \url{http://thematicmapping.org/downloads/world_borders.php}.
#' 
#' 
#' @name countryref
#' @docType data
#' @format A data frame with 5,142 observations on 10 variables.
#' #' \describe{ \item{iso3}{ISO-3 code for each country, in case of provinces also referring to the country.}
#' \item{iso2}{ISO-2 code for each country, in case of provinces also referring to the country.} \item{name}{a
#' factor; name of the country or province.} \item{adm1_code}{adm code for countries and provinces} 
#' \item{type}{identifying if the entry refers to a country or province level.} 
#' \item{centroid.lon}{Longitude of the country centroid}
#' \item{centroid.lat}{Latitude of the country centroid}
#' \item{capital}{Name of the country capital, empty for provinces}
#' \item{capital.lon}{Longitude of the country capital}
#' \item{capital.lat}{Latitude of the country capital}}
#' @source CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook},
#' Washington, DC.
#' 
#' \url{http://opengeocode.org/download/cow.php}
#' \url{http://thematicmapping.org/downloads/world_borders.php}
#' @keywords gazetteers
#' @examples
#' 
#' data(countryref)
#' head(countryref)
NULL

#' Deprecated functions in CoordinateCleaener
#' 
#' These functions still work but will be removed (defunct) in the next version.
#' 
#' \itemize{
#'  \item \code{\link{CleanCoordinates}}: This function is deprecated, and will
#'  be removed in the next version of this package. Use \code{\link{clean_coordinates}}
#'  instead
#'  \item \code{\link{CleanCoordinatesDS}}: This function is deprecated, and will
#'  be removed in the next version of this package. Use \code{\link{clean_dataset}}
#'  instead
#'  \item \code{\link{CleanCoordinatesFOS}}: This function is deprecated, and will
#'  be removed in the next version of this package. Use \code{\link{clean_fossils}}
#'  instead
#' }
#' 
#' @name CoordinateCleaner-deprecated
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
#' Herbariorum \url{http://sweetgum.nybg.org/science/ih/}
#' \item Botanic Gardens Conservation International \url{https://www.bgci.org/}
#' }
#' @keywords gazetteers
#' @examples
#' 
#' data(institutions)
#' str(institutions)
#' 
NULL


#' Example data from the Paleobiologydatabase
#' 
#' A dataset of 5000 flowering plant fossil occurrences as example for data of the paleobiology Database, downloaded using the paleobioDB packages as specified in the vignette \dQuote{Cleaning_PBDB_fossils_with_CoordinateCleaner}.
#' 
#' 
#' @name pbdb_example
#' @docType data
#' @format A data frame with 5000 observations on 36 variables.
#' @source \itemize{ 
#' \item The Paleobiology database \url{https://paleobiodb.org/} 
#' \item Sara Varela, Javier Gonzalez Hernandez and Luciano Fabris Sgarbi (2016). 
#' paleobioDB: Download and Process Data from the Paleobiology Database. 
#' R package version 0.5.0. \url{https://CRAN.R-project.org/package=paleobioDB}.
#' }
#' @keywords gazetteers
#' @examples
#' 
#' data(institutions)
#' str(institutions)
#' 
NULL
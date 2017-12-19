\name{CleanCoordinates}
\alias{CleanCoordinates}
\alias{summary.spatialvalid}
\alias{is.spatialvalid}

\title{
Geographic Cleaning of Coordinates from Biologic Collections
}
\description{
Cleaning geographic coordinates by multiple empirical tests to flag potentially erroneous coordinates, addressing issues common in biological collection databases.
}
\usage{
CleanCoordinates(x, lon = "decimallongitude", lat = "decimallatitude", 
                species = "species", countries = NULL, 
                capitals = T, centroids = T, 
                countrycheck = F, duplicates = F, equal = T, 
                GBIF = T, institutions = T, outliers = F, seas = T,
                urban = F, zeros = T, 
                capitals.rad = 0.05, centroids.rad = 0.01,
                centroids.detail = "both", inst.rad = 0.001, 
                outliers.method = "quantile", outliers.mtp = 3,
                outliers.td = 1000, outliers.size = 7, zeros.rad = 0.5,
                capitals.ref, centroids.ref, country.ref,
                inst.ref, seas.ref, urban.ref,
                value = "spatialvalid", verbose = T,
                report = F)
}

\arguments{
  \item{x}{
a data.frame. Containing geographical coordinates and species names.
}
  \item{lon}{
a character string. The column with the longitude coordinates. Default = \dQuote{decimallongitude}.
}
  \item{lat}{
a character string. The column with the longitude coordinates. Default = \dQuote{decimallatitude}.
}
  \item{species}{
a character string. A vector of the same length as rows in x, with the species identity for each record.  If missing, the outliers test is skipped.
}
  \item{countries}{
a character string. A vector of the same length as rows in x, with country information for each record in ISO3 format.  If missing, the countries test is skipped.
}
  \item{capitals}{
logical. If TRUE, tests a radius around adm-0 capitals. The radius is \code{capitals.rad}. Default = TRUE.
}
  \item{centroids}{
logical. If TRUE, tests a radius around country centroids. The radius is \code{centroids.rad}. Default = TRUE.
}
  \item{countrycheck}{
logical.  If TRUE, tests if coordinates are from the country indicated in the country column.  Default = FALSE.
}
  \item{duplicates}{
logical.  If TRUE, tests for duplicate records. This checks for identical coordinates or if a species vector is provided for identical coordinates within a species. All but the first records are flagged as duplicates.  Default = FALSE.
}
  \item{equal}{
logical.  If TRUE, tests for equal absolute longitude and latitude.  Default = TRUE.
}
  \item{GBIF}{
logical.  If TRUE, tests a one-degree radius around the GBIF headquarters in Copenhagen, Denmark.  Default = TRUE.
}
  \item{institutions}{
logical. If TRUE, tests a radius around known biodiversity institutions from \code{instiutions}. The radius is \code{inst.rad}. Default = TRUE.
}
  \item{outliers}{
logical. If TRUE, tests each species for outlier records. Depending on the \code{outliers.mtp} and \code{outliers.td} arguments  either flags records that are a minimum distance away from all other records of this species (\code{outliers.td}) or records that are outside a multiple of the interquartile range of minimum distances to the next neighbour of this species (\code{outliers.mtp}).  Default = TRUE. 
}
  \item{seas}{
logical. If TRUE, tests if coordinates fall into the ocean.  Default = TRUE.
}
  \item{urban}{
logical. If TRUE, tests if coordinates are from urban areas.  Default = FALSE.
}
  \item{zeros}{
logical. If TRUE, tests for plain zeros, equal latitude and longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.  Default = TRUE.
}
  \item{capitals.rad}{
numeric. The radius around capital coordinates in degrees. Default = 0.1.
}
  \item{centroids.rad}{
numeric. The side length of the rectangle around country centroids in degrees. Default = 0.01.
}
  \item{centroids.detail}{
a \code{character string}. If set to \sQuote{country} only country (adm-0) centroids are tested, if set to \sQuote{provinces} only province (adm-1) centroids are tested.  Default = \sQuote{both}.
}
  \item{inst.rad}{
numeric. The radius around biodiversity institutions coordinates in degrees. Default = 0.001.
}
  \item{outliers.method}{
The method used for outlier testing. See details.
}
  \item{outliers.mtp}{
numeric. The multiplier for the interquartile range of the outlier test.  If NULL \code{outliers.td} is used.  Default = 3.
}
  \item{outliers.td}{
numeric.  The minimum distance of a record to all other records of a species to be identified as outlier, in km. Default = 1000.
}
  \item{outliers.size}{
numerical.  THe minimum number of records in a dataset to run the taxon-specific outlier test.  Default = 7.
}
  \item{zeros.rad}{
numeric. The radius around 0/0 in degrees. Default = 0.5.
}
  \item{capitals.ref}{
a \code{data.frame} with alternative reference data for the country capitals test. If missing, the \code{capitals} dataset is used.  Alternatives must be identical in structure.
}
  \item{centroids.ref}{
a \code{data.frame} with alternative reference data for the centroid test. If missing, the \code{centroids} dataset is used.  Alternatives must be identical in structure.
}
  \item{country.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the countrycheck test. If missing, the \code{rnaturalearth:ne_countries('medium')} dataset is used.
}
  \item{inst.ref}{
a \code{data.frame} with alternative reference data for the biodiversity institution test. If missing, the \code{institutions} dataset is used.  Alternatives must be identical in structure.
}
  \item{seas.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the seas test. If missing, the \code{\link{landmass}} dataset is used.
}
  \item{urban.ref}{
a \code{SpatialPolygonsDataFrame} as alternative reference for the urban test. If missing, the test is skipped. See details for a reference gazetteers.
}
  \item{value}{
a character string defining the output value. See the value section for details. one of \sQuote{spatialvalid}, \sQuote{summary}, \sQuote{cleaned}. Default = \sQuote{\code{spatialvalid}}.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged
}
  \item{report}{
logical or character.  If TRUE a report file is written to the working directory, summarizing the cleaning results. If a character, the path to which the file should be written.  Default = FALSE.
}
}

\details{
The function needs all coordinates to be formally valid according to WGS84. If the data contains invalid coordinates, the function will stop and return a vector flagging the invalid records. TRUE = non-problematic coordinate, FALSE = potentially problematic coordinates. A reference gazetteer for the urban test is available at at \url{https://github.com/azizka/CoordinateCleaner/tree/master/extra_gazetteers}. Three different methods are available for the outlier test: "If \dQuote{outlier} a boxplot method is used and records are flagged as outliers if their \emph{mean} distance to all other records of the same species is larger than mltpl * the interquartile range of the mean distance of all records of this species. If \dQuote{mad} the median absolute deviation is used. In this case a record is flagged as outlier, if the \emph{mean} distance to all other records of the same species is larger than the median of the mean distance of all points plus/minus the mad of the mean distances of all records of the species * mltpl. If \dQuote{distance} records are flagged as outliers, if the \emph{minimum} distance to the next record of the species is > \code{tdi}}

\value{
Depending on the output argument:
\describe{
\item{\dQuote{spatialvalid}}{an object of class \code{spatialvalid} with one column for each test. TRUE = clean coordinate, FALSE = potentially problematic coordinates.  The summary column is FALSE if any test flagged the respective coordinate.}
\item{\dQuote{flags}}{a logical vector with the same order as the input data summarizing the results of all test. TRUE = clean coordinate, FALSE = potentially problematic (= at least one test failed).}
\item{\dQuote{cleaned}}{a \code{data.frame} of cleaned coordinates if \code{species = NULL} or a \code{data.frame} with cleaned coordinates and species ID otherwise}
}
}

\note{
Always tests for coordinate validity: non-numeric or missing coordinates and coordinates exceeding the global extent (lon/lat, WGS84).

See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{

exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(x = exmpl)

summary(test)
}
\keyword{ Coordinate cleaning wrapper }


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
CleanCoordinates(x, countries, species,
                output = "spatialvalid", report = F, capitals = T,
                centroids = T, countrycheck = F, duplicates = F, 
                GBIF = T, institutions = T, outliers = F, seas = T,
                urban = F, validity = T, zeros = T, verbose = T,
                capitals.rad = 0.05, centroids.rad = 0.01,
                centroids.detail = "both", inst.rad = 0.001, 
                outliers.method = "dist.quantile", outliers.mtp = 3, 
                outliers.td = 1000, zeros.rad = 0.5,
                capitals.ref, centroids.ref, country.ref,
                inst.ref, seas.ref, urban.ref)
}

\arguments{
  \item{x}{
a \code{matrix} or \code{data.frame} with two columns in the format: decimallongitude, decimallatitude.  
}
  \item{countries}{
character. A vector of the same length as rows in x, with country information for each record in ISO3 format.  If missing, the countries test is skipped.
}
  \item{species}{
character. A vector of the same length as rows in x, with the species identity for each record.  If missing, the outliers test is skipped.
}
  \item{output}{
a character string defining the output value. See the value section for details. one of \sQuote{spatialvalid}, \sQuote{summary}, \sQuote{cleaned}. Default = \sQuote{\code{spatialvalid}}.
}
  \item{report}{
logical or character.  If TRUE a report file is written to the working directory, summarizing the cleaning results. If a character, the path to which the file should be written.  Default = FALSE.
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
  \item{GBIF}{
logical.  If TRUE, tests a one-degree radius around the GBIF headquarters in Copenhagen, Denmark.  Default = TRUE.
}
  \item{institutions}{
logical. If TRUE, tests a radius around known biodiveristy institutions from \code{instiutions}. The radius is \code{inst.rad}. Default = TRUE.
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
  \item{validity}{
logical. If TRUE, tests for non-numeric or missing coordinates and coordinates exceeding the global extent (lon/lat, WGS84). Default = TRUE
}
  \item{zeros}{
logical. If TRUE, tests for plain zeros, equal latitude and longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.  Default = TRUE.
}
  \item{verbose}{
logical.  If TRUE the progress of testing is reported (to stdout).  Default = TRUE
}
  \item{capitals.rad}{
numeric. The radius around capital coordinates in degrees. Default = 0.1
}
  \item{centroids.rad}{
numeric. The side length of the rectangle around country centroids in degrees. Default = 0.01
}
  \item{centroids.detail}{
a \code{character string}. If set to \sQuote{country} only country (adm-0) centroids are tested, if set to \sQuote{provinces} only province (adm-1) centroids are tested.  Default = \sQuote{both}.
}
  \item{inst.rad}{
numeric. The radius around biodiveristy institutions coordinates in degrees. Default = 0.001
}
  \item{outliers.method}{
The method used for outlier testing. See details.
}

  \item{outliers.mtp}{
numeric. The multiplier for the interquartile range of the outlier test.  If NULL \code{outliers.td} is used.  Default = 3.
}
  \item{outliers.td}{
numeric.  The minimum distance of a record to all other records of a species to be indentified as outlier, in km. Default = 1000.
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
}

\details{
The function needs all coordinates to be formally valid according to WGS84. If the data contains invalid coordinates, the function will stop and return a vector flagging the invalid records. TRUE = non-problematic coordinate, FALSE = potentially problematic coordinates. A reference gazetteer for the urban test is available at at https://github.com/azizka/speciesgeocodeR/extra_gazetteers. Three different methods are available for the outlier test: "dist.distance" calculates the minimum distance to the next record of the same species and flags all distance above the outliers.td threshold provided; "dist.quantile" calculates the mean distance to all other records of the same species and uses
an approach based on the Tukey box plot method, with boundaries at outliers.mtp times the interquartile range (IQR); "dist.map" also calculates mean distance to all points, bu uses the absolute deviation from the median, with a
limit at outliers.mtp times the median absolute deviation (MAD).}

\value{
Depending on the output argument:
\describe{
\item{\dQuote{spatialvalid}}{a \code{data.frame} with one column for each test. TRUE = clean coordinate, FALSE = potentially problematic coordinates.  The summary column is FALSE if any test flagged the respective coordinate.}
\item{\dQuote{summary}}{a logical vector with the same order as the input data summarizing the results of all test. TRUE = clean coordinate, FALSE = potentially problematic (= at least one test failed).}
\item{\dQuote{cleaned}}{a \code{data.frame} of cleaned coordinates if \code{species = NULL} or a \code{data.frame} with cleaned coordinates and species ID otherwise}
}
}

\note{
See \url{https://github.com/azizka/speciesgeocodeR/wiki} for more details and tutorials.
}

\examples{

exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
                    decimallongitude = runif(250, min = 42, max = 51),
                    decimallatitude = runif(250, min = -26, max = -11))

test <- CleanCoordinates(exmpl[, 2:3],species = exmpl[, 1], verbose = FALSE)

summary(test)
plot(test)
}
\keyword{ Coordinate cleaning }


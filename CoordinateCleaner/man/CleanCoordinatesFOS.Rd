\name{CleanCoordinatesFOS}
\alias{CleanCoordinatesFOS}

\title{
Geographic and Temporal Cleaning of Records from Fossil Collections
}
\description{
Cleaning records by multiple empirical tests to flag potentially erroneous coordinates and time-spans, addressing issues common in fossil collection databases.
}
\usage{
CleanCoordinatesFOS(x, lon = "lng", lat = "lat", min.age = "min_ma", max.age = "max_ma", 
                    taxon = "identified_name", countries = "cc", 
                    centroids = T, countrycheck = T, 
                    equal = T, GBIF = T, institutions = T, 
                    temp.range.outliers = T, spatio.temp.outliers = T, 
                    temp.ages.equal = T, 
                    zeros = T, centroids.rad = 0.01, 
                    centroids.detail = "both", 
                    inst.rad = 0.001, outliers.method = "quantile", 
                    outliers.threshold = 3, outliers.size = 7, 
                    outliers.replicates = 5,
                    zeros.rad = 0.5, centroids.ref, country.ref, inst.ref, 
                    value = "spatialvalid", verbose = T, report = F)
}


%- maybe also 'usage' for other objects documented here.
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
  \item{min.age}{
a character string. The column with the minimum age. Default = \dQuote{min_ma}.
}
  \item{max.age}{
a character string. The column with the maximum age. Default = \dQuote{max_ma}.
}
  \item{taxon}{
a character string. The column with the taxon name. If \dQuote{}, searches for outliers over the entire dataset, otherwise per specified taxon. Default = \dQuote{identified_name}.
}
 \item{countries}{
a character string. A vector of the same length as rows in x, with country information for each record in ISO3 format.  If missing, the countries test is skipped.
}
  \item{centroids}{
logical. If TRUE, tests a radius around country centroids. The radius is \code{centroids.rad}. Default = TRUE.
}
  \item{countrycheck}{
logical.  If TRUE, tests if coordinates are from the country indicated in the country column.  Default = FALSE.
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
  \item{temp.range.outliers}{
logical. If TRUE, tests for records with unexpectedly large temporal ranges, using a quantile-based outlier test. Default = TRUE.
}
  \item{spatio.temp.outliers}{
logical. IF TRUE, test for records which are outlier in time and space. See \code{\link{dc_round}} for details.  Default = TRUE.
}
  \item{temp.ages.equal}{
logical. If TRUE, flags records with equal minimum and maximum age. Default = TRUE.
}
 \item{zeros}{
logical. If TRUE, tests for plain zeros, equal latitude and longitude and a radius around the point 0/0. The radius is \code{zeros.rad}.  Default = TRUE.
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
  \item{outliers.threshold}{
numerical.  The multiplier for the interquantile range for outlier detection. The higher the number, the more conservative the outlier tests.   See \code{\link{dc_round}} for details. Default = 3.
}
  \item{outliers.size}{
numerical.  The minimum number of records in a dataset to run the taxon-specific outlier test.  Default = 7.
}
  \item{outliers.replicates}{
numeric. The number of replications for the distance matrix calculation. See details.  Default = 5.
}
  \item{zeros.rad}{
numeric. The radius around 0/0 in degrees. Default = 0.5.
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
The outlier detection is based on an interquantile range test. In a first step a distance matrix of geographic distances among all records is calculate. Subsequently a similar distance matrix of temporal distances among all records is calculated based on a single point selected by random between the minimum and maximum age for each record. The mean distance for each point to all neighbours is calculated for both matrices and spatial and temporal distances are scaled to the same range. The sum of these distanced is then tested against the interquantile range and flagged as an outlier if $x > IQR(x) + q_75 * mltpl$. The test is replicated \sQuote{replicates} times, to account for temporal uncertainty. Records are flagged as outliers if they are flagged by a fraction of more than \sQuote{flag.thres} replicates. Only datasets/taxa comprising more than \sQuote{size.thresh} records are tested. Note that geographic distances are calculated as geospheric distances for datasets (or taxa) with less than 10,000 records and approximated as Euclidean distances for datasets/taxa with 10,000 to 25,000 records. Datasets/taxa comprising more than 25,000 records are skipped.
}
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
minages <- runif(250, 0, 65)
exmpl <- data.frame(identified_name = sample(letters, size = 250, replace = TRUE),
                    lng = runif(250, min = 42, max = 51),
                    lat = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65))

test <- CleanCoordinatesFOS(x = exmpl)

summary(test)
}
\keyword{ Fossil }
\keyword{ Coordinate cleaning }
\keyword{ Temporal cleaning }

\name{cc_cen}
\alias{cc_cen}

\title{
Flag Coordinates in Vicinity of Country and Province Centroids
}
\description{
Flags records within a radius around the geographic centroids of political countries and provinces. Poorly geo-referenced occurrence records in biological databases are often erroneously geo-referenced to centroids.
}
\usage{
cc_cen(x, lon = "decimallongitude", lat = "decimallatitude", 
       buffer = 0.1, test = "both", ref = NULL, value = "clean", verbose = TRUE)
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
  \item{buffer}{
numerical. The buffer around each province or country centroid, where records should be flagged as problematic, in decimal degrees.  Default = 0.1.
}
  \item{test}{
a character string. Specifying the details of the test. One of c(\dQuote{both}, \dQuote{country}, \dQuote{provinces}).  If both tests for country and province centroids.
}
  \item{ref}{
a SpatialPointsDataframe. Providing the geographic gazetteer. Can be any SpatialPointsDataframe, but the structure must be identical to \code{\link{centroids}}.  Default = \code{\link{centroids}}
}
  \item{value}{
a character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\details{
Note: the buffer radius is in degrees, thus will differ slightly between different latitudes.
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_cen(x)
cc_cen(x, value = "flags")
}

\keyword{ Coordinate cleaning }

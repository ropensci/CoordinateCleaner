\name{cc_coun}
\alias{cc_coun}

\title{
Flag coordinates outside their reported country
}
\description{
Identifies mismatches between geographic coordinates and additional country information (usually this information is reliably reported with specimens). Such a mismatch can occur for example, if latitude and longitude are switched.
}
\usage{
cc_coun(x, lon = "decimallongitude", lat = "decimallatitude", iso3 = "countrycode", value = "clean", ref = NULL, verbose = TRUE)
}

\arguments{
  \item{x}{
a data.frame. Containing geographical coordinates and species names, and a country assignment.
}
  \item{lon}{
a character string. The column with the longitude coordinates. Default = \dQuote{decimallongitude}.
}
  \item{lat}{
a character string. The column with the longitude coordinates. Default = \dQuote{decimallatitude}.
}
  \item{iso3}{
a character string. The column with the country assignment of each record in three letter ISO code. \dQuote{countrycode}.
}
  \item{ref}{
a SpatialPolygonDataframe. Providing the geographic gazetteer. Can be any SpatialPolygonsDataframe, but the structure must be identical to \code{\link{countryref}.  Default = \code{\link{centroids}}}
}
  \item{value}{
character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged
}
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

cc_coun(x)
cc_coun(x, value = "flags")
}

\keyword{ "Coordinate cleaning" }

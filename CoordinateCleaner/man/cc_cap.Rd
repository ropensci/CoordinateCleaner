\name{cc_cap}
\alias{cc_cap}

\title{
Flag Coordinates in Vicinity of Country Capitals.
}
\description{
Flags records within a certain radius around country capitals. Poorly georeferenced occurrence records in biological databases are often erroneously geo-referenced to capitals.
}
\usage{
cc_cap(x, lon = "decimallongitude", lat = "decimallatitude", 
       buffer = 0.1, ref = NULL, value = "clean", verbose = TRUE)
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
The buffer around each capital coordinate (the centre of the city), where records should be flagged as problematic, in decimal degrees.  Default = 0.1.
}
  \item{ref}{
a SpatialPointsDataframe. Providing the geographic gazetteer. Can be any SpatialPointsDataframe, but the structure must be identical to \code{\link{capitals}}.  Default = \code{\link{capitals}}
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

cc_cap(x)
cc_cap(x, value = "flags")
}

\keyword{ Coordinate cleaning }

\name{cc_urb}
\alias{cc_urb}

\title{
Flag records inside urban areas
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cc_urb(x, lon = "decimallongitude", lat = "decimallatitude", ref = NULL, value = "clean", verbose = T)
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
  \item{ref}{
a SpatialPolygonDataframe. Providing the geographic gazetteer with the urban areas. See details.
}
  \item{value}{
character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged
}
}
\details{
No default reference is provided with the package due to the large file size of such (global) gazetteers. You can download an example here: Can be any SpatialPolygonDataframe, but the structure must be identical to \code{\link{centroids}.  Default = \code{\link{centroids}}}
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
x <- data.frame(species = letters[1:10], 
                decimallongitude = c(runif(106, -180, 180), NA, "13W33'", "67,09", 305), 
                decimallatitude = runif(110, -90,90))
                
cc_urb(x)
cc_urb(x, value = "flags")
}

\keyword{ "Coordinate cleaning" }

\name{cc_urb}
\alias{cc_urb}

\title{
Flag records inside urban areas
}
\description{
Flags records from inside urban areas, absed on a geographic gazetteer. Often records from large databases spann subtantial time periods (centuries) and old records might represent habitats which today are replaced by city area.
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
character string. Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged
}
}
\details{
No default reference is provided with the package due to the large file size of such (global) gazetteers. You can download an example here: \url{https://github.com/azizka/CoordinateCleaner/blob/master/extra_gazetteers/urbanareas.rda}. Can be any SpatialPolygonDataframe, but the structure must be identical to \code{\link{centroids}.  Default = \code{\link{centroids}}}
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
\notrun{
# load reference 
#download from https://github.com/azizka/CoordinateCleaner/blob/master/extra_gazetteers/urbanareas.rda
load("extra_gazetteers/urbanareas.rda")

x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90))
                
cc_urb(x, ref = urbanareas)
cc_urb(x, value = "flags", ref = urbanareas)
}
}

\keyword{ "Coordinate cleaning" }

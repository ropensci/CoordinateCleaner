\name{cc_sea}
\alias{cc_sea}

\title{
Flag Non-terrestrial Coordinates
}
\description{
Flags coordinates outside the reference landmass. Can be used to restrict datasets to terrestrial taxa, or exclude records from the open ocean, when depending on the reference (see details). Often records of terrestrial taxa can be found in the open ocean, mostly due to switched latitude and longitude.
}
\usage{
cc_sea(x, lon = "decimallongitude", lat = "decimallatitude", 
       ref = NULL, value = "clean", verbose = TRUE)
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
a SpatialPolygonsDataframe. Providing the geographic gazetteer. Can be any SpatialPolygonsDataframe, but the structure must be identical to \code{\link{landmass}}. See details.  Default = \code{\link{landmass}}
}
  \item{value}{
a character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\details{
In some cases flagging records close of the coastline is not recommendable, because of the low precision of the reference dataset, minor GPS imprecision or because a dataset might include coast or marshland species. If you only want to flag records in the open ocean, consider using a buffered landmass reference, e.g.: \url{https://github.com/azizka/CoordinateCleaner/tree/master/extra_gazetteers}.
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
x <- data.frame(species = letters[1:10], 
                decimallongitude = runif(10, -30, 30), 
                decimallatitude = runif(10, -30, 30))
                
cc_sea(x, value = "flags")

}

\keyword{ Coordinate cleaning }

\name{cc_gbif}
\alias{cc_gbif}

\title{
Flag records assigned to GBIF headquarters
}
\description{
Flags records within 0.5 degree radius around the GBIF headquarters in Copenhagen, DK.
}
\usage{
cc_gbif(x, lon = "decimallongitude", lat = "decimallatitude", value = "clean", verbose = TRUE)
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
  \item{value}{
character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged
}
}
\details{
Not recommended if woring with records from DEnmark or the COpenhagen area.
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
x <- data.frame(species = "A", 
                decimallongitude = c(12.58, 12.58), 
                decimallatitude = c(55.67, 30.00))
                
cc_gbif(x)
cc_gbif(x, value = "flags")
}

\keyword{ "Coordinate cleaning" }

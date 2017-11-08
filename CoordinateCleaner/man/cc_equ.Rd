\name{cc_equ}
\alias{cc_equ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flags Records with Identical lat/lon
}
\description{
Flags records with equal latitude and longitude coordinates, either exact or absolute. Equal coordinates can often indicate data entry errors.
}
\usage{
cc_equ(x, lon = "decimallongitude", lat = "decimallatitude", 
       test = "absolute", value = "clean", verbose = T)
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
  \item{test}{
character string. Defines if coordinates are compared exactly (\dQuote{identical}) or on the absolute scale (i.e. -1 = 1, \dQuote{absolute}). Default is to \dQuote{absolute}.
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

cc_equ(x)
cc_equ(x, value = "flags")
}

\keyword{ "Coordinate cleaning" }

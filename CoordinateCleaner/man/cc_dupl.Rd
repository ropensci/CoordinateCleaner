\name{cc_dupl}
\alias{cc_dupl}

\title{
Flag duplicated records
}
\description{
Flags duplicated records based on species name and coordinates, as well as user-defined additional columns. True (specimen) duplicates or duplicates from the same species can make up the bulk of records in a biological collection database, but are undesirable for many analyses. Both can be flagged with this function, the former given enough additional information.
}
\usage{
cc_dupl(x, lon = "decimallongitude", lat = "decimallatitude", species = "species", additions = NULL, value = "clean", verbose = T)
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
  \item{species}{
a character string. The column with the species name. Default = \dQuote{species}.
}
  \item{additions}{
a vector of character strings. Additional columns to be included in the test for duplication. For example as below, collector name and collector number.
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
                decimallongitude = sample(x = 0:10, size = 100, replace = TRUE), 
                decimallatitude = sample(x = 0:10, size = 100, replace = TRUE),
                collector = "Bonpl",
                collector.number = c(1001, 354),
                collection = rep(c("K", "WAG","FR", "P", "S"), 20))

cc_dupl(x, value = "flags")
cc_dupl(x, additions = c("collector", "collector.number"))
}

\keyword{ "Coordinate cleaning" }

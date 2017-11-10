\name{institutions}
\alias{institutions}
\docType{data}
\title{
Global Locations of Biodiversity Institutions
}
\description{
A global gazetteer for biodiversity institutions from various sources, including zoos, museums, botanical gardens, GBIF contributors, herbaria, university collections.
}
\usage{data("institutions")}
\format{
  A data frame with 12170 observations on 12 variables.
}

\source{
Compiled from various sources:
\itemize{
\item{Global Biodiversity Information Facility \url{www.gbif.org}}
\item{Wikipedia \url{www.wikipedia.org}}
\item{Geonames \url{www.geonames.org}}
\item{The Global Registry of Biodiversity Repositories \url{www.grbio.org}}
\item{Index Herbariorum \url{http://sciweb.nybg.org/Science2/IndexHerbariorum.asp}}
\item{Botanic Gardens Conservation International \url{https://www.bgci.org/}}
}
}

\examples{
data(institutions)
str(institutions)
}
\keyword{gazetteers}

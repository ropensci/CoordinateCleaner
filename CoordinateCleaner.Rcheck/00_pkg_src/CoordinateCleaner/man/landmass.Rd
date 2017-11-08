\name{landmass}
\alias{landmass}
\docType{data}
\title{
Global Coastlines
}
\description{
A \code{SpatialPolygonsDataFrame} with global coastlines.
}
\usage{data("landmass")}

\note{
Most of the times it might be desirable to only flag records far away from the coast as problematic rather than thos close to the coastline (which might be due to disagreeements in coastlines, or low gps uncertainty). For these cases, there is a alternative coastline reference buffered by one degree available at \url{https://github.com/azizka/speciesgeocodeR/tree/master/extra_gazetteers}.
}

\source{
\url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
}

\examples{
data("landmass")
}
\keyword{gazetteers}

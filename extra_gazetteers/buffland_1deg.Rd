\name{buffland}
\alias{buffland}
\docType{data}
\title{
Global Coastlines buffered by 1 degree
}
\description{
A \code{SpatialPolygonsDataFrame} with global coastlines, with a 1 degree buffer to extent coastlines as input for speciesgeocodeR::CleanCoordinates. Can be useful to identify species in the sea, without flagging records in mangroves, marshes, etc.
}
\usage{data("landmass")}

\source{
\url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
}

\examples{
data("landmass")
plot(landmass)
}
\keyword{datasets}
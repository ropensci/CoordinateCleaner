\name{urbanareas}
\alias{urbanareas}
\docType{data}
\title{
Global Urban Areas
}
\description{
A \code{SpatialPolygonsDataFrame} of major global urban areas, derived from 2002-2003 MODIS remote sensing data.
}
\usage{data("urbanareas")}

\source{
\url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
}
\references{
Schneider, A., Friedl, M. A., McIver D. K., Woodcock C. E. (2003) Mapping urban areas by fusing multiple sources of coarse resolution remotely sensed data. \emph{Photogrammetric Engineering and Remote Sensing} \bold{69}, 1377--1386.
}
\examples{
data(urbanareas)
plot(urbanareas)
}
\keyword{datasets}

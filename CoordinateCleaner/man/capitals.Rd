\name{capitals}
\alias{capitals}
\docType{data}
\title{
Global Capital Locations
}
\description{
A gazetteer of global capital coordinates.
}
\usage{data("capitals")}
\format{
  A data frame with 225 observations on the following 4 variables.
  \describe{
    \item{\code{ISO3}}{a factor, ISO-3 country code.}
    \item{\code{capital}}{a factor, capital names.}
    \item{\code{longitude}}{a numeric vector.}
    \item{\code{latitude}}{a numeric vector.}
  }
}

\source{
CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook}, Washington, DC.

\url{https://www.cia.gov/library/publications/the-world-factbook/}
}

\examples{
data(capitals)
str(capitals)
}
\keyword{gazetteers}

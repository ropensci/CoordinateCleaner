\name{centroids}
\alias{centroids}
\docType{data}
\title{
Global Country and Province Centroids
}
\description{
A gazetteer of country and province centroids.
}
\usage{data("centroids")}
\format{
  A data frame with 5142 observations on the following 6 variables.
  \describe{
    \item{\code{adm1_code}}{a factor; province code.}
    \item{\code{iso3}}{a factor; country ISO-3 code.}
    \item{\code{name}}{a factor; name of the country or province.}
    \item{\code{type}}{a character vector; country or province.}
    \item{\code{longitude}}{a numeric vector}
    \item{\code{latitude}}{a numeric vector}
  }
}

\source{
\url{http://www.naturalearthdata.com/}

CENTRAL INTELLIGENCE AGENCY (2014) \emph{The World Factbook}, Washington, DC.

\url{https://www.cia.gov/library/publications/the-world-factbook/fields/2011.html}

}
\examples{
data(centroids)
str(centroids)
}
\keyword{gazetteers}

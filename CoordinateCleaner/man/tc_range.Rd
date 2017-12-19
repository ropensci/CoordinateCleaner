\name{tc_range}
\alias{tc_range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Flag Fossils with Extreme Age Ranges
}
\description{
Flags record with an unexpectedly large temporal range, based on a quantile outlier test. 
}
\usage{
tc_range(x, min.age = "min_ma", max.age = "max_ma", taxon = "identified_name", 
        method = "quantile", mltpl = 5,  size.thresh = 7, max.range = 500,
        value = "clean", verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a data.frame. Containing geographical coordinates and species names.
}
  \item{min.age}{
a character string. The column with the minimum age. Default = \dQuote{min_ma}.
}
  \item{max.age}{
a character string. The column with the maximum age. Default = \dQuote{max_ma}.
}
  \item{taxon}{
a character string. The column with the taxon name. If \dQuote{}, searches for outliers over the entire dataset, otherwise per specified taxon. Default = \dQuote{identified_name}.
}
  \item{method}{
a character string.  Defining the method for outlier selection.  See details. Either \dQuote{quantile} or \dQuote{mad}.  Default = \dQuote{quantile}.
}
  \item{mltpl}{
numeric. The multiplier of the interquartile range (\code{method == 'quantile'}) or median absolute deviation (\code{method == 'mad'})to identify outliers. See details.  Default = 3.
}
  \item{size.thresh}{
numeric.  The minimum number of records needed for a dataset to be tested. Default = 10.
}
  \item{max.range}{
%%     ~~Describe \code{max.range} here~~
}
  \item{value}{
a character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}

\examples{
minages <- runif(n = 11, min = 0.1, max = 25)
x <- data.frame(species = c(letters[1:10], "z"), 
                min_ma = minages, 
                max_ma = minages + c(runif(n = 10, min = 0, max = 5), 25))

tc_range(x, value = "flags", taxon = "")
}
\keyword{ Fossil }
\keyword{ Temporal cleaning }
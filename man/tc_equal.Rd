\name{tc_equal}
\alias{tc_equal}

\title{
Flag Fossils with equal min and max age
}
\description{
Flags records of fossil with equal minimum and maximum age.
}
\usage{
tc_equal(x, min.age = "min_ma", max.age = "max_ma", 
         value = "clean", verbose = TRUE)
}

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
minages <- runif(n = 10, min = 0.1, max = 25)
x <- data.frame(species = letters[1:10], 
                min_ma = minages, 
                max_ma = minages + runif(n = 10, min = 0, max = 10))
x <- rbind(x, data.frame(species = "z", 
                min_ma = 5, 
                max_ma = 5))
                
tc_equal(x, value = "flags")
}

\keyword{ Temporal cleaning }
\keyword{ Fossils }

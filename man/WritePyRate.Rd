\name{WritePyRate}
\alias{WritePyRate}

\title{
Create Input Files for PyRate
}
\description{
Creates the input necessary to run Pyrate, based on a data.frame with fossil ages (as derived e.g. from CleanCoordinatesFOS) and a vector of the extinction status for each sample. Creates files in the working directory!
}
\usage{
WritePyRate(x, taxon = "accepted_name", min.age = "min_ma", max.age = "max_ma", 
            status = NULL, trait = NULL, fname = NULL, path = getwd(), 
            replicates = 1, cutoff = NULL, random = TRUE)
}

\arguments{
  \item{x}{
a data.frame. Containing geographical coordinates and species names.
}
  \item{taxon}{
a character string. The column with the taxon name. If \dQuote{}, searches for outliers over the entire dataset, otherwise per specified taxon. Default = \dQuote{identified_name}.
}
  \item{min.age}{
a character string. The column with the minimum age. Default = \dQuote{min_ma}.
}
  \item{max.age}{
a character string. The column with the maximum age. Default = \dQuote{max_ma}.
}
  \item{status}{
a vector of character strings of length \code{nrow(x)}. Indicating for each record \dQuote{extinct} or \dQuote{extant}.
}
  \item{trait}{
a numeric vector of length \code{nrow(x)}. Indicating trait values for each record. Optional.  Default = NULL.
}
  \item{fname}{
a character string. The prefix to use for the output files.
}
  \item{path}{
a character string. giving the absolute path to write the output files. Default is the working directory.
}
  \item{replicates}{
a numerical. The number of replicates for the randomized age generation. See details. Default = 1.
}
  \item{cutoff}{
a numerical. Specify a threshold to exclude fossil occurrences with a high temporal uncertainty, i.e. with a wide temporal range between min.age and max.age. Examples: cutoff=NULL (default; all occurrences are kept in the data set) cutoff=5 (all occurrences with a temporal range of 5 Myr or higher are excluded from the data set)
}
  \item{random}{
logical. Specify whether to take a random age (between MinT and MaxT) for each occurrence or the midpoint age. Note that this option defaults to TRUE if several replicates are generated (i.e. replicates > 1). Examples: random = TRUE (default) random = FALSE (use midpoint ages)
}
}
\details{
The replicate option allows the user to generate several replicates of the data set in a single input file, each time re-drawing the ages of the occurrences at random from uniform distributions with boundaries MinT and MaxT. The replicates can be analyzed in different runs (see PyRate command -j) and combining the results of these replicates is a way to account for the uncertainty of the true ages of the fossil occurrences. Examples: replicates=1 (default, generates 1 data set), replicates=10 (generates 10 random replicates of the data set).
}
\value{
PyRate input files in the working directory.
}
\author{
Daniele Silvestro
}
\note{
See \url{https://github.com/dsilvestro/PyRate/wiki} for more details and tutorials on PyRate and PyRate input.
}

\examples{
minages <- runif(250, 0, 65)
exmpl <- data.frame(identified_name = sample(letters, size = 250, replace = TRUE),
                    lng = runif(250, min = 42, max = 51),
                    lat = runif(250, min = -26, max = -11),
                    min_ma = minages,
                    max_ma = minages + runif(250, 0.1, 65))

#a vector with the status for each record, 
#make sure species are only classified as either extinct or extant, 
#otherwise the function will drop an error

status <- sample(c("extinct", "extant"), size = nrow(exmpl), replace = TRUE)

#or from a list of species
status <- sample(c("extinct", "extant"), size = length(letters), replace = TRUE)
names(status) <- letters
status <- status[exmpl$identified_name]

\dontrun{
WritePyRate(x = exmpl,fname = "test", status = status)
}
}
\keyword{ Fossil }

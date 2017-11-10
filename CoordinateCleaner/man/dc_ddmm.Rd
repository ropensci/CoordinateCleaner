\name{dc_ddmm}
\alias{dc_ddmm}

\title{
Flag Datasets with a Degree Conversion Error
}
\description{
This test identifies datasets where a significant fraction of records has been subject to a common degree minute to decimal degree conversion error, where the degree sign is recognized as decimal delimiter.
}
\usage{
dc_ddmm(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset", 
        pvalue = 0.025, diff = 0.1, mat.size = 1000,
        value = "clean", verbose = TRUE)
}

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
  \item{ds}{
a character string. The column with the dataset of each record. In case \code{x} should be treated as a single dataset, identical for all records.  Default = \dQuote{dataset}.
}
  \item{pvalue}{
numeric. The p-value for the one-sided t-test to flag the test as passed or not. Both ddmm.pvalue and diff must be met. Default = 0.025.
}
  \item{diff}{
numeric. The threshold difference for the ddmm test. Indicates by which fraction the records with decimals below 0.6 must outnumber the records with decimals above 0.6. Default = 0.1
}
  \item{mat.size}{
numeric. The size of the matrix for the binomial test. Must be changed in decimals (e.g. 100, 1000, 10000). Adapt to dataset size, generally 100 is better for datasets < 10000 records, 1000 is better for datasets with 10000 - 1M records. Higher values also work reasonably well for smaller datasets, therefore, default = 1000. For large datasets try 10000. 
}
  \item{value}{
a character string.  Defining the output value. See value.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\details{
If the degree sign is recognized as decimal delimiter during coordinate conversion, no coordinate decimals above 0.59 (59') are possible. The test here uses a binomial test to test if a significant proportion of records in a dataset have been subject to this problem. The test is best adjusted via the diff argument. The lower \code{diff}, the stricter the test. Also scales with dataset size. Empirically, for datasets with < 5,000 unique coordinate records \code{diff = 0.1} has proven reasonable flagging most datasets with >25\% problematic records and all dataset with >50\% problematic records. For datasets between 5,000 and 100,000 geographic unique records \code{diff = 0.01} is recommended, for datasets between 100,000 and 1 M records diff = 0.001, and so on.  See \url{https://github.com/azizka/CoordinateCleaner/wiki/3.-Identifying-problematic-data-sets:-CleanCoordinatesDS} for explanation and simulation results.
}

\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
clean <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90),
                dataset = "FR")
                
dc_ddmm(x = clean, value = "flags")

#problematic dataset
lon <- sample(-180:180, size = 100, replace = TRUE) + runif(100, 0,0.59)
lat <- sample(-90:90, size = 100, replace = TRUE) + runif(100, 0,0.59)

prob <-  data.frame(species = letters[1:10], 
                decimallongitude = lon, 
                decimallatitude = lat,
                dataset = "FR")
                
dc_ddmm(x = prob, value = "flags")
}

\keyword{ "Coordinate cleaning" }

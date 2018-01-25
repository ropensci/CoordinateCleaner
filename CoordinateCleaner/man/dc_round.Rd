\name{dc_round}
\alias{dc_round}

\title{
Flags Datasets with Rasterized Coordinates
}
\description{
Flags datasets with periodicity patterns indicative of a rasterized (lattice) collection scheme, as often obtain from e.g. atlas data. Using a combination of autocorrelation and sliding-window outlier detection to identify periodicity patterns in the data. 
}
\usage{
dc_round(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset", 
         T1 = 7, reg.out.thresh = 2, reg.dist.min = 0.1, reg.dist.max = 2, 
         min.unique.ds.size = 4, graphs = T, test = "both", value = "clean")
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
  \item{T1}{
numeric.  The threshold for outlier detection in a in an interquantile range based test. This is the major parameter to specify the sensitivity of the test: lower values, equal higher detection rate. Values between 7-11 are recommended. Default = 7.
}
  \item{reg.out.thresh}{
numeric. Threshold on the number of equal distances between outlier points.  See details.  Default = 2.
}
  \item{reg.dist.min}{
numeric.  The minimum detection distance between outliers in degrees (the minimum resolution of grids that will be flagged).  Default = 0.1.
}
  \item{reg.dist.max}{
numeric.  The maximum detection distance between outliers in degrees (the maximum resolution of grids that will be flagged).  Default = 2.
}
  \item{min.unique.ds.size}{
numeric.  The minimum number of unique locations (values in the tested column) for datasets to be included in the test.  Default = 4.
}
  \item{graphs}{
logical. If TRUE, diagnostic plots are produced.  Default = TRUE.
}
  \item{test}{
character string.  Indicates which column to test. Either \dQuote{lat] for latitude, \dQuote{lon} for longitude, or \dQuote{both} for both.  In the latter case datasets are only flagged if both test are failed.  Default = \dQuote{both}}
}
  \item{value}{
a character string.  Defining the output value. See value.
}
}
\details{
see supplement
%Copy later from supplementary material
}
\value{
Depending on the \sQuote{value} argument, either a \code{data.frame} with summary statistics and flags for each dataset (\dQuote{dataset}) or a \code{data.frame} containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
#simulate bias grid, one degree resolution, 10\% error on a 1000 records dataset
  ##simulate biased fraction of the data, grid resolution = 1 degree
  
  
  
  #simulate non-biased fraction of the data
  bi <- sample(3 + 0:5, size = 100, replace = TRUE)
  mu <- runif(3, 0, 15)
  sig <- runif(3, 0.1, 5)
  cl <- rnorm(n = 900, mean = mu, sd = sig)
  lon <- c(cl, bi)
  
  bi <- sample(9:13, size = 100, replace = TRUE)
  mu <- runif(3, 0, 15)
  sig <- runif(3, 0.1, 5)
  cl <- rnorm(n = 900, mean = mu, sd = sig)
  lat <- c(cl, bi)
  
  #add biased data
  
  inp <- data.frame(decimallongitude = lon,
                    decimallatitude = lat,
                    dataset = "test")
            
  #plot overview
  suma <- inp
  suma[,1:2] <- round(suma[,1:2], 0)
  suma <- aggregate(dataset ~ decimallongitude + decimallatitude, FUN = "length", data = suma)
  colo <- rev(heat.colors(max(suma$dataset)))
  plot(suma$decimallatitude ~ suma$decimallongitude, col = colo[suma$dataset])
          
  #run test
  
  dc_round(inp, value = "dataset")
}

\keyword{ "Coordinate cleaning" }
\keyword{ "Dataset level cleaning" }

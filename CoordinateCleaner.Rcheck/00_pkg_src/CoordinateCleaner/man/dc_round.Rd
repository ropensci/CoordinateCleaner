\name{dc_round}
\alias{dc_round}

\title{
Flag Datasets with a Significant Fraction of Rounded Coordinates
}
\description{
Uses a three-rate Poisson process to model the distribution of coordinate decimals in the coordinates and to identify datasets with a significant fraction of records with low precision. Often these records have been subject to strong decimal rounding, or are based on rasterized data collection schemes.
}
\usage{
dc_round(x, lon = "decimallongitude", lat = "decimallatitude", 
         ds = "dataset", target = "lon_lat", 
         threshold.degree = 15, threshold.period = 3.5,  
         subsampling = NULL, diagnostics = FALSE, value = "clean", verbose = T)
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
  \item{target}{
a character string. Defining the target of the test. One of \dQuote{lon}, \dQuote{lat}, \dQuote{lon_lat}, \dQuote{lon_lat} is recommended.  Default = \dQuote{lon_lat}.
}
  \item{threshold.degree}{
numerical. Indicates the factor by which one of the two periodic bins must outnumber the other to identify rounding to zero decimals (one degree resolution). Default = 15. Higher values are more conservative/flag less datasets.
}
  \item{threshold.period}{
numerical. Indicates the factor by which one of the two periodic bins must outnumber the other to identify rounding to the first decimal (0.1 degree resolution). Default = 3.5. Higher values are more conservative/flag less datasets.
}
  \item{subsampling}{
numeric. If NULL, the entire dataset is tested, if not NULL a random subsample of size \code{subsampling} is tested. This is recommended for very large datasets, but \code{subsampling values below 1000 are not recommended.  Default = NULL}
}
  \item{diagnostics}{
logical. If TRUE, plots a series of diagnostics visualizing the periodicity test. Default = FALSE.
}
  \item{value}{
a character string.  Defining the output value. See value. Default = \dQuote{clean}.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\details{
To detect these patterns, we model the distribution of decimals in the range [0,1] as the result of a 3-rate Poisson process, where the first rate (lambda_0) is assigned to the range $R_0 = [0, t_0)]$ and the second and third rates (lambda_1 and lambda_2) are assigned to successive bins of sizes $s_1$ and s_2$. We use maximum likelihood to estimate the three rates and the sizes of the bins. The number of resulting bins ($N$) depends on the quantities $t_0,s_1,s_2$, all of which are constrained to be positive and smaller than 0.3. We note that, while the number of bins changes based on their size, the number of parameters in the model is constant and equal to 6. We expect this model to return high values of lambda_0 compared to lambda_1, lambda_2, and $t_0$ to be small, in the presence of a bias increasing the frequency of 0s in the decimals. Periodic biases are expected to result in strongly different rates in the following bins (e.g. lambda_1 >> lambda_2) and small estimated values of $s_1,s_2$ such that they allow for multiple repeated peaks. After empirically inspecting the behaviour of these estimates and their ability to detect biases in the data we defined arbitrary thresholds to flag a data set as potentially biased by poor precision
}
\value{
Depending on the \sQuote{value} argument, either a summary per dataset \code{dataset}, a dataframe containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}.
}
\note{
See \url{https://github.com/azizka/CoordinateCleaner/wiki} for more details and tutorials.
}

\examples{
clean <- data.frame(species = letters[1:10], 
                decimallongitude = runif(100, -180, 180), 
                decimallatitude = runif(100, -90,90),
                dataset = "clean")
#biased dataset        
bias.long <- c(round(runif(min = -42, max = -40, n = 500), 1),
               round(runif(min = -42, max = -40, n = 300), 0),
               runif(min = -42, max = -40, n = 200))
bias.lat <- c(round(runif(min = -12, max = -10, n = 500), 1),
              round(runif(min = -12, max = -10, n = 300), 0),
              runif(min = -12, max = -10, n = 200))
bias <- data.frame(species = letters[1:10],
                   decimallongitude = bias.long,
                   decimallatitude = bias.lat,
                   dataset = "rounded")
test <- rbind(clean, bias)

\dontrun{                  
#run CleanCoordinatesDS
flags <- CleanCoordinatesDS(test)

#check problems
#clean
hist(test[test$dataset == rownames(flags[flags$summary,]), "decimallongitude"])
#biased
hist(test[test$dataset == rownames(flags[!flags$summary,]), "decimallongitude"])
}
}

\keyword{ "Coordinate cleaning" }

\name{CleanCoordinatesDS}
\alias{CleanCoordinatesDS}

\title{
Geographic Coordinate Cleaning based on Dataset Properties
}
\description{
Identifies potentially problematic coordinates based on dataset properties. Includes test to flag potential errors with converting ddmm to dd.dd, and periodicity in the data decimals indicating rounding or a raster basis linked to low coordinate precision.
}
\usage{
CleanCoordinatesDS(x, lon = "decimallongitude", lat = "decimallatitude",
                   ds = "dataset",
                   ddmm = TRUE, periodicity = TRUE,
                   ddmm.pvalue = 0.025, ddmm.diff = 0.2, 
                   periodicity.target = "lon_lat", periodicity.thresh = 3.5, 
                   periodicity.diagnostics = FALSE, 
                   periodicity.subsampling = NULL,
                   value = "dataset", verbose = TRUE)
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
a character string. The column with the dataset of each record. In case \code{x} should be treated as a single dataset, identical for all records. Default = \dQuote{dataset}.
}
  \item{ddmm}{
logical. If TRUE, testing for erroneous conversion from a degree minute format (ddmm) to a decimal degree (dd.dd) format. See details.
}
  \item{periodicity}{
logical. If TRUE, testing for periodicity in the data, which can indicate imprecise coordinates, due to rounding or rasterization.
}
  \item{ddmm.pvalue}{
numeric. The p-value for the one-sided t-test to flag the ddmm test as passed or not. Both ddmm.pvalue and ddmm.diff must be met. Default = 0.025.
}
  \item{ddmm.diff}{
numeric. The threshold difference for the ddmm test. Indicates by which fraction the records with decimals below 0.6 must outnumber the records with decimals above 0.025. Default = 0.2
}
  \item{periodicity.target}{
a character string. One of \sQuote{lat}, \sQuote{lon}, \sQuote{lon_lat}. Sets the target for the periodicity test which runs on latitude and longitude separately. If \sQuote{lon_lat}. Tests run sequentially and results for both and a combined flag are returned.
}
  \item{periodicity.thresh}{
numerical. The threshold to for flagging in the periodicity test. Indicates the factor by which one of the two periodic bins must outnumber the other. Default = 1.5. Higher values are more conservative/ flag less datasets.
}
  \item{periodicity.diagnostics}{
logical. If TRUE, plots a series of diagnostics visualizing the periodicity test. Default = FALSE.
}
  \item{periodicity.subsampling}{
numerical. If defined, only a random subsample of n = subsampling records is used for the periodicity test. Speeds up analyses, for the use with many large datasets. 
}
  \item{value}{
a character string.  Defining the output value. See value. Default = \dQuote{dataset}.
}
  \item{verbose}{
logical. If TRUE reports the name of the test and the number of records flagged.
}
}
\details{
This function checks the statistical distribution of decimals within datasets of geographic distribution records to identify datasets with potential errors/biases. Three potential error sources can be identified. The ddmm flag tests for the particular pattern that emerges if geographical coordinates in a degree minute annotation are transferred into decimal degrees, simply replacing the degree symbol with the decimal point. This kind of problem has been observed by in older datasets first recorded on paper using typewriters, where e.g. a floating point was used as symbol for degrees. The function uses a binomial test to check if more records then expected have decimals blow 0.6 (which is the maximum that can be obtained in minutes, as one degree has 60 minutes) and if the number of these records is higher than those above 0.59 by a certain proportion. The periodicity test uses rate estimation in a poison process to estimate if there is periodicity in the decimals of a dataset (as would be expected by for example rounding or data that was collected in a raster format) and if there is an over proportional number of records with the decimal 0 (full degrees) which indicates rounding and thus low precision. The default values are empirically optimized by with GBIF data, but should probably be adapted.
}
\value{
Depending on the \sQuote{value} argument, either a summary per dataset \code{dataset}, a dataframe containing the records considered correct by the test (\dQuote{clean}) or a logical vector, with TRUE = test passed and FALSE = test failed/potentially problematic (\dQuote{flags}). Default = \dQuote{clean}. If \dQuote{dataset}: \code{data.frame} with one row for each dataset in \code{x} and columns depending on the output option: \sQuote{detail} shows most level of detail, \sQuote{flag} shows only  flags from the test and \sQuote{minimal} shows only the combined flags. Available columns are: \code{binomial.pvalue} = p-value compared to ddmm.pvalue; \code{perc.difference} = the percentage of difference from the expectation under a binomial test; \code{pass.ddmm} = logical flag summarizing the ddmm test, if TRUE: passed, if FALSE: potentially problematic; \code{mle} = the maximum likelihood for the rate parameters of the periodicity test; rate.ratio = rate ratio between the two rates of the periodicity model compared to \code{periodicity.thresh}; \code{zero.mle} = size of the maximum likelihood zero size bin from the zero test; \code{zero.rate.ratio} = ratio by which the number of zero decimals surpasses the number of records with other decimals; \code{pass.zero} = logical flag summarizing the zero test, if TRUE: passed, if FALSE: potentially problematic; \code{pass.periodicity} = logical flag summarizing the periodicity test, if TRUE: passed, if FALSE: potentially problematic. Flags for the periodicity test will be marked dependent on the meridional direction tested: \sQuote{lon} = longitude, \sQuote{lat} = latitude, \sQuote{com} = AND combination of the two former.
}
\author{
person("Alexander", "Zizka", email = "alexander.zizka@bioenv.gu.se",
                  role = c("aut", "cre"), comment = c(ORCID = "0000-0002-1680-9192"))
person("Daniele", "Silvestro", role = c("aut", "cre"), comment = c(ORCID = "0000-0003-0100-0961"))
}

\seealso{
\code{\link{CleanCoordinates}}
}
\examples{
#Create test dataset
clean <- data.frame(dataset = rep("clean", 1000),
                    decimallongitude = runif(min = -42, max = -40, n = 1000),
                    decimallatitude = runif(min = -12, max = -10, n = 1000))
                    
bias.long <- c(round(runif(min = -42, max = -40, n = 500), 1),
               round(runif(min = -42, max = -40, n = 300), 0),
               runif(min = -42, max = -40, n = 200))
bias.lat <- c(round(runif(min = -12, max = -10, n = 500), 1),
              round(runif(min = -12, max = -10, n = 300), 0),
              runif(min = -12, max = -10, n = 200))
bias <- data.frame(dataset = rep("biased", 1000),
                   decimallongitude = bias.long,
                   decimallatitude = bias.lat)
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
                  
\keyword{ Coordinate cleaning wrapper}


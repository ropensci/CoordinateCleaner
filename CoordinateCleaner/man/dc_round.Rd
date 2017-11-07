\name{dc_round}
\alias{dc_round}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dc_round(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset", target = "lon_lat", threshold = 3.5, subsampling = NULL, diagnostics = FALSE, value = "clean", verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{target}{
%%     ~~Describe \code{target} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{subsampling}{
%%     ~~Describe \code{subsampling} here~~
}
  \item{diagnostics}{
%%     ~~Describe \code{diagnostics} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, lon = "decimallongitude", lat = "decimallatitude", 
    ds = "dataset", target = "lon_lat", threshold = 3.5, subsampling = NULL, 
    diagnostics = FALSE, value = "clean", verbose = T) 
{
    match.arg(value, choices = c("clean", "flags"))
    if (verbose) {
        cat("Testing for rounded coordinates\n")
    }
    if (!is.null(subsampling)) {
        if (subsampling < 1000) {
            warning("Subsampling <1000 not recommended")
        }
    }
    if (sum(!complete.cases(x)) > 0) {
        warning(sprintf("ignored \%s cases with incomplete data", 
            sum(!complete.cases(x))))
    }
    dat <- x[complete.cases(x), ]
    if (nrow(dat) == 0) {
        stop("no complete cases found")
    }
    dat$lon.test <- abs(dat$decimallongitude) - floor(abs(dat$decimallongitude))
    dat$lat.test <- abs(dat$decimallatitude) - floor(abs(dat$decimallatitude))
    test <- split(dat, f = dat[[ds]])
    out <- lapply(test, function(k) {
        t2 <- k[, c("lon.test", "lat.test")]
        if (!is.null(subsampling)) {
            if (subsampling > nrow(t2)) {
                warning("Subsampling larger than rows in data, subsampling skipped")
            }
            else {
                warning(sprintf("Using subsampling for periodicity, n = \%s", 
                  subsampling))
                t2 <- t2[sample(nrow(t2), subsampling), ]
            }
        }
        if (target == "lon") {
            t2.res <- .AnalyzeBias(var = t2, nam = k[[ds]][1], 
                var_latlong = 1, plot_bias = diagnostics, ratio_threshold = threshold)
            names(t2.res) <- c("mle", "rate.ratio", "pass.lon", 
                "zero.mle", "zero.rate.ratio", "pass.zero.lon")
            t2.res$pass.zero.com <- t2.res$pass.zero.lon
            t2.res$pass.periodicity.com <- t2.res$pass.lon
        }
        if (target == "lat") {
            t2.res <- .AnalyzeBias(var = t2, nam = k[[ds]][1], 
                var_latlong = 2, plot_bias = diagnostics, ratio_threshold = threshold)
            names(t2.res) <- c("mle", "rate.ratio", "pass.lat", 
                "zero.mle", "zero.rate.ratio", "pass.zero.lat")
            t2.res$pass.zero.com <- t2.res$pass.zero.lat
            t2.res$pass.periodicity.com <- t2.res$pass.lat
        }
        if (target == "lon_lat") {
            lon <- .AnalyzeBias(t2, nam = k[[ds]][1], var_latlong = 1, 
                plot_bias = diagnostics, ratio_threshold = threshold)
            lat <- .AnalyzeBias(t2, nam = k[[ds]][1], var_latlong = 2, 
                plot_bias = diagnostics, ratio_threshold = threshold)
            t2.res <- cbind(lon, lat)
            names(t2.res) <- c("mle.lon", "rate.ratio.lon", "pass.periodicity.lon", 
                "zero.mle.lon", "zero.rate.ratio.lon", "pass.zero.lon", 
                "mle.lat", "rate.ratio.lat", "pass.periodicity.lat", 
                "zero.mle.lat", "zero.rate.ratio.lat", "pass.zero.lat")
            t2.res$pass.zero.com <- t2.res$pass.zero.lon & t2.res$pass.zero.lat
            t2.res$pass.periodicity.com <- t2.res$pass.periodicity.lon | 
                t2.res$pass.periodicity.lat
        }
        return(t2.res)
    })
    out.ds <- do.call("rbind.data.frame", out)
    flags <- x[[ds]] \%in\% rownames(out.ds[out.ds$pass.zero.com & 
        out.ds$pass.periodicity.com, ])
    if (verbose) {
        cat(sprintf("Flagged \%s records\n", sum(!flags)))
    }
    switch(value, dataset = return(out.ds), clean = return(x[flags, 
        ]), flags = return(flags))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

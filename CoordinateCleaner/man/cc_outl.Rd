\name{cc_outl}
\alias{cc_outl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cc_outl(x, lon = "decimallongitude", lat = "decimallatitude", species = "species", method = "quantile", mltpl = 3, tdi = 1000, value = "clean", verbose = T)
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
  \item{species}{
%%     ~~Describe \code{species} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{mltpl}{
%%     ~~Describe \code{mltpl} here~~
}
  \item{tdi}{
%%     ~~Describe \code{tdi} here~~
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
    species = "species", method = "quantile", mltpl = 3, tdi = 1000, 
    value = "clean", verbose = T) 
{
    match.arg(value, choices = c("clean", "flags"))
    match.arg(outl.method, choices = c("distance", "quantile", 
        "mad"))
    if (verbose) {
        cat("Testing outliers\n")
    }
    splist <- split(x, f = as.character(x[[species]]))
    test <- lapply(splist, "duplicated")
    test <- lapply(test, "!")
    test <- as.vector(unlist(lapply(test, "sum")))
    splist <- splist[test > 2]
    flags <- lapply(splist, function(k) {
        test <- nrow(k[!duplicated(k), ])
        if (test > 2) {
            if (method == "distance") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                mins <- apply(dist, 1, min, na.rm = T)
                out <- which(mins > tdi * 1000)
            }
            if (method == "quantile") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                mins <- apply(dist, 1, mean, na.rm = T)
                quo <- quantile(mins, c(0.25, 0.75), na.rm = T)
                out <- which(mins < quo[1] - IQR(mins) * mltpl | 
                  mins > quo[2] + IQR(mins) * mltpl)
            }
            if (method == "mad") {
                dist <- geosphere::distm(k[, c(lon, lat)], fun = geosphere::distHaversine)
                dist[dist == 0] <- NA
                mins <- apply(dist, 1, mean, na.rm = T)
                quo <- median(mins)
                tester <- mad(mins)
                out <- which(mins < quo - tester * mltpl | mins > 
                  quo + tester * mltpl)
            }
        }
        if (length(out) == 0) {
            ret <- NA
        }
        if (length(out) > 0) {
            ret <- rownames(k)[out]
        }
        return(ret)
    })
    flags <- as.numeric(as.vector(unlist(flags)))
    out <- flags[!is.na(flags)]
    out <- rep(TRUE, nrow(x))
    out[flags] <- FALSE
    if (verbose) {
        cat(sprintf("Flagged \%s records. \n", sum(!out)))
    }
    switch(value, clean = return(x[out, ]), flags = return(out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

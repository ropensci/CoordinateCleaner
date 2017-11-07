\name{cc_sea}
\alias{cc_sea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cc_sea(x, lon = "decimallongitude", lat = "decimallatitude", ref = NULL, value = "clean", verbose = TRUE)
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
  \item{ref}{
%%     ~~Describe \code{ref} here~~
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
    ref = NULL, value = "clean", verbose = TRUE) 
{
    match.arg(value, choices = c("clean", "flags"))
    if (verbose) {
        cat("Testing sea coordinates\n")
    }
    pts <- sp::SpatialPoints(x[, c(lon, lat)])
    if (is.null(ref)) {
        ref <- CoordinateCleaner::landmass
        ref <- raster::crop(ref, raster::extent(pts) + 1)
    }
    else {
        sp::proj4string(ref) <- ""
        warning("Assuming lat/lon for seas.ref")
    }
    out <- sp::over(x = pts, y = ref)[, 1]
    out <- !is.na(out)
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

\name{cc_inst}
\alias{cc_inst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cc_inst(x, lon = "decimallongitude", lat = "decimallatitude", buffer = 0.001, ref = NULL, value = "clean", verbose = TRUE)
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
  \item{buffer}{
%%     ~~Describe \code{buffer} here~~
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
    buffer = 0.001, ref = NULL, value = "clean", verbose = TRUE) 
{
    match.arg(value, choices = c("clean", "flags"))
    if (verbose) {
        cat("Testing coordinate validity\n")
    }
    dat <- sp::SpatialPoints(x[, c(lon, lat)])
    if (is.null(ref)) {
        ref <- CoordinateCleaner::institutions
        ref <- ref[!is.na(ref$decimallongitude) & !is.na(ref$decimallatitude), 
            ]
    }
    limits <- raster::extent(dat) + buffer
    ref <- raster::crop(sp::SpatialPoints(ref[, c("decimallongitude", 
        "decimallatitude")]), limits)
    if (is.null(ref)) {
        out <- rep(TRUE, nrow(x))
    }
    else {
        ref <- rgeos::gBuffer(ref, width = buffer, byid = T)
        out <- is.na(sp::over(x = dat, y = ref))
    }
    if (verbose) {
        warning(sprintf("Flagged \%s records\n", sum(!out)))
    }
    switch(value, clean = return(x[out, ]), flags = return(out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

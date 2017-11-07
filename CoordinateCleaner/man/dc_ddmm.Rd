\name{dc_ddmm}
\alias{dc_ddmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dc_ddmm(x, lon = "decimallongitude", lat = "decimallatitude", ds = "dataset", pvalue = 0.025, diff = 0.2, value = "clean", verbose = TRUE)
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
  \item{pvalue}{
%%     ~~Describe \code{pvalue} here~~
}
  \item{diff}{
%%     ~~Describe \code{diff} here~~
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
    ds = "dataset", pvalue = 0.025, diff = 0.2, value = "clean", 
    verbose = TRUE) 
{
    match.arg(value, choices = c("clean", "flags", "dataset"))
    if (verbose) {
        cat("Testing datasets for dd.mm to dd.dd conversion errors\n")
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
        dat.unique <- k[!duplicated(k[, 1:2]), ]
        r <- raster::raster(xmn = 0, xmx = 1, ymn = 0, ymx = 1)
        raster::res(r) <- 0.01
        dat.t1 <- raster::rasterize(SpatialPoints(dat.unique[, 
            c("lon.test", "lat.test")]), r, fun = "count")
        dat.t1 <- raster::as.matrix(dat.t1)
        dat.t1[is.na(dat.t1)] <- 0
        P_smaller_than_06 <- 59 * 59/10000
        subt <- dat.t1[1:59, 1:59]
        p06 <- sum(subt >= 1)
        pAll <- sum(dat.t1 >= 1)
        B <- stats::binom.test(p06, pAll, p = P_smaller_than_06, 
            alternative = c("greater"))
        v1 <- B$p.value
        v2 <- (B$estimate - P_smaller_than_06)/P_smaller_than_06
        if (v1 < pvalue & v2 > diff) {
            flag.t1 <- FALSE
        }
        else {
            flag.t1 <- TRUE
        }
        c(round(v1, 4), round(v2, 3), flag.t1)
    })
    out.ds <- do.call("rbind.data.frame", out)
    names(out.ds) <- c("binomial.pvalue", "perc.difference", 
        "pass")
    flags <- x[[ds]] \%in\% rownames(out.ds[out.ds$pass == 1])
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

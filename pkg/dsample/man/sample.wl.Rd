\name{sample.wl}
\alias{sample.wl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample.wl(X, nc = 10000, n = 1000, wconst = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{nc}{
%%     ~~Describe \code{nc} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{wconst}{
%%     ~~Describe \code{wconst} here~~
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
function (X, nc = 10000, n = 1000, wconst = NULL) 
{
    if (is.data.frame(X) | is.matrix(X)) 
        X <- as.data.frame(X)
    else stop("'X' must be either a matrix or a data.frame.\n")
    if (nrow(X) < nc) 
        stop("The number of rows in 'X' should be larger than 'nc'")
    X <- X[which(X[, 1] > 0), ]
    measure <- hist(X[, 1], breaks = seq(from = min(X[, 1]), 
        to = max(X[, 1]), length.out = nc + 1), plot = FALSE)
    gpdf <- rev(measure$counts * measure$mids)
    if (!is.null(wconst)) 
        gpdf[nc] <- wconst * gpdf[nc]
    cdf <- c(0, cumsum(gpdf)/sum(gpdf))
    pps <- hist(runif(n), breaks = cdf, plot = FALSE)$counts
    scnt <- mapply(sample, MoreArgs = list(replace = TRUE), rev(measure$counts), 
        pps)
    sind <- unlist(mapply("+", as.list(c(0, cumsum(rev(measure$counts)))[-(nc + 
        1)]), scnt))
    X <- X[order(X[, 1], decreasing = TRUE)[sind], -1]
    row.names(X) <- NULL
    class(X) <- c("samplefwl", "data.frame")
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

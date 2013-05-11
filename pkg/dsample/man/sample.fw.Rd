\name{sample.fw}
\alias{sample.fw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample.fw(X, nc = 10000, n = 1000)
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
function (X, nc = 10000, n = 1000) 
{
    if (is.data.frame(X) | is.matrix(X)) 
        X <- as.data.frame(X)
    else stop("'X' must be either a matrix or a data.frame.\n")
    if (nrow(X) < nc) 
        stop("The number of rows in 'X' should be larger than 'nc'")
    ndp <- nrow(X)
    X <- X[order(X[, 1], decreasing = TRUE), ]
    X$g <- gl(nc, ndp/nc)
    clvls <- tapply(X[, 1], X$g, mean)
    cdf <- c(0, cumsum(clvls)/sum(clvls))
    pps <- hist(runif(n), breaks = cdf, plot = FALSE)$counts
    scnt <- mapply(sample, MoreArgs = list(replace = TRUE), rep(ndp/nc, 
        nc), pps)
    sind <- unlist(mapply("+", as.list(c(0, cumsum(rep(ndp/nc, 
        nc)))[-(nc + 1)]), scnt))
    X <- X[sind, c(-1, -length(names(X)))]
    row.names(X) <- NULL
    class(X) <- c("samplefwl", "data.frame")
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

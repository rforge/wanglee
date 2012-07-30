\name{sample.wl}
\alias{sample.wl}

\title{
Random Samples Drawn Through The Wang-Lee Algorithm
}

\description{
\code{sample.wl} takes a sample of specified size \code{n} from the elements \code{X} based on the Wang-Lee algorithm
}

\usage{
sample.wl(X, nc = 10000, n = 1000, wconst = NULL)
}

\arguments{
\item{X}{
a \code{data.frame} or a \code{matrix}.  See \sQuote{Details}.
}
\item{nc}{
a positive integer, the number of contours.  See \sQuote{Details}.
}
\item{n}{
a non-negative integer giving the number of items to choose. 
}
\item{wconst}{
a real number in (0,1).  See \sQuote{Details}.
}
}

\details{
\code{X} must be formed in such a way that, in each row, the first column is an evaluated value of a given target function and the other columns are values in multidimensions which are used to evaluate the target function.  

Note that a number of rows of \code{X} implies a number of discretized random points in a compact region of the target function. 

\code{wconst} is a constant for making an adjustment on a volumn of the last contour. 
}

\value{
\code{sample.wl} gives a sample as a \code{data.frame} with rows of specified size \code{n} and columns of size \code{ncol(x)-1}.
}

\references{
Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.

Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48(4):631-653.

Wang, L. and Lee, C.H. (2008). A discretization-based efficient random sampling algorithm (Working paper), Department of Satistics, University of Manitoba, Canada
}

\author{
Liqun Wang and Chel Hee Lee 
}

\seealso{
\code{\link{sample.fw}}
}

\examples{
## The following example is taken from the study of West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the Royal
##   Statistical Society - B, 55(2):409-422.


## More accurate results can be achieved by increasing a number of dicretization points 
## and a number of contours.  Attempt to set the number of discretization points 
## as 1e7 and the number of contours as 1e5


pi1 <- runif(1e5)
pi2 <- runif(1e5)
val <- (pi1*(1-pi2))^5 * (pi2*(1-pi1))^3 * (1-pi1*(1-pi2)-pi2*(1-pi1))^(45-5-3)
support <- as.data.frame(cbind(val, pi1, pi2))

output(sample.fw(X=support, nc=1e4, n=1e3))
}

%% \keyword{ sampling }
%% \keyword{ discretization }

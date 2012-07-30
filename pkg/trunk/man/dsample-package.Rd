\name{dsample-package}
\alias{dsample-package}
\docType{package}

\title{
Discretization-Based Direct Random Sample Generation
}

\description{
Two variants of efficient discretization-based Monte Carlo algorithms named with the Fu-Wang algorithm and the Wang-Lee algorithm for random
sample generation from a complex structure in high dimensions.
}

\details{
	\tabular{ll}{
		Package: \tab dsample\cr
		Type: \tab Package\cr
		Version: \tab 0.9\cr
		Date: \tab 2012-07-28\cr
		License: \tab GPL-3\cr
	}
}

\author{
Liqun Wang and Chel Hee Lee

Maintainer: Chel Hee Lee <gnustats@gmail.com>
}

\references{
Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.

Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48(4):631-653.

Wang, L. and Lee, C.H. (2008). A discretization-based efficient random sampling algorithm (Working paper), Department of Satistics, University of Manitoba, Canada
}

\keyword{ package }

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

## Applying the Fu-Wang algorithm
out1 <- sample.fw(X=support, nc=1e4, n=1e3)
output(out1)

## Applying the Wang-Lee algorithm
out2 <- sample.wl(X=support, nc=1e4, n=1e3)
output(out2)
}

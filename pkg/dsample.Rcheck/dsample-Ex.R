pkgname <- "dsample"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dsample')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("dsample-package")
### * dsample-package

flush(stderr()); flush(stdout())

### Name: dsample-package
### Title: Discretization-based Direct Random Sample Generation
### Aliases: dsample-package dsample
### Keywords: package

### ** Examples

## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the 
## Royal Statistical Society - B, 55, 409-422.

## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours.  The default value for the number of discretization points 
## is 1e7 and for the number of contours is 1e5

x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

## Applying the Fu-Wang algorithm
out1 <- sample.fw(X=support, nc=1e4, n=1e3)
summary(out1)

## Applying the Wang-Lee algorithm
out2 <- sample.wl(X=support, nc=1e4, n=1e3)
summary(out2)



cleanEx()
nameEx("sample.fw")
### * sample.fw

flush(stderr()); flush(stdout())

### Name: sample.fw
### Title: Random Sample Generation Through The Fu-Wang Algorithm
### Aliases: sample.fw

### ** Examples

## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the Royal
##   Statistical Society - B, 55, 409-422.

## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours. The default value for the number of discretization points 
## is 1e7 and for the number of contours is 1e5.


x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

summary(sample.fw(X=support, nc=1e4, n=1e3))



cleanEx()
nameEx("sample.wl")
### * sample.wl

flush(stderr()); flush(stdout())

### Name: sample.wl
### Title: Random Samples Generation Through The Wang-Lee Algorithm
### Aliases: sample.wl

### ** Examples

## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the Royal
##   Statistical Society - B, 55, 409-422.


## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours.  The default value of the number of discretization points 
## is 1e7 and for the number of contours is 1e5


x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

summary(sample.fw(X=support, nc=1e4, n=1e3))



cleanEx()
nameEx("summary.samplefwl")
### * summary.samplefwl

flush(stderr()); flush(stdout())

### Name: summary.samplefwl
### Title: Generating Basic Summary Statistics of Marginal Distributions
### Aliases: summary.samplefwl

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    cat("\nMeans: \n")
    print(round(sapply(object, mean), 4))
    cat("\nStandard deviations: \n")
    print(round(sapply(object, mean), 4))
    cat("\nFirst five modes: \n")
    print(round(object[1:5, ], 4))
  }



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

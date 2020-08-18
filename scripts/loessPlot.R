loessPlot <- function(x, y, xlab=deparse(substitute(x)), 
        ylab=deparse(substitute(y)), 
        evalPoints=50, span=.5, degree=1, 
        family=c("gaussian", "symmetric"), level=.95, 
        col=c("black", "red"), ylim=range(y, lower, upper), ...){
# Scatterplot with loess curve and pointwise confidence envelope
# Arguments:
#   x: horizontal coordindates
#   y: vertical coordinates
#   xlab, ylab: axis labels
#   evalPoints: number of equally spaced points at which to evaluate
#       regression curve
#   span, degree, family: arguments passed to loess()
#   level: confidence level
#   col: colours of smooth and confidence limits
#   ylim: limits for vertical axis
#   ...: other arguments passed to plot()
    family <- match.arg(family)
    xlab  # force evaluation of labels
    ylab
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
    mod <- loess(y ~ x, span=span, degree=degree, family=family)
    X <- seq(min(x), max(x), length=evalPoints)
    fit <- predict(mod, data.frame(x=X), se=TRUE)
    z <- qnorm((1 + level)/2)
    lower <- fit$fit - z*fit$se
    upper <- fit$fit + z*fit$se
    plot(x, y, xlab=xlab, ylab=ylab, ylim=ylim, ...)
    lines(X, fit$fit, lty=1, lwd=3, col=col[1])
    lines(X, lower, lty=2, lwd=2, col=col[2])
    lines(X, upper, lty=2, lwd=2, col=col[2])
    }

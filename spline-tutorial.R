#
# spline-tutorial.q
#
#----------------------------------------------------------------------
#
# PURPOSE:  demonstrate linear and cubic splines
#
# AUTHOR:  P. Heagerty
#
# DATE:  22 Jan 2010
#
#----------------------------------------------------------------------
#
# Introduction:  Regression splines provide one approach to 
#                allowing a regression model to capture functions/trends
#                in a predictor, X, while allowing *flexible* relationships
#                between X and the average response as a function of
#                the variable X, or E(Y|X).
#
# Purpose:       This code will show how simple linear splines can be
#                directly coded in R, and then how models can be fit 
#                and displayed in R.  The code will then show what
#                cubic spline models look like, and also show how to 
#                fit such a model and view the fitted curve.  Finally, 
#                comments are given that say what a "natural spline"
#                is in relation to a cubic spline, and suggest a way 
#                to use these splines in R.
#
#----------------------------------------------------------------------
#
##### If you want to run this code interactively then change the
##### following option:
#
go.pdf <- T
#
#----------------------------------------------------------------------
#
#####
#####  A regression relationship...
#####
#
##### Suppose we have a predictor that takes the values 1:24
#
x <- c(1:24)
#
##### Suppose we have an outcome variable that is predicted by
##### the variable X, but in some non-linear fashion:
#
mu <- 10 + 5 * sin( x * pi / 24 )  - 2 * cos( (x-6)*4/24 )
#
##### errors
#
set.seed(2010)
eee <- rnorm( length(mu) )
#
##### data
#
y <- mu + eee
#
##### Look at these data
#
if( go.pdf) pdf( file="TheData.pdf" )
plot( x, y )
lines( x, mu, col="red" )
title("Data and mean curve")
if( go.pdf ) graphics.off()
#
#----------------------------------------------------------------------
#
#####
#####  Linear Splines
#####
#
##### It's pretty clear that a line won't describe mu very well 
##### for these data.  (try it!)
#####
##### An alternative is to assume a PIECEWISE LINEAR function.  Here's
##### how you'd do it, and you'll see what it looks like.
#####
##### First -- decide on some (small number) of knot locations.  These
##### are places where you will allow the line to change direction and
##### track a new line.  How to pick these is worth discussion, but for
##### this example I will use the equally spaced values (6, 12, 18).
#####
##### Q: What is the form of a linear spline model?  
#####
##### ANSWER:
#####
#####      mean = b0 + b1 * X + 
#####                  b2 *[ (X-6)^+ ] + 
#####                  b3 * [ (X-12)^+ ] + 
#####                  b4 * [ (X-18)^+ ]
#####
#####  Here we define (X-k)^+ = max( 0, X-k ) -- this simply measures the
#####  the distance from the point X=k "on the right side of k" and is 
#####  set to 0 for any value of X below the knot location, k.  You'll
#####  see the result in the code below.
#####
##### Q:  What does this model do?
#####
##### ANSWER:  It allows the mean curve to be linear at any position of X, 
#####          while the slope is actually changed as the curve moves past
#####          a knot.  For example, this model implies:
#####         
#####          Slope of curve at X=3 equals:   b1
#####          Slope of curve at X=9 equals:   b1 + b2
#####          Slope of curve at X=15 equals:  b1 + b2 + b3
#####          Slope of curve at X=21 equals:  b1 + b2 + b3 + b4
#####
##### Let's fit this model and look at this result!
#####
##### First, define some new predictors:
#####
#
x6 <- ( x - 6 )
x6[ x6<0 ] <- 0
#
x12 <- ( x - 12 )
x12[ x12<0 ] <- 0
#
x18 <- ( x - 18 )
x18[ x18<0 ] <- 0
#
##### Let's look at these new predictors:
#
print( cbind( x, x6, x12, x18 ) )
#
#####
##### Now we can fit the linear spline
#####
#
fit <- lm( y ~ x + x6 + x12 + x18 )
#
##### Look at the fitted coefficients:
#
print( summary( fit ) )
#
##### Look at the fitted model:
#
fitted.mean <- predict( fit )
#
##### Plot it!
#
if( go.pdf) pdf( file="TheLinearSpline.pdf" )
plot( x, y )
lines( x, mu, col="red" )
lines( x, fitted.mean, col="blue", lwd=2 )
title("Data, true mean curve (red), and fitted (blue) using linear spline")
if( go.pdf ) graphics.off()
#
#####
##### Comments:  you can see that the linear spline allows a change at
#####            the position of the knots.  For these data there is almost
#####            no change that is used at X=6, and obvious changes at
#####            X=12 and X=18
#
#----------------------------------------------------------------------
#
#####
#####  Cubic Splines
#####
#
##### One way to allow a flexible fit is to use a polynomial model.  A cubic
##### model could be used.  Let's fit this and view it:
#####
#
x.squared <- x^2
x.cubed <- x^3
#
fit <- lm( y ~ x + x.squared + x.cubed )
#
print( summary( fit ) )
#
##### Look at the fitted model:
#
fitted.mean <- predict( fit )
#
##### Plot it!
#
if( go.pdf) pdf( file="TheCubicModel.pdf" )
plot( x, y )
lines( x, mu, col="red" )
lines( x, fitted.mean, col="blue", lwd=2 )
title("Data, true mean curve (red), and fitted (blue) using cubic")
if( go.pdf ) graphics.off()
#
#####
##### Comments: This fit seems "close" to the data, and is "smooth" meaning
#####           the curve is continuous, AND the derivative of the curve is
#####           also continuous -- so no sharp changes of direction like the
#####           linear spline.
#
##### CUBIC SPLINE:  like the linear spline we add specific functions to 
#####                increase the flexibility.
#
##### Q: What is the form of a CUBIC spline model?  
#####
##### ANSWER:
#####
#####      mean = b0 + b1 * X +
#####                  b2 * X^2 +
#####                  b3 * X^3 +
#####                  b4 *[ (X-6)^+ ]^3 + 
#####                  b5 * [ (X-12)^+ ]^3 + 
#####                  b6 * [ (X-18)^+ ]^3
#####
##### Note:  quite similar in spirit to the linear spline, but now we add
#####        additional cubic terms rather than additional linear terms.
#####        The resulting model permits more flexibility than the cubic
#####        polynomial model, and we'll see that clearly once we fit this
#####        model (maybe it is too flexible for these data?)
#
##### Define the cubic spline terms:
#
x6.cubed <- x6^3
x12.cubed <- x12^3
x18.cubed <- x18^3
#
##### Fit the model:
#
fit <- lm( y ~ x + x.squared + x.cubed + x6.cubed + x12.cubed + x18.cubed )
#
print( summary( fit ) )
#
##### Look at the fitted model:
#
fitted.mean <- predict( fit )
#
##### Plot it!
#
if( go.pdf) pdf( file="TheCubicSpline.pdf" )
plot( x, y )
lines( x, mu, col="red" )
lines( x, fitted.mean, col="blue", lwd=2 )
title("Data, true mean curve (red), and fitted (blue) using cubic spline")
if( go.pdf ) graphics.off()
#
#####
##### Comments: This fit also seems "close" to the data, and is smooth.
#####           However, unlike a cubic polynomial it allows a more flexible
#####           fit to the data.  It's not clear whether this additional 
#####           flexibility is really desirable since the fitted curve is
#####           clearly more variable than the underlying mean curve...
#####
#----------------------------------------------------------------------
#
#####
#####  Natural Splines
#####
#
#####  Briefly:  natural splines are another type of flexible 
#####            polynomial-based function that starts with a cubic
#####            spline, and then imposes the constraint that the function
#####            for the mean is to be linear (rather than cubic) beyond
#####            some boundary points -- usual the min and max of X.
#####
#####            Writing the form of the predictors used for this is 
#####            not simple.  Therefore, one usually relies on a 
#####            package = "splines"
#
#####  Example below...
#
library( splines )
#
fit <- lm( y ~ ns( x, knots=c(6,12,18) ) )
#
print( summary( fit ) )
#
##### Look at the fitted model:
#
fitted.mean <- predict( fit )
#
##### Plot it!
#
if( go.pdf) pdf( file="TheNaturalSpline.pdf" )
plot( x, y )
lines( x, mu, col="red" )
lines( x, fitted.mean, col="blue", lwd=2 )
title("Data, true mean curve (red), and fitted (blue) using natural spline")
if( go.pdf ) graphics.off()
#
#####
##### Comments: This fit also seems "close" to the data, and is smooth.
#####
#----------------------------------------------------------------------
##### Note:    If this "taste" of flexible curve fitting is interesting to you
#####          then you should consider taking 527 in the Spring!!!
#----------------------------------------------------------------------
#
# end-of-file...

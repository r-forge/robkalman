### constant, vector
setMethod("createwExo", "vector", function (object)    
{
##  w ... vector, constant value of exogenous variable 'w'
    w <- object

    funcW <- function (t, x1, uNew = NULL, wOld = NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t, 'global' variable!
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!

        if (length(w) < length(y)) {
            w <- rep(w, length.out=length(y))
        }

        if (length(w) > length(y)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(w))
        
    }
    return(new("OptionalFunctionWithControl",funcW))
})


### time-discrete, matrix
setMethod("createwExo", "matrix", function (object)    
{
##  w ... matrix, columnwise values of exogenous variable 'w'
    w <- object

    funcW <- function (t, x1, uNew = NULL, wOld = NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t, 'global' variable!
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!

        if (nrow(w) != length(y)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(w[, t]))
        
    }
    return(new("OptionalFunctionWithControl",funcW))
})


### time-continuous, function
setMethod("createwExo", "function", function (object)    
{
##  w ... function, w(t, x1, ...)
    w <- object

    funcW <- function (t, x1, uNew = NULL, wOld = NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t, 'global' variable!
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!

        retW <- as.vector(w(t, x1, uNew, wOld))
 
        if (length(retW) != length(y)) {
            stop("Dimensions do not match!")
        }

        return(retW)
    }
    return(new("OptionalFunctionWithControl",funcW))
})

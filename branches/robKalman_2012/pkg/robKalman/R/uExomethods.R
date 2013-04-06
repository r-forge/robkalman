### constant, vector
setMethod("createuExo", "vector", function (object)    
{
##  u ... vector, constant value of exogenous variable 'u'
    u <- object

    funcU <- function (t, x0, uOld = NULL, wNew = NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!

        if (length(u) < length(x0)) {
            u <- rep(u, length.out=length(x0))
        }

        if (length(u) > length(x0)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(u))
        
    }
    return(new("OptionalFunctionWithControl",funcU))
})


### time-discrete, matrix
setMethod("createuExo", "matrix", function (object)    
{
##  u ... matrix, columnwise values of exogenous variable 'u'
    u <- object

    funcU <- function (t, x0, uOld = NULL, wNew = NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!

        if (nrow(u) != length(x0)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(u[, t]))
        
    }
    return(new("OptionalFunctionWithControl",funcU))
})


### time-continuous, function
setMethod("createuExo", "function", function (object)    
{
##  u ... function, u(t, x0, ...)
    u <- object

    funcU <- function (t, x0, uOld = NULL, wNew = NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!

        retU <- as.vector(u(t, x0, uOld, wNew))
 
        if (length(retU) != length(x0)) {
            stop("Dimensions do not match!")
        }

        return(retU)
    }
    return(new("OptionalFunctionWithControl",funcU))
})

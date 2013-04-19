### constant, vector
setMethod("createuExo", "numeric", function (object)    
{
##  u ... vector, constant value of exogenous variable 'u'
    u <- object

    funcU <- function (i, t, x0, uOld = NULL, wNew = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
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

    funcU <- function (i, t, x0, uOld = NULL, wNew = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!

        if (nrow(u) != length(x0)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(u[, i]))
        
    }
    return(new("OptionalFunctionWithControl",funcU))
})


### time-continuous, function
setMethod("createuExo", "function", function (object)    
{
##  u ... function, u(t, x0, ...)
    u <- object

    funcU <- function (i=NULL, t, x0, uOld = NULL, wNew = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!

        retU <- as.vector(u(i=i, t=t, x0=x0, uOld=uOld, wNew=wNew))
 
        if (length(retU) != length(x0)) {
            stop("Dimensions do not match!")
        }

        return(retU)
    }
    return(new("OptionalFunctionWithControl",funcU))
})

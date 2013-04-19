### constant, vector
setMethod("createwExo", "numeric", function (object)    
{
##  w ... vector, constant value of exogenous variable 'w'
    w <- object

    funcW <- function (i, t, x1, y, uNew = NULL, wOld = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t
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

    funcW <- function (i, t, x1, y, uNew = NULL, wOld = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!

        if (nrow(w) != length(y)) {
            stop("Dimensions do not match!")
        }

        return(as.vector(w[, i]))
        
    }
    return(new("OptionalFunctionWithControl",funcW))
})


### time-continuous, function
setMethod("createwExo", "function", function (object)    
{
##  w ... function, w(t, x1, y, ...)
    w <- object

    funcW <- function (i=NULL, t, x1, y, uNew = NULL, wOld = NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!

        retW <- as.vector(w(i=i, t=t, x1=x1, y=y, uNew=uNew, wOld=wOld))
 
        if (length(retW) != length(y)) {
            stop("Dimensions do not match!")
        }

        return(retW)
    }
    return(new("OptionalFunctionWithControl",funcW))
})

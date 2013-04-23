### time-invariant case, linear
setMethod("createV", "matrix",
function (object,
          controlV = NULL, ...)    
{
##  V ... covariance matrix of innovations
    V <- object

    dots.propagated <- list(...)

    funcV <- function(i, t, x1, y,
                      control=controlV,
                      dots=dots.propagated)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        retV <- new("SSretValueV",
                    V = V, t = t,
                    x1 = x1, y = y,
                    control = control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
      	return(retV)
    }
    return(new("FunctionWithControl",funcV))
})


### time-variant case, linear
setMethod("createV", "array",
function (object,
          controlV = NULL, ...)    
{
##  V ... array of covariance matrices of innovations, V[, , t]
    V <- object

    dots.propagated <- list(...)

    funcV <- function(i, t, x1, y,
                      control=controlV,
                      dots=dots.propagated)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        retV <- new("SSretValueV",
                    V = V[, , i, drop=TRUE], t = t,
                    x1 = x1, y = y,
                    control = control, 
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
      	return(retV)
    }
    return(new("FunctionWithControl",funcV))
})


### function case
setMethod("createV", "function", function (object)    
{
##  V ... function, V(t, ...)
    V <- object

    funcV <- function(i=NULL, t, x1=0, y = 0, control=NULL, dots=NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- V(i=i, t=t, x1=x1, y = y,
                  control=control, dots=dots)
        if (is(ret0, "SSretValueV")) return(ret0)

        retV <- new("SSretValueV",
                    V = ret0$V, t = t,
                    x1 = x1, y = y,
                    control=control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
      	return(retV)
    }
    return(new("FunctionWithControl",funcV))
})

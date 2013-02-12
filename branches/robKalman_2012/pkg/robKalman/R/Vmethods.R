### time-invariant case, linear
setMethod("createV", "matrix", function (object)    
{
##  V ... covariance matrix of innovations
    V <- object

    funcV <- function(t, x1, exV, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exV ... exogenous variable exV_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        retV <- new("SSretValueV",
                    V = V, t=t,
                    x1 = x1, exV = exV,
                    control=control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
      	return(retV)
    }
    return(new("FunctionWithControl",funcV))
})


### time-variant case, linear
setMethod("createV", "array", function (object)    
{
##  V ... array of covariance matrices of innovations, V[, , t]

    V <- object

    funcV <- function(t, x1, exV, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exV ... exogenous variable exV_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()


        retV <- new("SSretValueV", V = V[,,t,drop=TRUE], t=t,
                    x1 = x1, exV = exV,
                    control=control, dots = dots, call = call,
                    diagnostics = list())


      	return(retV)
    }

    return(new("FunctionWithControl",funcV))

})


### function case
setMethod("createV", "function", function (object)    
{
##  V ... covariance matrix of innovations

    V <- object

    funcV <- function(t, x1, exV, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exV ... exogenous variable exV_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- V(t, x1, exV, control, dots)

        retV <- new("SSretValueV", V = ret0$V, t=t,
                    x1 = x1, exV = exV,
                    control=control, dots = dots, call = call,
                    diagnostics = list())

      	return(retV)
    }

    return(new("FunctionWithControl",funcV))

})

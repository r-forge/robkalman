setMethod("createQ", "array", function (object)    # time-variant case, linear
{
##  Q ... array of covariance matrices of innovations, Q[, , t]

    Q <- object

    funcQ <- function (t, x0, exQ, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()


        retQ <- new("SSretValueQ", Q = Q[,,t,drop=TRUE], t=t,
                    x0 = x0, exQ = exQ,
                    control=control, dots = dots, call = call,
                    diagnostics = list())


      	return(retQ)
    }

    return(new("FunctionWithControl",funcQ))

})

setMethod("createQ", "matrix", function (object)    # time-variant case, linear
{
##  Q ... covariance matrix of innovations

    Q <- object

    funcQ <- function (t, x0, exQ, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()


        retQ <- new("SSretValueQ", Q = Q, t=t,
                    x0 = x0, exQ = exQ,
                    control=control, dots = dots, call = call,
                    diagnostics = list())

      	return(retQ)
    }

    return(new("FunctionWithControl",funcQ))

})


setMethod("createQ", "function", function (object)    # time-variant case, linear
{
##  Q ... covariance matrix of innovations

    Q <- object

    funcQ <- function (t, x0, exQ, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- Q(t, x0, exQ, control, dots)

        retQ <- new("SSretValueQ", Q = ret0$Q, t=t,
                    x0 = x0, exQ = exQ,
                    control=control, dots = dots, call = call,
                    diagnostics = list())

      	return(retQ)
    }

    return(new("FunctionWithControl",funcQ))
})

### time-invariant case, linear
setMethod("createZ", "matrix", function (object, T = NULL)    
{
##  Z ... observation matrix
##  T ... selection matrix (observation noise)
    Z <- object

    if (is.null(T)) {
        T <- diag(nrow(Z))
    }

    funcZ <- function (t, x1, eps, w, control, dots)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  w ... exogenous variable w_t, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        y <- Z%*%x1 + w + T%*%eps

        retZ <- new("SSretValueZ", y = as.vector(y), Zmat = Z,
                    Tmat = T, t=t, x1 = x1, eps = eps, w = w,
                    control=control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retZ)
    }
    return(new("FunctionWithControl",funcZ))
})


### time-variant case, linear
setMethod("createZ", "array", function (object, T = NULL)    
{
##  Z ... array of observation matrices, Z[, , t]
##  T ... selection matrix array (observation noise)
    Z <- object

    if(length(dim(Z))==2) return(getMethod("Z", "matrix")(as.matrix(object),T))

    if (is.null(T)) {
        T <- array(diag((dim(Z))[1]), dim=dim(Z))
    }

    funcZ <- function (t, x1, eps, w, control, dots)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  w ... exogenous variable w_t, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        y <- Z[, , t]%*%x1 + w + T[, , t]%*%eps

        retZ <- new("SSretValueZ", y = y, Z = Z[,,t,drop=TRUE],
                    T = T[,,t,drop=TRUE], t=t, x1 = x1, eps = eps, w = w,
                    control=control, dots = dots, call = call,
                    diagnostics = list())
        return(retZ)
    }

    return(new("FunctionWithControl",funcZ))
})


### function case
setMethod("createZ", "function", function (object)    
{
##  Z ... observation matrix
##  T ... selection matrix (observation noise)
    Z <- object

    ### some Z checking possible and needed

    funcZ <- function (t, x1, eps, w, control, dots)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  w ... exogenous variable w_t, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- Z(t, x1, eps, w, control, dots)

        retZ <- new("SSretValueZ", y = ret0$y, Z = ret0$Z,
                    T = NULL, t=t, x1 = x1, eps = eps, w = w,
                    control=control, dots = dots, call = call,
                    diagnostics = list())
        return(retZ)
    }

    return(new("FunctionWithControl",funcZ))
})

### time-invariant case, linear
setMethod("createZ", "matrix", function (object, T = NULL)    
{
##  Z ... observation matrix
##  T ... selection matrix (observation noise)
    Z <- object

    if (is.null(T)) {
        T <- diag(nrow(Z))
    }

    funcZ <- function (t, x1, eps=rep(0, ncol(T)),
                       wFct, uNew=NULL, wOld=NULL,
                       control=list(whenEvalwExo=c("pre"=TRUE, "post"=FALSE)),
                       dots=NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (control$whenEvalwExo["pre"]) {
            w <- wFct(t=t, x1=x1, uNew=uNew, wOld=wOld)
        } else {
            w <- wOld
        }

        y <- as.vector(Z%*%x1 + w + T%*%eps)

        if (control$whenEvalwExo["post"]) {
            w <- wFct(t=t, x1=x1, uNew=uNew, wOld=wOld)
        }

        retZ <- new("SSretValueZ",
                    y = y, ZJcb = Z, TJcb = T,
                    t = t, x1 = x1, eps = eps, wNew = w, control=control,
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

    if (length(dim(Z))==2) {
        return(getMethod("Z", "matrix")(as.matrix(object), T))
    }

    if (is.null(T)) {
        nrowZ <- dim(Z)[1]
        T <- array(diag(nrowZ), dim=c(nrowZ, nrowZ, dim(Z)[3]))
    }

    funcZ <- function (t, x1, eps=rep(0, ncol(T[, , t])),
                       wFct, uNew=NULL, wOld=NULL,
                       control=list(whenEvalwExo=c("pre"=TRUE, "post"=FALSE)),
                       dots=NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (control$whenEvalwExo["pre"]) {
            w <- wFct(t=t, x1=x1, uNew=uNew, wOld=wOld)
        } else {
            w <- wOld
        }

        y <- as.vector(Z[, , t]%*%x1 + w + T[, , t]%*%eps)

        if (control$whenEvalwExo["post"]) {
            w <- wFct(t=t, x1=x1, uNew=uNew, wOld=wOld)
        }

        retZ <- new("SSretValueZ",
                    y = y, ZJcb = Z[, , t, drop=TRUE],
                    TJcb = T[, , t, drop=TRUE], t = t, x1 = x1,
                    eps = eps, wNew = w, control = control, 
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retZ)
    }
    return(new("FunctionWithControl",funcZ))
})


### function case
setMethod("createZ", "function", function (object)    
{
##  Z ... function , Z(t, x1, ...)
    Z <- object

    funcZ <- function (t, x1, eps=0,
                       wFct=NULL, uNew=NULL, wOld=NULL,
                       control=NULL, 
                       dots=NULL)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- Z(t, x1, eps, wFct, uNew, wOld, control, dots)
        if (is(ret0, "SSretValueZ")) return(ret0)

        retZ <- new("SSretValueZ",
                    y = ret0$y, ZJcb = ret0$C,
                    TJcb = ret$D, t = t, x1 = x1,
                    eps = eps, wNew = ret0$wNew, control=control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retZ)
    }

    return(new("FunctionWithControl",funcZ))
})

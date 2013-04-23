### time-invariant case, linear
setMethod("createZ", "matrix",
function (object, T = NULL,
          controlZ = list(whenEvalwExo=c("pre"=TRUE, "post"=FALSE)), ...)
{
##  Z ... observation matrix
##  T ... selection matrix (observation noise)
    Z <- object

    if (is.null(T)) {
        T <- diag(nrow(Z))
    }

    dots.propagated <- list(...)
    
    funcZ <- function (i, t, x1, y, eps=rep(0, ncol(T)),
                       wFct=NULL, uNew=NULL, wOld=NULL,
                       control=controlZ,
                       dots=dots.propagated)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations y_t
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (is.null(wFct)) wFct <- createwExo(0)

        if (control$whenEvalwExo["pre"]) {
            w <- wFct(i=i, t=t, x1=x1, y=y, uNew=uNew, wOld=wOld)
        } else {
            w <- wOld
        }

        yhat <- as.vector(Z%*%x1 + w + T%*%eps)

        if (control$whenEvalwExo["post"]) {
            w <- wFct(i=i, t=t, x1=x1, y=yhat, uNew=uNew, wOld=wOld)
        }

        retZ <- new("SSretValueZ",
                    y = yhat, ZJcb = Z, TJcb = T,
                    t = t, x1 = x1, eps = eps, wNew = w, control=control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retZ)
    }
    return(new("FunctionWithControl",funcZ))
})


### time-variant case, linear
setMethod("createZ", "array",
function (object, T = NULL,
          controlZ = list(whenEvalwExo=c("pre"=TRUE, "post"=FALSE)), ...)
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

    dots.propagated <- list(...)
    
    funcZ <- function (i, t, x1, y, eps=rep(0, ncol(T[, , t])),
                       wFct=NULL, uNew=NULL, wOld=NULL,
                       control=controlZ,
                       dots=dots.propagated)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations, y_t
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (is.null(wFct)) wFct <- createwExo(0)
        
        if (control$whenEvalwExo["pre"]) {
            w <- wFct(i=i, t=t, x1=x1, y=y, uNew=uNew, wOld=wOld)
        } else {
            w <- wOld
        }

        yhat <- as.vector(Z[, , i]%*%x1 + w + T[, , i]%*%eps)

        if (control$whenEvalwExo["post"]) {
            w <- wFct(i=i, t=t, x1=x1, y=yhat, uNew=uNew, wOld=wOld)
        }

        retZ <- new("SSretValueZ",
                    y = yhat, ZJcb = Z[, , i, drop=TRUE],
                    TJcb = T[, , i, drop=TRUE], t = t, x1 = x1,
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

    funcZ <- function (i=NULL, t, x1, y=NULL, eps=0,
                       wFct=NULL, uNew=NULL, wOld=NULL,
                       control=NULL, 
                       dots=NULL)
    {
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  y ... observations, y_t
    ##  eps ... observation noise \eps_t, vector!
    ##  wFct ... function of exogenous variable w, yields vector w_t
    ##  uNew ... exogenous variable u_t, vector!
    ##  wOld ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        ret0 <- Z(i=i, t=t, x1=x1, y=y, eps=eps,
                  wFct=wFct, uNew=uNew, wOld=wOld,
                  control=control, dots=dots)
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

### time-invariant case, linear
setMethod("createF", "matrix", function (object, R = NULL)    
{
##  F ... matrix of state equation
##  R ... selection matrix (cf. Durbin & Koopman, 2001, p.38)
    F <- object

    if (is.null(R)) {
        R <- diag(nrow(F))
    }

    funcF <- function (t, x0, v=rep(0, ncol(R)),
                       uFct, uOld=NULL, wNew=NULL,
                       control=list(whenEvaluExo=c("pre"=TRUE, "post"=FALSE)),
                       dots=NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  uFct ... function of exogenous variable u, yields vector u_t
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (control$whenEvaluExo["pre"]) {
            u <- uFct(t=t, x0=x0, uOld=uOld, wNew=wNew)
        } else {
            u <- uOld
        }

        x1 <- F%*%x0 + u + R%*%v

        if (control$whenEvaluExo["post"]) {
            u <- uFct(t=t, x0=as.vector(x1), uOld=uOld, wNew=wNew)
        }

        retF <- new("SSretValueF",
                    x1 = as.vector(x1), FJcb = F, RJcb = R,
                    t = t, x0 = x0, v = v, uNew = u, control = control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retF)
    }
    return(new("FunctionWithControl",funcF))
})


### time-variant case, linear
setMethod("createF", "array", function (object, R = NULL)    
{
##  F ... array of state equation, F[, , t]
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)
    F <- object

    if (length(dim(F))==2) {
        return(getMethod("createF", "matrix")(as.matrix(object), R))
    }

    if (is.null(R)) {
        nrowF <- dim(F)[1]
        R <- array(diag(nrowF), dim=c(nrowF, nrowF, dim(F)[3]))
    }

    funcF <- function (t, x0, v=rep(0, ncol(R[, , t])),
                       uFct, uOld=NULL, wNew=NULL,
                       control=list(whenEvaluExo=c("pre"=TRUE, "post"=FALSE)),
                       dots=NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  uFct ... function of exogenous variable u, yields vector u_t
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        if (control$whenEvaluExo["pre"]) {
            u <- uFct(t=t, x0=x0, uOld=uOld, wNew=wNew)
        } else {
            u <- uOld
        }

        x1 <- F[, , t]%*%x0 + u + R[, , t]%*%v

        if (control$whenEvaluExo["post"]) {
            u <- uFct(t=t, x0=as.vector(x1), uOld=uOld, wNew=wNew)
        }

        retF <- new("SSretValueF",
                    x1 = as.vector(x1), FJcb = F[, , t, drop=TRUE],
                    RJcb = R[, , t, drop=TRUE], t = t, x0 = x0,
                    v = v, uNew = u, control = control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retF)
    }
    return(new("FunctionWithControl",funcF))
})


### function case
setMethod("createF", "function", function (object)    
{
##  F ... function, F(t, x0, ...)
    F <- object

    funcF <- function (t, x0, v=0,
                       uFct=NULL, uOld=NULL, wNew=NULL,
                       control=NULL,
                       dots=NULL)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  uFct ... function of exogenous variable u, yields vector u_t
    ##  uOld ... exogenous variable u_{t-1}, vector!
    ##  wNew ... exogenous variable w_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()
 
        ret0 <- F(t, x0, v, uFct, uOld, wNew, control, dots)
        if (is(ret0, "SSretValueF")) return(ret0)
 
        retF <- new("SSretValueF",
                    x1 = ret0$x1, FJcb = ret0$A,
                    RJcb = ret0$B, t = t, x0 = x0,
                    v = v, uNew = ret0$uNew, control = control,
                    dots.propagated = dots, call = call,
                    diagnostics = new("SSDiagnosticRetValue"))
        return(retF)
    }
    return(new("FunctionWithControl",funcF))
})

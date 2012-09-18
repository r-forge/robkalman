setMethod("createF", "array", function (object, R = NULL)    # time-variant case, linear
{
##  F ... array of state equation, F[, , t]
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)

    F <- object

    if(length(dim(F))==2) return(getMethod("F", "matrix")(as.matrix(object),R))

    if (is.null(R)) {
        R <- array(diag((dim(F))[1]), dim=dim(F))
    }

    funcF <- function (t, x0, v, u, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        x1 <- F[, , t]%*%x0 + u + R[, , t]%*%v

        retF <- new("SSretValueF", x1 = x1, F = F[,,t,drop=TRUE],
                    R = R[,,t,drop=TRUE], t=t, x0=x0, control=control,
                    dots = dots, call = call, diagnostics = list())
        return(retF)
    }
    return(new("FunctionWithControl",funcF))
}

setMethod("createF", "matrix", function (object, R = NULL)    # time-variant case, linear
{
##  F ... array of state equation, F[, , t]
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)
    F <- object

    if (is.null(R)) {
        R <- diag(nrow(F))
    }

    funcF <- function (t, x0, v, u, control, dots)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector!
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        call <- match.call()

        x1 <- F%*%x0 + u + R%*%v

        retF <- new("SSretValueF", x1 = x1, F = F[,,t,drop=TRUE],
                    R = R[,,t,drop=TRUE], t=t, x0=x0, control=control,
                    dots = dots, call = call, diagnostics = list())
        return(retF)
    }
    return(new("FunctionWithControl",funcF))
}

setMethod("createF", "function", function (object)    # function case
{
##  F ... array of state equation, F[, , t]
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)
    F <- object

#    ### some F checking possible and needed
#
#
#    funcF <- function (t, x0, v, u, control, dots)
#    {
#   ##  t ... time index
#   ##  x0 ... filter estimate x_{t-1|t-1}, vector
#   ##  v ... innovations v_t, vector!
#   ##  u ... exogenous variable u_{t-1}, vector!
#   ##  control ... control parameters, list
#   ##  dots ... additional parameters, list
#       call <- match.call()
#
#       ret0 <- F(t, x0, v, u, control, dots)
#       if(is(ret0,"SSretValueF")) return(ret0)
#
#       retF <- new("SSretValueF", x1 = ret0$x1, F = ret0$F,
#                   R = NULL, t=t, x0=x0, control=control,
#                   dots = dots, call = call, diagnostics = list())
#       return(retF)
#   }
    return(new("FunctionWithControl",F))
#    return(new("FunctionWithControl",funcF))
}

#######################################################
## 
##  generating functions for the (extended) Kalman filter
##  author: Bernhard Spangl
##  version: 0.3 (changed: 2011-06-21, created: 2011-06-09)
##
#######################################################

#######################################################
## 
##  Function:  'F_t'
##  Arguments: t, x_{t-1}, v_t, 
##             u_{t-1} (exogenous), control 
##  Value:     x_t, A_t, B_t (Jacobian matrices), 
##             original arguments (t, x_{t-1}, v_t, u_{t-1}, control), 
##             call, 
##             diagnostics 

createF <- function (F, ...) 
{
    UseMethod("createF")
}

createF.matrix <- function (F, R)    # time-invariant case, linear
{
##  F ... matrix of state equation
##  R ... selection matrix (cf. Durbin & Koopman, 2001, p.38)

    if (is.null(R)) {
        R <- diag(nrow(F))
    }

    funcF <- function (t, x0, v, u, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        x1 <- F%*%x0 + u + R%*%v

        return(list(x1=x1, A=F, B=R, 
                    t=t, x0=x0, v=v, u=u, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcF)

}

createF.array <- function (F, R)    # time-variant case, linear
{
##  F ... array of state equation, F[, , t] 
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)

    if(length(dim(F))==2) return(createF.matrix(F,R))
    
    
    if (is.null(R)) {
        R <- array(diag((dim(F))[1]), dim=dim(F))
    }

    funcF <- function (t, x0, v, u, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        x1 <- F[, , t]%*%x0 + u + R[, , t]%*%v

        return(list(x1=x1, A=F[, , t], B=R[, , t], 
                    t=t, x0=x0, v=v, u=u, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcF)

}


#######################################################
## 
##  Function:  'Z_t'
##  Arguments: t, x_t, eps_t, 
##             w_t (exogenous), control 
##  Value:     y_t, C_t, D_t (Jacobian matrices), 
##             original arguments (t, x_t, eps_t, w_t, control), 
##             call, 
##             diagnostics 

createZ <- function (Z, ...) 
{
    UseMethod("createZ")
}

createZ.matrix <- function (Z, T)    # time-invariant case, linear
{
##  Z ... observation matrix
##  T ... selection matrix (observation noise)

    if (is.null(T)) {
        T <- diag(nrow(Z))
    }

    funcZ <- function (t, x1, eps, w, control)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  w ... exogenous variable w_t, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        y <- Z%*%x1 + w + T%*%eps

        return(list(y=y, C=Z, D=T, 
                    t=t, x1=x1, eps=eps, w=w, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcZ)

}

createZ.array <- function (Z, T)    # time-variant case, linear
{
##  Z ... array of observation matrices, Z[, , t]
##  T ... selection matrix array (observation noise)

    if (is.null(T)) {
        T <- array(diag((dim(Z))[1]), dim=dim(Z))
    }

    funcZ <- function (t, x1, eps, w, control)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  eps ... observation noise \eps_t, vector!
    ##  w ... exogenous variable w_t, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        y <- Z[, , t]%*%x1 + w + T[, , t]%*%eps

        return(list(y=y, C=Z[, , t], D=T[, , t], 
                    t=t, x1=x1, eps=eps, w=w, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcZ)

}


#######################################################
## 
##  Function:  'Q_t'
##  Arguments: t, x_{t-1}, 
##             exQ_{t-1} (exogenous), control 
##  Value:     Q_t (Jacobian matrix), 
##             original arguments (t, x_{t-1}, exQ_{t-1}, control), 
##             call, 
##             diagnostics 

createQ <- function (Q, ...) 
{
    UseMethod("createQ")
}

createQ.matrix <- function (Q)    # time-invariant case, linear
{
##  Q ... covariance matrix of innovations

    funcQ <- function (t, x0, exQ, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

	return(list(Q=Q, 
                    t=t, x0=x0, exQ=exQ, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcQ)

}

createQ.array <- function (Q)    # time-variant case, linear
{
##  Q ... array of covariance matrices of innovations, Q[, , t] 

    funcQ <- function (t, x0, exQ, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

	return(list(Q=Q[, , t], 
                    t=t, x0=x0, exQ=exQ, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcQ)

}


#######################################################
## 
##  Function:  'V_t'
##  Arguments: t, x_t, 
##             exV_t (exogenous), control 
##  Value:     V_t, (Jacobian matrix), 
##             original arguments (t, x_t, exV_t, control), 
##             call, 
##             diagnostics 

createV <- function (V, ...) 
{
    UseMethod("createV")
}

createV.matrix <- function (V)    # time-invariant case, linear
{
##  V ... covariance matrix of observation noise

    funcV <- function (t, x1, exV, control)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  exV ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        return(list(V=V, 
                    t=t, x1=x1, exV=exV, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcV)

}

createV.array <- function (V)    # time-variant case, linear
{
##  V ... array of observation noise covariance matrices, V[, , t]

    funcV <- function (t, x1, exV, control)
    {
    ##  t ... time index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  exV ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        return(list(V=V[, , t], 
                    t=t, x1=x1, exV=exV, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

    return(funcV)

}





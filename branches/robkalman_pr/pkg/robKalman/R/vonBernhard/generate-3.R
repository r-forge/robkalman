#######################################################
## 
##  generating functions for the (extended) Kalman filter
##  author: Bernhard Spangl
##  version: 0.1 (2011-06-09)
##
#######################################################

#######################################################
## 
##  Function 'F_t':
##  arguments: t, x_{t-1}, v_t, 
##             u_{t-1} (exogenous), control 
##  returning: x_t, A_t, B_t (Jacobian matrices), 
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

    pd <- (dim(F))[1]
    rd <- (dim(R))[2]

    funcF <- function (t, x0, v, u, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        x1 <- F%*%x0 + u + R%*%v
        A <- diag(pd)    # Jacobian, could also be calc. in 'create.F'
        B <- diag(rd)

        return(list(x1=x1, A=A, B=B, 
                    t=t, x0=x0, v=v, u=u, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

}

createF.array <- function (F, R)    # time-variant case, linear
{
##  F ... array of state equation, F[, , t] 
##  R ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)

    pd <- (dim(F))[1]
    rd <- (dim(R))[2]

    funcF <- function (t, x0, v, u, control)
    {
    ##  t ... time index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  v ... innovations v_t, vector!
    ##  u ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
        call <- match.call()

        x1 <- F[, , t]%*%x0 + u + R[, , t]%*%v
        A <- diag(pd)    # Jacobian, could also be calc. in 'create.F'
        B <- diag(rd)

        return(list(x1=x1, A=A, B=B, 
                    t=t, x0=x0, v=v, u=u, control=control, 
                    call=call, 
                    diagnostics=list())) 
    }

}





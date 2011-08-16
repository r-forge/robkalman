#######################################################
## 
##  generating functions for the (extended) Kalman filter
##  author: Bernhard Spangl  & Peter Ruckdeschel
##  version: 0.3 (changed: 2011-08-15, created: 2011-06-09)
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

createF.matrix <- function (F, F.s)    # time-invariant case, linear
{
##  F ... matrix of state equation
##  F.V ... selection matrix (cf. Durbin & Koopman, 2001, p.38)

    if (is.null(F.s)) {
        F.s <- diag(nrow(F))
    }

    funcF <- function (t, i, x0, mu.v, exF, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  mu.v ... expectation of innovations v_t, vector!
    ##  exF ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        x1 <- F%*%x0 + exF + F.s%*%mu.v

        return(list(x1=x1, F=F, F.s=F.s, 
                    t=t, i = i, x0=x0, mu.v=mu.v, exF=exF, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcF, control=NULL))

}

createF.array <- function (F, F.s)    # time-variant case, linear
{
##  F ... array of state equation, F[, , t] 
##  F.s ... selection matrix array (cf. Durbin & Koopman, 2001, p.38)

    if (is.null(F.s)) {
        F.s <- array(diag((dim(F))[1]), dim=dim(F))
    }

    funcF <- function (t, i, x0, mu.v, exF, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  mu.v ... expectation of innovations v_t, vector!
    ##  exF ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        x1 <- F[, , i]%*%x0 + exF + F.s[, , i]%*%mu.v

        return(list(x1=x1, F=F[, , i], F.s=F.s[, , i], 
                    t=t, i = i, x0=x0, mu.v=mu.v, exF=exF, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcF, control=NULL))

}


#######################################################
## 
##  Function:  'Z_t'
##  Arguments: t, x_t, eps_t, 
##             w_t (exogenous), control 
##  Value:     y_t, C_t, D_t (Jacobian matrices), 
##             original arguments (t, x_t, mu.eps_t, w_t, control), 
##             call, 
##             diagnostics 

createZ <- function (Z, ...) 
{
    UseMethod("createZ")
}

createZ.matrix <- function (Z, Z.V)    # time-invariant case, linear
{
##  Z ... observation matrix
##  Z.s ... selection matrix (observation noise)

    if (is.null(Z.s)) {
        Z.s <- diag(nrow(Z))
    }

    funcZ <- function (t, i, x1, mu.eps, exZ, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  mu.eps ... expectation of observation noise \mu.eps_t, vector!
    ##  exZ ... exogenous variable w_t, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        y <- Z%*%x1 + exZ + Z.s%*%mu.eps

        return(list(y=y, Z=Z, Z.s=Z.s, 
                    t=t, i = i, x1=x1, mu.eps=mu.eps, exZ=exZ, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcZ, control=NULL))

}

createZ.array <- function (Z, Z.s)    # time-variant case, linear
{
##  Z ... array of observation matrices, Z[, , t]
##  Z.s ... selection matrix array (observation noise)

    if (is.null(Z.s)) {
        Z.s <- array(diag((dim(Z))[1]), dim=dim(Z))
    }

    funcZ <- function (t, i, x1, mu.eps, exZ, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  mu.eps ... expectation of observation noise \mu.eps_t, vector!
    ##  exZ ... exogenous variable w_t, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        y <- Z[, , i]%*%x1 + exZ + Z.s[, , i]%*%mu.eps

        return(list(y=y, Z=Z[, , i], Z.s=Z.s[, , i], 
                    t=t, i = i, x1=x1, mu.eps=mu.eps, exZ=exZ, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcZ, control=NULL))

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

    funcQ <- function (t, i, x0, exQ, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

	return(list(Q=Q, 
                    t=t, i = i, x0=x0, exQ=exQ, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcQ, control=NULL))

}

createQ.array <- function (Q)    # time-variant case, linear
{
##  Q ... array of covariance matrices of innovations, Q[, , t] 

    funcQ <- function (t, i, x0, exQ, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x0 ... filter estimate x_{t-1|t-1}, vector
    ##  exQ ... exogenous variable exQ_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

	return(list(Q=Q[, , i], 
                    t=t, i = i, x0=x0, exQ=exQ, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcQ, control=NULL))

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

    funcV <- function (t, i, x1, exV, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  exV ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        return(list(V=V, 
                    t=t, i = i, x1=x1, exV=exV, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcV, control=NULL))

}

createV.array <- function (V)    # time-variant case, linear
{
##  V ... array of observation noise covariance matrices, V[, , t]

    funcV <- function (t, i, x1, exV, control, additinfofrompast, ...)
    {
    ##  t ... time 
    ##  i ... state index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  exV ... exogenous variable u_{t-1}, vector! 
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        return(list(V=V[, , i], 
                    t=t, i = i, x1=x1, exV=exV, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
    }

    return(list(fun=funcV, control=NULL))

}

createExo <- function(exo){
   funcExo <- function(t, i, x1, control, additinfofrompast, ...){
    ##  t ... time 
    ##  i ... state index
    ##  x1 ... one-step ahead predictor x_{t|t-1}, vector
    ##  control ... control parameters, list
    ##  additinfofrompast ... an updated list which comes from the filter past
    ##  ... additional arguments -- unspecified whether from past or from ex ante
        call <- match.call()

        return(list(exo = if(is.null(exo)) 0 else exo[ , i], 
                    t=t, i = i, x1=x1, control=control, 
                    additinfofrompast = additinfofrompast, dots=list(...), 
                    call=call, 
                    diagnostics=list())) 
   
   } 
   return(list(fun=funcExo))
}

createStateEq <- function (F, F.s = NULL, Q, exo = NULL, mu.v, distribution = NULL, ...) 
{
    Fl <- createF(F, F.s, ...)
    Ql <- createQ(Q, ...)
    exol <- createExo(exo,...)
    return(list(F=Fl, Q=Ql, exo= exol, mu.v = mu.v, distr = distribution))
} 

createObsEq <- function (Z, Z.s= NULL, V, exo = NULL, mu.eps, distribution = NULL, ...) 
{
    Zl <- createZ(Z, Z.s, ...)
    Vl <- createV(V, ...)
    exol <- createExo(exo,...)
    return(list(Z=Zl, V=Vl, exo = exol, mu.eps = mu.eps, distr = distribution))
} 

createStartEq <- function(a0, Sigma0, distribution = NULL){
   return(list(a0=a0, Sigma0=Sigma0, distr = distribution))
}

createModel.h <- function(StartEq, StateEq, ObsEq){
  return(list(StartEq = StartEq, StateEq = StateEq, ObsEq = ObsEq))
}

createModel <- function(a0, Sigma0, distr.start = NULL,
                        F, F.s = NULL, Q, exo.state = NULL, mu.v = 0, distr.state = NULL, 
                        Z, Z.s = NULL, V, exo.obs = NULL, mu.eps = 0, distr.obs = NULL, ... )
     return(createModel.h(createStartEq(a0, Sigma0, distr.start),
                          createStateEq(F, F.s, Q, exo.state, mu.v, distr.state, ...),
                          createObsEq(Z, Z.s, V, exo.obs, mu.eps, distr.obs, ...)))


createObsInput <- function(y, timestamps.y = NULL, timestamps.x = NULL){
      if(is.zoo(y)) timestamps.y <- index(y)
         else if(is.null(y)) timestamps.y <- seq(along = y)
      if(is.null(timestamps.x)) timestamps.x <- timestamps.y
      y0 <- coredata(y)
      return(list(y = y0, timestamps.y = timestamps.y, 
                          timestamps.x = timestamps.x))
}

createFctCtrl <- function(fct, control) return(list(fct = fct, control = control))

createFilterProc <-  function(init.fct, init.ctrl = NULL, 
                              pred.fct, pred.ctrl = NULL,               
                              corr.fct, corr.ctrl = NULL){
                     return(list(init = createFctCtrl(init.fct, init.ctrl),
                                 pred = createFctCtrl(corr.fct, corr.ctrl)
                                 corr = createFctCtrl(pred.fct, pred.ctrl)))         
                              }

createRobFilterProc <-  function(init.fct.cla, init.ctrl.cla = NULL, 
                                 pred.fct.cla, pred.ctrl.cla = NULL,               
                                 corr.fct.cla, corr.ctrl.cla = NULL,
                                 init.fct.rob = NULL, init.ctrl.rob = NULL, 
                                 pred.fct.rob = NULL, pred.ctrl.rob = NULL,               
                                 corr.fct.rob = NULL, corr.ctrl.rob = NULL){
                     return(list(classic = createFilterProc(
                                 init.fct.cla, init.ctrl.cla, 
                                 pred.fct.cla, pred.ctrl.cla,               
                                 corr.fct.cla, corr.ctrl.cla
                                            ),
                                 robust = createFilterProc(
                                 init.fct.rob, init.ctrl.rob, 
                                 pred.fct.rob, pred.ctrl.rob,               
                                 corr.fct.rob, corr.ctrl.rob
                                            )))         
                              }                              
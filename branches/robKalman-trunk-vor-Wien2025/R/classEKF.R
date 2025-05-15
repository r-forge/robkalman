#######################################################
## 
##  classical extended Kalman filter routines
##  author: Bernhard Spangl  & Peter Ruckdeschel
##  version: 0.3 (changed: 2011-08-16, created: 2011-06-10)
##
#######################################################

.getDelta <-  function (S1, C, D, V)
{
##  calculates the Cov(Delta y_t) 
##      for S1 = S_{t|t-1}, C (=Z), D (=Id), V as above
    H <- S1 %*% t(C)
    ginv( C %*% H + D %*% V %*% t(D) ) 
}

.getKG <-  function (S1, Z, Delta)
{
##  calculates the Kalman Gain for S1 = S_{t|t-1}, Z, V as above
    S1 %*% t(Z) %*% Delta 
}

.getcorrCov <-  function (S1, K, Z)
{
##  calculates S_{t|t} for S1 = S_{t|t-1}, K_t, Z as above
    S1 - K %*% Z %*% S1 
}

.getpredCov <-  function (S0, A, B, Q)
{
##  calculates S_{t|t-1} for S0 = S_{t-1|t-1}, A (=F), B (=Id), Q as above
    A %*% S0 %*% t(A) + B %*% Q %*% t(B)
}

.cEKFinitstep <- function (a, S, controlInit, ...) 
{
##  a ... E(x_0)
##  S ... S_{0|0} = Cov(x_0 - x_{0|0}) 
##                = E((x_0 - x_{0|0})(x_0 - x_{0|0})^\top), error covariance
##  controlInit ... control parameters of used filter

    return(list(x0=a, S0=S, controlInit=controlInit))

}

.cEKFpredstep <- function (x0, S0, stateEq, mu.v,
                          # F, Q, 
                           t, i, additinfofromCorr, ...) 
                          # v, u, controlF,    # arguments of F
                          # exQ, controlQ,    # arguments of Q
                          # controlPred, ...)    # arguments of used filter
{
##  x0 ... x_{t-1|t-1}, filter estimate
##  S0 ... S_{t-1|t-1}, conditional filtering error covariance matrix
##  F ... F(t, x_{t-1}, u_t, v_t, control), function of state equation
##  Q ... Q(t, x_{t-1}, exQ_{t-1}, control), function of innovations cov-matrix
##  i ... time index
##  v ... innovations v_t
##  u ... u_{t-1}, exogenous variables of F
##  controlF ... control parameters of F
##  exQ ... exQ_{t-1}, exogenous variables of Q
##  controlQ ... control parameters of Q
##  controlPred ... control parameters of used filter

    F <- stateEq$F$fun(t=t, i=i, x0=x0, mu.v=mu.v, 
                     exF = stateEq$exo$fun(t=t, i=i, x1=x0, 
                                         control=stateEq$F$control,
                                         additinfofrompast = additinfofromCorr,
                                         ...), 
                     control = stateEq$F$control,
                     additinfofrompast = additinfofromCorr,...)
    x1 <- F$x1
    A <- F$A
    B <- F$B
    Q <- stateEq$Q$fun(t=t, i=i, x0=x0,  
                     exQ = stateEq$exo$fun(t=t, i=i, x1=x0, 
                                         control=stateEq$Q$control,
                                         additinfofrompast = additinfofromCorr,
                                         ...), 
                     control = stateEq$Q$control,
                     additinfofrompast = additinfofromCorr,...)
    Q.m <- Q$Q

    return(list(x1=x1, S1=.getpredCov(S0=S0, A=A, B=B, Q=Q.m), 
                additinfofromPred=additinfofrompast, F=F, Q=Q))

}

.cEKFcorrstep <- function (y, x1, S1, obsEq, mu.eps,
                          # F, Q, 
                           t, i, additinfofromPred, ...) 
                          #y, x1, S1, Z, V, i, 
                        #   eps, w, controlZ,    # arguments of Z
                        #   exV, controlV,    # arguments of V 
                        #   controlCorr, ...)    # arguments of used filter
{
##  y ... observations
##  x1 ... x_{t|t-1}, one-step-ahead prediction 
##  S1 ... S_{t|t-1}, conditional prediction error covariance matrix
##  Z ... Z(t, x_t, eps_t, w_t, control), function of observation equation
##  V ... V(t, x_t, exV_t, control), function of cov-matrix of observation error
##  i ... time index 
##  eps ... observation error \eps_t
##  w ... exogenous variable w_t 
##  controlZ ... control parameters of Z
##  exV ... exV_t, exogenous variables of V
##  controlV ... control parameters of V
##  controlCorr ... control parameters of used filter

    Z <- obsEq$Z$fun(t=t, i=i, x1=x1, mu.eps=mu.eps, 
                     exZ = obsEq$exo$fun(t=t, i=i, x1=x1, 
                                         control=obsEq$Z$control,
                                         additinfofrompast = additinfofromPred,
                                         ...), 
                     control = obsEq$Z$control,
                     additinfofrompast = additinfofromPred,...)
    yhat <- Z$y
    C <- Z$Z
    D <- Z$Z.s

    V <- obsEq$V$fun(t=t, i=i, x1=x1,  
                     exV = obsEq$exo$fun(t=t, i=i, x1=x1, 
                                         control=obsEq$V$control,
                                         additinfofrompast = additinfofromPred,
                                         ...), 
                     control = obsEq$V$control,
                     additinfofrompast = additinfofromPred,...)
    V.m <- V$V

    Delta <- .getDelta(S1=S1, C=C, D=D, V=V)
    K <- .getKG(S1=S1, Z=C, Delta=Delta)
    DeltaY <- y - yhat 
    x0 <- x1 + K %*% DeltaY
    S0 <- .getcorrCov(S1=S1, K=K, Z=C)

    return(list(x0=x0, K=K, S0=S0, Delta=Delta, DeltaY=DeltaY, 
                additinfofromCorr=additinfofrompast, Z=Z, Q=Q))

}



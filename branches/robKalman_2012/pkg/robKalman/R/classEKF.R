#######################################################
## 
##  classical extended Kalman filter routines
##  author: Bernhard Spangl
##  version: 0.3 (changed: 2011-12-13, created: 2011-06-10)
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

.cinitstep <- function (a, S, controlInit, ...)
{
##  a ... E(x_0)
##  S ... S_{0|0} = Cov(x_0 - x_{0|0}) 
##                = E((x_0 - x_{0|0})(x_0 - x_{0|0})^\top), error covariance
##  controlInit ... control parameters of used filter

    return(list(x0=a, S0=S, controlInit=controlInit))

}

.cpredstep <- function (x0, S0, F, Q, i,
                           v, u, controlF,    # arguments of F
                           exQ, controlQ,    # arguments of Q
                           controlPred, ...)    # arguments of used filter
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
    Freturn <- F(t=i, x0=x0, v=v, u=u, control=controlF)
    x1 <- Freturn$x1
    A <- Freturn$A
    B <- Freturn$B

    Qreturn <- Q(t=i, x0=x0, exQ=exQ, control=controlQ)
    Q <- Qreturn$Q

    return(list(x1=x1, S1=.getpredCov(S0=S0, A=A, B=B, Q=Q), 
                controlPred=controlPred))

}

.ccorrstep <- function (y, x1, S1, Z, V, i,
                           eps, w, controlZ,    # arguments of Z
                           exV, controlV,    # arguments of V 
                           controlCorr, ...)    # arguments of used filter
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

    Zreturn <- Z(t=i, x1=x1, eps=eps, w=w, control=controlZ)
    yhat <- Zreturn$y
    C <- Zreturn$C
    D <- Zreturn$D

    Vreturn <- V(t=i, x1=x1, exV=exV, control=controlV)
    V <- Vreturn$V

    Delta <- .getDelta(S1=S1, C=C, D=D, V=V)
    K <- .getKG(S1=S1, Z=C, Delta=Delta)
    DeltaY <- y - yhat 

    x0 <- x1 + K %*% DeltaY
    S0 <- .getcorrCov(S1=S1, K=K, Z=C)

    return(list(x0=x0, K=K, S0=S0, Delta=Delta, DeltaY=DeltaY, 
                controlCorr=controlCorr))

}



.getcorrCovACM  <- function (S1, K,  Z, W=1)
{
###########################################
##
##  R-function: .corrCov - computes filtering error covarince matrix
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.1 (2009-07-29)
##
###########################################

##  Paramters:
##  S1 ... prediction error covariance matrix
##  K ... Kalman gain
##  W ... weight (scalar)
##  Z ... observation matrix

    S1 - (K * W) %*% Z %*% S1
}

##  better: .mACMinitstep <- .cKinitstep

.ACMpredstep <- function (x0, S0, F, Q, i, rob0, s0, ...)  ### S=P F= Phi
{
###########################################
##
##  R-function: .ACMpredstep - prediction step (internal function)
##  author: Bernhard Spangl
##  version: 1.1 (2009-07-29)
##
###########################################

##  Paramters:
##  x0 ... state vector (filter estimate), pd x runs matrix
##  S0 ... filtering error covariance matrix (formerly: P), 
##         pd x pd x runs array (!!!)
##  F ... design matrix of state equation (formerly: Phi)
##  Q ... covariance matrix of state innovation process
##  i ... time index, currently not used
##  rob0 ... list, general robust parameter 
##           here: sqrt of variance 'Rt', 
##                 i.e., of Var(Delta y_t) 

    S1 <- array(apply(S0, 3, .getpredCov, F, Q), dim=dim(S0))
    rob1 <- NULL
    return(list(x1 = F %*% x0, S1 = S1, rob1 = rob1, Ind = 0))
}

.ACMcorrstep <- function (y, x1, S1, Z, V, i, rob1=NULL, 
                          psi, apsi, bpsi, cpsi, flag, ...)
{
###########################################
##
##  R-function: .ACMcorrstep - correction step (internal function)
##  author: Bernhard Spangl
##  version: 1.1 (2009-07-29)
##
###########################################

##  Paramters:
##  y ... observed univariate time series, qd(=1) x runs matrix 
##  x1 ... state vector (one-step-ahead predictor), pd x runs matix
##  S1 ... prediction error covariance matrix (formerly: M), 
##         pd x pd x runs array (!!!)
##  Z ... observation matrix (formerly: H), qd(=1) x pd
##  V ... covariance matrix of observation noise (formerly: R), qd(=1) x qd(=1)
##  i ... time index, currently not used
##  psi ... influence function to be used 
##  a, b, c ... tuning constants for Hampel's psi-function
##              (default: a=b=2.5, c=5.0)
##  flag ... character, if "weights" (default), use psi(t)/t to calculate 
##           the weights; if "deriv", use psi'(t)

##  Warning: Algorithm can not be completely vectorized!

    runs <- dim(S1)[3]
    foo <- function (x, m) x %*% t(m)
    S0 <- array(0, dim=dim(S1))

    D <- apply(S1, 3, foo, m=Z)    # \
    Rt <- drop(Z %*% D) + V        #  > st^2 = Z %*% S1 %*% t(Z) + V 
    st <- sqrt(Rt)                 # /

    K <- t(t(D)/Rt)    # Kalman Gain

    rst <- drop(y - Z %*% x1)/st

    ps <- psi(rst, apsi, bpsi, cpsi)
    dx <- t(t(K) * (st * ps))
    x0 <- x1 + dx

    ind <- abs(rst) > apsi
    
    w <- psi(rst, apsi, bpsi, cpsi, flag)

    for (j in 1:runs) {
        S0[, , j] <- .getcorrCovACM(S1=S1[, , j], K=K[, j], Z=Z, W=w[j])
    }

    return(list(x0 = x0, 
                K = aperm(array(K, dim=c(dim(K), 1)), c(1, 3, 2)),  
                S0 = S0, 
                Ind=ind, 
                rob0=st, 
                Delta=array(Rt, dim=c(1, 1, runs)), 
                DeltaY = matrix(rst*st, 1, runs)))
}


.getcorrCovmACM  <- function (S1, K, W=diag(ncol(K)))
{
###########################################
##
##  R-function: .getcorrCovmACM - computes filtering error covarince matrix
##              (internal function)
##  author: Bernhard Spangl
##  version: 0.1 (2008-03-23)
##
###########################################

##  Paramters:
##  S1 ... prediction error covariance matrix (formerly: M)
##  K ... dummy matrix, i.e., S1 %*% t(Z) %*% st
##  W ... weight matrix

    S1 - K %*% W %*% t(K)
}

.mACMcorrstep <- function (y, x1, S1, Z, V, rob1=NULL, 
                           psi, apsi, bpsi, cpsi, flag, ...)
{
###########################################
##
##  R-function: .mACMcorrstep - correction step (internal function)
##  author: Bernhard Spangl
##  version: 0.2 (2008-03-31)
##
###########################################

##  Paramters:
##  y ... observed vector-valued time series 
##  x1 ... state vector (one-step-ahead predictor)
##  S1 ... prediction error covariance matrix (formerly: M)
##  Z ... observation matrix (formerly: H)
##  V ... covariance matrix of observation noise (formerly: R)
##  psi ... influence function to be used 
##  a, b, c ... tuning constants for Hampel's psi-function
##              (default: a=b=2.5, c=5.0)
##  flag ... character, weight matrix to be used in correction step, 
##           if "deriv" (default), Jacobian matrix of multivariate analogue 
##           of Hampel's psi-function is used (only default is available 
##           at the moment)

    D <- S1 %*% t(Z)
    Rt <- Z %*% D + V
    sqrtR <- rootMatrix(Rt)
    st <- sqrtR$X.sqrt.inv

    K <- D %*% st

    yDelta <- drop(st %*% (y - Z %*% x1)) 

    yPsi <- psi(yDelta, apsi, bpsi, cpsi)
    xDelta <- K %*% yPsi
    x0 <- x1 + xDelta

    ind <- sqrt(yDelta%*%yDelta) > apsi

    jacobian.psi <- .weighting(flag)
    W <- jacobian.psi(yDelta, a=apsi, b=bpsi, c=cpsi)
    
    S0 <- .getcorrCovmACM(S1, K, W = W)

    return(list(x0 = x0, K = K, S0 = S0, Ind=ind, rob0=st))
}


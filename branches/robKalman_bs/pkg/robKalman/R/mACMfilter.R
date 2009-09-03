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

##  better: .mACMinitstep <- .cKinitstep

.mACMpredstep <- function (x0, S0, F, Q, i, ..., rob0) 
{
###########################################
##
##  R-function: .mACMpredstep - prediction step (internal function)
##  author: Bernhard Spangl
##  version: 0.3 (2009-07-27)
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
##           here: inverse sqrt of covariance matrix 'Rt', 
##                 i.e., of Cov(Delta y_t) 

##  Warning: Algorithm can not be completely vectorized!

    runs <- dim(x0)[2]
    S1 <- array(0, dim=dim(S0))

    for (j in 1:runs) {
        S1[, , j] <- .getpredCov(S0[, , j], F, Q)
    }
    rob1 <- NULL
    return(list(x1 = F %*% x0, S1 = S1, rob1 = rob1, Ind=0))
}

.mACMcorrstepInt <- function (y, x1, S1, Z, V, rob1=NULL, norm, 
                              psi, apsi, bpsi, cpsi, flag, ...)
{
###########################################
##
##  R-function: .mACMcorrstepInt - inner correction step 
##                                 (internal function)
##  author: Bernhard Spangl
##  version: 0.3 (2009-07-24)
##
###########################################

##  Paramters:
##  y ... observed vector-valued time series 
##  x1 ... state vector (one-step-ahead predictor)
##  S1 ... prediction error covariance matrix (formerly: M)
##  Z ... observation matrix (formerly: H)
##  V ... covariance matrix of observation noise (formerly: R)
##  norm ... which norm should be used? (default: EuclideanNorm)
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

    K <- D %*% st    # not exactly the classical Kalman Gain, 
                     # 'K %*% st' wolud be

    DeltaY <- y - Z %*% x1 
    yDelta <- st %*% DeltaY
    ##  yDelta <- drop(st %*% (y - Z %*% x1))

    yPsi <- psi(yDelta, apsi, bpsi, cpsi, norm)
    xDelta <- K %*% yPsi
    x0 <- x1 + xDelta

    ind <- norm(yDelta) > apsi

    jacobian.psi <- .weighting(flag)
    W <- jacobian.psi(drop(yDelta), a=apsi, b=bpsi, c=cpsi, norm)
    
    S0 <- .getcorrCovmACM(S1, K, W = W)

    return(list(x0 = x0, K = K, S0 = S0, Ind = ind, rob0 = st, Delta = Rt, 
                DeltaY = DeltaY))
}

.mACMcorrstep <- function (y, x1, S1, Z, V, i, rob1=NULL, norm, 
                           psi, apsi, bpsi, cpsi, flag, ...)
{
###########################################
##
##  R-function: .mACMcorrstep - correction step (internal function)
##  author: Bernhard Spangl
##  version: 0.3 (2009-07-24)
##
###########################################

##  Paramters:
##  y ... observed vector-valued time series, qd x runs matrix 
##  x1 ... state vector (one-step-ahead predictor), pd x runs matix
##  S1 ... prediction error covariance matrix (formerly: M), 
##         pd x pd x runs array (!!!)
##  Z ... observation matrix (formerly: H)
##  V ... covariance matrix of observation noise (formerly: R)
##  i ... time index, currently not used
##  norm ... which norm should be used? (default: EuclideanNorm)
##  psi ... influence function to be used 
##  a, b, c ... tuning constants for Hampel's psi-function
##              (default: a=b=2.5, c=5.0)
##  flag ... character, weight matrix to be used in correction step, 
##           if "deriv" (default), Jacobian matrix of multivariate analogue 
##           of Hampel's psi-function is used (only default is available 
##           at the moment)

##  Warning: Algorithm can not be completely vectorized!

    pd <- dim(x1)[1]
    qd <- dim(y)[1]
    runs <- dim(S1)[3]

    x0 <- matrix(0, pd, runs)
    K <- array(0, dim = c(pd, qd, runs))
    S0 <- array(0, dim = c(pd, pd, runs))
    Ind <- NULL
    rob0 <- array(0, dim = c(qd, qd, runs))
    Delta <- array(0, dim = c(qd, qd, runs))
    DeltaY <- matrix(0, qd, runs)

    for (j in 1:runs) {
         mACMcs <- .mACMcorrstepInt(y[, j], x1[, j], S1[, , j], Z, V, 
                       rob1, norm, psi, apsi, bpsi, cpsi, flag, ...)

         x0[, j] <- mACMcs$x0
         K[, , j] <- mACMcs$K
         S0[, , j] <- mACMcs$S0
         Ind[j] <- mACMcs$Ind
         rob0[, , j] <- mACMcs$rob0
         Delta[, , j] <- mACMcs$Delta
         DeltaY[, j] <- mACMcs$DeltaY
    }

    return(list(x0 = x0, K = K, S0 = S0, Ind=Ind, rob0 = rob0, 
           Delta = Delta, DeltaY = DeltaY))

}


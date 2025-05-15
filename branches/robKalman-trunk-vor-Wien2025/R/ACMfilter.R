.getcorrCovACM  <- function (S1, K,  Z, W=diag(nrow(Z)))
{
###########################################
##
##  R-function: .corrCov - computes filtering error covarince matrix
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-22)
##
###########################################

##  Paramters:
##  S1 ... prediction error covariance matrix
##  K ... Kalman gain
##  W ... weight matrix
##  Z ... observation matrix

    S1 - K %*% W %*% Z %*% S1
}

##steps for classical Kalman filter (cK)
.ACMinitstep <- function(a, S, ...) 
              {dots <- list(...)
               if(hasArg("s0")) 
                    s0<-dots$"s0"
               else
                    s0<-NULL       
               list( x0 = a,  S0 = S, s0 = s0)}

.ACMpredstep <- function (x0, S0, F, Q, i, rob0, s0, ...)  ### S=P F= Phi
{
###########################################
##
##  R-function: .ACMpredstep - prediction step (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-22)
##
###########################################

##  Paramters:
##  x0 ... state vector (filter estimate)
##  F=Phi ... design matrix of state equation
##  S0 ... filtering error covariance matrix
##  Q ... covariance matrix of state innovation process
##  rob0 ... general robust parameter --- here: scale s0 of nominal Gaussain component of additive noise
    S1 <- .getpredCov(S0, F, Q)
    return(list(x1 = F %*% x0, S1 = S1, rob1 = sqrt(S1[1, 1] + s0), Ind=1))
}

.ACMcorrstep <- function (y, x1, S1, Z, V, i, rob1, dum=NULL, psi, apsi, bpsi, cpsi, flag, ...)
{
###########################################
##
##  R-function: .ACMcorrstep - correction step (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-22)
##
###########################################

##  Paramters:
##  y ... univariate time series 
##  x1 ... state vector (one-step-ahead predictor)
##  rob1 ... general robust parameter --- here st ... time-dependent scale parameter
##  S1 ... prediction error covariance matrix 
##  Z ... observation matrix
##  dum ... dummy variable for compatibility with ... argument of calling function
##  V ... covariance matrix of observation noise
##  psi ... influence function to be used 
##  a, b, c ... tuning constants for Hampel's psi-function
##              (defaul: a=b=2.5, c=5.0)
##  flag ... character, if "weights" (default), use psi(t)/t to calculate 
##           the weights; if "deriv", use psi'(t)
    st <- rob1

    K <- .getKG(S1, Z, V)

    rst <- (y - x1[1])/st

    ps <- psi(rst, apsi, bpsi, cpsi)[1,1]
    dx <- K * st * ps
    x0 <- x1 + dx

    ind <- (abs(rst-ps)>10^-8)
    
    w <- psi(rst,  apsi, bpsi, cpsi, flag)
    
    S0 <- .getcorrCovACM(S1, K,  Z, W = w*diag(rep(1, nrow(Z))))
    Delta <- Z %*% S0 %*% t(Z) + V

    return(list(x0 = x0, K = K,  S0 = S0, Delta=Delta, Ind=ind, rob0=rob1, DeltaY = rst))
}

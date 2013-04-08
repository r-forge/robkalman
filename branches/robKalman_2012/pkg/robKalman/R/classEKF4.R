#######################################################
## 
##  classical extended Kalman filter routines, S4
##  author: Bernhard Spangl
##  version: 0.1 (changed: 2013-04-07, created: 2013-04-07)
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



cEKFpredS <- function (t,
                       PredOrFilt,
                       stateEq,
                       controlPred = NULL, ...)
{
    ##  t ... time index
    ##  PredOrFilt ... object of S4 class 'SSPredOrFilt'
    ##  stateEq ... object of S4 class 'SSstateEq'
    ##  controlPred ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    

    SSPredicted <- new("SSPredicted",
                       values = x1,
                       call = call,
                       variance = S1,
                       uExo = ,
                       wExo = wOld,
                       dots.propagated = dots.propagated,
                       control = controlPred,
                       diagnostics = new("SSDiagnosticFilter"))
    return(SSPredicted)
}

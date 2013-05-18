#######################################################
## 
##  classical extended Kalman filter routines, S4
##  author: Bernhard Spangl
##  version: 0.3 (changed: 2013-05-08, created: 2013-04-07)
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

new("FunctionWithControl", 
cEKFinitS <- function (initEq,
                       controlInit = NULL, ...)
{
    ##  initEq ... object of S4 class 'SSinitEq'
    ##  controlInit ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    x0 <- initEq@a0
    S0 <- initEq@Sigma0
    
    SSInitialized <- new("SSInitialized",
                         values = x0,
                         call = call,
                         variance = S0,
                         uExo = NULL,
                         wExo = NULL,
                         dots.propagated = dots.propagated,
                         control = controlInit,
                         diagnostics = new("SSDiagnosticFilter"))
    return(SSInitialized)
}
)

new("FunctionWithControl", 
cEKFpredS <- function (i, t,
                       PredOrFilt,
                       stateEq,
                       controlPred = NULL, ...)
{
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  PredOrFilt ... object of S4 class 'SSPredOrFilt'
    ##  stateEq ... object of S4 class 'SSstateEq'
    ##  controlPred ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    x0 <- PredOrFilt@values
    S0 <- PredOrFilt@variance
    uExo <- PredOrFilt@uExo
    wExo <- PredOrFilt@wExo
    Ffct <- stateEq@Ffct
    Qfct <- stateEq@Qfct
    uExofct <- stateEq@uExofct
    if (is.null(uExofct)) uExofct <- createuExo(0)

    Freturn <- Ffct(i=i, t=t, x0=x0,
                    uFct=uExofct, uOld=uExo, wNew=wExo)
    x1 <- Freturn@x1
    A <- Freturn@FJcb
    B <- Freturn@RJcb
    uNew <- Freturn@uNew

    Qreturn <- Qfct(i=i, t=t, x0=x0)
    Q <- Qreturn@Q

    S1 <- .getpredCov(S0=S0, A=A, B=B, Q=Q)

    SSPredicted <- new("SSPredicted",
                       values = x1,
                       call = call,
                       variance = S1,
                       uExo = uNew,
                       wExo = wExo,
                       dots.propagated = dots.propagated,
                       control = controlPred,
                       diagnostics = new("SSDiagnosticFilter"))
    return(SSPredicted)
}
)

new("FunctionWithControl", 
cEKFcorrS <- function (i, t,
                       Obs,
                       PredOrFilt,
                       obsEq,
                       controlCorr = NULL, ...)
{
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  Obs ... object of S4 class 'SSObs'
    ##  PredOrFilt ... object of S4 class 'SSPredOrFilt'
    ##  obsEq ... object of S4 class 'SSobsEq'
    ##  controlCorr ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    y <- Obs@Y[, i]
    x1 <- PredOrFilt@values
    S1 <- PredOrFilt@variance
    uExo <- PredOrFilt@uExo
    wExo <- PredOrFilt@wExo
    Zfct <- obsEq@Zfct
    Vfct <- obsEq@Vfct
    wExofct <- obsEq@wExofct
    if (is.null(wExofct)) wExofct <- createwExo(0)

    Zreturn <- Zfct(i=i, t=t, x1=x1, y=y, 
                    wFct=wExofct, uNew=uExo, wOld=wExo)
    yhat <- Zreturn@y
    C <- Zreturn@ZJcb
    D <- Zreturn@TJcb
    wNew <- Zreturn@wNew

    Vreturn <- Vfct(i=i, t=t, x1=x1)
    V <- Vreturn@V

    Delta <- .getDelta(S1=S1, C=C, D=D, V=V)
    K <- .getKG(S1=S1, Z=C, Delta=Delta)
    DeltaY <- y - yhat 

    x0 <- x1 + K %*% DeltaY
    S0 <- .getcorrCov(S1=S1, K=K, Z=C)

    SSFiltered <- new("SSFiltered",
                      values = x0,
                      call = call,
                      variance = S0,
                      uExo = uExo,
                      wExo = wNew,
                      KalmanGain = K,
                      CovObs = Delta,
                      DeltaY = DeltaY,
                      dots.propagated = dots.propagated,
                      control = controlCorr,
                      diagnostics = new("SSDiagnosticFilter"))
    return(SSFiltered)
}
)


new("FunctionWithControl",
initSim <- function (initEq,
                       controlInit = NULL, ...)
{
    ##  initEq ... object of S4 class 'SSinitEq'
    ##  controlInit ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    x0 <- initEq@a0
    S0 <- initEq@Sigma0
    x1 <- generateRV(initEq@distrfct, x0, Sigma0) ### auch initEq@iExofct?

    SSInitialized <- new("SSInitialized",
                         values = x1,
                         call = call,
                         variance = S0,
                         uExo = NULL,
                         wExo = NULL,
                         dots.propagated = dots.propagated,
                         crtl.prpgtd = NULL,
                         control = controlInit,
                         diagnostics = new("SSDiagnosticFilter"))
    return(SSInitialized)
}
)

new("FunctionWithControl",
stateSim <- function (i, t,
                       StateSimulated,
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

    x0 <- StateSimulated@values
    S0 <- StateSimulated@variance
    uExo <- StateSimulated@uExo
    wExo <- StateSimulated@wExo
    ctrl.prpgtd <- StateSimulated@ctrl.prpgtd
    Ffct <- stateEq@Ffct
    Qfct <- stateEq@Qfct
    uExofct <- stateEq@uExofct
    if (is.null(uExofct)) uExofct <- createuExo(0)

    Freturn <- Ffct(i=i, t=t, x0=x0,
                    uFct=uExofct, uOld=uExo, wNew=wExo)
    x1 <- Freturn@x1
    uNew <- Freturn@uNew

    Qreturn <- Qfct(i=i, t=t, x0=x0)
    Q <- Qreturn@Q

    innov <- generateRV(stateEq@distrfct, 0*x0, Q)
    x1 <- x1 + innov
    
    SSPredicted <- new("SSStateSimulated",
                       values = x1,
                       call = call,
                       variance = Q,
                       uExo = uNew,
                       wExo = wExo,
                       dots.propagated = dots.propagated,
                       crtl.prpgtd = crtl.prpgtd,
                       control = controlPred,
                       diagnostics = new("SSDiagnosticFilter"))
    return(SSPredicted)
}
)

new("FunctionWithControl",
Ysim <- function (i, t, ydim,
                     StateSimulated,
                     obsEq,
                     controlCorr = NULL, ...)
{
    ##  i ... loop index
    ##  t ... time, t[i]
    ##  Obs ... object of S4 class 'SSObs'
    ##  StateSimulated ... object of S4 class 'SSStateSimulated'
    ##  obsEq ... object of S4 class 'SSobsEq'
    ##  controlCorr ... control parameters, list
    call <- match.call()
    dots.propagated <- list(...)

    y <- numeric(ydim)
    x1 <- StateSimulated@values
    S1 <- StateSimulated@variance
    uExo <- StateSimulated@uExo
    wExo <- StateSimulated@wExo
    ctrl.prpgtd <- StateSimulated@ctrl.prpgtd
    Zfct <- obsEq@Zfct
    Vfct <- obsEq@Vfct
    wExofct <- obsEq@wExofct
    if (is.null(wExofct)) wExofct <- createwExo(0)

    Zreturn <- Zfct(i=i, t=t, x1=x1, y=y,
                    wFct=wExofct, uNew=uExo, wOld=wExo)
    y <- Zreturn@y
    C <- Zreturn@ZJcb
    D <- Zreturn@TJcb
    wNew <- Zreturn@wNew

    Vreturn <- Vfct(i=i, t=t, x1=x1) ### nicht auch von y abhängig??
    V <- Vreturn@V
    eps <-generateRV(obsEq@distrfct, 0*y, V)

    y <- y + eps

    SSFiltered <- new("SSObsSimulated",
                      values = y,
                      call = call,
                      variance = V,
                      uExo = uExo,
                      wExo = wNew,
                      dots.propagated = dots.propagated,
                      crtl.prpgtd = crtl.prpgtd,
                      control = controlCorr,
                      diagnostics = new("SSDiagnosticFilter"))
    return(SSFiltered)
}
)


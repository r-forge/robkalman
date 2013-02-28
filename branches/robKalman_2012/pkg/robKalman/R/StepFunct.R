#######################################################
## 
##  file: StepFunct.R
##        functions to transform original initialization,
##        prediction and correction step functions handling
##        only S3 objects into initialization, prediction
##        and correction step functions that are able to
##        handle S4 oblects and will return the corresponding
##        S4 objects
##  author: Bernhard Spangl
##  version: 0.1 (changed: 2013-02-09, created: 2013-02-09)
##
#######################################################

CreateInit <- function (initS, control=list())
{
    ##  initS ... original initialization step function handling S3 objects
    ##            arguments:  a, S, controlInit, dots-arg
    ##            returns:    x0, S0, controlInit
    ##  control ... control argument of step function

    initS <- function (initEq, controlInit=control, ...)
    {
        ##  initEq ... object of S4 class 'SSinitEq'
        ##  controlInit ... control parameters, list
        call <- match.call()
        dots.propagated <- list(...)

        a <- initEq@a0
        S <- initEq@Sigma0

        retInitS <- initS(a=a, S=S, controlInit=controlInit, ...)

        SSInitialized <- new("SSInitialized",
                             values = retInitS$x0,
                             call = call,
                             variance = retInitS$S0,
                             dots.propagated = dots.propagated,
                             control = retInitS$controlInit,
                             diagnostics = list())
        return(SSInitialized)
    }
    return(new("FunctionWithControl", initS))
}

##  CreatePrep <- still TBD!

CreatePred <- function (predS, control=list())
{
    ##  predS ... original prediction step function handling S3 objects
    ##            arguments:  x0, S0, F, Q, i, v, u,
    ##                        controlF, exQ, controlQ, 
    ##                        controlPred, dots-arg
    ##            returns:    x1, S1, controlPred
    ##  control ... control argument of step function

    predS.fct <- function (i, PredOrFilt, statesEq, controlPred=control,
                           whenEvalExo =c("pre"=TRUE,post="TRUE"), ...)
    {
        ##  i ... time index
        ##  PredOrFilt ... object of S4 class 'SSPredOrFilt'
        ##  statesEq ... object of S4 class 'SSstatesEq'
        ##  controlPred ... control parameters, list
        call <- match.call()
        dots.propagated <- list(...)

        x0 <- PredOrFilt@values
        S0 <- PredOrFilt@variance
        F <- statesEq@Ffct
        Q <- statesEq@Qfct
        v <-     # ???
        u <-     # ???
        controlF <-     # ???
        exQ <-     # ???
        controlQ <-     # ???
        
        if(whenEvalExo["pre"]) u <- exofun(...)
        
        retPredS <- predS(x0=x0, S0=S0, F=F, Q=Q, i=i,
                          v=v, u=u, controlF=controlF,
                          exQ=exQ, controlQ=controlQ,
                          controlPred=controlPred, ...)

        if(whenEvalExo["post"]) u <- exofun(...)

        SSPredicted <- new("SSPredicted",
                           values = retPredS$x1,
                           call = call,
                           variance = retPredS$S1,
                           dots.propagated = dots.propagated,
                           control = retInitS$controlPred,
                           diagnostics = list())
        return(SSPredicted)
    }
    return(new("FunctionWithControl", predS.fct))
}

CreateCorr <- function (corrS, control=list())
{
    ##  corrS ... original correction step function handling S3 objects
    ##            arguments:  y, x1, S1, Z, V, i, eps, w,
    ##                        controlZ, exV, controlV, 
    ##                        controlCorr, dots-arg
    ##            returns:    x0, K, S0, Delta, DeltaY, controlCorr
    ##  control ... control argument of step function

    corrS <- function (i, PredOrFilt, obsEq, controlCorr=control, ...)
    {
        ##  i ... time index
        ##  Obs ... object of S4 class 'SSObs'
        ##  PredOrFilt ... object of S4 class 'SSPredOrFilt'
        ##  obsEq ... object of S4 class 'SSobsEq'
        ##  controlCorr ... control parameters, list
        call <- match.call()
        dots.propagated <- list(...)
        
        y <- Obs@Y
        x1 <- PredOrFilt@values
        S1 <- PredOrFilt@variance
        Z <- obsEq@Zfct
        V <- obsEq@Vfct
        eps <-     # ???
        w <-     # ???
        controlZ <-     # ???
        exV <-     # ???
        controlV <-     # ???
        
        retCorrS <- corrS(y=y, x1=x1, S1=S1, Z=Z, V=V, i=i,
                          eps=eps, w=w, controlZ=controlZ,
                          exV=exV, controlV=controlV,
                          controlCorr=controlCorr, ...)

        SSFiltered <- new("SSFiltered",
                          values = retCorrS$x0,
                          call = call,
                          variance = retCorrS$S0,
                          dots.propagated = dots.propagated,
                          control = retCorrS$controlCorr,
                          diagnostics = list(),
                          KalmanGain = retCorrS$K,
                          CovObs = retCorrS$Delta, 
                          DeltaY = retCorrS$DeltaY)
        return(SSFiltered)
    }
    return(new("FunctionWithControl", PredS))
}

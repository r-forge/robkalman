#######################################################
## 
##  simulating data from SSM, S4
##  author: Peter Ruckdeschel
##  version: 0.1 (created: 2013-08-02)
##
#######################################################

### S4-method: input distrib of type OptionalDistribution

if(!isGeneric("generateRV"))
    setGeneric("generateRV", function(distrib,...) standardGeneric("generateRV"))

setMethod("generateRV", "distribution", function(distrib,...){
   r(distrib)(1)
})

setMethod("generateRV", "function", function(distrib,mu,Sigma){
   distrib(mu,Sigma)
})

setMethod("generateRV", "NULL", function(distrib,mu,Sigma){
   rmvnorm(1, mean=mu,sigma=Sigma)
})


simSSM <- function (Model, times, seed = NULL, ...)
{
     ##  Model ... object of S4 class 'SSM'
     ##  Obs ... object of S4 class 'SSObs'
     ##  times ... object of S4 class 'SStimes'
     ##  Steps ... object of S4 class 'SSFilterOrSmoother'

     if(!is.null(seed)) set.seed(seed)
     
     call <- match.call()
     dots.propagated <- list(...)

     ##  unwrapping:
     initEq <- Model@initEq
     stateEq <- Model@stateEq
     obsEq <- Model@obsEq

     ##  time management:
     tt <- times@times
     inX <- times@inX
     tY <- tt[inX]
     loopIndex <- 1:length(tt)

     withuExo <- !is.null(stateEq@uExofct)
     withwExo <- !is.null(obsEq@wExofct)
     withdots.prop <- (length(dots.propagated)>0)

     ### noch herauszufinden: woher findet man raus,
     ##       ob der prepstep/predstep/corrstep control/Diagnostic hat..

     withcontrol <- withDiagnostic <- FALSE


     ##  initialization of resulting objects:
     X <- initSSPredOrFiltRet(Model@pdim, length(tt)+1,  Model@pdim, length(tt)+1,
                              Model@qdim, length(tY),
                              withuExo, withwExo, withdots.prop,
                              withcontrol, withDiagnostic)
     Y <- initSSPredOrFiltRet(Model@qdim, length(tY)+1,  Model@pdim, length(tt)+1,
                              Model@qdim, length(tY),
                              withuExo, withwExo, withdots.prop,
                              withcontrol, withDiagnostic)


     StateSimulated <- initSim(initEq, controlInit = NULL, ...)
     for(ix in loopIndex+1){
         ##  state simulation
             StateSimulated <- stateSim(i=ix, t=tt[ix],
                                         StateSimulated=StateSimulated,
                                         stateEq=stateEq, ...)
             X <- updateSSPredOrFilt(X, StateSimulated, ix)

         ##  correction:
         if(inX[ix]){ ## have an observation available
            iy <- iy + 1
            ObsSimulated <- obsSim(i=iy, t=tt[ix], ydim = Model@qdim,
                                   StateSimulated = StateSimulated,
                                   obsEq=obsEq, ...)
            Y <- updateSSPredOrFilt(Y, ObsSimulated, iy)
         }
    }
    Simulated <- new("SSSimulated", stateSimulated=X, obsSimulated=Y)
    ### fehlt noch: zusammenführen der Daten....
}
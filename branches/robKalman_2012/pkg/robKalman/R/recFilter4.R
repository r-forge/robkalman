#######################################################
## 
##  recursive filter algorithm for Kalman filter routines, S4
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2013-07-16, created: 2013-05-08)
##
#######################################################

recFilter <- function (Model,
                       Obs,
                       times,
                       Steps,
                       ...)
{
     ##  Model ... object of S4 class 'SSM'
     ##  Obs ... object of S4 class 'SSObs'
     ##  times ... object of S4 class 'SStimes'
     ##  Steps ... object of S4 class 'SSFilterOrSmoother'
     call <- match.call()
     dots.propagated <- list(...)

     ##  unwrapping:
     initEq <- Model@initEq
     stateEq <- Model@stateEq
     obsEq <- Model@obsEq

     ##  time management:
     tt <- times@times
     inX <- times@inX
     loopIndex <- 1:length(tt)

     nrSteps <- length(Steps)

     ##  initialization of resulting objects:
     ini <- vector("list", nrSteps)
     ps <- vector("list", nrSteps)
     cs <- vector("list", nrSteps)
     iniRet <- vector("list", nrSteps)
     psRet <- vector("list", nrSteps)
     csRet <- vector("list", nrSteps)

     ##  initialization:
     for (iStep in 1:nrStep) {
         ini[[iStep]] <- Steps[[iStep]]@initStep(initEq=initEq, ...)
     }
     ### ab hier weiter wie oben!, 2013-07-16 ###
     ### Schleife über Zeitpunkte t mit entsprechendem Time-Management ###

         ##  preparation: TBD!
         if (prep) {
         }
     
         ##  prediction:
         ps <- predSc(i=i, t=t,
                      PredOrFilt=ini,
                      stateEq=stateEq, ...)

         ##  correction:
         cs <- corrSc(i=i, t=t,
                      Obs=Obs,
                      PredOrFilt=ps,
                      obsEq=obsEq, ...)
     

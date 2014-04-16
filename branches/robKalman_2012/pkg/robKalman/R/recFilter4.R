#######################################################
## 
##  recursive filter algorithm for Kalman filter routines, S4
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2013-07-16, created: 2013-05-08)
##
#######################################################

# _<
## ist erledigt durch initSSPredOrFiltRet() in updateinitSSPredOrFiltRet.R
# _>
#     initPsRet <- function(SSM,tt, exosDim ){
#        ###exosDim ist die Dimensionierung der Rueckgabewerte der exoFct
#
#        if modell has uexo uexos = matrix(...) else uexos = NULL
#        psret0 <- new("SSPredictedRet",
#                              values=matrix(0,Model@pdim,length(tt)+1),
#                              call = vector("list",length(tt)+1 ),
#                              variance = array(0,dim=c(Model@pdim,Model@pdim,length(tt)+1)),
 #                             uexo = uexos,...)
#        return(psret0)
#      }


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
     tT <- length(tt)+1
     tY <- sum(inX)
     loopIndex <- 1:length(tt)

     nrSteps <- length(Steps)

     ##  initialization of resulting objects:
     ps <- vector("list", nrSteps)
     cs <- vector("list", nrSteps)
     psRet <- vector("list", nrSteps)
     csRet <- vector("list", nrSteps)

     withuExo <- !is.null(stateEq@uExofct)
     withwExo <- !is.null(obsEq@wExofct)
     withdots.prop <- (length(dots.propagated)>0)

     ### noch herauszufinden: woher findet man raus,
     ##       ob der prepstep/predstep/corrstep control/Diagnostic hat..

     withcontrol.prep <- ##!is.null(stateEq@uExofct)
     withDiagnostic.prep <- ##!!is.null(stateEq@uExofct)

     withcontrol.pred <- ##!!is.null(stateEq@uExofct)
     withDiagnostic.pred <- ##!!is.null(stateEq@uExofct)

     withcontrol.corr <- ##!!is.null(stateEq@uExofct)
     withDiagnostic.corr <- ##!!is.null(stateEq@uExofct)

     if (prep) prepret0 <- initSSPredOrFiltRet(Model@pdim, tT,
                              Model@pdim, tT, Model@qdim, tY,
                              withuExo, withwExo, withdots.prop,
                              withcontrol.prep, withDiagnostic.prep)
     psret0 <- <- initSSPredOrFiltRet(Model@pdim, tT,
                              Model@pdim, tT, Model@qdim, tY,
                              withuExo, withwExo, withdots.prop,
                              withcontrol.pred, withDiagnostic.pred)
     csret0 <- <- initSSPredOrFiltRet(Model@pdim, tY,
                              Model@pdim, tT, Model@qdim, tY,
                              withuExo, withwExo, withdots.prop,
                              withcontrol.corr, withDiagnostic.corr)

     for(iStep in 1:nrStep){
         if (prep) prepRet[[iStep]] <- prepret0
         psRet[[iStep]] <- psret0
         csRet[[iStep]] <- csret0
     }
     ##  initialization:     iStep = index within different procedures
     for (iStep in 1:nrStep) {
          cs[[iStep]] <- Steps[[iStep]]@initStep(initEq=initEq, ...)
     }
     iniRet <- cs
     ### ab hier weiter wie oben!, 2013-07-16 ###
     ### Schleife Ã¼ber Zeitpunkte t mit entsprechendem Time-Management ###

     iy <- 0
     for(ix in loopIndex){
         ##  preparation: TBD!
         for (iStep in 1:nrStep) {
              if (!is.null(Steps[[iStep]]@prepStep) {
                  ### to be filled
                  ##preps[[iStep]] <- Steps[[iStep]]@prepStep(i=ix, t=tt[ix],
                  #                                   PredOrFilt=cs[[iStep]],
                  #                                   stateEq=stateEq, ...)
                  #prepsRet[[iStep]] <- updateSSPredOrFilt(prepsRet[[iStep]], preps[[iStep]],
                  #                                 ix)
                  #cs[[iStep]] <- preps[[iStep]]
              }
         }
     
         ##  prediction:
         for (iStep in 1:nrStep) {
              ps[[iStep]] <- Steps[[iStep]]@predStep(i=ix, t=tt[ix],
                                                     PredOrFilt=cs[[iStep]],
                                                     stateEq=stateEq, ...)
              psRet[[iStep]] <- updateSSPredOrFilt(psRet[[iStep]], ps[[iStep]],
                                                   ix)
         }

         ##  correction:
         if(inX[ix]){ ## have an observation available
            iy <- iy + 1
            for (iStep in 1:nrStep) {
                 cs[[iStep]] <- Steps[[iStep]]@corrStep(i=iy, t=tt[ix], Obs=Obs,
                                                        PredOrFilt=ps[[iStep]],
                                                        obsEq=obsEq, ...)
                 csRet[[iStep]] <- updateSSPredOrFilt(csRet[[iStep]],
                                                         cs[[iStep]], iy)
            }
         }else{
            cs <- ps
         }
    }
    zusammenhängen von list(initRet, prepRet, psRet, csRet) ## SSOutput ist jetzt
       eine Liste mit 4 Elementen; jedes einzelne der 4 ist wieder Liste
       und enthält zB ideal robust -> unterschied zu Output.dia
    
 }
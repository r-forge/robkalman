#######################################################
## 
##  recursive filter algorithm for Kalman filter routines, S4
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2013-07-16, created: 2013-05-08)
##
#######################################################


     initPsRet <- function(SSM,tt, exos){

        if modell has uexo uexos = matrix(...) else uexos = NULL
        psret0 <- new("SSPredictedRet",
                              values=matrix(0,Model@pdim,length(tt)+1),
                              call = vector("list",length(tt)+1 ),
                              variance = array(0,dim=c(Model@pdim,Model@pdim,length(tt)+1)),
                              uexo = uexos,...)
        return(psret0)
      }

## analog initPrepRet initCsRet


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
     ps <- vector("list", nrSteps)
     cs <- vector("list", nrSteps)
     psRet <- vector("list", nrSteps)
     csRet <- vector("list", nrSteps)

     if (prep) prepret0 <- initPrepRet(SSM,tt,exox...)
     psret0 <- initPsRet(SSM,tt,exox...)
     csret0 <- initCsRet(SSM,tt,exox...)

     for(i in nrSteps){
           # if exists prepRet ..
           #     prepRet0 <- new("SSPreparedRet",values=matrix)

          ### aus diesem Code eine Funktion machen
          if (prep) prepRet[[i]] <- prepret0
          psRet[[i]] <- psret0
          csRet[[i]] <- csret0
     }
     for(iStep in 1:nrStep){
         psRet[[iStep]] <- initSSPredOrFiltRet(pdim = Model@pdim,
                                   qdim = Model@qdim,
                                   tfdim = sum(inX), tpdim = length(tt),
                                   withuExo=!is.null(Model@SSstateEq@uExofct,
                                   withwExo=!is.null(Model@SSobsEq@wExofct,
                                   withdots.prop=(length(dots.propagated)>0),
                                   withcontrol=(length(control)>0), ##?
                                   withDiagnosticFilter=TRUE  ##?
                                   )
         csRet[[iStep]] <- initSSPredOrFiltRet(pdim = Model@pdim,
                                   qdim = Model@qdim,
                                   tfdim = sum(inX), tpdim = length(tt),
                                   withuExo=!is.null(Model@SSstateEq@uExofct,
                                   withwExo=!is.null(Model@SSobsEq@wExofct,
                                   withdots.prop=(length(dots.propagated)>0),
                                   withcontrol=(length(control)>0), ##?
                                   withDiagnosticFilter=TRUE  ##?
                                   )

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
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


recSmoother <- function (Model,
                         Obs,
                         times,
                         Steps,
                         FilterOutput,
						 ...)
{
     ##  Model ... object of S4 class 'SSM'
     ##  Obs ... object of S4 class 'SSObs'
     ##  times ... object of S4 class 'SStimes'
     ##  Steps ... object of S4 class 'SSFilterOrSmoother'
	 ##  FilterOutput ... object of S4 class 'SSOutput'
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
     ss <- vector("list", nrSteps)
     ssRet <- vector("list", nrSteps)

     withuExo <- !is.null(stateEq@uExofct)
     withwExo <- !is.null(obsEq@wExofct)
     withdots.prop <- (length(dots.propagated)>0)

     ### noch herauszufinden: woher findet man raus,
     ##       ob der prepstep/predstep/corrstep control/Diagnostic hat..

     withcontrol.smooth <- ##!is.null(stateEq@uExofct)
     withDiagnostic.smooth <- ##!!is.null(stateEq@uExofct)

     smoothret0 <- initSSPredOrFiltRet(Model@pdim, tY,
                              Model@pdim, tT, Model@qdim, tY,
                              withuExo, withwExo, withdots.prop,
                              withcontrol.smooth, withDiagnostic.smooth,
							  smooth = TRUE)

     for(iStep in 1:nrStep) smoothRet[[iStep]] <- smoothret0

     ### Schleife ueber Zeitpunkte t mit entsprechendem Time-Management ###

     iy <- 0
     for(ix in loopIndex){
         ##  correction:
         if(inX[ix]){ ## have an observation available
            iy <- iy + 1
            for (iStep in 1:nrStep) {
                 ss[[iStep]] <- Steps[[iStep]]@smoothStep(i=iy, t=tt[ix], Obs=Obs,
                                                          PredOrFilt=ps[[iStep]],
                                                          smoothEq=smoothEq, ...)
                 ssRet[[iStep]] <- updateSSPredOrFilt(csRet[[iStep]],
                                                         cs[[iStep]], iy, smooth = TRUE)
            }
         }else{
            cs <- ps
         }
    }
    zusammenhängen von list(initRet, prepRet, psRet, csRet) ## SSOutput ist jetzt
       eine Liste mit 4 Elementen; jedes einzelne der 4 ist wieder Liste
       und enthält zB ideal robust -> unterschied zu Output.dia
    
 }
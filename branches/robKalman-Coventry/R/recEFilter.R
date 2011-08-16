#######################################################
## 
##  recursive filter algorithm for Kalman filter routines
##  author: Bernhard Spangl  & Peter Ruckdeschel
##  version: 0.3 (changed: 2011-08-16, created: 2011-06-21)
##
#######################################################

recEFilter <- function(obs, model, proc)
#
#recEFilter <- function (Y, a, S, F, Q, Z, V, 
#%              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
#              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
#              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
#              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
#              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
#              initSc=.cEKFinitstep, predSc=.cEKFpredstep, corrSc=.cEKFcorrstep,
#              initSr=NULL, predSr=NULL, corrSr=NULL,
#              controlc=NULL, controlr=NULL, 
#              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
#              ...)
{
##  a generalization of the extended Kalman filter, no vectorization!
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm
##  +  u, w            : exogenous variables, ?? x tt matrix
##  +  mu.v, mu.eps          : expectation of innivations and observation noise
##  +  R, T            : selection matrices of innovations and observation 
##                       noise
##  +  exQ, exV        : exogenous variables of Q and V
##  +  initSc, predSc, corrSc: (classical) initialization-, prediction-, and 
##                             correction-step function
##  +  initSr, predSr, corrSr: (robust) initialization-, prediction-, and 
##                             correction-step function
##  +  control_        : control paramaters, list
##  +  ...: additional arguments for initS, predS, corrS

#    if (!(is.function(F))) createF(F=F, R=R)
#    if (!(is.function(Z))) createZ(Z=Z, T=T)
#    if (!(is.function(Q))) createQ(Q=Q)
#    if (!(is.function(V))) createV(V=V)

    
    pd <- length(model$Start$a0)
    qd <- (dim(obs$y))[1]
    tt.y <- length(obs$timestamps.y)
    tt.x <- length(obs$timestamps.x)-1

    robust <- !(is.null(proc$robust$init) && is.null(proc$robust$pred) && 
                is.null(proc$robust$corr))

    Xf  <- array(0, dim = c(pd, tt.x + 1))
    Xp  <- array(0, dim = c(pd, tt.x))
    DeltaY  <- array(0, dim = c(qd, tt.x))
    St0 <- array(0, dim = c(pd, pd, tt.x + 1))
    St1 <- array(0, dim = c(pd, pd, tt.x))
    KG  <- array(0, dim = c(pd, qd, tt.x))
    Delta  <- array(0, dim = c(qd, qd, tt.x))

    if (robust) {
        Xrf <- array(0, dim = c(pd, tt.x + 1))
        Xrp <- array(0, dim = c(pd, tt.x))
        DeltaYr  <- array(0, dim = c(qd, tt.x))
        Str0 <- array(0, dim = c(pd, pd, tt.x + 1))
        Str1 <- array(0, dim = c(pd, pd, tt.x))
        KGr  <- array(0, dim = c(pd, qd, tt.x))
        Deltar  <- array(0, dim = c(qd, qd, tt.x))
    }

    ##  initialization

    ini <- proc$classic$init$fct(model$Start$a0, model$Start$Sigma0, 
                                 proc$classic$init$control, t = tt.x[1], ...)
    x0 <- ini$x0
    S0 <- ini$S0
    controlc <- ini$control
   
    Xf[, 1] <- ini$x0
    St0[, , 1] <- ini$S0
   
    if (robust) {
        if (!is.null(proc$robust$init)) {
            inir <- proc$robust$init$fct(model$Start$a0, model$Start$Sigma0, 
                                 proc$robust$init$control, t = tt.x[1], ...)
            xr0 <- inir$x0
            Sr0 <- inir$S0
            controlr <- inir$control
        } else {
            xr0 <- x0
            Sr0 <- S0
            controlr <- controlc
        }
        Xrf[, 1] <- xr0
        Str0[, , 1] <- Sr0
    } else {
        Xrf <- NULL
        Xrp <- NULL
        DeltaYr <- NULL
        Str0 <- NULL
        Str1 <- NULL
        KGr <- NULL
        Deltar <- NULL
    }
    j <- 1
    for (i in (1:tt.x)) {

        
        
        ##  prediction
        ps <- proc$classic$pred$fct(x0 = x0, S0 = S0, stateEq=model$StateEq, 
                                    mu.v=mu.v[,i], t = tt.x[i+1], i = i,
                                    additinfofromCorr = controlc, ...)
        x1 <- ps$x1
        S1 <- ps$S1
        controlc <- ps$additinfofromPred
  
        Xp[, i] <- x1
        St1[, , i] <- S1
  
        if (robust) {
            if (!is.null(proc$robust$pred)) {
                psr <- proc$robust$pred$fct(x0 = xr0, S0 = Sr0, stateEq=model$StateEq, 
                                    mu.v=mu.v[,i], t = tt.x[i+1], i = i,
                                    additinfofromCorr = controlr, ...)
            } else {
                psr <- proc$classic$pred$fct(x0 = xr0, S0 = Sr0, stateEq=model$StateEq, 
                                    mu.v=mu.v[,i], t = tt.x[i+1], i = i,
                                    additinfofromCorr = controlr, ...)
            }
            xr1 <- psr$x1
            Sr1 <- psr$S1
            controlr <- psr$controlPred
  
            Xrp[, i] <- xr1
            Str1[, , i] <- Sr1
        }

        if (tt.x[i+1] %in% tt.y){
        ##  correction
        cs <- proc$classic$corr(y = Y0[,j], x1 = x1, S1 = S1, obsEq=model$ObsEq,
                     mu.eps = mu.eps[, i], t = tt.x[i+1], i = i, 
                     additinfofromPred = controlc, ...)
        x0 <- cs$x0
        S0 <- cs$S0
        controlc <- cs$additinfofromCorr
  
        Xf[, i + 1] <- x0
        St0[, , i + 1] <- S0
        KG[, , i] <- cs$K
        Delta[, , i] <- cs$Delta
        DeltaY[, i] <- cs$DeltaY
  
        if (robust) {
            if (!is.null(proc$robust$corr)) {
                csr <- proc$robust$corr(y = Y0[,j], x1 = xr1, S1 = Sr1, obsEq=model$ObsEq,
                     mu.eps = mu.eps[, i], t = tt.x[i+1], i = i, 
                     additinfofromPred = controlr, ...)
            } else {
                csr <- proc$classic$corr(y = Y0[,j], x1 = xr1, S1 = Sr1, obsEq=model$ObsEq,
                     mu.eps = mu.eps[, i], t = tt.x[i+1], i = i, 
                     additinfofromPred = controlr, ...)
            }
            xr0 <- csr$x0
            Sr0 <- csr$S0
            controlr <- csr$controlCorr
  
            Xrf[, i + 1] <- xr0
            Str0[, , i + 1] <- Sr0
            KGr[, , i] <- csr$K
            Deltar[, , i] <- csr$Delta
            DeltaYr[, i] <- csr$DeltaY
        }
    j <- j + 1
    }
    }
    return(list(Xf = Xf, Xp = Xp, Xrf = Xrf, Xrp = Xrp,
                S0 = St0, S1 = St1, KG = KG, Delta = Delta,
                DeltaY = DeltaY,
                Sr0 = Str0, Sr1 = Str1, KGr = KGr, Deltar = Deltar,
                DeltaYr = DeltaYr))

}


#######################################################
##  simple wrappers:
#######################################################

ExtendedKF <- function (Y, a, S, F, Q, Z, V, 
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recEFilter(Y, a, S, F, Q, Z, V, 
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV)

}

UnscentedKF <- function (Y, a, S, F, Q, Z, V,  
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              controlc=list(alpha=0.0001, beta=2, kappa=length(a)-3), 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recEFilter(Y, a, S, F, Q, Z, V, 
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               initSc=.cUKFinitstep, predSc=.cUKFpredstep, 
               corrSc=.cUKFcorrstep, 
               controlc=controlc, 
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV)

}



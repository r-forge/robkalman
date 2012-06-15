#######################################################
## 
##  recursive filter algorithm for Kalman filter routines
##  author: Bernhard Spangl
##  version: 1.1 (changed: 2012-03-26, created: 2011-06-21)
##
#######################################################

recFilter <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              initSc=.cinitstep, prepSc=NULL, predSc=.cpredstep, corrSc=.ccorrstep,
              prepSr= NULL, initSr=NULL, predSr=NULL, corrSr=NULL,
              controlc=NULL, controlr=NULL, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
##            unscented=FALSE, 
              ...)
{
##  a generalization of the extended Kalman filter, no vectorization!
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm
##  +  u, w            : exogenous variables, ?? x tt matrix
##  +  v, eps          : expectation of innivations and observation noise
##  +  R, T            : selection matrices of innovations and observation 
##                       noise
##  +  exQ, exV        : exogenous variables of Q and V
##  +  initSc, predSc, corrSc: (classical) initialization-, prediction-, and 
##                             correction-step function
##  +  initSr, predSr, corrSr: (robust) initialization-, prediction-, and 
##                             correction-step function
##  +  control_        : control paramaters, list
##  +  unscented       : logical (default=FALSE), should sigma points be
##                       calculated?
##  +  ...: additional arguments for initS, predS, corrS

    if (!(is.function(F))) F <- createF(F=F, R=R)
    if (!(is.function(Z))) Z <- createZ(Z=Z, T=T)
    if (!(is.function(Q))) Q <- createQ(Q=Q)
    if (!(is.function(V))) V <- createV(V=V)
    
    V0 <- V(t=1, x1=a, exV=exV, control=controlV)$V
    
    
    pd <- length(a)
    qd <- length(V0)^.5
    tt <- if(qd==1) length(Y) else (dim(Y))[2]
    if(qd==1) Y <- matrix(Y,nrow=1,ncol=tt)

    robust <- !(is.null(initSr) && is.null(predSr) && is.null(corrSr))

    Xf  <- array(0, dim = c(pd, tt + 1))
    Xp  <- array(0, dim = c(pd, tt))
    DeltaY  <- array(0, dim = c(qd, tt))
    St0 <- array(0, dim = c(pd, pd, tt + 1))
    St1 <- array(0, dim = c(pd, pd, tt))
    KG  <- array(0, dim = c(pd, qd, tt))
    Delta  <- array(0, dim = c(qd, qd, tt))
 
    if (robust) {
        Xrf <- array(0, dim = c(pd, tt + 1))
        Xrp <- array(0, dim = c(pd, tt))
        DeltaYr  <- array(0, dim = c(qd, tt))
        Str0 <- array(0, dim = c(pd, pd, tt + 1))
        Str1 <- array(0, dim = c(pd, pd, tt))
        KGr  <- array(0, dim = c(pd, qd, tt))
        Deltar  <- array(0, dim = c(qd, qd, tt))
    }

    ##  initialization

    ini <- initSc(a, S, controlc, i = 0, ...)
    x0 <- ini$x0
    S0 <- ini$S0
    controlc <- ini$controlInit
    Xf[, 1] <- ini$x0
    St0[, , 1] <- ini$S0
   
    if (robust) {
        if (!is.null(initSr)) {
            inir <- initSr(a, S, controlr, i = 0, ...)
        } else {
            inir <- initSc(a, S, controlr, i = 0, ...)
        }
        xr0 <- inir$x0
        Sr0 <- inir$S0
        controlr <- inir$controlInit
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

    for (i in (1:tt)) {

##          ##  calculation of sigma points
##          if (!is.null(prepSr)) {
##              controlc <- .SigmaPointsCalc(x0=x0, S0=S0, Q=Q, V=V, i=i, 
##                                           v=v[, i], eps=eps[, i], 
##                                           exQ=exQ[, i], controlQ=controlQ, 
##                                           exV=exV[, i], controlV=controlV, 
##                                           controlSigma=controlc, ...) 
##              if (robust) {
##                  controlr <- .SigmaPointsCalc(x0=xr0, S0=Sr0, Q=Q, V=V, i=i, 
##                                               v=v[, i], eps=eps[, i], 
##                                               exQ=exQ[, i], controlQ=controlQ, 
##                                               exV=exV[, i], controlV=controlV, 
##                                               controlSigma=controlr, ...) 
##  ##              controlr$X0x <- controlc$X0x 
##  ##              controlr$Xv <- controlc$Xv 
##  ##              controlr$Xe <- controlc$Xe 
##              }
##          }
        v0 <- v[, i]
        u0 <-  u[, i]
        exQ0  <-  if(!is.null(exQ)) exQ[, i] else NULL

        ##  prediction
        ps <- predSc(x0 = x0, S0 = S0, F = F, Q = Q, i = i, 
                     v = v0, u = u0, controlF = controlF, 
                     exQ = exQ0,    # also works, if exQ=NULL 
                     controlQ = controlQ, 
                     controlPred = controlc, ...)
        x1 <- ps$x1
        S1 <- ps$S1
#        print(c(S1=S1))
        controlc <- ps$controlPred
  
        Xp[, i] <- x1
        St1[, , i] <- S1
  
        if (robust) {
            if (!is.null(predSr)) {
                psr <- predSr(x0 = xr0, S0 = Sr0, F = F, Q = Q, i = i, 
                              v = v0, u = u0, controlF = controlF, 
                              exQ = exQ0,    # also works, if exQ=NULL 
                              controlQ = controlQ, 
                              controlPred = controlr, ...)
            } else {
                psr <- predSc(x0 = xr0, S0 = Sr0, F = F, Q = Q, i = i, 
                              v = v0, u = u0, controlF = controlF, 
                              exQ = exQ0,    # also works, if exQ=NULL 
                              controlQ = controlQ, 
                              controlPred = controlr, ...)
            }
            xr1 <- psr$x1
            Sr1 <- psr$S1
            controlr <- psr$controlPred
  
            Xrp[, i] <- xr1
            Str1[, , i] <- Sr1
        }

#########################################################################
## here Y-time-steps if i in J
#########################################################################

        ##  correction
        Y0 <- Y[, i]
        eps0 <- eps[, i]
        exV0  <-  if(!is.null(exV)) exV[, i] else NULL
        w0  <-  w[, i]
        cs <- corrSc(y = Y0, x1 = x1, S1 = S1, Z = Z, V = V, i = i, 
                     eps = eps0, w = w0, controlZ = controlZ, 
                     exV = exV0, controlV = controlV, 
                     controlCorr = controlc, ...)
        x0 <- cs$x0
        S0 <- cs$S0
#        print(c(S0=S0))
        controlc <- cs$controlCorr
  
        Xf[, i + 1] <- x0
        St0[, , i + 1] <- S0
        KG[, , i] <- cs$K
        Delta[, , i] <- cs$Delta
        DeltaY[, i] <- cs$DeltaY
  
        if (robust) {
            if (!is.null(corrSr)) {
                csr <- corrSr(y = Y0, x1 = xr1, S1 = Sr1, Z = Z, V = V, i = i, 
                              eps = eps0, w = w0, controlZ = controlZ, 
                              exV = exV0, controlV = controlV, 
                              controlCorr = controlr, ...)
            } else {
                csr <- corrSc(y = Y0, x1 = xr1, S1 = Sr1, Z = Z, V = V, i = i, 
                              eps = eps0, w = w0, controlZ = controlZ, 
                              exV = exV0, controlV = controlV, 
                              controlCorr = controlr, ...)
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
    }

    return(list(Xf = Xf, Xp = Xp, Xrf = Xrf, Xrp = Xrp,
                S0 = St0, S1 = St1, KG = KG, Delta = Delta,
                DeltaY = DeltaY,
                Sr0 = Str0, Sr1 = Str1, KGr = KGr, Deltar = Deltar,
                DeltaYr = DeltaYr, controlr= controlr,
                controlc= controlc))

}


#######################################################
##  simple wrappers:
#######################################################

KalmanFilter <- function (Y, a, S, F, Q, Z, V,
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

    recFilter(Y, a, S, F, Q, Z, V,
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV)

}

rLS.AO.Filter <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              controlr=NULL, b=NULL, norm=Euclideannorm)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recFilter(Y, a, S, F, Q, Z, V,
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               initSc=.cinitstep, predSc=.cpredstep,
               corrSc=.ccorrstep,
               initSr=.cinitstep, predSr=.cpredstep,
               corrSr=.rLS.AO.corrstep,
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV, 
               controlr=controlr, b=b, norm=norm)

}

rLS.IO.Filter <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              controlr=NULL, b=NULL, norm=Euclideannorm)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recFilter(Y, a, S, F, Q, Z, V,
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               initSc=.cinitstep, predSc=.cpredstep,
               corrSc=.ccorrstep,
               initSr=.cinitstep, predSr=.cpredstep,
               corrSr=.rLS.IO.corrstep,
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV, 
               controlr=controlr, b=b, norm=norm)

}

UnscentedKF <- function (Y, a, S, F, Q, Z, V,  
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
##            controlc=list(alpha=0.0001, beta=2, kappa=length(a)-3), 
              controlc, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recFilter(Y, a, S, F, Q, Z, V,
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               initSc=.cUKFinitstep, predSc=.cUKFpredstep, 
               corrSc=.cUKFcorrstep, 
               controlc=controlc, 
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV)#, 
##             unscented=TRUE)

}

rLS.AO.UKFilter <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
##            controlc=list(alpha=0.0001, beta=2, kappa=length(a)-3), 
              controlc, controlr=controlc,
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
              b, norm=Euclideannorm)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recFilter(Y, a, S, F, Q, Z, V,
               u=u, w=w, v=v, eps=eps, R=R, T=T, exQ=exQ, exV=exV,
               initSc=.cUKFinitstep, predSc=.cUKFpredstep, 
               corrSc=.cUKFcorrstep, 
               initSr=.cUKFinitstep, predSr=.cUKFpredstep, 
               corrSr=.rLS.AO.UKFcorrstep, 
               controlc=controlc, controlr=controlr, 
               controlF=controlF, controlQ=controlQ, 
               controlZ=controlZ, controlV=controlV, 
##             unscented=TRUE, 
               b=b, norm=norm)

}



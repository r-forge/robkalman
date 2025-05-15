#######################################################
## 
##  recursive filter algorithm for Kalman filter routines
##  author: Bernhard Spangl
##  version: 0.1 (changed: 2011-06-21, created: 2011-06-21)
##
#######################################################

recEFilter <- function (Y, a, S, F, Q, Z, V, 
              u=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              v=matrix(0, nrow=length(a), ncol=ncol(Y)), 
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)), 
              R=NULL, T=NULL, exQ=NULL, exV=NULL, 
              initSc=.cEKFinitstep, predSc=.cEKFpredstep, corrSc=.cEKFcorrstep,
              initSr=NULL, predSr=NULL, corrSr=NULL,
              controlc=NULL, controlr=NULL, 
              controlF=NULL, controlQ=NULL, controlZ=NULL, controlV=NULL, 
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
##  +  ...: additional arguments for initS, predS, corrS

    if (!(is.function(F))) createF(F=F, R=R)
    if (!(is.function(Z))) createZ(Z=Z, T=T)
    if (!(is.function(Q))) createQ(Q=Q)
    if (!(is.function(V))) createV(V=V)

    pd <- length(a)
    qd <- (dim(Y))[1]
    tt <- (dim(Y))[2]

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

    ini <- initSc(a, S, control, i = 0, ...)
    x0 <- ini$x0
    S0 <- ini$S0
    controlc <- ini$controlInit
   
    Xf[, 1] <- ini$x0
    St0[, , 1] <- ini$S0
   
    if (robust) {
        if (!is.null(initSr)) {
            inir <- initSr(a, S, controlr, i = 0, ...)
            xr0 <- inir$x0
            Sr0 <- inir$S0
            controlr <- inir$controlInit 
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

    for (i in (1:tt)) {

        ##  prediction
        ps <- predSc(x0 = x0, S0 = S0, F = F, Q = Q, i = i, 
                     v = [, i], u = u[, i], controlF = controlF, 
                     exQ = exQ[, i],    # also works, if exQ=NULL 
                     controlQ = controlQ, 
                     controlPred = controlc, ...)
        x1 <- ps$x1
        S1 <- ps$S1
        controlc <- ps$controlPred
  
        Xp[, i] <- x1
        St1[, , i] <- S1
  
        if (robust) {
            if (!is.null(predSr)) {
                psr <- predSr(x0 = xr0, S0 = Sr0, F = F, Q = Q, i = i, 
                              v = [, i], u = u[, i], controlF = controlF, 
                              exQ = exQ[, i],    # also works, if exQ=NULL 
                              controlQ = controlQ, 
                              controlPred = controlc, ...)
            } else {
                psr <- predSc(x0 = xr0, S0 = Sr0, F = F, Q = Q, i = i, 
                              v = [, i], u = u[, i], controlF = controlF, 
                              exQ = exQ[, i],    # also works, if exQ=NULL 
                              controlQ = controlQ, 
                              controlPred = controlc, ...)
            }
            xr1 <- psr$x1
            Sr1 <- psr$S1
            controlr <- psr$controlPred
  
            Xrp[, i] <- xr1
            Str1[, , i] <- Sr1
        }

        ##  correction
#       Z0 <- matrix(Z[, , i], nrow = qd, ncol = pd)
        V0 <- matrix(V[, , i], nrow = qd, ncol = qd)
        Y0 <- Y[, i]
        if (is.null(C)) {    # case 'C=NULL': calculate Jacobian
            C0 <- C
        } else {
            if (is.array(C)) {    # case 'C is array': use corresponding matrix
                C0 <- matrix(C[, , i], nrow = qd, ncol = pd)
            } else {    # case 'C is function': evaluate at x_{t|t-1}
                C0 <- C(x=x1, t=i)
            }
        }

        cs <- corrSc(y = Y0, x1 = x1, S1 = S1, Z = Z, V = V, i = i, 
                     eps = eps[, i], w = w[, i], controlZ = controlZ, 
                     exV = exV[, i], controlV = controlV, 
                     controlCorr = controlc, ...)
        x0 <- cs$x0
        S0 <- cs$S0
        controlc <- cs$controlCorr
  
        Xf[, i + 1] <- x0
        St0[, , i + 1] <- S0
        KG[, , i] <- cs$K
        Delta[, , i] <- cs$Delta
        DeltaY[, i] <- cs$DeltaY
  
        if (robust) {
            if (!is.null(corrSr)) {
                csr <- corrSr(y = Y0, x1 = x1, S1 = S1, Z = Z, V = V, i = i, 
                              eps = eps[, i], w = w[, i], controlZ = controlZ, 
                              exV = exV[, i], controlV = controlV, 
                              controlCorr = controlc, ...)
            } else {
                csr <- corrSc(y = Y0, x1 = x1, S1 = S1, Z = Z, V = V, i = i, 
                              eps = eps[, i], w = w[, i], controlZ = controlZ, 
                              exV = exV[, i], controlV = controlV, 
                              controlCorr = controlc, ...)
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
                DeltaYr = DeltaYr))

}


#######################################################
##  simple wrappers:
#######################################################

ExtendedKF <- function (Y, a, S, F, Q, Z, V)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, S, F, Q, Z, V: Hyper-parameters of the ssm

    recEFilter(Y, a, S, F, Q, Z, V)

}



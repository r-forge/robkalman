recursiveFilter.old <- function(Y, a, S, F, Q, Z, V,
                   initSc = .cKinitstep, predSc = .cKpredstep,
                   corrSc = .cKcorrstep,
                   initSr = NULL, predSr = NULL, corrSr = NULL,
                   nsim = 0, seed = NULL, ..., dropRuns = TRUE)
                   # a generalization of the Kalmanfilter
#arguments:
# +  Y               :observations in an array with dimensions qd x runs x tt
#                    :for backward compatibility: or a vector /a matrix in
#                     dimensions qd x tt
# +  a, S, F, Q, Z, V: Hyper-parameters of the ssm
# +  initSc, predSc, corrSc:  (classical) initialization-, prediction-, and correction-step function
# +  initSr, predSr, corrSr:  (robust) initialization-, prediction-, and correction-step function
# +  robustIO: if TRUE indicators are recorded whether prediction step does clipping
# +  robustAO: if TRUE indicators are recorded whether correction step does clipping
# +  nsim: if >0 we simulate a bunch of nsim paths (acc. to ideal model) to get emp. covariances
# +  seed: seed for the simulations
# +  ... additional arguments for initS, predS, corrS
# +  dropRuns: shall run-dimension be collapsed if it is one?
{
 qd <- ifelse(length(Z)==1, 1, (dim(Y))[1])
 ########################
 # for backward compatibility
 if (!is.array(Y)){
      Y0 <- Y
      tt <- ifelse(length(Z)==1, length(Y), (dim(Y))[2])
      Y <- aperm(array(Y, dim = c(qd,tt,1)),c(1,3,2))
 }
 ########################

 tt <- (dim(Y))[3]
 runs <- (dim(Y))[2]

 pd <- length(a)
 IndIO <- NULL
 IndAO <- NULL
 St0s <- St1s <- NULL
 DeltaYr <- NULL
 Deltar <- NULL

 if(pd==1) F <- array(F, dim = c(pd,pd,tt))
 if(pd==1 && qd==1) Z <- array(Z, dim = c(pd,qd,tt))
 if(pd==1) Q <- array(Q, dim = c(pd,pd,tt))
 if(qd==1) V <- array(V, dim = c(qd,qd,tt))
 if(is.matrix(F)) F <- array(F, dim = c(pd,pd,tt))
 if(is.matrix(Z)) Z <- array(Z, dim = c(pd,qd,tt))
 if(is.matrix(Q)) Q <- array(Q, dim = c(pd,pd,tt))
 if(is.matrix(V)) V <- array(V, dim = c(qd,qd,tt))

 robust <- !(is.null(initSr)&&is.null(predSr)&&is.null(corrSr))

 Xf  <- array(0, dim = c(pd, runs, tt + 1))
 Xp  <- array(0, dim = c(pd, runs, tt))
 DeltaY  <- array(0, dim = c(qd, runs, tt))
 St0 <- array(0, dim = c(pd, pd, tt + 1))
 St1 <- array(0, dim = c(pd, pd, tt))
 KG  <- array(0, dim = c(pd, qd, tt))
 Delta  <- array(0, dim = c(qd, qd, tt))

 if (nsim)
   {if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)
    if (is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
        }
   }else{
    RNGstate <- NULL
   }

 if(robust)
    {Xrf <- array(0, dim = c(pd, runs, tt + 1))
     Xrp <- array(0, dim = c(pd, runs, tt))
     Str0 <- array(0, dim = c(pd, pd, tt + 1))
     Str1 <- array(0, dim = c(pd, pd, tt))
     KGr  <- array(0, dim = c(pd, qd, tt))
     Deltar  <- array(0, dim = c(qd, qd, tt))
     DeltaYr  <- array(0, dim = c(qd, runs, tt))
    }

 if(!is.null(predSr))
     IndIO <- matrix(FALSE,runs,tt)

 if(!is.null(corrSr))
     IndAO <- matrix(FALSE,runs,tt)

 ini <- initSc(a, S, i = 0, ...)
 x0  <- ini$x0
 S0  <- ini$S0

 Xf[, , 1] <- ini$x0
 St0[, , 1] <- ini$S0

 if(robust)
      {
       if(nsim){
           Xs <- t(mvrnorm(nsim, a, S))
           St0s <- array(0, c(pd, pd, tt))
           St1s <- array(0, c(pd, pd, tt))
       }
       if(!is.null(initSr))
           {inir <- initSr(a, S, i = 0, ...)
            xr0  <- inir$x0
            Sr0  <- inir$S0
            rob0  <- inir$rob
           }
       else
           {
            xr0  <- x0
            Sr0  <- S0
            rob0  <- NULL
           }
       Xrf[,, 1] <- xr0
       Str0[,, 1] <- xr0
       rob0L <- list(rob0)

      }
 else{Xrf <- NULL
      Xrp <- NULL
      Str0 <- NULL
      Str1 <- NULL
      KGr <- NULL
      Deltar <- NULL
      rob0L <- NULL
      rob1L <- NULL
     }
 for (i in (1:tt))
     {#prediction
      F0 <- matrix(F[,,i], nrow = pd, ncol = pd)
      Q0 <- matrix(Q[,,i], nrow = pd, ncol = pd)

      ps  <- predSc(x0 = x0, S0 = S0, F = F0, Q = Q0, i = i, ...)
      x1  <- matrix(ps$x1, nrow = pd, ncol = runs)
      S1  <- ps$S1

      Xp[,, i]   <- x1
      St1[,, i] <- S1

      if(robust)
          {if(!is.null(predSr))
               {psr <- predSr(x0 = xr0, S0 = Sr0, F = F0, Q = Q0, i = i,
                              ..., rob0 = rob0)
                IndIO[,i]  <- as.logical(psr$Ind)
                if(nsim){
                     vs <- t(mvrnorm(nsim, a*0, Q0))
                     Xs <- F0 %*% Xs + vs
                     xr1s <- predSr(x0 = xr0s, S0 = Sr0, F = F0, Q = Q0, i = i,
                                    ..., rob0 = rob0)$x1
                     St1s[,,i] <- cov(t(xr1s))
                    }
           }else{
                psr <- predSc(x0 = xr0, S0 = Sr0, F = F0, Q = Q0, i = i, ...)
                if(nsim){
                     vs <- t(mvrnorm(nsim, a*0, Q0))
                     Xs <- F %*% Xs + vs
                     xr1s <- predSc(x0 = xr0s, S0 = Sr0, F = F0, Q = Q0, i = i,
                                    ...)$x1
                     St1s[,,i] <- cov(t(xr1s))
                    }
           }
           xr1       <- psr$x1
           Sr1       <- psr$S1
           rob1      <- psr$rob1

           Xrp[,, i]  <- xr1
           Str1[,, i]<- S1
           if(i==1)  rob1L <- list(rob1)
           else      rob1L[[i]] <- rob1
        }


      #correction
      Z0 <- matrix(Z[,,i], nrow = qd, ncol = pd)
      V0 <- matrix(V[,,i], nrow = qd, ncol = qd)

      Y0 <- matrix(Y[,,i], nrow = qd, ncol = runs)
      cs <- corrSc(y = Y0, x1 = x1, S1 = S1, Z = Z0, V = V0, i = i,...)
      x0 <- cs$x0
      S0 <- cs$S0
      DeltaY[,,i] <- cs$DeltaY

      Xf[,,  i + 1]  <- x0
      St0[,, i + 1] <- S0
      KG[,,  i]     <- cs$K
      Delta[,,  i]     <- cs$Delta

      if(robust)
          {if (!is.null(corrSr))
               {csr <- corrSr(y = Y0, x1 = xr1, S1 = Sr1,
                              Z = Z0, V = V0, i = i, ..., rob1 = rob1)
               IndAO[,i]  <- as.logical(csr$Ind)
               if(nsim){
                    es <- t(mvrnorm(nsim, Y[,1]*0, V0))
                    Ys <- Z0 %*% Xs + es
                    xr0s <- corrSr(y = Ys, x1 = xr1s, S1 = Sr1,
                                   Z = Z0, V = V0, i = i, ..., rob1 = rob1)$x0
                    St0s[,,i] <- cov(t(xr0s))
                   }
          }else{
                csr <- corrSc(y = Y0, x1 = xr1, S1 = Sr1, Z = Z0, V = V0,
                              i = i, ...)
               if(nsim){
                    es <- t(mvrnorm(nsim, Y[,1]*0, V0))
                    Ys <- Z0 %*% Xs + es
                    xr0s <- corrSc(y = Ys, x1 = xr1s, S1 = Sr1,
                                   Z = Z0, V = V0, i = i, ...)$x0
                    St0s[,,i] <- cov(t(xr0s))
                   }
          }
           xr0       <- csr$x0
           Sr0       <- csr$S0
           rob0      <- csr$rob0
           DeltaYr[,,i] <- csr$DeltaY

           Xrf[,, i + 1]  <- xr0
           Str0[,, i + 1] <- S0
           rob0L[[i + 1]] <- rob0
           KGr[,, i]      <- csr$K
           Deltar[,,  i]  <- csr$Delta
          }
 }

if((runs==1)&&(dropRuns))
   {Xf <- matrix(Xf,pd,tt+1)
    Xp <- matrix(Xp,pd,tt)
    if(!is.null(Xrp)) {
       Xrf <- matrix(Xrf,pd,tt+1)
       Xrp <- matrix(Xrp,pd,tt)
       IndIO <- as.logical(IndIO)
       IndAO <- as.logical(IndAO)
    }}
list(Xf = Xf, Xp = Xp, Xrf = Xrf, Xrp = Xrp,
     S0 = St0, S1 = St1, KG = KG, Delta = Delta,
     DeltaY = DeltaY,
     Sr0 = Str0, Sr1 = Str1,
     KGr = KGr, Deltar = Deltar, DeltaYr = DeltaYr,
     IndIO = IndIO, IndAO = IndAO,
     rob0L = rob0L, rob1L = rob1L,
     nsim = nsim, RNGstate = RNGstate,
     St0s = St0s, St1s = St1s)
}


######################################################
# simple wrappers:
######################################################

KalmanFilter <- function(Y, a, S, F, Q, Z, V, dropRuns = TRUE)#
#arguments:
# +  Y               :observations
# +  a, S, F, Q, Z, V:Hyper-parameters of the ssm
{recursiveFilter(Y, a, S, F, Q, Z, V, dropRuns = dropRuns)}

rLSFilter <- function(Y, a, S, F, Q, Z, V,
                      b = NULL, eff = NULL, r = NULL,
                      rlow = 0, rup = NULL, repl = 100000, with.b.eff.r = TRUE,
                      upto = 20,
                      norm = Euclideannorm, dropRuns = TRUE)#
#arguments:
# +  Y               :observations
# +  a, S, F, Q, Z, V:Hyper-parameters of the ssm
# +  b               :clipping height
{if( (!is.null(b)&&with.b.eff.r)||is.null(b)){
    SS <- limitS(S = S, F = F, Q = Q, Z = Z, V = V)
    b.erg <- rLScalibrateB(Z = Z, S = SS, V = V,
                           b = b, eff = eff, r = r, rlow = rlow, rup = rup,
                           repl = repl, with.b.eff.r = with.b.eff.r,
                           upto = upto, norm = norm)#
    cat("calibrating values:\n", "b = ", round(b.erg$b,3), "eff = ",
         round(b.erg$eff,3), "r = ", round(b.erg$r,3), "\n")
 }
 b <- b.erg$b
 recursiveFilter(Y, a, S, F, Q, Z, V,
                 initSc = .cKinitstep, predSc = .cKpredstep,
                 corrSc = .cKcorrstep,
                 #initSr=NULL, predSr=NULL,
                 initSr = .cKinitstep, predSr = .cKpredstep,
                 corrSr = .rLScorrstep, b = b, norm = norm, dropRuns = dropRuns)
 }

##just a synonym for AO/SO robust filter
rLS.AO.Filter <- rLSFilter

## IO robust filter
rLS.IO.Filter <- function(Y, a, S, F, Q, Z, V, b = NULL, eff = NULL, r = NULL,
                      rlow = 0, rup = NULL, repl = 100000, with.b.eff.r = TRUE,
                      upto = 20,
                      norm = Euclideannorm, dropRuns = TRUE)#
#arguments:
# +  Y               :observations
# +  a, S, F, Q, Z, V:Hyper-parameters of the ssm
# +  b               :clipping height
{if( (!is.null(b)&&with.b.eff.r)||is.null(b)){
    SS <- limitS(S = S, F = F, Q = Q, Z = Z, V = V)
    b.erg <- rLScalibrateB(Z = Z, S = SS, V = V,
                           b = b, eff = eff, r = r, rlow = rlow, rup = rup,
                           repl = repl, with.b.eff.r = with.b.eff.r,
                           upto = upto, norm = norm, IO = TRUE)#
    cat("calibrating values:\n", "b = ", round(b.erg$b,3), "eff = ",
         round(b.erg$eff,3), "r = ", round(b.erg$r,3), "\n")
 }
 b <- b.erg$b
 recursiveFilter(Y, a, S, F, Q, Z, V,
                 initSc = .cKinitstep, predSc = .cKpredstep,
                 corrSc = .cKcorrstep,
                 #initSr=NULL, predSr=NULL,
                 initSr = .cKinitstep, predSr = .cKpredstep,
                 corrSr = .rLS.IO.corrstep, b = b, norm = norm, dropRuns = dropRuns)}

## IOAO robust filter
rLS.IOAO.Filter <- function(Y, a, S, F, Q, Z, V,
                      window = 5, quantile = 0.95,
                     b.AO = NULL, eff.AO = NULL, r.AO = NULL,
                      rlow.AO = 0, rup.AO = NULL, repl.AO = 100000,
                      with.b.eff.r.AO = TRUE,
                      upto.AO = 20, norm.AO = Euclideannorm,
                     b.IO = NULL, eff.IO = NULL, r.IO = NULL,
                      rlow.IO = 0, rup.IO = NULL, repl.IO = 100000,
                      with.b.eff.r.IO = TRUE,
                      upto.IO = 20, norm.IO = Euclideannorm,
                      dropRuns = TRUE)#
#arguments:
# +  Y               :observations
# +  a, S, F, Q, Z, V:Hyper-parameters of the ssm
# +  b               :clipping height
{SS <- NULL
 if( (!is.null(b.IO)&&with.b.eff.r.IO)||is.null(b.IO)){
    SS <- limitS(S = S, F = F, Q = Q, Z = Z, V = V)
    b.IO.erg <- rLScalibrateB(Z = Z, S = SS, V = V,
                           b = b.IO, eff = eff.IO, r = r.IO, rlow = rlow.IO,
                           rup = rup.IO, repl = repl.IO,
                           with.b.eff.r = with.b.eff.r.IO,
                           upto = upto.IO, norm = norm.IO, IO = TRUE)#
    cat("calibrating values:\n", "b.IO = ", round(b.IO.erg$b,3), "eff.IO = ",
         round(b.IO.erg$eff,3), "r.IO = ", round(b.IO.erg$r,3), "\n")
 }
 b.IO <- b.IO.erg$b
 if( (!is.null(b.AO)&&with.b.eff.r.AO)||is.null(b.AO)){
    if(is.null(SS))
       SS <- limitS(S = S, F = F, Q = Q, Z = Z, V = V)
    b.AO.erg <- rLScalibrateB(Z = Z, S = SS, V = V,
                           b = b.AO, eff = eff.AO, r = r.AO, rlow = rlow.AO,
                           rup = rup.AO, repl = repl.AO,
                           with.b.eff.r = with.b.eff.r.AO,
                           upto = upto.AO, norm = norm.AO)#
    cat("calibrating values:\n", "b.AO = ", round(b.AO.erg$b,3), "eff.AO = ",
         round(b.AO.erg$eff,3), "r.AO = ", round(b.AO.erg$r,3), "\n")
 }
 b.AO <- b.AO.erg$b

 diA <- c(dim(F)[1],dim(Y)[2],window)
 diAy <- c(dim(Y)[1:2],window)
 diB0 <- c(dim(F)[1],dim(Y)[2],dim(Y)[3]+1)
 diB1 <- c(dim(F)[1],dim(Y)[2],dim(Y)[3])
 diC <- c(dim(Y)[2],dim(Y)[3])

 rob0 <- list(dy = numeric(window),
              dx0.io = array(0, dim = diA),
              dx1.io = array(0, dim = diA),
              Ind.io = matrix(0, dim = diAy[2:3]),
              DeltaY.io = array(0, dim = diAy),
              dx0 = array(0, dim = diB0),
              dx1 = array(0, dim = diB1),
              Ind.IO = matrix(0, dim = diB1[2:3]),
              Ind.AO = matrix(0, dim = diB1[2:3]),
              DeltaY = 0*Y
              )

 recursiveFilter(Y, a, S, F, Q, Z, V,
                 initSc = .cKinitstep, predSc = .cKpredstep,
                 corrSc = .cKcorrstep,
                 #initSr=NULL, predSr=NULL,
                 initSr = .cKinitstep, predSr = .rLS.IOAO.predstep,
                 corrSr = .rLS.IOAO.corrstep, window = window,
                 quantile = quantile, b.IO = b.IO, norm.IO = norm.IO,
                 b.AO = b.AO, norm.AO = norm.AO, rob0 = rob0,
                 dropRuns = dropRuns)
 }

ACMfilter <- function(Y, a, S, F, Q, Z, V, s0, psi, apsi, bpsi, cpsi, flag, dropRuns = TRUE)#
#arguments:
# +  Y               :observations
# +  a, S, F, Q, Z, V:Hyper-parameters of the ssm
# +  b               :clippingheight
##  Y=x ... observed time series
##  a=m0 ... unconditional mean
##  S=Cx ... covariance matrix
##  F=Phi ... design matrix of state equation
##  Q ... covariance matrix of state innovation process
##  Z=H ... observation matrix
##  V ... covariance matrix of observation noise
##  s0 ... scale of nominal Gaussian component of additive noise
##  psi ... influence function to be used (default: Hampel's psi function,
##          only that is available at the moment)
##  apsi, bpsi, cpsi ... tuning constants for Hampel's psi-function
##              (default: a=b=2.5, c=5.0)
##  flag ... character, if "weights" (default), use psi(t)/t to calculate
##           the weights; if "deriv", use psi'(t)
{ recursiveFilter(Y, a, S, F, Q, Z, V,
                 initSc = .cKinitstep, predSc = .cKpredstep,
                 corrSc = .cKcorrstep,
                 initSr = .cKinitstep, predSr = .ACMpredstep,
                 corrSr = .ACMcorrstep, s0=s0, psi=psi,
                 apsi=2.5, bpsi=2.5, cpsi=5.0, flag=flag, dropRuns = dropRuns)}
###########################################
##
##  R-function: ACMfilter - approximate conditional-mean filtering
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-21)
##
###########################################

##  Paramters:
##  Y=x ... observed time series
##  a=m0 ... unconditional mean
##  S=Cx ... covariance matrix
##  F=Phi ... design matrix of state equation
##  Q ... covariance matrix of state innovation process
##  Z=H ... observation matrix
##  V ... covariance matrix of observation noise
##  s0 ... scale of nominal Gaussian component of additive noise
##  psi ... influence function to be used (default: Hampel's psi function,
##          only that is available at the moment)
##  a, b, c ... tuning constants for Hampel's psi-function
##              (defaul: a=b=2.5, c=5.0)
##  flag ... character, if "weights" (default), use psi(t)/t to calculate
##           the weights; if "deriv", use psi'(t)

mACMfilter <- function(Y, a, S, F, Q, Z, V,
                       psi=mvpsiHampel, apsi=2.5, bpsi=2.5, cpsi=5.0,
                       flag="deriv", dropRuns = TRUE)
{
###########################################
##
##  R-function: mACMfilter - approximate conditional-mean filtering
##  author: Bernhard Spangl
##  version: 0.2 (2008-03-31)
##
###########################################

##  Paramters:
##  Y ... observed vector-valued time series
##        (column-wise, matrix: q rows, number of columns equal to time points)
##  a ... unconditional mean vector (formerly: m0)
##  S ... covariance matrix (formerly: Cx)
##  F ... design matrix of state equation (formerly: Phi)
##  Q ... covariance matrix of state innovation process
##  Z ... observation matrix (formerly: H)
##  V ... covariance matrix of observation noise (formerly: R)
##  psi ... influence function to be used (default: Hampel's psi function,
##          only that is available at the moment)
##  apsi, bpsi, cpsi ... tuning constants for Hampel's psi-function
##                       (default: a=b=2.5, c=5.0)
##  flag ... character, weight matrix to be used in correction step,
##           if "deriv" (default), Jacobian matrix of multivariate analogue
##           of Hampel's psi-function is used (only default is available
##           at the moment)

    recursiveFilter(Y, a, S, F, Q, Z, V,
                initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep,
                initSr=.cKinitstep, predSr=.cKpredstep, corrSr=.mACMcorrstep,
                psi, apsi, bpsi, cpsi, flag, dropRuns = dropRuns)
}


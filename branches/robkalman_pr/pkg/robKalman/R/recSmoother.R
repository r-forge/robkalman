recursiveFPSmoother <- function(Y, F, Q, V, Z,
                          smoothSc = .cKsmoothStep, smoothSr = NULL,
                          KG, xf.c, S0.c, S1.c, xf.r, S0.r, S1.r, KG.r,
                          Ind.IO, Ind.AO, ..., dropRuns = FALSE){

  ## dimensions
  p    <- dim(xf)[1]
  q    <- dim(y)[1]
  runs <- dim(xf)[2]
  tt    <- dim(xf)[3]-1

 if(p==1) F <- array(F, dim = c(p,p,tt))
 if(p==1 && q==1) Z <- array(Z, dim = c(p,q,tt))
 if(p==1) Q <- array(Q, dim = c(p,p,tt))
 if(q==1) V <- array(V, dim = c(q,q,tt))
 if(is.matrix(F)) F <- array(F, dim = c(p,p,tt))
 if(is.matrix(Z)) Z <- array(Z, dim = c(p,q,tt))
 if(is.matrix(Q)) Q <- array(Q, dim = c(p,p,tt))
 if(is.matrix(V)) V <- array(V, dim = c(q,q,tt))

 ## generation of arrays
  xS   <- array(0,dim=c(p,runs,tt+1))
  SS   <- array(0,dim=c(p,p,tt+1))
  SS1  <- array(0,dim=c(p,p,tt))
  J    <- array(0,dim=c(p,p,tt))
  if(is.null(smoothSr)){
  xSr   <- NULL
  SSr   <- NULL
  SS1r  <- NULL
  Jr    <- NULL
  Ind.IO <- NULL
  Ind.AO <- NULL
  }else{
  xSr   <- array(0,dim=c(p,runs,tt+1))
  SSr   <- array(0,dim=c(p,p,tt+1))
  SS1r  <- array(0,dim=c(p,p,tt))
  Jr    <- array(0,dim=c(p,p,tt))
  Ind.IO <- matrix(FALSE,runs,tt)
  Ind.AO <- matrix(FALSE,runs,tt)
  }
  xS[,,tt+1] <- xf[,,tt+1]
  xSr[,,tt+1] <- xf.r[,,tt+1]
  SS[,,tt+1] <- S0[,,tt+1]
  SSr[,,tt+1] <- S0.r[,,tt+1]
  KZ <- KG[,,tt] %*% Z[,,tt]
  KZr <- KG.r[,,tt] %*% Z[,,tt]
  SS10 <- F[,,tt]%*%S0[,,tt-1]
  SS10r <- F[,,tt]%*%S0.r[,,tt-1]
  SS1[,,tt]  <- SS10-KZ%*%SS10       ## (A12)
  SS1r[,,tt]  <- SS10r-KZr%*%SS10r   ## (A12)
  for(t in tt:1){
    if(is.null(smoothSr)){
    resR <- smoothSc(xf = xf[,,t], xS = xS[,,t+1],
                     t, F = F[,,t], S0 = S0[,,t],
                      S1 = S1[,,t], SS = SS[,,t+1],
                      SS1 = SS1[,,t+1], J = J[,,t],
                      ...)
    J[,,t]   <- resR$J
    xS[,,t]  <- resR$xS
    SS[,,t]  <- resR$SS
    if(t<tt) SS1[,,t] <- resR$SS1

    }else{

    resR <- smoothSr(xf = xf[,,t], xS = xS[,,t+1],
                     t = t, F = F[,,t], S0 = S0[,,t],
                      S1 = S1[,,t], SS = SS[,,t+1],
                      SS1 = SS1[,,t+1], J = J[,,t],
                      xfr = xrf[,,t], xSr = xSr[,,t+1],
                      F = F[,,t], S0r = S0r[,,t],
                      S1r = S1r[,,t], SSr = SSr[,,t+1],
                      SS1r = SS1r[,,t+1], Jr = Jr[,,t], ...)
    J[,,t]   <- resR$J
    Jr[,,t]   <- resR$Jr
    xS[,,t]  <- resR$xS
    xSr[,,t]  <- resR$xSr
    SS[,,t]  <- resR$SS
    SSr[,,t]  <- resR$SSr
    Ind.IO[,t] <- if(is.null(resR$Ind.IO)) Ind.IO[,t] else resR$Ind.IO
    Ind.AO[,t] <- if(is.null(resR$Ind.AO)) Ind.AO[,t] else resR$Ind.AO
    if(t<tt){
       SS1[,,t] <- resR$SS1
       SS1r[,,t] <- resR$SS1r
       }
    }

    if((runs==1)&&(dropRuns))
     {xS <- matrix(xS,pd,tt+1)
      if(!is.null(Xrp)) {
       XSr <- matrix(XSr,pd,tt+1)
       Ind.IO <- as.logical(Ind.IO)
       Ind.AO <- as.logical(Ind.AO)
    }}


    return(list(J = J, xS = xS, SS = SS, SS1 = SS1,
                Jr = Jr, xSr = xSr, SSr = SSr, SS1r = SS1r,
                Ind.IO = Ind.IO, Ind.AO = Ind.AO  ))

}

.cKsmoothStep <- function(xf,xS,t,F,S0,S1,SS,SS1,J){
    J1   <- S0 %*% t(F) %*% ginv(S1) ## (A8)
    xS1  <- xf + J1 %*% (xS - F %*% xf)  ## (A9)
    SS.1  <- S0 + J1 %*% (SS - S1) %*% t(J1) ## (A10)
    if(t<tt) SS11 <- S0 %*% t(J1) +     ## (A11)
                        J %*% (SS1 - F %*% S0) %*% t(J)
    return(list(J = J1, xS = xS1, SS = SS.1, SS1 = SS11))
}

.rLSsmoothStep <- function(xf,xS,xfr,xSr,t,F,S0,S1,SS,SS1,
                   J,S0r,S1r,SSr,SS1r,Jr){
    erg.c <- .cKsmoothStep(xf,xS,t,F,S0,S1,SS,SS1,J)
    erg.r <- .cKsmoothStep(xfr,xSr,t,F,S0r,S1r,SSr,SS1r,Jr)

    return(list(J = erg.c$J, xS = erg.c$xS, SS = erg.c$SS, SS1 = erg.c$SS1,
                Jr = erg.r$J, xSr = erg.r$xS, SSr = erg.r$SS, SS1r = erg.r$SS1))

}

recursiveFISmoother <- function(Y, a, S, F, Q, V, Z, window,
                                initSc = .cKinitstep, predSc = .cKpredstep,
                                corrSc = .cKcorrstep,
                                initSr = NULL, predSr = NULL, corrSr = NULL,
                                smoothSc = .cKsmoothStep, smoothSr = NULL,
                                nsim = 0, seed = NULL, ..., dropRuns = ttRUE){

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

 robust <- !(is.null(initSr)&&
             is.null(predSr)&&
             is.null(corrSr)&&
             is.null(smoothSr))

 Xf  <- array(NA, dim = c(pd, runs, tt + 1))
 Xp  <- array(NA, dim = c(pd, runs, tt))
 XS  <- array(NA, dim = c(pd, runs, window, tt + 1))
 DeltaY  <- array(NA, dim = c(qd, runs, tt))
 St0 <- array(NA, dim = c(pd, pd, tt + 1))
 St1 <- array(NA, dim = c(pd, pd, tt))
 StS <- array(NA, dim = c(pd, pd, window, tt+1))
 J    <- array(NA,dim=c(pd,pd,window,tt))
 KG  <- array(NA, dim = c(pd, qd, tt))
 Delta  <- array(NA, dim = c(qd, qd, tt))
 NULLpp <- matrix(0,pd,pd)

 ## generation of arrays
  if(!robust){
  xrf     <- NULL
  xrp     <- NULL
  xrS     <- NULL
  DeltarY <- NULL
  Str0    <- NULL
  Str1    <- NULL
  StrS    <- NULL
  Jr      <- NULL
  KGr     <- NULL
  Deltar  <- NULL
  Ind.IO <- NULL
  Ind.AO <- NULL
  }else{
  Xrf <- array(NA, dim = c(pd, runs, tt + 1))
  Xrp <- array(NA, dim = c(pd, runs, tt))
  XrS <- array(NA, dim = c(pd, runs, window, tt+1))
  DeltarY  <- array(NA, dim = c(qd, runs, tt))
  Str0 <- array(NA, dim = c(pd, pd, tt + 1))
  Str1 <- array(NA, dim = c(pd, pd, tt))
  StrS <- array(NA, dim = c(pd, pd, window, tt+1))
  Jr    <- array(NA,dim=c(pd,pd,window, tt))
  KGr  <- array(NA, dim = c(pd, qd, tt))
  Deltar  <- array(NA, dim = c(qd, qd, tt))
  IndIO <- array(FALSE,dim = c(runs,window,tt))
  IndAO <- array(FALSE,dim = c(runs,window,tt))
  }

 ## ---------- end of declarations ----------

 #----------------------------------------------
 ## init step ###
 #----------------------------------------------

ini <- initSc(a, S, i = 0, ...)
 x0  <- ini$x0
 S0  <- ini$S0

 Xf[, , 1] <- ini$x0
 St0[, , 1] <- ini$S0

 if(robust)
      {
       if(nsim){
           Xs <- t(mvrnorm(nsim, a, S))
           St0s <- array(NA, c(pd, pd, tt))
           St1s <- array(NA, c(pd, pd, tt))
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

 #----------------------------------------------
 ### begin time loop
 #----------------------------------------------

 for (i in (1:tt))
     {
      #-----------------------------------------
      #prediction
      #-----------------------------------------

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
                IndIO[,1,i]  <- as.logical(psr$Ind)
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
           Str1[,, i] <- S1
           if(i==1)  rob1L <- list(rob1)
           else      rob1L[[i]] <- rob1
        }


      #-----------------------------------------
      #correction
      #-----------------------------------------

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
               IndAO[,1,i]  <- as.logical(csr$Ind)
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
           DeltarY[,,i] <- csr$DeltaY

           Xrf[,, i + 1]  <- xr0
           Str0[,, i + 1] <- S0
           rob0L[[i + 1]] <- rob0
           KGr[,, i]      <- csr$K
           Deltar[,,  i]  <- csr$Delta
          }

      #-----------------------------------------
      #smoothing
      #-----------------------------------------

      xS[,,1,i+1] <- xf[,,i+1]
      StS[,,1,i+1] <- S0[,,i+1]
      if(robust){
         xrS[,,1,i+1] <- xrf[,,1,i+1]
         StrS[,,1,i+1] <- S0[,,i+1]
      }

      KZ <- KG[,,i] %*% Z[,,i]
      KZr <- KG.r[,,i] %*% Z[,,i]
      SS10 <- F[,,i]%*%S0[,,i]
      SS10r <- F[,,i]%*%S0.r[,,i]


      if(window > 1 && i>1)
      for(j in (1:min(window-1,i-1))){

          if(is.null(smoothSr)){

             resR <- smoothSc(xf = xf[,,i+1-j], xS = xS[,,j,i+1],
                        i, F = F[,,i+1-j], S0 = St0[,,i+1-j],
                        S1 = St1[,,i+1-j], SS = StS[,,j,i+1],
                        SS1 = NULLpp, J = NULLpp,
                        ...)
             J[,,j,i]        <- resR$J
             xS[,,j+1,i+1]   <- resR$xS
             StS[,,j+1,i+1]  <- resR$SS

          }else{

             resR <- smoothSr(xf = xf[,,i+1-j], xS = xS[,,i+1],
                      t = i+1-j, F = F[,,i+1-j], S0 = St0[,,i+1-j],
                      S1 = St1[,,i+1-j], SS = StS[,,j,i+1],
                      SS1 = NULLpp, J = NULLpp,
                      xfr = xrf[,,i+1-j], xSr = xSr[,,j,i+1],
                      S0r = Str0[,,i+1-j], S1r = Str1[,,i+1-j],
                      SSr = StrS[,,j,i+1],
                      SS1r = NULLpp, Jr = NULLpp, ...)
             J[,,j,i]   <- resR$J
             Jr[,,j,i]  <- resR$Jr
             xS[,,j+1,i+1]  <- resR$xS
             xrS[,,j+1,i+1]  <- resR$xSr
             StS[,,j+1,i+1]  <- resR$SS
             StrS[,,j+1,i+1] <- resR$SSr
             IndIO[,j+1,i]   <- if(is.null(resR$Ind.IO))
                                   IndIO[,j+1,i] else resR$Ind.IO
             IndAO[,j+1,i]   <- if(is.null(resR$Ind.AO))
                                   IndAO[,j+1,i] else resR$Ind.AO

          }

 }

 #----------------------------------------------
 ### end time loop
 #----------------------------------------------

if((runs==1)&&(dropRuns))
   {Xf <- matrix(Xf,pd,tt+1)
    Xp <- matrix(Xp,pd,tt)
    Xs <- array(Xp, dim = c(pd,window,tt))
    if(!is.null(Xrp)) {
       Xrf <- matrix(Xrf,pd,tt+1)
       Xrp <- matrix(Xrp,pd,tt)
       Xrs <- array(Xp, dim = c(pd,window,tt))
       IndIO <- matrix(IndIO,window,tt)
       IndAO <- matrix(IndAO,window,tt)
    }}


    return(list(Xf = Xf, Xp = Xp, Xrf = Xrf, Xrp = Xrp,
               S0 = St0, S1 = St1, KG = KG,
               Delta = Delta, DeltaY = DeltaY,
               Sr0 = Str0, Sr1 = Str1, KGr = KGr,
               Deltar = Deltar, DeltaYr = DeltaYr,
               IndIO = IndIO, IndAO = IndAO,
               rob0L = rob0L, rob1L = rob1L,
               nsim = nsim, RNGstate = RNGstate,
               St0s = St0s, St1s = St1s,
               J = J, xS = xS, SS = StS,
               Jr = Jr, xSr = xrS, SSr = StrS,
               Ind.IO = Ind.IO, Ind.AO = Ind.AO  ))

}

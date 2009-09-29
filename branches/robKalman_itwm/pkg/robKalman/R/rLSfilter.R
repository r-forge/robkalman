################################################################
#rLS filter
################################################################

# P. Ruckdeschel 10.06.06
#

## we assume a time-invariant SSM of the following form
# Hyper-parameters:
# F (p x p), 0<= S (p x p), 0< Q (p x p) 
# Z (q x p), 0<  V (q x q) 
# distributional assumptions 
#    +initial condition     x_0 ~ N_p(a,S)
#    +innovations           v_t ~ N_p(0,Q), t>=1
#    +observation errors    e_t ~ N_q(0,V), t>=1
# 
# Model equations 
#    +state equation        x_t = F x_{t-1} +  v_t, t>=1
#    +observation equation  y_t = Z x_t + e_t     , t>=1
##



### Notation for the Kalman filter

#    +initial step          x_{0|0}   = a 
#     error covariance      S_{0|0}   = Cov(x_0-x_{0|0})   = S
#    +prediction step       x_{t|t-1} = F x_{t-1|t-1},                             t>=1
#     error covariance      S_{t|t-1} = Cov(x_t-x_{t|t-1}) = F S_{t-1|t-1} F' + Q 
#    +correction step       x_{t|t}   = x_{t|t-1} + K_t (y_t - Z x_{t|t-1})        t>=1
#           for Kalman Gain K_t       = S_{t|t-1} Z' (Z S_{t|t-1} Z' + V )^- 
#     error covariance      S_{t|t}   = Cov(x_t-x_{t|t}) = S_{t|t-1} - K_t Z S_{t|t-1}

#########################################################################################
#
#rLS filter routines
#

##steps for rLS filter (rLS) (xr0, xr1)
## (simultaneously for cK (x0, x1) )

.rLScorrstep <- function(y, x1, S1, Z, V, i, rob1 = NULL,
                         b = NULL, norm = Euclideannorm, ...)
  {Delta <- .getDelta(S1, Z, V)
   K   <- .getKG(S1, Z, Delta)
   DeltaY <- y - Z %*% x1
   dx <- K %*% DeltaY
   if(length(b)>1)
      b <- b[min(i,length(b))]
   x0 <- x1 + Huberize(dx, b, norm = norm)
   S0  <- .getcorrCov(S1, K, Z)
   if  (ncol(x1)==1)
      Ind <- (norm(dx)>b)
   else
      Ind <- apply(dx, 2, function(xx) norm(xx)>b)
   list(x0  = x0, K = K, S0 = S0, Delta = Delta, Ind = Ind, DeltaY = DeltaY)}


.rLS.IO.corrstep <- function(y, x1, S1, Z, V, i, rob1 = NULL,
                         b = NULL, norm = Euclideannorm, ...)
  {Delta <- .getDelta(S1, Z, V)
   K   <- .getKG(S1, Z, Delta)
   DeltaY <- y - Z %*% x1
   dx <- K %*% DeltaY
   de <- DeltaY-Z%*%dx
   if(length(b)>1)
      b <- b[min(i,length(b))]
   x0 <- x1 + ginv(Z)%*%(DeltaY-Huberize(de, b, norm = norm))
   S0  <- .getcorrCov(S1, K, Z)
   if  (ncol(x1)==1)
      Ind <- (norm(de)>1)
   else
      Ind <- apply(de, 2, function(xx) norm(xx)>1)
   list(x0  = x0, K = K, S0 = S0, Delta = Delta, Ind = Ind, DeltaY = DeltaY)}

.rLS.IOAO.corrstep <- function(y, x1, S1, Z, V, i, rob1, window = 5,
                         quantile = 0.95,
                         b.IO, norm.IO = Euclideannorm,
                         b.AO, norm.AO = Euclideannorm, ...)
  {
   erg.IO <- .rLS.IO.corrstep(y = y, x1 = x1, S1 = S1, Z = Z,
                              V = V, i = i, rob1 = NULL,
                              b = b.IO, norm = norm.IO, ...)
   erg.AO <- .rLS.AO.corrstep(y = y, x1 = x1, S1 = S1, Z = Z,
                              V = V, i = i, rob1 = NULL,
                              b = b.AO, norm = norm.AO, ...)

   window <- min(window,i)
   tt <- dim(rob1$x0)[1]-1
   win1 <- 2:window; win0 <- win1 - 1
   iwin <- (1:window)+i-1

   rob1$dx0.io[,,win0] <- rob1$dx0.io[,,win1]
   rob1$dx1.io[,,win0] <- rob1$dx1.io[,,win1]
   rob1$DeltaY.io[,,win0] <- rob1$DeltaY.io[,,win1]
   rob1$Ind.io[,win0] <- rob1$Ind.io[,win1]

   rob1$dx0.io[,,window] <- erg.IO$x0
   rob1$dx1.io[,,window] <- 0*erg.IO$x0
   rob1$Ind.io[,,window] <- erg.IO$IndIO
   rob1$DeltaY.io[,,window] <- erg.IO$DeltaY

   rob1$dx0[,,i]    <- erg.AO$dx0
   rob1$dx1[,,i]    <- 0*erg.AO$dx0
   rob1$DeltaY[,,i] <- erg.AO$DeltaY
   rob1$Ind.AO[,i]     <- erg.AO$Ind
   rob1$Ind.IO[,i]     <- FALSE

   rob1$dy[win0] <- rob1$dy[win1]
   rob1$dy[window] <- sqrt(colSums(solve(t(chol(erg.AO$Delta)),erg.AO$DeltaY)^2))

   S0 <- erg.AO$S0
   K <- erg.AO$K
   Delta <- erg.AO$Delta

   if(max(rob1$dy)> qchisq(windows*log(1-quantile), log.p = TRUE, lower = FALSE)){
       rob1$dx0[,,iwin] <- rob1$dx0.io
       rob1$dx1[,,iwin] <- rob1$dx1.io
       rob1$DeltaY[,iwin] <- rob1$DeltaY.io
       rob1$Ind.AO[,iwin] <- FALSE
       rob1$Ind.IO[,iwin] <- rob1$Ind.io
       x0 <- erg.IO$dx0
       Ind.AO <- FALSE
       Ind.IO <- erg.IO$Ind
       Delta <- erg.IO$Delta
       DeltaY <- erg.IO$DeltaY
   }else{
       x0 <- erg.AO$dx0
       Ind.IO <- FALSE
       Ind.AO <- erg.AO$Ind
       Delta <- erg.AO$Delta
       DeltaY <- erg.AO$DeltaY
   }

   list(x0  = x0, K = K, S0 = S0, Delta = Delta, Ind.IO = Ind.IO, Ind.AO = Ind.AO
   DeltaY = DeltaY, rob0 = rob1)}

.rLS.IOAO.predstep <- function (x0, S0, F, Q, i, rob0, s0, ...)  ### S=P F= Phi
{   diA <- rob0$dx0.io
    S1 <- .getpredCov(S0, F, Q)
    rob0$dx1.io <- array(apply(rob0$dx0.io, 3, function(m) F %*% m),
                         dim = diA)

    rob0$dx1[,,i] <- x1 <- F %*% x0
    return(list(x1 = x1, S1 = S1, rob1 = rob0, Ind = FALSE))

}
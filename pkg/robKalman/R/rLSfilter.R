################################################################
#rLS filter
################################################################

# P. Ruckdeschel 10.06.06
#    revised 15.08.11
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

.rLScorrstep <- function(y, x1, S1, Z, V, i, rob1 = NULL, b,
                         norm = EuclideanNorm, ...)
  {Delta <- .getDelta(S1, Z, V)
   K   <- .getKG(S1, Z, Delta)
   DeltaY <- y - Z %*% x1
   dx <- K %*% DeltaY
   if(length(b)>1)
      b <- b[min(i,length(b))]
   x0 <- x1 + Huberize(dx, b, norm = norm)
   S0  <- .getcorrCov(S1, K, Z)
   if  (ncol(x1)==1)
      Ind <- (norm(dx)>1)
   else
      Ind <- apply(dx, 2, function(xx) norm(xx)>1)
   list(x0  = x0, K = K, S0 = S0, Delta = Delta, Ind = Ind, DeltaY = DeltaY)}


.rLS.IO.corrstep <- function(y, x1, S1, Z, V, i, rob1 = NULL, b,
                         norm = EuclideanNorm, ...)
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

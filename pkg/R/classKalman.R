################################################################
#classical Kalman filter
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
#Kalman filter routines
#
#
.getKG <-  function(S1, Z, V)# calculates the Kalman Gain for S1 = S_{t|t-1}, Z, V as above
{ H <- S1 %*% t(Z)
  H %*% ginv( Z%*%H + V ) }

.getcorrCov <-  function(S1, K, Z)# calculates S_{t|t} for S1 = S_{t|t-1}, K_t, Z as above
{ S1 - K %*% Z %*% S1 }

.getpredCov <-  function(S0, F, Q)# calculates S_{t|t-1} for S0 = S_{t-1|t-1}, F, Q as above
{ F %*% S0 %*% t(F) + Q }

##steps for classical Kalman filter (cK)
.cKinitstep <- function(a, S, ...) 
              {list( x0 = a,  S0 = S )}
              
.cKpredstep <- function(x0, S0, F, Q, ...) 
              {list( x1  = F %*% x0, S1 = .getpredCov(S0, F, Q), Ind=1)}

.cKcorrstep <- function(y, x1, S1, Z, V, ...) 
  {K  <- .getKG(S1, Z, V) 
   x0 <- x1 + K %*% (y - Z %*% x1)
   S0 <- .getcorrCov(S1, K, Z)
   list(x0  = x0, K = K, S0 = S0, Ind=1)}

#### for Testing purposes:
###
## generation of ideal and contaminated realisations of
## multivariate, Gaussian, linear state space models

rcvmvnorm <-  function(mi, Si, mc, Sc, r) 
        {U<-rbinom(1, size = 1, prob = r); 
        (1-U) * mvrnorm(1, mi, Si) + U * mvrnorm(1, mc, Sc)}

simulateState <- function(a, S, F, Q, tt){
  states<-matrix(0, length(a), tt+1)
  states[ ,1] <- mvrnorm(1, m = a, S = S)
  for (i in (1:tt))
         states[, i+1] <- F %*% states[, i] + mvrnorm(1, 0*a, Q)
  states
}

simulateObs <- function(X, Z, Vi, mc, Vc, r){
  tt <- (dim(X))[2]-1 
  obs<-matrix(0, length(mc), tt)
  for (i in 1:tt)
      obs[, i] <- Z %*% X[, (i+1)] + rcvmvnorm(0*mc, Vi, mc, Vc, r)
  obs
}

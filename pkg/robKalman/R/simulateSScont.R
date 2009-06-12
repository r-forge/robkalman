#### for Testing purposes:
###
## generation of ideal and contaminated realisations of
## multivariate, Gaussian, linear state space models

rcvmvnorm <-  function(runs, mi, Si, mc, Sc, r)
        {U<-rbinom(runs, size = 1, prob = r);
        (1-U) * mvrnorm(runs, mi, Si) + U * mvrnorm(runs, mc, Sc)}

simulateState <- function(a, S, F, Qi, mc, Qc, runs = 1, tt, r=0){
  pd <- length(a)
  if(length(dim(F))<3) F <- array(F, dim=c(pd,pd,tt))
  if(!is.matrix(mc)) mc <- matrix(mc, pd,tt)
  if(length(dim(Qi))<3)  Qi <- array(Qi, dim=c(pd,pd,tt))
  if(length(dim(Qc))<3) Qc <- array(Qc, dim=c(pd,pd,tt))
  states <- array(0, dim=c(pd,runs, tt+1))
  states[ ,,1] <- t(matrix(mvrnorm(runs, m = a, S = S),runs,pd))
  for (i in (1:tt))
         states[,, i+1] <- F[,,i] %*% states[,, i] +
                 t(matrix(rcvmvnorm(runs, mi = a*0, Si = Qi[,,i], mc = mc[,i],
                            Sc = Qc[,,i], r=r),runs,pd))
  states
}

simulateObs <- function(X, Z, Vi, mc, Vc, runs = 1, r){
  tt <- (dim(X))[3]-1
  qd <- if(!is.null(dim(Vi))) (dim(Vi))[1] else 1
  pd <- (dim(X))[1]
  if(length(dim(Z))<3) Z <- array(Z, dim=c(qd,pd,tt))
  if(!is.matrix(mc)) mc <- matrix(mc, qd,tt)
  if(length(dim(Vi))<3) Vi <- array(Vi, dim=c(qd,qd,tt))
  if(length(dim(Vc))<3) Vc <- array(Vc, dim=c(qd,qd,tt))
  obs<-array(0, dim=c(qd, runs, tt))
  for (i in 1:tt){
      obs[,, i] <- Z[,,i] %*% X[,, (i+1)] +
                  t(matrix(rcvmvnorm(runs, 0*mc[,1], Vi[,,i], mc[,i],
                            Vc[,,i], r=r),runs,qd))
  }
  obs
}

#### for Testing purposes:
###
## generation of ideal and contaminated realisations of
## multivariate, Gaussian, linear state space models


simulateState <- function(a, S, F, Qi, mc=0, Qc=Qi, runs = 1, tt, r=0){
  pd <- length(a)
  if(length(dim(F))<3) F <- array(F, dim=c(pd,pd,tt))
  if(!is.matrix(mc)) mc <- matrix(mc, pd,tt)
  if(length(dim(Qi))<3)  Qi <- array(Qi, dim=c(pd,pd,tt))
  if(length(dim(Qc))<3) Qc <- array(Qc, dim=c(pd,pd,tt))
  states <- array(0, dim=c(pd,runs, tt+1))
  states[ ,,1] <- t(matrix(mvrnorm(runs, m = a, S = S),runs,pd))
  if(r>0)   states.r <- states
  for (i in (1:tt)){
           v <- t(matrix(mvrnorm(runs, m = 0*a, S = as.matrix(Qi[,,i])),runs,pd))
           states[,, i+1] <- as.matrix(F[,,i]) %*% states[,, i] + v
           if (r>1e-8){
              state0 <- as.matrix(F[,,i]) %*% states.r[,, i] + v
              U <- rbinom(runs, size = 1, prob = r)
              X.ir <- t(state0)
              X.di <- t(t(rmvt(runs,as.matrix(Qc[,,i]),df=1))+mc[,i])
              states.r[,,i+1] <- t( (1-U) * X.ir + U * X.di)
           }
  }
  if (r<1e-8) return(states) else return(list(id=states,re=states.r))
}

simulateObs <- function(X, Z, Vi, mc=0, Vc=Vi, r=0){
  tt <- (dim(X))[3]-1
  runs <- (dim(X))[2]
  qd <- if(!is.null(dim(Vi))) (dim(Vi))[1] else 1
  pd <- (dim(X))[1]
  if(length(dim(Z))<3) Z <- array(Z, dim=c(qd,pd,tt))
  if(!is.matrix(mc)) mc <- matrix(mc, qd,tt)
  if(length(dim(Vi))<3) Vi <- array(Vi, dim=c(qd,qd,tt))
  if(length(dim(Vc))<3) Vc <- array(Vc, dim=c(qd,qd,tt))
  obs<-array(0, dim=c(qd, runs, tt))
  if(r>0)   obs.r <- obs
  for (i in 1:tt){
      a <- Z[,,i] %*% if(pd>1) X[,, (i+1)] else matrix(X[,,i+1],nrow=1)
      b <- t(matrix(mvrnorm(runs, 0*mc[,1], as.matrix(Vi[,,i])),runs,qd))
      obs[,, i] <- a + b
      if(r>1e-8){
         U <- rbinom(runs, size = 1, prob = r)
         Y.id <- if(qd>1) t(obs[,,i]) else matrix(obs[,,i],nrow=runs)
         Y.di <- t(t(rmvt(runs, as.matrix(Vc[,,i]),df=1))+mc[,i])
         obs.r[,,i] <- t( (1-U)  * Y.id +  U * Y.di )
      }
  }
  if(r<1e-8) return(obs) else return(list(id=obs,re=obs.r))
}

if(FALSE){
X <- simulateState(a=c(0,0), F=matrix(c(1,-1,1,0),nrow=2),Qi=diag(2),
     Qc=matrix(10*c(4,2,2,1),2,2),runs=10,r=0.1,tt=60,S=diag(2),mc=c(100,1))

Y <- simulateObs(X$id, Z=matrix(c(1,-1),nrow=1),Vi=.4,Vc=3,r=0.1)
Y$re
X2 <- simulateState(a=c(0,0), F=matrix(c(1,-1,1,0),nrow=2),Qi=diag(2),
     Qc=matrix(10*c(4,2,2,1),2,2),runs=1,r=0.1,tt=60,S=diag(2),mc=c(100,1))

Y2 <- simulateObs(X2$id, Z=matrix(c(1,-1),nrow=1),Vi=.4,Vc=3,r=0.1)
X3 <- simulateState(a=0, F=1,Qi=1,Qc=4,runs=10,r=0.1,tt=60,S=1,mc=10)
Y3 <- simulateObs(X3$id, Z=matrix(c(1,-1),ncol=1),
                  Vi=.4*diag(2),Vc=5*diag(2),r=0.1,mc=c(-9,0))
Y3a <- simulateObs(X3$id, Z=1,Vi=.4,Vc=5,r=0.1,mc=10)


}

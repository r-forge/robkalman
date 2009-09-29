initStepSSMclass <- function(y,Z)
{

n <- dim(y)[2]
q <- dim(y)[1]

null <- 1:(n-2)
ynull <- y[,null] - apply(y[,null],1,mean)
yone  <- y[,null+1] - apply(y[,null+1],1,mean)
ytwo  <- y[,null+2] - apply(y[,null+2],1,mean)


C0 <- t(ynull) %*%  ynull /(n-2)
C1 <- t(ynull) %*%  yone  /(n-2)
C2 <- t(ynull) %*%  ytwo  /(n-2)

CC0 <- ginv(Z) %*% C2 %*% t(ginv(Z))
F <- CC0 %*% ginv( ginv(Z) %*% C1 %*% t(ginv(Z)) )
CC <- ginv(F) %*% CC0
Q <- CC - F %*% CC %*% t(F)
V <- C0 - Z %*% C0 %*% t(Z)

ZF <- matrix(0, q,p^2)
FI <- F
for(i in 1:p){
  ZF[,((i-1)*p+1):(i*p)] <- Z%*%FI
  FI <- FI %*% F
}
a <- ginv(t(ZF))%*%c(y[,1:p])
  list(F=F,Q=MakePositive(Q),V=MakePositive(V), a=a)
}



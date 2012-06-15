
recSmoother <- function (Y, a, S, F, Q, Z, V,
              Xf = Xf, Xp = Xp, Xrf = Xrf, Xrp = Xrp,
              S0 = S0, S1 = S1, Sr0 = Sr0, Sr1 = Sr1,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)),
              v=matrix(0, nrow=length(a), ncol=ncol(Y)),
              R=NULL, T=NULL, 
              # initSr=NULL, predSr=NULL, corrSr=NULL, 
              controlF=NULL,
              smooth = .smooth, smoothcov = .smoothcov, lagoneCov=.lagoneCov,...)
{
##  a generalization of the extended Kalman smoother
##  +  Y            : observations in a matrix with dimensions qd x tt
##  +  F, Q, Z, V   : Hyper-parameters of the ssm
##  +  Xf, Xp, Xrf, Xrp, S0, S1, Sr0, Sr1 :results from recEFilter
##  +  R, T         : selection matrices of innovations and observation  noise
##  +  initSr, predSr, corrSr: (robust) initialization-, prediction-, and 
##                             correction-step function
##  +  ...: additional arguments

if (!(is.function(F))) F <- createF(F=F, R=R)
if (!(is.function(Z))) Z <- createZ(Z=Z, T=T)
if (!(is.function(Q))) Q <- createQ(Q=Q)
if (!(is.function(V))) V <- createV(V=V)

pd <- length(a)
qd <- (dim(Y))[1]
tt <- (dim(Y))[2]

J <- array(0, dim=c(pd,pd,tt))

for (i in (1:tt)) {
    A <- F(t=i, x0=Xf[,i+1], v=v[,i], u=u[,i], control=controlF)$A
    J[,,i] <- S0[,,i]%*%t(A)%*%ginv(S1[,,i])
}

robust <- !(is.null(Xrf) && is.null(Xrp) && is.null(Sr0) && is.null(Sr1))

Xs <- smooth (Xfilt = Xf, Xpred= Xp, J = J)
S2 <- smoothcov (Sfilt = S0, Spred= S1, J = J)
S3 <- lagoneCov (Ssmoo = S2, J = J)

if (robust) {
    Xrs <- smooth (Xfilt = Xrf, Xpred= Xrp, J=J)
    Sr2 <- smoothcov (Sfilt = Sr0, Spred= Sr1, J = J)
    Sr3 <- lagoneCov (Ssmoo = Sr2, J = J)
} else {
    Xrs <- NULL
    Sr2 <- NULL
    Sr3 <- NULL
  }

return(list(Xs=Xs, S2=S2, S3=S3, Xrs=Xrs, Sr2=Sr2, Sr3=Sr3))

}

##################################################################################
# Shumway&Stoffer smoother
##################################################################################

.smooth <- function(Xfilt, Xpred, J){

pd <- (dim(Xfilt))[1]
tt <- (dim(Xfilt))[2]-1

Xsmooth <- array (0,dim=c(pd, tt+1))
Xsmooth[,tt+1] <- Xfilt[,tt+1]

new <- J[,,tt]%*%(Xfilt[,tt+1]-Xpred[,tt])
Xsmooth[,tt] <- Xfilt[,tt]+new
    
if(!tt<2){
    for (i in (tt-1):1){
        new <- J[,,i]%*%(new+Xfilt[,i+1]-Xpred[,i])
        Xsmooth[,i] <- Xfilt[,i]+new
    }
}

return(Xsmooth=Xsmooth)

}

##################################################################################

.smoothcov <- function(Sfilt, Spred, J){

pd <- (dim(Sfilt))[1]
tt <- (dim(Sfilt))[3]-1

Ssmooth <- array (0,dim=c(pd,pd,tt+1))
Ssmooth[,,tt+1] <- Sfilt[,,tt+1]

new <- J[,,tt]%*%(Sfilt[,,tt+1]-Spred[,,tt])%*%t(J[,,tt])
Ssmooth[,,tt] <- Sfilt[,,tt]+new
    
if(!tt<2){
    for (i in (tt-1):1){
        new <- J[,,i]%*%(new+Sfilt[,,i+1]-Spred[,,i])%*%t(J[,,i])
        Ssmooth[,,i] <- Sfilt[,,i]+new
    }
}

return(Ssmooth=Ssmooth)

}

###################################################################################

# lag-one covariance smoother

.lagoneCov <- function(Ssmoo, J){

pd <- (dim(Ssmoo))[1]
tt <- (dim(Ssmoo))[3]-1

Smix <- array (0,dim=c(pd,pd,tt))
    
for (i in 1:tt){
    Smix[,,i] <- Ssmoo[,,i]%*%t(J[,,i])
}

return(Smix=Smix)

}

#####################################################################################

ExtendedKS <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)),
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              v=matrix(0, nrow=length(a), ncol=ncol(Y)),
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              R=NULL, T=NULL, exQ=NULL, exV=NULL,
              controlF=NULL, controlQ=NULL, controlZ=NULL,
              controlV=NULL, ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, F, Q, Z, V: Hyper-parameters of the ssm
    erg <- ExtendedKF(Y = Y, a = a, S=S, F =F , Q = Q, Z = Z, V = V,
              u=u, w=w, v=v, eps=eps,
              R=R, T=NULL, exQ=exQ, exV=exV,
              controlF=controlF, controlQ=controlQ,
              controlZ=controlZ, controlV=controlV,
              ...)


    erg2 <- recSmoother(Y = Y, a = a, S=S, F =F , Q = Q, Z = Z, V = V,
              Xf = erg[["Xf"]], Xp = erg[["Xp"]],
              Xrf = erg[["Xrf"]], Xrp = erg[["Xrp"]],
              S0 = erg[["S0"]], S1 = erg[["S1"]],
              Sr0 = erg[["Sr0"]], Sr1 = erg[["Sr1"]],
              u=u, v=v,
              R=R, T=T, controlF=controlF,...)

    return(c(erg,erg2))
}

rLS.AO.EKSmoother <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)),
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              v=matrix(0, nrow=length(a), ncol=ncol(Y)),
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              R=NULL, T=NULL, exQ=NULL, exV=NULL,
              controlF=NULL, controlQ=NULL, controlZ=NULL,
              controlV=NULL, b = NULL, norm=Euclideannorm, ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, F, Q, Z, V: Hyper-parameters of the ssm

    erg <- rLS.AO.EKFilter(Y = Y, a = a, S=S, F =F , Q = Q, Z = Z, V = V,
              u=u, w=w, v=v, eps=eps,
              R=R, T=NULL, exQ=exQ, exV=exV,
              controlF=controlF, controlQ=controlQ,
              controlZ=controlZ, controlV=controlV,
               b = b, norm=norm, ...)
    erg2 <- recSmoother(Y = Y, a = a, S=S, F =F , Q = Q, Z = Z, V = V,
              Xf = erg[["Xf"]], Xp = erg[["Xp"]],
              Xrf = erg[["Xrf"]], Xrp = erg[["Xrp"]],
              S0 = erg[["S0"]], S1 = erg[["S1"]],
              Sr0 = erg[["Sr0"]], Sr1 = erg[["Sr1"]],
              u=u, v=v, R=R, T=T, controlF=controlF,...)

    return(c(erg,erg2))
}

rLS.IO.EKSmoother <- function (Y, a, S, F, Q, Z, V,
              u=matrix(0, nrow=length(a), ncol=ncol(Y)),
              w=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              v=matrix(0, nrow=length(a), ncol=ncol(Y)),
              eps=matrix(0, nrow=nrow(Y), ncol=ncol(Y)),
              R=NULL, T=NULL, exQ=NULL, exV=NULL,
              controlF=NULL, controlQ=NULL, controlZ=NULL,
              controlV=NULL, b = NULL, norm=Euclideannorm, ...)
{
##  arguments:
##  +  Y               : observations in a matrix with dimensions qd x tt
##  +  a, F, Q, Z, V: Hyper-parameters of the ssm

    erg <- rLS.IO.EKFilter(Y = Y, a = a, S=S, F =F , Q = Q, Z = Z, V = V,
              u=u, w=w, v=v, eps=eps,
              R=R, T=NULL, exQ=exQ, exV=exV,
              controlF=controlF, controlQ=controlQ,
              controlZ=controlZ, controlV=controlV,
               b = b, norm=norm, ...)
    erg2 <- recSmoother(Y = Y, a = a, S=S, F = F, Q = Q, Z = Z, V = V,
              Xf = erg[["Xf"]], Xp = erg[["Xp"]],
              Xrf = erg[["Xrf"]], Xrp = erg[["Xrp"]],
              S0 = erg[["S0"]], S1 = erg[["S1"]],
              Sr0 = erg[["Sr0"]], Sr1 = erg[["Sr1"]],
              u=u, v=v, R=R, T=T, controlF=controlF,...)

    return(c(erg,erg2))
}

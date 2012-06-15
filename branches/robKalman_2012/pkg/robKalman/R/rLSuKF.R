#######################################################
## 
##  rLS unscented Kalman filter routines
##  [cf. Ruckdeschel, 2009]
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2012-01-07, created: 2011-12-16)
##
#######################################################

.rLS.AO.UKFcorrstep <- function (y, x1, S1, Z, V, i, 
                           eps, w, controlZ,    # arguments of Z
                           exV, controlV,    # arguments of V
                           controlCorr,    # arguments of used filter
                           b=NULL, norm=Euclideannorm, ...)
{
##  y ... observations
##  x1 ... x_{t|t-1}, one-step-ahead prediction 
##  S1 ... S_{t|t-1}, conditional prediction error covariance matrix
##  Z ... Z(t, x_t, eps_t, w_t, control), function of observation equation
##  V ... V(t, x_t, exV_t, control), function of cov-matrix of observation error
##  i ... time index 
##  eps ... observation error \eps_t
##  w ... exogenous variable w_t 
##  controlZ ... control parameters of Z
##  exV ... exV_t, exogenous variables of V
##  controlV ... control parameters of V
##  controlCorr ... control parameters of used filter
##  b ... clipping height
##  norm ... norm, here: Euclidean norm

    gamma <- controlCorr$gamma
    Wm <- controlCorr$Wm
    Wc <- controlCorr$Wc
##  td <- controlPred$rd    # FIXME: Dimensionen bereits als Slot ins 
##                          #        Funktionsobjekt (bzw. zugehörige 
##                          #        S4-Klasse)!

    Zreturn <- Z(t=i, x1=x1, eps=eps, w=w, control=controlZ)
    D <- Zreturn$D

    Vreturn <- V(t=i, x1=x1, exV=exV, control=controlV)
    V <- Vreturn$V

    td <- ncol(V)

##  S1 <- bdsmatrix(pd, as.vector(S1))
    X1x.new <- .SigmaPoints(x=x1, gamma=gamma, Sigma=S1)

    Y <- apply(X1x.new, 2, .Eval, 
               G=Z, t=i, err=rep(0, td), ex=w, control=controlZ)
    yhat <- Y %*% Wm
    Sy <- .CovEst(X=Y, muX=yhat, Y=Y, muY=yhat, w=Wc) + D %*% V %*% t(D)
    Sxy <- .CovEst(X=X1x.new, muX=x1, Y=Y, muY=yhat, w=Wc)
    K  <- Sxy %*% ginv( Sy )
    DeltaY <- y - yhat
    dx <- K %*% DeltaY
    if(is.null(b)){
       bcal <- .getb.rls(b = b, controlCorr = controlCorr,
                         Z = Zreturn$C , S = S1, V = V, D = D, IO = FALSE)
       b <- rev(bcal$b.v)[1]
    }
    x0 <- x1 + Huberize(matrix(dx, ncol=1), b, norm=norm)
    S0 <- S1 - K %*% Sy %*% t(K)

    return(list(x0=x0, K=K, S0=S0, Delta=Sy, DeltaY=DeltaY, 
                controlCorr=controlCorr))

}



#######################################################
## 
##  rLS extended Kalman filter routines
##  [cf. Ruckdeschel, 2009]
##  author: Bernhard Spangl
##  version: 0.3 (changed: 2012-03-26, created: 2011-12-16)
##           added routine to determine b (P.R., 2012-04-10)
##
#######################################################

.rLS.AO.corrstep <- function (y, x1, S1, Z, V, i,
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

    Zreturn <- Z(t=i, x1=x1, eps=eps, w=w, control=controlZ)
    yhat <- Zreturn$y
    C <- Zreturn$C
    D <- Zreturn$D

    Vreturn <- V(t=i, x1=x1, exV=exV, control=controlV)
    V <- Vreturn$V

    Delta <- .getDelta(S1=S1, C=C, D=D, V=V)
    K <- .getKG(S1=S1, Z=C, Delta=Delta)
    DeltaY <- y - yhat 
    dx <- K %*% DeltaY

    if(is.null(b)){
#       controlCorr$verbose <- TRUE
       bcal <- .getb.rls(b = b, controlCorr = controlCorr,
                         Z = C , S = S1, V = V, D = D, IO = FALSE)
       controlCorr <- bcal
       b <- rev(bcal[["b.v"]])[1]
    }
#    if (length(b) > 1) {
#        b <- b[min(i, length(b))]
#    }
    x0 <- x1 + Huberize(matrix(dx, ncol=1), b, norm=norm)
    S0 <- .getcorrCov(S1=S1, K=K, Z=C)

    return(list(x0=x0, K=K, S0=S0, Delta=Delta, DeltaY=DeltaY, 
                controlCorr=controlCorr))

}

.rLS.IO.corrstep <- function (y, x1, S1, Z, V, i,
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

    Zreturn <- Z(t=i, x1=x1, eps=eps, w=w, control=controlZ)
    yhat <- Zreturn$y
    C <- Zreturn$C
    D <- Zreturn$D

    Vreturn <- V(t=i, x1=x1, exV=exV, control=controlV)
    V <- Vreturn$V

    Delta <- .getDelta(S1=S1, C=C, D=D, V=V)
    K <- .getKG(S1=S1, Z=C, Delta=Delta)
    DeltaY <- y - yhat 
    dx <- K %*% DeltaY
    de <- DeltaY - Z(t=i, x1=dx, eps=eps, w=w, control=controlZ)$y


    if(is.null(b)){
#       controlCorr$verbose <- TRUE
       bcal <- .getb.rls(b = b, controlCorr = controlCorr,
                         Z = C , S = S1, V = V, D = D, IO = TRUE)
       controlCorr <- bcal
       b <- rev(bcal[["b.v"]])[1]
    }
    
    SZ <- S1 %*% t(C)
    ZSZ <- C %*% SZ
    Zsigma <-  SZ %*% ginv(ZSZ)
#    piq <- diag(length(DeltaY)) -  C %*% Zsigma
#    dxq <- SZ %*% piq %*% Delta %*% DeltaY
#     print(dxq)
    dy <- DeltaY - Huberize(matrix(de, ncol=1), b, norm=norm)
    x0 <- x1 + Zsigma %*% dy #+ dxq
    S0 <- .getcorrCov(S1=S1, K=K, Z=C)
    return(list(x0=x0, K=K, S0=S0, Delta=Delta, DeltaY=DeltaY, 
                controlCorr=controlCorr))

}


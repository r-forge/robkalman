#######################################################
## 
##  classical unscented Kalman filter routines
##  author: Bernhard Spangl
##  version: 0.7 (changed: 2012-01-07, created: 2011-06-17)
##
#######################################################

.SigmaPoints <- function (x, gamma, Sigma)
{
##  x ... vector
##  gamma ... scaling parameter
##  Sigma ... covariance matrix
    sqrtSigma <- t(chol(Sigma))    # FIXME! (doesn't work for 
                                   # positiv semi-definite matricies)
##  sqrtSigma <- diag(sqrt(diag(Sigma)))    # Mist!
##  Q <- chol(Sigma, pivot=TRUE)   # FIXED!
##  pivot <- attr(Q, "pivot")
##  sqrtSigma <- t(Q[, order(pivot)])
##  ch <- gchol(Sigma)             # better!!!
##  L <- as.matrix(ch)
##  sqrtD <- diag(sqrt(diag(ch)))
##  sqrtSigma <- L %*% sqrtD
    SP1 <- as.vector(x) %o% rep(1, ncol(Sigma)) + gamma * sqrtSigma
    SP2 <- as.vector(x) %o% rep(1, ncol(Sigma)) - gamma * sqrtSigma
    cbind(x, SP1, SP2)
}

##  .SigmaPointsCalc <- function (x0, S0, Q, V, i, 
##                                v, eps, 
##                                exQ, controlQ,    # arguments of Q
##                                exV, controlV,    # arguments of V
##                                controlSigma, ...)    # arguments of used filter
##  {
##  ##  x0 ... x_{t-1|t-1}, filter estimate
##  ##  S0 ... S_{t-1|t-1}, conditional filtering error covariance matrix
##  ##  Q ... Q(t, x_{t-1}, exQ_{t-1}, control), function of innovations cov-matrix
##  ##  V ... V(t, x_{t-1}, exV_t, control), function of cov-matrix of 
##  ##                                       observation error
##  ##  i ... time index
##  ##  v ... innovations v_t
##  ##  eps ... observation error \eps_t
##  ##  exQ ... exQ_{t-1}, exogenous variables of Q
##  ##  controlQ ... control parameters of Q
##  ##  exV ... exV_t, exogenous variables of V
##  ##  controlV ... control parameters of V
##  ##  controlSigma ... control parameters of used filter
##  
##      gamma <- controlSigma$gamma
##      pd <- controlSigma$pd
##      rd <- controlSigma$rd
##      td <- controlSigma$td
##  
##      Qreturn <- Q(t=i, x0=x0, exQ=exQ, control=controlQ)
##      Q <- Qreturn$Q
##      Vreturn <- V(t=i, x1=x0, exV=exV, control=controlV)
##      V <- Vreturn$V
##  
##      x <- c(x0, v, eps)
##  ##  Sigma <- blockDiag(S0, Q, V)
##      Sigma <- bdsmatrix(c(pd, rd, td), 
##                         c(as.vector(S0), as.vector(Q), as.vector(V)))
##      SigmaPoints <- .SigmaPoints(x=x, gamma=gamma, Sigma=Sigma)
##      controlSigma$X0x <- SigmaPoints[1:pd, ]
##      controlSigma$Xv <- SigmaPoints[(pd+1):(pd+rd), ]
##      controlSigma$Xe <- SigmaPoints[(pd+rd+1):(pd+rd+td), ]
##  
##      return(controlSigma)
##  
##  }

.CovEst <- function (X, muX, Y, muY, w)
{
##  X ... matrix of sigma points 
##  muX ... mean vector of matrix X
##  Y ... matrix of sigma points 
##  muY ... mean vector of matrix Y
##  w ... vector of weights
    X <- X - as.vector(muX) %o% rep(1, ncol(X))
    X <- X * (rep(1, nrow(X)) %o% w)
    Y <- Y - as.vector(muY) %o% rep(1, ncol(Y))
    X %*% t(Y)
}

.Eval <- function (vec, G, t, err, ex, control) 
{
    G <- G(t, vec, err, ex, control)
    return(G[[1]])
}

.cUKFinitstep <- function (a, S, controlInit, ...)
{
##  a ... E(x_0)
##  S ... S_{0|0} = Cov(x_0 - x_{0|0}) 
##                = E((x_0 - x_{0|0})(x_0 - x_{0|0})^\top), error covariance
##  controlInit ... list containing alpha, beta and kappa, 
##                  and dimensions of ssm

    alpha <- controlInit$alpha
    beta <- controlInit$beta
    kappa <- controlInit$kappa
##  pd <- controlInit$pd

    pd <- length(a)
    L <- pd

    lambda <- alpha^2*(L + kappa) - L 
    Wm <- Wc <- rep(1/(2*(L + lambda)), 2*L)
    Wm <- c(lambda/(L + lambda), Wm)
    Wc <- c(lambda/(L + lambda) + (1 + alpha^2 + beta), Wc)
    gamma <- sqrt(L + lambda)

    controlInit$gamma <-gamma
    controlInit$Wm <- Wm
    controlInit$Wc <- Wc

    return(list(x0=a, S0=S, controlInit=controlInit))

}

.cUKFpredstep <- function (x0, S0, F, Q, i, 
                           v, u, controlF,    # arguments of F
                           exQ, controlQ,    # arguments of Q
                           controlPred, ...)    # arguments of used filter
{
##  x0 ... x_{t-1|t-1}, filter estimate
##  S0 ... S_{t-1|t-1}, conditional filtering error covariance matrix
##  F ... F(t, x_{t-1}, v_t, u_t, control), function of state equation
##  Q ... Q(t, x_{t-1}, exQ_{t-1}, control), function of innovations cov-matrix
##  i ... time index
##  v ... innovations v_t
##  u ... u_{t-1}, exogenous variables of F
##  controlF ... control parameters of F
##  exQ ... exQ_{t-1}, exogenous variables of Q
##  controlQ ... control parameters of Q
##  controlPred ... control parameters of used filter

    gamma <- controlPred$gamma
    Wm <- controlPred$Wm
    Wc <- controlPred$Wc
##  rd <- controlPred$rd    # FIXME: Dimensionen bereits als Slot ins 
##                          #        Funktionsobjekt (bzw. zugehörige 
##                          #        S4-Klasse)!

    Freturn <- F(t=i, x0=x0, v=v, u=u, control=controlF)
    B <- Freturn$B

    Qreturn <- Q(t=i, x0=x0, exQ=exQ, control=controlQ)
    Q <- Qreturn$Q

    rd <- ncol(Q)

##  S0 <- bdsmatrix(pd, as.vector(S0))
    X0x <- .SigmaPoints(x=x0, gamma=gamma, Sigma=S0)

    X1x <- apply(X0x, 2, .Eval, 
                 G=F, t=i, err=rep(0, rd), ex=u, control=controlF)
    x1 <- X1x %*% Wm
    S1 <- .CovEst(X=X1x, muX=x1, Y=X1x, muY=x1, w=Wc) + B %*% Q %*% t(B) 

##  controlPred$X1x <- X1x

    return(list(x1=x1, S1=S1, controlPred=controlPred))

}

.cUKFcorrstep <- function (y, x1, S1, Z, V, i, 
                           eps, w, controlZ,    # arguments of Z
                           exV, controlV,    # arguments of V
                           controlCorr, ...)    # arguments of used filter
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
    x0 <- x1 + K %*% DeltaY
    S0 <- S1 - K %*% Sy %*% t(K)

    return(list(x0=x0, K=K, S0=S0, Delta=Sy, DeltaY=DeltaY, 
                controlCorr=controlCorr))

}



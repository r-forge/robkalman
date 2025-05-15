#######################################################
## 
##  classical unscented Kalman filter routines
##  author: Bernhard Spangl
##  version: 0.1 (changed: 2011-06-17, created: 2011-06-17)
##
#######################################################

.SigmaPoints <- function (x, gamma, Sigma)
{
##  x ... vector
##  gamma ... scaling parameter
##  Sigma ... covariance matrix
    sqrtSigma <- t(chol(Sigma))
    SP1 <- x %o% rep(1, ncol(Sigma)) + gamma * sqrtSigma
    SP2 <- x %o% rep(1, ncol(Sigma)) - gamma * sqrtSigma
    cbind(x, SP1, SP2)
}

.CovEst <- function (X, muX, Y, muY, w)
{
##  X ... matrix of sigma points 
##  muX ... mean vector of matrix X
##  Y ... matrix of sigma points 
##  muY ... mean vector of matrix Y
##  w ... vector of weights
    X <- X - muX %o% rep(1, ncol(X))
    X <- X * rep(1, nrow(X)) %o% w
    Y <- Y - muY %o% rep(1, ncol(Y))
    X %*% Y
}

.Eval <- function (vec, G, t, ex, control, dim) 
{
    v1 <- vec[1:dim]
    v2 <- vec[-(1:dim)]
    G <- G(t, v1, v2, ex, control)
    return(G[[1]])
}

.cUKFinitstep <- function (a, S, ...)
{
##  a ... E(x_0)
##  S ... S_{0|0} = Cov(x_0 - x_{0|0}) 
##                = E((x_0 - x_{0|0})(x_0 - x_{0|0})^\top), error covariance
##  control ... list containing alpha, beta and kappa

    alpha <- control$alpha
    beta <- control$beta
    kappa <- control$kappa

    pd <- length(a)

    lambda <- alpha^2*(pd + kappa) - pd 
    Wm <- Wc <- rep(1/(2*(pd + lambda)), 2*pd)
    Wm <- c(lambda/(pd + lambda), Wm)
    Wc <- c(lambda/(pd + lambda) + (1 + alpha^2 + beta), Wc)
    gamma <- sqrt(pd + lambda)

    control$gamma <-gamma
    control$Wm <- Wm
    control$Wc <- Wc

    return(list(x0=a, S0=S, control=control))

}

.cUKFpredstep <- function (x0, S0, F, Q, i, 
                           v, u, controlF,    # arguments of F
                           exQ, controlQ,    # arguments of Q
                           control, ...)    # arguments of used filter
{
##  x0 ... x_{t-1|t-1}, filter estimate
##  S0 ... S_{t-1|t-1}, conditional filtering error covariance matrix
##  F ... F(t, x_{t-1}, u_t, v_t, control), function of state equation
##  Q ... Q(t, x_{t-1}, exQ_{t-1}, control), function of innovations cov-matrix
##  i ... time index
##  v ... innovations v_t
##  u ... u_{t-1}, exogenous variables of F
##  controlF ... control parameters of F
##  exQ ... exQ_{t-1}, exogenous variables of Q
##  controlQ ... control parameters of Q
##  control ... control parameters of used filter

    gamma <- control$gamma
    Wm <- control$Wm
    Wc <- control$Wc

    Qreturn <- Q(t=i, x0=x0, exQ=exQ, control=controlQ)
    Q <- Qreturn$Q

    X0x <- .SigmaPoints(x=x0, gamma=gamma, Sigma=S0)
    Xv <- .SigmaPoints(x=v, gamma=gamma, Sigma=Q)

    X1x <- apply(rbind(X0x, Xv), 2, .Eval, 
                 G=F, t=i, ex=u, control=controlF, dim=length(x0))
    x1 <- X1x %*% Wm
    S1 <- .CovEst(X=X1x, muX=x1, Y=X1x, muY=x1, w=Wc)

    control$X1x <- X1x

    return(list(x1=x1, S1=S1, control=conrol))

}

.cUKFcorrstep <- function (y, x1, S1, Z, V, i, 
                           eps, w, controlZ,    # arguments of Z
                           exV, controlV,    # arguments of V
                           control, ...)    # arguments of used filter
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
##  control ... control parameters of used filter

    gamma <- control$gamma
    Wm <- control$Wm
    Wc <- control$Wc
    X1x <- control$X1x

    Vretrun <- V(t=i, x1=x1, exV=exV, control=controlV)
    V <- Vreturn$V

    Xe <- .SigmaPoints(x=eps, gamma=gamma, Sigma=V)

    Y <- apply(rbind(X1x, Xe), 2, .Eval, 
               G=Z, t=i, ex=w, control=controlZ, dim=nrow(X1x))
    yhat <- Y %*% Wm
    Sy <- .CovEst(X=Y, mnX=yhat, Y=Y, muY=yhat, w=Wc)
    Sxy <- .CovEst(X=X1x, muX=x1, Y=Y, muY=yhat, w=Wc)
    K  <- Sxy %*% ginv( Sy )
    DeltaY <- y - yhat
    x0 <- x1 + K %*% DeltaY
    S0 <- S1 - K %*% Sy %*% t(K)

    return(list(x0=x0, K=K, S0=S0, Delta=Sy, DeltaY=DeltaY))

}



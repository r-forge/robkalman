#######################################################
## 
##  extended Kalman filter simulation routines_redesigned
##  [original code by Peter Ruckdeschel]
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2011-12-10, created: 2011-12-09)
##
#######################################################

rcvmvnorm <-  function (runs, mi, Si, mc, Sc, r)
{
    U <- rbinom(runs, size = 1, prob = r)
    (1-U) * mvrnorm(runs, mi, Si) + U * mvrnorm(runs, mc, Sc)
}

simulateStateEKF <- function (a, S, F, Qi, mc=0, Qc=Qi, u, 
                              exQi=NULL, controlQi=NULL, 
                              exQc=NULL, controlQc=NULL, 
                              controlF=NULL, R=NULL,
                              tt, r=0)
{
##  a ... E(x_0)
##  S ... S_{0|0} = Cov(x_0 - x_{0|0}) 
##                = E((x_0 - x_{0|0})(x_0 - x_{0|0})^\top), error covariance
##  F ... F(t, x_{t-1}, v_t, u_t, control), function of state equation
##  Qi, Qc ... function of innovations covariance matrix
##  mc ... mean vector of contaminating distribution
##  u ... exogenous variables, ?? x tt matrix
##  runs=1 ... number of simulations, has to be 1 because of F() !
##  exQ_ ... exogenous variables of Qi and Qc
##  control_ ... control paramaters, list
##  R ... selection matrix of innovations noise 
##  tt ... number of observations, time
##  r ... proportion of contamination

    runs <- 1

    if (!(is.function(F))) F <- createF(F=F, R=R)
    if (!(is.function(Qi))) Qi <- createQ(Q=Qi)
    if (!(is.function(Qc))) Qc <- createQ(Q=Qc)

    pd <- length(a)
    rd <- dim(Qi(t=1, x0=a, exQ=exQi, control=controlQi)$Q)[2]
##  if (length(dim(F)) < 3) F <- array(F, dim=c(pd, pd, tt))
    if (!is.matrix(mc)) mc <- matrix(mc, rd, tt)
    if (!is.matrix(u)) u <- matrix(u, length(u), tt)
##  if(length(dim(Qi))<3) Qi <- array(Qi, dim=c(pd,pd,tt))
##  if(length(dim(Qc))<3) Qc <- array(Qc, dim=c(pd,pd,tt))
    states <- matrix(NA, nrow=pd, ncol=tt+1)
    traj <- matrix(NA, nrow=pd, ncol=tt+1)
    states[, 1] <- mvrnorm(runs, m=a, S=S)
    traj[, 1] <- a
    for (i in (1:tt)) {
         Qit <- Qi(t=i, x0=states[, i], exQ=exQi, control=controlQi) 
         Qct <- Qc(t=i, x0=states[, i], exQ=exQc, control=controlQc) 
         vt <- rcvmvnorm(runs, mi=rep(0, rd), Si=Qit$Q, 
                               mc=mc[, i],    Sc=Qct$Q, r=r)
         states[ , i+1] <- F(t=i, x0=states[, i], v=vt, u=u[, i], 
                             control=controlF)$x1
         traj[, i+1] <- F(t=i, x0=states[, i], v=rep(0, rd), u=u[, i], 
                          control=controlF)$x1
    }
    return(list(states=states, traj=traj))
}

simulateObsEKF <- function (X, Z, Vi, mc=0, Vc=Vi, w,
                            exVi=NULL, controlVi=NULL, 
                            exVc=NULL, controlVc=NULL, 
                            controlZ=NULL, T=NULL, r=0)
{
##  X ... states, pd x (tt+1) matrix
##  Z ... Z(x_t, t), function of observation equation
##  Vi, Vc ... covariance matrix of observation error
##  mc ... mean vector of contaminating distribution
##  w ... exogenous variables, ?? x tt matrix
##  runs=1 ... number of simulations, has to be 1 because of F() !
##  exV_ ... exogenous variables of Vi and Vc
##  control_ ... control paramaters, list
##  T ... selection matrix of observation noise 
##  r ... proportion of contamination

    runs <- 1

    if (!(is.function(Z))) Z <- createZ(Z=Z, T=T)
    if (!(is.function(Vi))) Vi <- createV(V=Vi)
    if (!(is.function(Vc))) Vc <- createV(V=Vc)

    tt <- (dim(X))[2]-1
    if (!is.matrix(w)) w <- matrix(w, length(w), tt)
##  pd <- (dim(X))[1]
##  qd <- if (!is.null(dim(Vi))) (dim(Vi))[1] else 1
    td <- dim(Vi(t=1, x1=X[, 2], exV=exVi, control=controlVi)$V)[2]
    qd <- length(Z(t=1, x1=X[, 2], eps=rep(0, td), w=w[, 1], 
                   control=controlZ)$y)
##  if(length(dim(Z))<3) Z <- array(Z, dim=c(qd,pd,tt))
    if (!is.matrix(mc)) mc <- matrix(mc, td, tt)
##  if (length(dim(Vi)) < 3) Vi <- array(Vi, dim=c(qd, qd, tt))
##  if (length(dim(Vc)) < 3) Vc <- array(Vc, dim=c(qd, qd, tt))
    obs <- matrix(NA, nrow=qd, ncol=tt)
    for (i in 1:tt) {
        Vit <- Vi(t=i, x1=X[, i+1], exV=exVi, control=controlVi)
        Vct <- Vc(t=i, x1=X[, i+1], exV=exVc, control=controlVc)
        epst <- rcvmvnorm(runs, rep(0, td), Vit$V, 
                                mc[, i],    Vct$V, r=r)
        obs[, i] <- Z(t=i, x1=X[, i+1], eps=epst, w=w[, i], 
                      control=controlZ)$y
    }
    return(obs)
}



.ARmodel <- function (x, p)
{
##################################
##
##  R-function: .ARmodel - creates design matrix 'Z' and 
##              response vector 'y' of an AR(p) model (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-21)
##
##################################

##  x ... univarite time series (vector)
##  p ... order of AR(p) process

    n <- length(x)
    y <- x[(p+1):n]
    Z <- embed(x, p)[1:(n-p), ]
    return(list(y=y, Z=as.matrix(Z)))
}

.invCp <- function (p, s, Phi)
{
##################################
##
##  R-function: .invCp - computes the inverse p x p covariance matrix
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  p ... order of AR(p) process
##  s ... vector of 'sx' and innovation scale estimates for 
##        AR(p-1) models of order 1 to (p-1)
##  Phi ... (p-1)x(p-1) matrix of AR(p-1) model parameters

    if (p > 1) {
        M1 <- matrix(rep(rev(s), p), p)
        M2 <- cbind(rep(0, (p-1)), (-1)*Phi)
        M2 <- rbind(M2, rep(0, p))
        diag(M2) <- 1
        Ap <- (1/M1)*M2
        invCp <- t(Ap)%*%Ap 
    } else { 
        invCp <- 1/s[1]^2
    }
    return(invCp)
}

.Weights <- function (p, Z, invCp, type, psi2, c)
{
##################################
##
##  R-function: .Weights - computes weights for 
##              Mallows- or Schweppe-type GM-estimates (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  p ... order 
##  Z ... AR(p) model matrix
##  invCp ... martix from function '.invCp' to compute metric
##  type ... Mallows- or Schweppe-type GM-estimates (character)
##  psi2 ... influence function (character)
##  c ... tuning constant 

    psi <- .psi(psi2)
    d <- sqrt(diag(Z%*%invCp%*%t(Z))/p)
    if (psi2 == "Huber") {
        v <- psi(d, k=c)/d
    } else if (psi2 == "Tukey") {
    v <- psi(d, c=c)/d
    } else if (psi2 == "Ident") {
        v <- rep(1, nrow(Z)) 
    } else {
        warning("error in function '.Weights': psi function ", psi2, 
            "not defined \n")
    }
    if (type=="Mallows") {
        u <- rep(1, length(v))
    } else if (type=="Schweppe") { 
        u <- v
    } else {
        warning("error in function '.Weights': wrong GM-estimates type \n")
    }
    return(list(u=u, v=v))
}

.startval <- function (y, Z, tol)
{
##################################
##
##  R-function: .startval - computes appropriate starting values
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  y ... response vector of AR(p) model
##  Z ... design matrix of AR(p) model
##  tol ... tolerance level 

    type <- FALSE     # hard-coded, TRUE as proposed in [MarT82b], [StoDut87]
                      #             FALSE as proposed in [MarZ78]
    ar.ls <- lm.fit(Z, y, tol)
    phi <- ar.ls$coefficients
    if (type) {
        s <- sqrt(sum((ar.ls$residuals)^2)/ar.ls$df.residual)
    } else {
        s <- mad(ar.ls$residuals)
    }
    return(list(phi=phi, s=s))
}

.BH <- function (k=1.345) 
{
##################################
##
##  R-function: .BH - computes appropriate constant to obtain a 
##              consistent estimate for sigma when using Huber's psi function
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  k ... tuning constant (default: k=1.345)

    -2*k*dnorm(k) + 2*pnorm(k) + 2*k^2*(1-pnorm(k)) -1
}

.BB <- function (c=4.685)
{
##################################
##
##  R-function: .BB - computes appropriate constant to obtain a 
##              consistent estimate for sigma when using Tukey's psi function
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  c ... tuning constant (default: c=4.685)

    (2/c^7)*dnorm(c)*(c^6 - 13*c^4 + 105*c^2 - 945) + 
    (2/c^8)*pnorm(c)*(c^8 - 12*c^6 + 90*c^4 - 420*c^2 + 945) -  
    (c^8 - 12*c^6 + 90*c^4 - 420*c^2 + 945)/c^8 
}

.weights <- function (r, s, u, v, psi1, ...)
{
##################################
##
##  R-function: .weights - computes appropriate weights for reweighting
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  r ... residuals
##  s ... innovations scale parameter
##  u ... weights
##  v ... weights
##  psi1 ... influence function (character)
##  ... ... passing tuning constants to influence functions

    psi <- .psi(psi1)
    n <- length(r)
    w <- rep(NA, n)
    for (i in 1:n) {
        if (r[i] == 0) {
            if (u[i] != 0) {
                w[i] <- v[i]/u[i]
            } else {
                w[i] <- 1
            }
        } else if (u[i] != 0) {
            dummy <- r[i]/s
            w[i] <- v[i]*psi(dummy/u[i], ...)/dummy
        } else if (psi1 == "Ident") {
            w[i] <- 1
        } else {
            w[i] <- 0
        }
    }
    return(w)
}

.IWLS <- function (y, Z, phi.ini, s.ini, u, v, psi1, niter, tol, ...)
{
##################################
##
##  R-function: .IWLS - iteratively reweighted least squares algorithm 
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  y ... response vector of AR(p) model
##  Z ... design matrix of AR(p) model
##  phi.ini ... initial AR(p) model parameters
##  s.ini ... initial innovations scale parameter
##  u ... weights
##  v ... weights
##  psi1 ... influence function (character)
##  niter ... maximal number of iterations
##  tol ... tolerance level 
##  ... ... passing tuning constants to influence functions

    stop1 <- sqrt(diag(solve(t(Z)%*%Z)))
    B <- NA
    phi <- phi.ini
    s.new <- s.ini
    iter <- 0
    psi <- .psi(psi1)
    if (psi1=="Huber") { 
        B <- .BH(...)
    } else { 
        B <- .BB(...)
    }

    while (iter < niter) {
        iter <- iter + 1
        r <- y - Z%*%phi 
        s.old <- s.new
        s2 <- (1/(B*sum(u*v)))*sum(u*v*(psi(r/(u*s.old), ...))^2)*s.old^2
        s.new <- sqrt(s2)
        w <- .weights(r, s.new, u, v, psi1, ...)
        ar.wls <- lm.wfit(Z, y, w, tol)
        tau <- ar.wls$coefficients - phi
        omega <- 1     # hard-coded, 0 < omega < 2, as proposed in [StoDut87]
        phi <- phi + omega*tau
        stop2 <- abs(c(s.old - s.new, omega*tau)) < tol*s.new*c(1, stop1)
        if (sum(stop2) == length(stop2)) break
    }
    return(list(phi=phi, s=s.new, w=w, B=B, niter=iter))
}

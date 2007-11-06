ACMfilt <- function (x, gm, s0=0, 
                     psi="Hampel", a=2.5, b=a, c=5.0, 
                     flag="weights", lagsmo=TRUE)
{
###########################################
##
##  R-function: ACMfilt - approximate conditional-mean filtering (wrapper)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-21)
##  References: 
##  [Mart79c] R.D. Martin, Approximate Conditional-mean Type Smoothers 
##                and Interpolators (1979)
##  [Mart81b] R.D. Martin, Robust Methods for Time Series (1981)
##  [MarT82b] R.D. Martin & D.J. Thomson, Robust-resistent Spectrum 
##                Estimation (1982)
##
###########################################

##  Paramters:
##  x ... univariate time series (vector)
##  gm ... list as produced by function 'arGM' which includes components 
##         'ar' containing the AR(p) coefficient estimates, 'sinnov' containing 
##         innovation scale estiamtes from AR(p) fits of orders 1 through p;
##         'Cx' containing an estimate of the p by p autocovariance matrix, 
##         and 'mu', the estimated mean of 'x'. 
##  s0 ... scale of nominal Gaussian component of the additive noise
##  psi ... influence function to be used (default: "Hampel", 
##          only Hampel's psi function available at the moment)
##  a, b, c ... tuning constants for Hampel's psi-function
##              (defaul: a=b=2.5, c=5.0)
##  flag ... character, if "weights" (default), use psi(t)/t to calculate 
##           the weights; if "deriv", use psi'(t)
##  lagsmo ... logical, if TRUE (default) lag p-1 smoothing is performed; 
##             if FALSE filtering from the top of ^X_t is performed

##  Variable definitions:

    N <- length(x)
    phi <- gm$ar
    p <- length(phi)
    si <- gm$sinnov[p]
    Cx <- gm$Cx
    Phi <- cbind(rbind(phi[-p], diag(rep(1, (p-1)))), c(phi[p], rep(0, (p-1))))
    Q <- diag(rep(0, p))
    Q[1, 1] <- si^2
    
    m0 <- rep(0, p)
    H <- matrix(c(1, rep(0, (p-1))), 1, p)
    V <- matrix(s0^2)
    psi <- .psi(psi)
    
    ##  Centering: 
    x <- x - gm$mu
    ACMres <- ACMfilter(Y=matrix(x,1,N), a=m0, S=Cx, F=Phi, Q=Q, Z=H, V=V, s0=s0, psi=psi, apsi=a, bpsi=b, cpsi=c, flag=flag)

    X.ck <- ACMres$Xf;  X.ck <- X.ck[,2:(N+1)]
    X   <- ACMres$Xrf; X <- X[,2:(N+1)]
    st <- as.numeric(unlist(ACMres$rob1L))


    if (!lagsmo) {
        x.ck <- X.ck[1, ]
        x <- X[1, ]
    } else {
        x.ck <- c(X.ck[p, p:N], X.ck[(p-1):1, N])
        x <- c(X[p, p:N], X[(p-1):1, N])
    }

    ARmodel <- .ARmodel(x, p)
    y <- ARmodel$y
    Z <- ARmodel$Z
    r <- resid(lm.fit(Z, y))
    
    return(list(filt.ck=x.ck +gm$mu, filt=x + gm$mu, st=st, 
                r=c(rep(NA, p), r)))

}

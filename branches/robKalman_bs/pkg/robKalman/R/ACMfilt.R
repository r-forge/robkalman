ACMfilt <- function (x, gm, s0=0, 
                     psi="Hampel", a=2.5, b=a, c=5.0, 
                     flag="weights", lagsmo=TRUE)
{
###########################################
##
##  R-function: ACMfilt - approximate conditional-mean filtering (wrapper)
##  author: Bernhard Spangl
##  version: 1.2 (2009-07-30)
##  References: 
##  [Mart79c] R.D. Martin, Approximate Conditional-mean Type Smoothers 
##                and Interpolators (1979)
##  [Mart81b] R.D. Martin, Robust Methods for Time Series (1981)
##  [MarT82b] R.D. Martin & D.J. Thomson, Robust-resistent Spectrum 
##                Estimation (1982)
##
###########################################

##  Paramters:
##  x ... univariate time series (vector) or runs x tt matrix
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
    if (is.null(dim(x))) {
        runs <- 1
        tt <- length(x) 
    } else {
        runs <- dim(x)[1]
        tt <- dim(x)[2]
    }
    Y <- array(x, dim=c(1, dim(x)))
    phi <- gm$ar
    pd <- length(phi)
    m0 <- rep(0, pd)
    Cx <- gm$Cx
    Phi <- cbind(rbind(phi[-pd], diag(rep(1, (pd-1)))), 
                 c(phi[pd], rep(0, (pd-1))))
    F <- array(Phi, dim=c(pd, pd, tt))
    si <- gm$sinnov[pd]
    Q <- array(0, dim=c(pd, pd, tt))
    Q[1, 1, ] <- si^2
    
    Z <- array(0, dim=c(1, pd, tt))
    Z[1, 1, ] <- 1
    V <- array(s0^2, dim=c(1, 1, tt))
    psi <- .psi(psi)

    ##  Centering: 
    x <- x - gm$mu
    ACMres <- ACMfilter(Y=Y, a=m0, S=Cx, F=F, Q=Q, Z=Z, V=V, 
                        psi=psi, apsi=a, bpsi=b, cpsi=c, flag=flag, 
                        dropRuns=FALSE, 
                        saveOpt=FALSE, dimsCheck=c(pd, 1, runs, tt))

    X.ck <- ACMres$Xf[, , 2:(tt+1)]
    X <- ACMres$Xrf[, , 2:(tt+1)]
    st <- matrix(unlist(ACMres$rob0L), runs, tt)

    if (!lagsmo) {
        x.ck <- X.ck[1, , ]
        x <- X[1, , ]
    } else {
        if (runs==1) {
            x.ck <- c(X.ck[pd, , pd:tt], t(X.ck[(pd-1):1, , tt]))
            x <- c(X[pd, , pd:tt], t(X[(pd-1):1, , tt]))
        } else {
            x.ck <- cbind(X.ck[pd, , pd:tt], t(X.ck[(pd-1):1, , tt]))
            x <- cbind(X[pd, , pd:tt], t(X[(pd-1):1, , tt]))
        }
    }

    return(list(filt.ck=x.ck +gm$mu, filt=x + gm$mu, st=st))

}

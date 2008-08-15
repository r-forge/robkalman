arGM <- function (x, order=1, 
                  chr=1.5, iterh=maxiter, cbr=5.0, iterb=maxiter, 
                  psi2="Tukey", c=4.0, type="Mallows", 
                  k=1.5, maxiter=100, tol=1e-08, equal.LS=FALSE, ...) 
{
##################################
##
##  R-function: arGM - GM-estimates for AR parameters
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##  References: 
##  [MarZ78]   R.D. Martin & J.E. Zeh, Generalized M-estimates for  
##                 Autoregression Including Small-sample Efficiency 
##                 Robustness (1978)
##  [Mart80]   R.D. Martin, Robust Estimation of Autoregressive Models (1980)
##  [MarT82b]  R.D. Martin & D.J. Thomson, Robust-resistent Spectrum 
##                 Estimation (1982)
##  [StoDut87] N. Stockinger & R. Dutter, Robust Time Series Analysis: 
##                 A Survey (1987)
##
##################################

##  Parameters:
##  x ... univarite time series (vector) 
##  order ... order of AR(p) process (default: order=1)
##  chr ... tuning konstant for Huber's psi function (default: chr=1.5)
##  iterh ... number of iterations for IWLS-alogrithm using 
##            Huber's psi function (default: iterh=maxiter)
##  cbr ... tuning konstant for Tukey's psi function (default: cbr=5.0)
##  iterb ... number of iterations for IWLS-alogrithm using 
##            Tukey's psi function (default: iterb=maxiter)
##  psi2 ... influence function to determine the 'largness of z_i', 
##           either "Ident", "Huber" or "Tukey" (default: "Tukey")
##  c ... tuning constant for psi2 (default: c=4.0)
##  type ... type of GM-estimates, either "Mallows" or "Schweppe" 
##           (default: "Mallows")
##  k ... tuning constant for centering (default: k=1.5)
##  maxiter ... maximal number of iteration (default: maxiter=100)
##  tol ... tolerance level (default: tol=1e-08)
##  equal.LS ... logical, for testing purpose only (default: equal.LS=FALSE)
##  ... ... further parameters to be passed to the functions 'HuberM' or 
##          'hubers'  

    ##  Variable definitions:
    
    s <- c()
    Phi <- matrix()
    w <- NA
    BH <- BB <- NA
    niterh <- niterb <- niter.testing <- NA
    
    ##  Centering:
    
    ## x.huber <- HuberM(x, ...)     # as proposed in [StoDut87]
    x.huber <- hubers(x, ...)     # as proposed in [MarZ78], [Mart80]
    x <- x - x.huber$mu
    sx <- x.huber$s
    
    ##  Main:
    
    for (p in 1:order) {
        ARmodel <- .ARmodel(x, p)
        y <- ARmodel$y
        Z <- ARmodel$Z
        invCp <- .invCp(p, c(sx, s), Phi)
        Weights <- .Weights(p, Z, invCp, type, psi2, c)
        u <- Weights$u
        v <- Weights$v
        startval <- .startval(y, Z, tol)
        phi <- startval$phi
        s[p] <- startval$s
        if (equal.LS) {     # for testing purpose only
            psi1 <- "Ident"
            niter <- maxiter
            IWLS <- .IWLS(y, Z, phi, s[p], u, v, psi1, niter, tol)
            phi <- IWLS$phi
            w <- IWLS$w
        niter.testing <- IWLS$niter
        } else {
            if ((iterh > 0) & (is.numeric(iterh))) {
                psi1 <- "Huber"
                niter <- iterh
                IWLS <- .IWLS(y, Z, phi, s[p], u, v, psi1, niter, tol, k=chr)
                phi <- IWLS$phi
                s[p] <- IWLS$s
                w <- IWLS$w
                BH <- IWLS$B
            niterh <- IWLS$niter
            }
            if ((iterb > 0) & (is.numeric(iterb))) {
                psi1 <- "Tukey"
                niter <- iterb
                IWLS <- .IWLS(y, Z, phi, s[p], u, v, psi1, niter, tol, c=cbr)
                phi <- IWLS$phi
                s[p] <- IWLS$s
                w <- IWLS$w
                BB <- IWLS$B
            niterb <- IWLS$niter
            }
        }
        if (p > 1) {
            Phi <- cbind(rep(0, (p-1)), Phi)
            Phi <- rbind(phi, Phi)
        } else {
            Phi <- as.matrix(phi)
        }
    }
    Cx <- solve(invCp)
    
    return(list(ar=phi, sinnov=s, Cx=Cx, mu=x.huber$mu, sx=sx, u=u, v=v, w=w, 
                BH=BH, BB=BB, 
                niterh=niterh, niterb=niterb, niter.testing=niter.testing))
    
}

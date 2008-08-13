##############################################################
# psi functions
##############################################################


psiLS <- function(t, rho=FALSE)
{
##################################
##
##  R-function: psiLS - 'psi-function' for LS estimation
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  t ... input vector
##  rho ... logical, wether the psi- (default) or rho-function is used

    if (!rho) {
        t
    } else {
        t^2/2
    }
}

psiHuber <- function (t, k=1.345, rho=FALSE) 
{
##################################
##
##  R-function: psiHuber - Huber's psi-function
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  t ... input vector
##  k ... tuning constant (default: k=1.345)
##  rho ... logical, wether the psi- (default) or rho-function is used

    if (!rho) {
        pmin(k, pmax(-k, t))
    } else {
        r <- abs(t)
        i <- r < k
        r[i] <- r[i]^2/2
        r[!i] <- k * (r[!i] - k/2)
        r
    }
} 

psiTukey <- function (t, c=4.685, rho=FALSE)
{
##################################
##
##  R-function: psiTukey - Tukey's psi-function
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-26)
##
##################################

##  t ... input vector
##  c ... tuning constant (default: c=4.685)
##  rho ... logical, wether the psi- (default) or rho-function is used

    if (!rho) {
        r <- abs(t)
        i <- r < c
        t[i] <- t[i]*(1-(t[i]/c)^2)^2
        t[!i] <- 0
        t
    } else {
        pmin((c^2/6)*(1-(1-(t/c)^2)^3), c^2/6)
    }
}


psiHampel <- function (t, a=2, b=4, c=8, flag="psi")
{
###########################################
##
##  R-function: psiHampel - Hampel's psi-function
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-21)
##
###########################################

##  Paramters:
##  t ... input vector
##  a, b, c ... tuning constants
##  flag ... character, either "psi", "weights" or "deriv" to use 
##           psi-function (default), weight function psi(t)/t or 
##           its derivative respectively

    at <- abs(t)
    if (flag == "psi") {
        dummy <- pmin(at, a, a/(c-b)*(c-at))
        sign(t)*pmax(dummy, 0)
    } else {
        T <- pmin(at, c)
        if (flag == "weights") {
            ifelse(T <= a, 1, ifelse(T <= b, a/T, a*(c - T)/(c - b)/T))
        } else if (flag == "deriv") {
            ifelse(at <= c, ifelse(T <= a, 1, ifelse(T <= b, 0, -a/(c - b))), 0)
        } else {
            warning("error in function 'psiHampel': wrong flag type \n")
        }
    }
}


.psi <- function (type)
{
##################################
##
##  R-function: .psi - switches to appropriate psi-function
##              (internal function)
##  author: Bernhard Spangl
##  version: 1.0 (2006-05-21)
##
##################################

##  type ... which psi-function, "Huber", "Tukey", , "Hampel", "Ident"

    switch(type, 
        Ident = get("psiLS", mode="function"), 
        Huber = get("psiHuber", mode="function"), 
        Tukey = get("psiTukey", mode="function"),
        Hampel = get("psiHampel", mode="function")) 
}

mvpsiHampel <- function (x, a=2, b=4, c=8) 
{
###########################################
##
##  R-function: mvpsiHampel - multivariate analogue of 
##                            Hampel's psi-function
##  author: Bernhard Spangl
##  version: 0.1 (2008-02-23)
##
###########################################

##  Paramters:
##  x ... vector 
##  a, b, c ... tuning constants
    x.norm <- EuclidianNorm(x)
    small <- (x.norm <= a)
    dummy <- pmin(a, a/(c-b)*(c-x.norm))
    dummy <- pmax(dummy, 0)/(x.norm+small)*(!small) + small
    return(x*dummy)
}

jacobian.Hampel <- function (x, ...) 
{
###########################################
##
##  R-function: jacobian.Hampel - Jacobian matrix of multivariate 
##                                analogue of Hampel's psi-function
##              using R-function 'jacobian' of package 'numDeriv'
##  author: Bernhard Spangl, based on work of Paul Gilbert
##  version: 0.1 (2008-02-23)
##
###########################################

##  Paramters:
##  x ... vector 

    jacobian(mvpsiHampel, x, ...)
}


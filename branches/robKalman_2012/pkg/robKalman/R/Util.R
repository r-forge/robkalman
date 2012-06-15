#######################################################
##
##  rLS Kalman filter routines
##  [cf. Ruckdeschel, 2009]
##  author: Peter Ruckdeschel,
##  version: limitS taken from r-forge 0.3
##           rest: (P.R., 2012-04-10)
##
#######################################################

limitS <- function(S, F, Z, Q, V, B = NULL, D = NULL, tol = 10^-4, itmax = 1000)#
## determines lim_{t->infty} S_{t|t-1}
#------------------------------------
# inputs:
#------------------------------------
# S = Cov(x_0) (a matrix!)
# F, Z, Q, V  Hyperparameters as matrices
# B, D notation from EKF by default set to unit matrices in code
# tol: accuracy : when change from S_{t|t-1} to S_{t+1|t}
#      smaller than tol in abs. value, we stop
# itmax: maximal number of iterations
#------------------------------------
# outputs:
#------------------------------------
# lim_{t->infty} S_{t|t-1} (as matrix)
#------------------------------------
     {SO0 <- S + 1
      S0  <- S
      if(is.null(B)) B <- diag(length(Q)^.5)
      if(is.null(D)) D <- diag(length(V)^.5)


      i   <- 0
#      print(S0)
      while( (i<3 ||(sum( (SO0 - S0)^2 ) > tol^2)) && (i < itmax) )
        {i   <- i + 1
         S1  <- .getpredCov(S0, F, B, Q)
         SO0 <- S0
         K   <- .getKG(S1, Z, .getDelta(S1, Z, D, V))
         S0  <- .getcorrCov(S1, K, Z)

#         print(list(S0,i))
        }
     S1
     }

.timeInvModel <- function(S, F, Z, Q, V, B = NULL, D = NULL, tol = 10^-4,
                         itmax = 1000, repl = 100000, eff=NULL, r=0.1,
                         rlow = 0, rup = NULL, upto = 20, blow = 1e-6,
                         IO = TRUE, AO = TRUE, seed = NULL,
                          verbose = FALSE)#
{#
## determines lim_{t->infty} b_{t} in time-invariant model
#------------------------------------
# inputs:
#------------------------------------
# S = Cov(x_0) (a matrix!)
# F, Z, Q, V  Hyperparameters as matrices
# B, D notation from EKF by default set to unit matrices in code
# tol, itmax: as in limitS
# blow, rlow, rup, upto, seed: as in rLScalibrateB
# IO: do we compute b_IO (TRUE) or not (FALSE)
# AO: do we compute b_AO (TRUE) or not (FALSE)
# verbose: shall we output intermediate results?
#------------------------------------
# outputs:
#------------------------------------
#  list with items
#  Slim = lim_t Sigma_{t|t-1}
#  b.AO, b.IO (respective clipping heights),
#  r.AO, r.IO (respective radii)
#------------------------------------

  #
  Slim <- limitS(S=S, F=F, Z=Z, Q=Q, V=V, B=B, D=D, tol=tol, itmax=itmax)
  if(verbose) print(Slim)
  if(AO){
     br.AO <-  rLScalibrateB(Z = Z, S = Slim, V = V,  repl = repl,
                             b = NULL, eff = eff, r = r,
                          rlow = rlow, rup = rup, upto = upto,
                          blow = blow, IO = FALSE,
                          seed = seed, verbose = verbose)
     b.AO <- br.AO$b
     r.AO <- br.AO$r
  } else {
     b.AO <- NULL
     r.AO <- NULL
  }
  if(IO){
     br.IO <-  rLScalibrateB(Z = Z, S = Slim, V = V,  repl = repl,
                             b = NULL, eff = eff, r = r,
                             rlow = rlow, rup = rup, upto = upto,
                             blow = blow, IO = TRUE,
                             seed = seed, verbose = verbose)
     b.IO <- br.IO$b
     r.IO <- br.IO$r
  } else {
     b.IO <- NULL
     r.IO <- NULL
  }
  list(Slim = Slim, b.AO = b.AO, b.IO = b.IO, r.AO = r.AO, r.IO = r.IO)
}

.getb.rls <- function(b = NULL, controlCorr, Z, S, V, D=NULL,IO=FALSE){
## determines b_{t} in time-variant model / in particular in EKF, UKF
#------------------------------------
# inputs:
#------------------------------------
# b clipping height: if given, computes r and eff else is NULL
# controlCorr: control list for correction step rLS
# Z, V  Hyperparameters as matrices
# S = Sigma_{t|t-1} (a matrix!)
# D notation from EKF by default set to unit matrices in code
# IO: do we compute b_IO (TRUE) or b_AO (FALSE)
#------------------------------------
# outputs:
#------------------------------------
#  modified control list (with new entries b, eff, r in
#    list items b.v, eff.v, r.v
#------------------------------------
    ## vorlaeufig explizite Setzung /
    ## mittelfristig auslesen in einer Paketoption vgl options()
    bcal <- r <- eff <- NULL
    if((is.null(controlCorr))||(is.null(names(controlCorr)))){
       repl <- 10000; blow <- 1e-6; rlow <- 0; rup <- NULL
       upto <- 20; seed <- NULL; verbose <- FALSE; r <- 0.1
       controlCorr <- vector("list",0); nam <- NULL
    }else{
       nam <- names(controlCorr)
       if(! "repl" %in% nam) repl <- 10000
       if(! "blow" %in% nam) blow <- 1e-6
       if(! "rlow" %in% nam) rlow <- 0
       if(! "rup" %in% nam) rup <- NULL
       if(! "upto" %in% nam) upto <- 20
       if(! "seed" %in% nam) seed <- NULL
       if(! "verbose" %in% nam) verbose <- FALSE
       if(! (("eff" %in% nam) ||("r" %in% nam))) r <- 0.1
    }
    if(is.null(b)){
       with(controlCorr,
            { bcal <<- rLScalibrateB(Z = Z, S = S, V = V, D = D, repl = repl,
                                  b = NULL, eff = eff, r = r, blow = blow,
                                  rlow = rlow, rup = rup, upto = upto, IO = IO,
                                  seed = seed, verbose = verbose)
            }
           )
    }else{
       with(controlCorr,
            bcal <<- rLScalibrateB(Z = Z, S = S, V = V, D = D, repl = repl,
                                  b = b, eff = NULL, r = NULL, blow = blow,
                                  rlow = rlow, rup = rup, upto = upto, IO = IO,
                                  seed = seed, verbose = verbose)
           )
    }
    if(!is.null(controlCorr) && "b.v" %in% nam) 
        controlCorr$b.v <- c(controlCorr[["b.v"]], bcal$b)
    else{controlCorr$b.v <- bcal$b}    
    if(!is.null(controlCorr) && "eff.v" %in% nam) 
    controlCorr$eff.v <- c(controlCorr[["eff.v"]], bcal$eff)
    else{controlCorr$eff.v <- bcal$eff}    
    if(!is.null(controlCorr) && "r.v" %in% nam) 
    controlCorr$r.v <- c(controlCorr[["r.v"]], bcal$r)
    else{controlCorr$r.v <- bcal$r}    
    return(controlCorr)
}
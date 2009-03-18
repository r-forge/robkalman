KalmanControl <- function() new("KalmanControl")

ACMControl <- function(s0 = 0, psi = "Hampel",
                       apsi = 2.5, bpsi = 2.5, cpsi = 5.0,
                       flag = "weight"){
           new("robrecControl",
                name = paste(gettext(
                       "Control set and init, prediction and"
                           ),gettext(
                       "correction step for the classical Kalman Filter\n"
                           ),gettext(
                       "and the rLS Filter"
                       )),
                init.rob = .cKinitstep,
                predict.rob = .ACMpredstep,
                correct.rob = .ACMcorrstep,
                controls = list(s0 = s0, psi = psi, apsi = apsi, bpsi = bpsi,
                                cpsi = cpsi, flag = flag)
               )}
               
rLSControl <- function(b = NULL, b.length = 0,
                       r, eff, norm = EuclideanNorm,
                       SSM = NULL){
           if(is.null(b)){
              if(b.length){
           ### just to be more flexible in the beginning:
           ### time variable b
                 b <- numeric(b.length)
                 r <- numeric(b.length)
                 eff <- numeric(b.length)
                 if(!is(SSM,"SSM"))
                     stop("argument SSM needs to be of class 'SSM'")
                 SSM <- makeArrayRepresentation(SSM)
                 S0 <- getS(SSM); p <- getp(SSM); q <- getq(SSM)
                 for (i in 1: b.length){
                      Z0 <- matrix(getZ(SSM)[,,i],q,p)
                      V0 <- matrix(getV(SSM)[,,i],q,q)
                      if(!missing(r))
                         erg <- rLScalibrateB(r = r, S = S0, Z = Z0, V = V0)
                      else if(!missing(eff))
                         erg <- rLScalibrateB(eff = eff, S = S0, Z = Z0, V = V0)
                      else stop("need at least one of arguments 'r' and 'eff'")
                      b[i] <- erg$b
                      eff[i] <- erg$eff
                      r[i] <- erg$r
                      S1  <- .getpredCov(S0, matrix(getF(SSM)[,,i],p,p),
                                             matrix(getQ(SSM)[,,i],p,p))
                      K   <- .getKG(S1, Z0, V0)
                      S0  <- .getcorrCov(S1, K, Z0)
                      }
              }else{
                 if(is(try(as(SSM,"TimeInvariantSSM"),silent  = TRUE), "try-error"))
                     stop("argument SSM needs to be of class 'TimeInvariantSSM'")
                 SS <- limitS(S = getS(SSM), F = getF(SSM), Q = getQ(SSM), Z = getZ(SSM),
                              V = getV(SSM))
                 if(!missing(r))
                     erg <- rLScalibrateB(r = r, S = SS, Z = getZ(SSM), V = getV(SSM))
                 else if(!missing(eff))
                     erg <- rLScalibrateB(eff = eff, S = SS, Z = getZ(SSM), V = getV(SSM))
                 else stop("need at least one of arguments 'r' and 'eff'")
                 b <- erg$b
                 eff <- erg$eff
                 r <- erg$r
              }
           }
           
           new("robrecControl",
                name = paste(gettext(
                    "Control set and init, prediction and"),
                             gettext(
                    "correction step for the classical Kalman Filter"
                             )),
                name.rob = paste(gettext(
                    "Control set and init, prediction and"),
                                 gettext(
                    "correction step for the rLS Filter"
                             )),
                init = .cKinitstep,
                predict = .cKpredstep,
                correct = .cKcorrstep,
                init.rob = .cKinitstep,
                predict.rob = .cKpredstep,
                correct.rob = .rLScorrstep,
                controls = list(b=b, r=r, eff = eff, norm = norm)
               )}
               
setMethod("init", "RecFiltControl", function(object) {
           return(object@init)
           })
setMethod("predict", "RecFiltControl", function(object) {
           return(object@predict)
           })
setMethod("correct", "RecFiltControl", function(object) {
           return(object@correct)
           })
setMethod("name", "RecFiltControl", function(object) {
           return(object@name)
           })
setMethod("init.rob", "robrecControl", function(object) {
           return(object@init.rob)
           })
setMethod("predict.rob", "robrecControl", function(object) {
           return(object@predict.rob)
           })
setMethod("correct.rob", "robrecControl", function(object) {
           return(object@correct.rob)
           })
setMethod("name.rob", "robrecControl", function(object) {
           return(object@name.rob)
           })
setMethod("controls", "robrecControl", function(object) {
           return(object@controls)
           })

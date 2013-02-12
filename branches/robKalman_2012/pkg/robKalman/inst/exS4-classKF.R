#######################################################
## 
##  file: exS4-classKF.R
##        classical Kalman filter example using only S4 classes
##        (cf. 'exampleLinearEKF.R')
##  author: Bernhard Spangl
##  version: 0.1 (changed: 2013-02-10, created: 2013-02-10)
##
#######################################################

##  for testing:
path <- "~/university/svn/itwm/robKalman/"
load(paste(path,
           "Paper/StatisticsPaper/R-Code/bernhard/exampleLinearEKF_sp.RData",
           sep=""))

source("allClass.R")
setGeneric("createF",
           function(object, ...) standardGeneric("createF"))
setGeneric("createZ",
           function(object, ...) standardGeneric("createZ"))
setGeneric("createQ",
           function(object, ...) standardGeneric("createQ"))
setGeneric("createV",
           function(object, ...) standardGeneric("createV"))
source("Fmethods.R")
source("Zmethods.R")
source("Qmethods.R")
source("Vmethods.R")

##  S4 class 'SSM'

initEq <- new("SSinitEq",
              a0=c(20, 0),
              Sigma0=matrix(0, 2, 2))

## Ffct <- new("FunctionWithControl",
##             function (t, x0, v=c(0,0), u=c(0,0), control=NULL, dots=NULL)
##             {
##                 call <- match.call()
##                 F <- matrix(c(1, 0, 1, 0), ncol=2)
##                 R <- diag(nrow(F))
##                 x1 <- F%*%x0 + u + R%*%v
##                 retF <- new("SSretValueF",
##                             x1 = as.vector(x1), Fmat = F, Rmat = R,
##                             t=t, x0=x0, v=v, u=u, control=control,
##                             dots.propagated = dots, call = call,
##                             diagnostics = new("SSDiagnosticRetValue"))
##                 return(retF)
##             })

## besser:
F <- matrix(c(1, 0, 1, 0), ncol=2)
Z <- matrix(c(0.3, -0.3, 1, 1), ncol=2)
Q <- diag(c(0, 9))
V <- diag(c(9, 9))

Ffct <- createF(F)
Zfct <- createZ(Z)
Qfct <- createQ(Q)
Vfct <- createV(V)

## testing:
## Ffct(1, 1:2, c(0, 0), c(0, 0), NULL, NULL)
## Zfct(1, 1:2, c(0, 0), c(0, 0), NULL, NULL)
## Qfct(1, 1:2, NULL, NULL, NULL)
## Vfct(1, 1:2, NULL, NULL, NULL)

stateEq <- new("SSstateEq",
               Ffct=Ffct,
               Qfct=Qfct)

obsEq <- new("SSobsEq",
             Zfct=Zfct,
             Vfct=Vfct)

mySSM <- new("SSM",
             initEq=initEq,
             statesEq=stateEq,
             obsEq=obsEq,
             pdim=2,
             qdim=2)

Obs <- new("SSObs",
           Y=Y,
           origData=NULL)

times <- new("SStimes",
             times=1:50,
             inX=rep(TRUE, 50))


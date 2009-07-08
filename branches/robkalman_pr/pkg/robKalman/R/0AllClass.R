################ general package preparation code, nothing to do with classes:

.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE)

}


.onAttach <- function(library, pkg)
{
buildStartupMessage(pkg="robKalman", library=library, packageHelp=TRUE #, 
                    #MANUAL=""
                    )
  invisible()
}

#.onUnload <- function(libpath)
#{
#    library.dynam.unload("distrEx", libpath)
#}
#


################ Matrix class

#### Code borrowed from package distrMod

if (!(isClass("PosSemDefSymmMatrix")))
setClass("PosSemDefSymmMatrix", contains = "matrix",
            prototype = prototype(matrix(1)),
            validity = function(object){
                if(nrow(object) != ncol(object))
                    stop("no square matrix")
                if(any(!is.finite(object)))
                    stop("inifinite or missing values in matrix")
                if(!isTRUE(all.equal(object, t(object), .Machine$double.eps^0.5)))
                    stop("matrix is not symmetric")
                if(!all(eigen(object)$values > -100*.Machine$double.eps))
                   stop("matrix is (numerically) not positive semi - definite")
               return(TRUE)
            })

## positive definite, symmetric matrices with finite entries
if (!(isClass("PosDefSymmMatrix")))
setClass("PosDefSymmMatrix", contains = "PosSemDefSymmMatrix",
            validity = function(object){
               if(!all(eigen(object)$values > 100*.Machine$double.eps))
                   stop("matrix is (numerically) not positive definite")
               valid <- getValidity(getClass("PosSemDefSymmMatrix"))
               valid(as(object, "PosSemDefSymmMatrix"))
               return(TRUE)
            })



#

### infra-structure classes / class unions

setClassUnion("IntegerOrNULL", c("integer", "NULL"))
setClassUnion("ArrayOrNULL", c("array", "NULL"))
setClassUnion("ArrayOrMatrix", c("array", "matrix"))
setClassUnion("Hyperparamtype", 
               c("NULL","ArrayOrMatrix", "OptionalFunction"))
setClassUnion("sHyperparamtype", 
               c("Hyperparamtype", "numeric"))
setClassUnion("MatrixOrLogical", c("logical", "matrix"))


###############################################################################
#
# State Space Model classes (SSMs)
#
###############################################################################


# class SSM --- State space model
setClass("SSM",
          representation = representation(
                                name = "character",   ## name of the ssm
                                F = "Hyperparamtype", ## transition matrix/ces or NULL
                                Z = "Hyperparamtype", ## observation matrix/ces or NULL
                                Q = "Hyperparamtype", ## innovation covariance or NULL
                                V = "Hyperparamtype", ## observation error covariance or NULL
                                p = "numeric",  ## state dimension
                                q = "numeric",  ## observation dimension
                                a = "numeric", ##  mean value of starting state
                                S = "Hyperparamtype", ##  variance of starting state
                                time = "timeSeries"), ## time index
          prototype = prototype(name = gettext("a state space"), 
                                F = NULL,
                                Z = NULL,
                                Q = NULL,
                                V = NULL,
                                p = 1, 
                                q = 1,
                                a = 0,
                                S = NULL,
                                time = timeSeries(1)),
          )

# class TimeInvariantSSM 
setClass("TimeInvariantSSM",
          prototype = prototype(name = gettext("a time-invariant state space"), 
                                F = matrix(1),
                                Z = matrix(1),
                                Q = matrix(1),
                                V = matrix(1),
                                p = 1, 
                                q = 1,
                                a = 0,
                                S = matrix(1)), 
          validity = function(object){
            if(!is.matrix(object@F)|!is.matrix(object@Z)|
               !is.matrix(object@Q)|!is.matrix(object@V)|
               !is.matrix(object@S))
               stop("Hyperparameters have to be matrices")
            return(TRUE)   
          },
          contains = "SSM")          

###############################################################################
#
# Filter classes 
#
###############################################################################

setClass("recFilter", representation(name = "character",
                      SSM = "SSM", 
                      Y = "array",
                      X.filtered = "array",
                      X.predicted = "array",
                      Cov.filtered = "array",
                      Cov.predicted = "array",
                      Kalman.Gain = "array",
                      Delta = "array",
                      DeltaY = "array",
                      time = "timeSeries"),
         prototype = prototype(name="classical Kalman Filter",
                              SSM = new("TimeInvariantSSM"),
                              Y = array(1,dim=c(1,1,1)),
                              X.filtered = array(1,dim=c(1,1,1)),
                              X.predicted = array(1,dim=c(1,1,1)),
                              Cov.filtered = array(1,dim = c(1,1,1)),
                              Cov.predicted = array(1,dim = c(1,1,1)),
                              Kalman.Gain = array(1,dim = c(1,1,1)),
                              Delta = "array",
                              DeltaY = "array",
                              time = timeSeries(1))
                              
         )

                              
setClass("robrecFilter", representation(
                      name.rob = "character",
                      X.rob.filtered = "array",
                      X.rob.predicted = "array",
                      Cov.rob.filtered = "array",
                      Cov.rob.predicted = "array",
                      Kalman.rob.Gain = "array",
                      Deltar = "array",
                      DeltaYr = "array",
                      IndIO = "MatrixOrLogical", 
                      IndAO = "MatrixOrLogical",
                      rob.correction.ctrl = "list",
                      rob.prediction.ctrl = "list",
                      nsim = "numeric",
                      RNGstate = "IntegerOrNULL",
                      Cov.rob.filtered.sim = "ArrayOrNULL",
                      Cov.rob.predicted.sim = "ArrayOrNULL",
                      ),
         prototype = prototype(
                      name="rLS Filter",
                      X.rob.filtered = array(1,dim=c(1,1,1)),
                      X.rob.predicted = array(1,dim=c(1,1,1)),
                      Cov.rob.filtered = array(1,dim = c(1,1,1)),
                      Cov.rob.predicted = array(1,dim = c(1,1,1)),
                      Kalman.rob.Gain = array(1,dim = c(1,1,1)),
                      IndIO = FALSE, 
                      IndAO = FALSE,
                      nsim = 0,
                      RNGstate = as.integer(0),
                      rob.correction.ctrl = list(NULL),
                      rob.prediction.ctrl = list(NULL),
                      Cov.rob.filtered.sim = array(1,dim = c(1,1,1)),
                      Cov.rob.predicted.sim = array(1,dim = c(1,1,1))),
         contains = "recFilter")
                              
###############################################################################
#
# Smoother classes
#
###############################################################################

setClass("recSmoother", representation(
                      X.smoothed = "array",
                      Cov.smoothed = "array",
                      J = "array"),
         prototype = prototype(name="classical Kalman Smoother",
                              SSM = new("TimeInvariantSSM"),
                              Y = array(1,dim=c(1,1,1)),
                              X.filtered = array(1,dim=c(1,1,1)),
                              X.predicted = array(1,dim=c(1,1,1)),
                              Cov.filtered = array(1,dim = c(1,1,1)),
                              Cov.predicted = array(1,dim = c(1,1,1)),
                              Kalman.Gain = array(1,dim = c(1,1,1)),
                              time = timeSeries(1)),

         contains="recFilter")


setClass("robrecSmoother", representation(
                      X.rob.smoothed = "array",
                      Cov.rob.smoothed = "array",
                      J.rob = "array"),
         prototype = prototype(
                      name="rLS Smoother",
                      X.rob.smoothed = array(1,dim=c(1,1,1,1)),
                      Cov.rob.smoothed = array(1,dim = c(1,1,1,1)),
                      J.rob = array(1,dim = c(1,1,1,1))),
         contains = "recSmoother")

###############################################################################
#
# Control classes 
#
###############################################################################
#setClass("ACMcontrol",representation())
#setClass("rLScontrol",representation())
                                           

setClass("RecFiltControl", representation(
                      name = "character",
                      init = "function",
                      predict = "function",
                      correct = "function"),                                 
          contains = "VIRTUAL")

setClass("RecSmoothControl", representation(
                      smooth = "function"),
          contains = c("VIRTUAL","RecFiltControl"))

setClass("KalmanFiltControl",
          prototype = prototype(
                      name = "classical Kalman Filter",
                      init = .cKinitstep, 
                      predict = .cKpredstep, 
                      correct = .cKcorrstep),
          contains="RecFiltControl"            
          )
setClass("KalmanSmoothControl",
          prototype = prototype(
                      name = "classical Kalman Smoother",
                      smooth = .cKsmoothstep),
          contains="KalmanFiltControl"
          )

setClass("RobRecFilterControl", representation(
                       controls = "list",
                       name.rob = "character",
                       init.rob = "function",
                       predict.rob = "function",
                       correct.rob = "function"),                                 
          prototype = prototype(
                      name.rob = "rLS Filter",
                      init.rob = .cKinitstep, 
                      predict.rob = .cKpredstep, 
                      correct.rob = .rLScorrstep,
                      controls = list(b = 2, norm = EuclideanNorm)
                      ),
          contains="RecFiltControl"            
              )
setClass("RobRecSmoothControl", representation(
                       smooth.rob = "function"),
          prototype = prototype(
                      name.rob = "rLS Filter",
                      init.rob = .cKinitstep,
                      predict.rob = .cKpredstep,
                      correct.rob = .rLScorrstep,
                      smooth.rob = .cKsmoothstep,
                      controls = list(b = 2, norm = EuclideanNorm)
                      ),
          contains="RecFiltControl"
              )
#                                = paste(gettext(
#                            "Control set and init, prediction and"
#                                 ),gettext(
#                            "correction step for the classical Kalman Filter\n"
#                                 ),gettext(
#                            "and the rLS Filter"
#                                 )), 

###############################################################################
#
# multivariate Distribution classes 
#
###############################################################################

setClass("SSMDistribution.f", representation(
                              r.init = "function",
                              r.innov = "function",
                              r.obs = "function"),
          prototype = prototype(r.init = mvrnorm, 
                                r.innov = mvrnorm, 
                                r.obs = mvrnorm))                         

setClass("SSMellDistribution.f", representation(
                              m.init = "sHyperparamtype",
                              S.init = "Hyperparamtype",
                              m.innov = "sHyperparamtype",
                              S.innov = "Hyperparamtype",
                              m.obs = "sHyperparamtype",
                              S.obs = "Hyperparamtype"),
          prototype = prototype(m.init = 0, S.init = matrix(1),
                                m.innov = 0, S.innov = matrix(1),
                                m.obs = 0, S.obs = matrix(1)),
          contains = "SSMDistribution.f")

setClass("SSMConvDistribution.f", representation(
                              ideal = "SSMDistribution.f",
                              cont = "SSMellDistribution.f",
                              r.IO = "numeric",
                              r.AO = "numeric"),
          prototype = prototype(ideal = new("SSMellDistribution.f"),
                                cont = new("SSMellDistribution.f"),
                                r.IO = 0, r.AO = 0),
          validity = function(object){
                     if (object@r.IO<0||object@r.AO<0||object@r.IO>1||object@r.AO>1)                      
                         stop("Radii must be between 0 and 1")
                     return(TRUE)})


###############################################################################
#
# SSM + Distribution classes 
#
###############################################################################
setClass("SSMwithDistribution", representation(
                              SSM = "SSM",
                              Distribution = "SSMDistribution.f"),
          prototype = prototype(SSM = new("SSM"), 
                      Distribution = SSMDistribution.f(new("SSM"))))
setClass("SSMwithConvDistribution", representation(
                              SSM = "SSM",
                              Distribution = "SSMConvDistribution.f"),
          prototype = prototype(SSM = new("SSM"), 
                      Distribution = SSMContDistribution.f(new("SSM"))))

setClassUnion("SSMDistr", c("SSMDistribution.f", 
              "SSMConvDistribution.f"))

###############################################################################
#
# SSM - Simulation classes 
#
###############################################################################
setClass("SSMsimulation", representation(
                              SSM = "SSM",
                              Distr = "SSMDistr",
                              RNGstate = "numeric",
                              states = "ArrayOrMatrix",
                              obs = "ArrayOrMatrix"),
          prototype = prototype(SSM = new("SSM"), 
                      Distr = new("SSMDistribution.f"),
                      RNGstate = structure(1, kind = as.list(RNGkind())), 
                      states = matrix(1), obs = matrix(1)))

setClass("SSMcontSimulation", representation(
                              states.id = "ArrayOrMatrix",
                              obs.id = "ArrayOrMatrix",                              
                              Ind.IO = "MatrixOrLogical",
                              Ind.AO = "MatrixOrLogical"
                              ),
          prototype = prototype(
                      Distr = new("SSMConvDistribution.f"),
                      states.id = matrix(1), obs.id = matrix(1),
                      Ind.IO = FALSE, Ind.AO = FALSE
                      ),
          validity = function(object){
                fct <- function(m){ 
                   mt <- paste(deparse(substitute(m)),sep="",collapse="")
                   if(is.matrix(m)){
                      if(!all(is.logical(m)))
                          stop(gettextf("Matrix %s has to have logical entries.", mt))
                      return(TRUE)
                   }
                   else return(TRUE)
                 }                      
               return(fct(object@Ind.IO)&&fct(object@Ind.AO))
            },
          contains = "SSMsimulation")
          
                      
                                                                                                     
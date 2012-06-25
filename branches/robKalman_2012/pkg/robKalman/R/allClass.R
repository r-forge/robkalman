########################################################
##
##  S4 classes for robust filtering
##  author: Bernhard Spangl  & Peter Ruckdeschel
##  version: 0.1 (last changed: 2012-04-27, created: 2011-08-19)
##
#######################################################


.onLoad <- function(lib, pkg){
}

.onAttach <- function(library, pkg)
{
buildStartupMessage(pkg = "robKalman", msga, msgb, library = library, packageHelp = TRUE,
#                    MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
VIGNETTE = gettext("Package \"robKalman\" provides a vignette to this package; try vignette(\"robKalman\")."))
  invisible()
}

## ClassUnion: OptionalList
setClassUnion("OptionalList",
               c("list","NULL")
               )
setClassUnion("OptionalFunction",
               c("function","NULL")
               )


## Class: FunctionWithControl
setClass("FunctionWithControl", contains = "function")
### in validity method check whether has args dots and control

setClassUnion("OptionalFunctionWithControl",
               c("FunctionWithControl","NULL")
               )

setClassUnion("OptionalDistribution",
               c("Distribution","NULL")
               )

setClass("SSstateEq",
          representation = representation(F = "FunctionWithControl",
                                          Q = "FunctionWithControl",
                                          Exo = "OptionalFunctionWithControl",
                                          distr = "OptionalDistribution"),
          prototype = prototype(F = new("SSTransform",
                                         fct=function(...)1, control = NULL,
                                         name="state transition"),
                                Q = new("SSVar",
                                         fct=function(...)1, control = NULL,
                                         name="state variance"),
                                Exo = new("SSExo",
                                         fct=function(...)1, control = NULL,
                                         name="state Exogenous variable")
                                distr = Norm())
 )
setClass("SSobsEq",
          representation = representation(Z = "FunctionWithControl",
                                          V = "FunctionWithControl",
                                          Exo = "OptionalFunctionWithControl",
                                          distr = "OptionalDistribution"),
          prototype = prototype(Z = new("SSTransform",
                                         fct=function(...)1, control = NULL,
                                         name="state transition"),
                                V = new("SSVar",
                                         fct=function(...)1, control = NULL,
                                         name="state variance"),
                                Exo = new("SSExo",
                                         fct=function(...)1, control = NULL,
                                         name="state Exogenous variable")
                                distr = Norm())
 )
setClass("SSinitEq",
          representation = representation(a0 = "numeric",
                                          Sigma0 = "matrix",
                                          Exo = "OptionalFunctionWithControl",
                                          distr = "OptionalDistribution"),
          prototype = prototype(a0 = 1,
                                Sigma0 = matrix(1,1,1),
                                Exo = new("SSExo",
                                         fct=function(...)0, control = NULL,
                                         name="state Exogenous variable"),
                                distr = Norm())

 )
setClass("SSM",
          representation = representation(initEq  = "SSinitEq",
                                          statesEq = "SSstateEq",
                                          obsEq = "SSobsEq",
                                          p = "numeric", q = "numeric"))
)
setClass("SStimes", representation = representation(times = "numeric",
                                   inX = "logical"))

setClass("SSObs",
          representation = representation(Y = "numeric",
                                          origData = "ANY",
                                          Exo = "SSVar",
                                          mu = "function"),
          prototype = prototype(Y = 1,
                                origData = 1,
                                Exo = new("SSExo",
                                         fct=function(...)0, control = NULL,
                                         name="state Exogenous variable"),
                                mu = function(...)0)

 )

setClass("SSFilter", representation = representation(initStep = "FunctionWithControl",
                                    prepStep = "OptionalFunctionWithControl",
                                    predStep = "FunctionWithControl",
                                    corrStep = "FunctionWithControl"))
setClass("SSrobFilter", representation = representation(classFilter = "SSFilter",
                                       robFilter = "SSFilter"))

setClass("SSSmoother", representation = representation(filt = "SSfilter",
                                    smoothStep = "FunctionWithControl",
                                    smoothCov = "FunctionWithControl",
                                    lagoneCov = = "FunctionWithControl"))
                                    
setClass("SSrobSmoother", representation = representation(classSmoother = "SSSmoother",
                                       robSmoother = "SSSmoother"))

setClassUnion("SSClassOrRobFilter",
               c("SSFilter","SSrobFilter")
               )

setClassUnion("SSClassOrRobSmoother",
               c("SSSmoother","SSrobSmoother")
               )
setClassUnion("SSClassOrRobSmootherOrFilter",c("SSClassOrRobFilter",
              "SSClassOrRobSmoother"))

setClass("SSDiagnostic", contains = c("VIRTUAL"))
setClass("SSDiagnosticFilter", contains = c("SSDiagnostic","list"))
setClass("SSVariances", contains = "array")
setClass("SSStateReconstr", contains = "matrix")


setClass("SSPredOrFilt", representation = representation(values = "matrix",
                      variances = "array", diagnostics = "SSDiagnostic"),
                      contains = "VIRTUAL")
setClass("SSPredicted", contains = "SSPredFiltSmooth")
setClass("SSFiltered",  representation = representation(KalmanGain = "array",
                      CovObs = "array", DeltaY = "matrix"),
                      contains = "SSPredFiltSmooth")
setClass("SSSmoothed", representation = representation(lagoneCov = "array"),
                     contains = "SSPredOrFilt")
setClassUnion("OptionalSSPredicted",
               c("SSPredicted","NULL")
               )
setClassUnion("OptionalSSFiltered",
               c("SSFiltered","NULL")
               )
setClassUnion("OptionalSSSmoothed",
               c("SSSmoothed","NULL")
               )

setClass("SSInput", representation = representation(steps = "SSClassOrRobFilter"
                                                   model = "SSM",
                                                   obs = "SSObs",
                                                   times = "SStimes"))

setClass("SSOutput", representation = representation(pred.cl = "SSPredicted",
                                                     filt.cl = "SSFiltered",
                                                     smooth.cl = "OptionalSSSmoothed",
                                                     pred.rob = "OptionalSSPredicted",
                                                     filt.rob = "OptionalSSFiltered",
                                                     smooth.rob = "OptionalSSSmoothed"))

setClass("SSrecResult", representation = representation(input="SSInput", output="SSOutput"))

setClass("SSISimulation", representation = representation(name = "character",
           obs = "array", states = "array"))
setClass("SSCSimulation", representation = representation(radius = "numeric"),
          contains = "SSISimulation")

setClass("SSSimulation", representation = representation(model = "SSM",
                 runs = "numeric", seed = "numeric", times = "SStimes"),
          contains = "SSISimulation")
setClass("SSSimList", contains = "list")

setClass("SSContSimulation", representation = representation(SimList = "SSSimList")
          contains = "SSimulation")

setClass("SSretValueF", representation = representation(x1 = "numeric",
                           F = "matrix", R = "matrix", t = "numeric",
                           x0 = "numeric", v = "numeric", u = "numeric",
                           control = "OptionalList", dots = "OptionalList",
                           call = "call", diagnostics = "SSDiagnostic"))

setClass("SSretValueZ", representation = representation(y = "numeric",
                           Z = "matrix", T = "matrix", t = "numeric",
                           x1 = "numeric", eps = "numeric", w = "numeric",
                           control = "OptionalList", dots = "OptionalList",
                           call = "call", diagnostics = "SSDiagnostic"))
setClass("SSretValueQ", representation = representation( Q = "matrix",
                    t = "numeric", x0 = "numeric", exQ = "ANY",
                    control = "OptionalList", dots = "OptionalList",
                    call = "call", diagnostics = "SSDiagnostic"))

setClass("SSretValueV", representation = representation( V = "matrix",
                    t = "numeric", x1 = "numeric", exV = "ANY",
                    control = "OptionalList", dots = "OptionalList",
                    call = "call", diagnostics = "SSDiagnostic"))

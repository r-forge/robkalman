########################################################
##
##  file: S4 classes for robust filtering
##  author: Bernhard Spangl & Peter Ruckdeschel
##  version: 0.3 (changed: 2013-02-08, created: 2011-08-19)
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


### ClassUnion: 
setClassUnion("OptionalNumeric",
              c("NULL", "numeric")
              )
setClassUnion("OptionalMatrix",
              c("NULL", "matrix")
              )
setClassUnion("OptionalArray",
              c("NULL", "array")
              )
setClassUnion("OptionalList",
              c("list","NULL")
              )
## setClassUnion("OptionalFunction",    # existiert bereits!
##               c("function","NULL")
##               )
setClassUnion("OptionalCall",
              c("call","NULL")
              )
setClass("ListOfCalls", contains = "list",
         validity = function (object) {
             all(sapply(object, function (u) is(u)=="call"))
         }
         )
setClassUnion("OptionalListOfCalls",
              c("ListOfCalls","NULL")
              )


### Class: FunctionWithControl
setClass("FunctionWithControl", contains = "function")
    # in validity method check whether has args dots.propagated and control

setClassUnion("OptionalFunctionWithControl",
              c("FunctionWithControl","NULL")
              )

setClassUnion("OptionalDistribution",
##               c("Distribution","NULL")    # S4 class 'Distribution' missing!
               "ANY"
#              c("NULL", "function")
              )


### SSM definitions
setClass("SSstateEq",
         representation = representation(Ffct = "FunctionWithControl",
                                         Qfct = "FunctionWithControl",
                                         muqfct = "OptionalFunction",
                                         distrfct = "OptionalDistribution",
                                         uExofct = "OptionalFunctionWithControl")
         )
setClass("SSobsEq",
         representation = representation(Zfct = "FunctionWithControl",
                                         Vfct = "FunctionWithControl",
                                         muvfct = "OptionalFunction",
                                         distrfct = "OptionalDistribution",
                                         wExofct = "OptionalFunctionWithControl")
         )
setClass("SSinitEq",
         representation = representation(a0 = "numeric",
                                         Sigma0 = "matrix",
                                         distrfct = "OptionalDistribution",
                                         iExofct = "OptionalFunctionWithControl")
         )
setClass("SSM",
         representation = representation(initEq  = "SSinitEq",
                                         statesEq = "SSstateEq",
                                         obsEq = "SSobsEq",
                                         pdim = "numeric", qdim = "numeric")
         )
setClass("SStimes",
         representation = representation(times = "numeric",
                                         inX = "logical")
         )
setClass("SSObs",
         representation = representation(Y = "matrix", 
                                         origData = "ANY"),
         prototype = prototype(Y = as.matrix(1),
                               origData = 1)
         )


### SSM procedures
setClass("SSFilter",
         representation = representation(initStep = "FunctionWithControl",
                                         prepStep = "OptionalFunctionWithControl",
                                         predStep = "FunctionWithControl",
                                         corrStep = "FunctionWithControl")
         )
setClass("SSSmoother",
         representation = representation(smoothStep = "FunctionWithControl",
                                         smoothCov = "FunctionWithControl",
                                         lagoneCov = "FunctionWithControl"),
         contains = "SSFilter"
         )

### ---- List of Filters/Smoothers similar to DistrList ---- ###

setClass(Class = "SSFilterList",
            prototype = prototype(list(new("SSFilter"))),
            contains = "list",
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "SSFilter"))
                        stop("element ", i, " is no 'SSFilter'")
                return(TRUE)
            })

setClass(Class = "SSSmootherList",
            prototype = prototype(list(new("SSSmoother"))),
            contains = "list",
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "SSSmoother"))
                        stop("element ", i, " is no 'SSSmoother'")
                return(TRUE)
            })

## setClassUnion("SSClassOrRobFilter",
##               c("SSFilter", "SSrobFilter")
##               )
## setClassUnion("SSClassOrRobSmoother",
##               c("SSSmoother", "SSrobSmoother")
##               )
setClassUnion("SSFilterOrSmoother",
              c("SSFilterList", "SSSmootherList")
              )

setClass("SSDiagnostic",
         contains = c("VIRTUAL")
         )
setClass("SSDiagnosticFilter",
         contains = c("SSDiagnostic","OptionalList")
         )
setClass("SSDiagnosticRetValue",
         contains = c("SSDiagnostic","OptionalList")
         )
setClass("SSVariances",
         contains = "array"
         )
setClass("SSStateReconstr",
         contains = "matrix"
         )


### serve as class of return values for stepfunction initstep, predstep,
### @Bern: prepstep?,
### correction step (in variant as only 1-dim in time)
### and as slot classes (in variant as multi-step in time) for return value
### of recFilter


setClass("SSPredOrFilt",
         representation = representation(values = "numeric",
                                         call = "OptionalCall",
                                         variance = "matrix",
                                         uExo = "OptionalNumeric",
                                         wExo = "OptionalNumeric",
                                         dots.propagated = "OptionalList",
                                         ctrl.prpgtd = "OptionalList",
                                         control = "OptionalList",
                                         diagnostics = "SSDiagnosticFilter"),
         contains = c("VIRTUAL")
         )

setClass("SSStateSimulated",
         contains = "SSPredOrFiltRet"
         )
setClass("SSObsSimulated",
         contains = "SSPredOrFiltRet"
         )
setClass("SSSimulated",
         representation = representation(stateSimulated = "SSStateSimulated",
                                         obsSimulated = "SSObsSimulated")
         )

setClass("SSInitialized",
         contains = "SSPredOrFilt"
         )
setClass("SSPrepared",
         contains = "SSPredOrFilt"
         )
setClass("SSPredicted",
         contains = "SSPredOrFilt"
         )
setClass("SSFiltered",
         representation = representation(KalmanGain = "matrix",
                                         CovObs = "matrix",
                                         DeltaY = "numeric"),
         contains = "SSPredOrFilt"
         )
setClass("SSSmoothed",
         representation = representation(lagoneCov = "matrix"),
         contains = "SSPredOrFilt"
         )

setClassUnion("OptionalSSInitialized",
              c("SSInitialized","NULL")
              )
setClassUnion("OptionalSSPrepared",
              c("SSPrepared","NULL")
              )
setClassUnion("OptionalSSPredicted",
              c("SSPredicted","NULL")
              )
setClassUnion("OptionalSSFiltered",
              c("SSFiltered","NULL")
              )
setClassUnion("OptionalSSSmoothed",
              c("SSSmoothed","NULL")
              )

setClass("SSPredOrFiltRet",
         representation = representation(values = "matrix",
                                         call = "OptionalListOfCalls",
                                         variances = "array",
                                         lag1variances = "OptionalArray",
                                         uExo = "OptionalMatrix",
                                         wExo = "OptionalMatrix",
                                         dots.propagated = "OptionalList",
                                         control = "OptionalList",
                                         diagnostics = "SSDiagnosticFilter"),
         contains = "VIRTUAL"
         )

setClass("SSPreparedRet",
         contains = "SSPredOrFiltRet"
         )
setClass("SSPredictedRet",
         contains = "SSPredOrFiltRet"
         )
setClass("SSFilteredRet",
         representation = representation(KalmanGain = "array",
                                         CovObs = "array",
                                         DeltaY = "matrix"),
         contains = "SSPredOrFiltRet"
         )
setClass("SSSmoothedRet",
         representation = representation(lagoneCov = "array"),
         contains = "SSPredOrFiltRet"
         )

setClassUnion("OptionalSSPreparedRet",
              c("SSPreparedRet","NULL")
              )
setClassUnion("OptionalSSPredictedRet",
              c("SSPredictedRet","NULL")
              )
setClassUnion("OptionalSSFilteredRet",
              c("SSFilteredRet","NULL")
              )
setClassUnion("OptionalSSSmoothedRet",
              c("SSSmoothedRet","NULL")
              )


### User interfaces
setClass("SSInput",
         representation = representation(model = "SSM",
                                         obs = "SSObs",
                                         times = "SStimes",
                                         steps = "SSFilterOrSmoother")
         )
setClass("SSOutput",
         representation = representation(init = "list",
                                         prep = "OptionalList",
                                         pred = "list",
                                         filt = "list",
                                         smooth = "OptionalList")
         )

setClass("SSrecResult",
         representation = representation(input="SSInput",
                                         output="SSOutput")
         )


### Simulation
setClass("SSISimulation",
         representation = representation(name = "character",
                                         obs = "array",
                                         states = "array")
         )
setClass("SSCSimulation",
         representation = representation(radius = "numeric"),
         contains = "SSISimulation"
         )
setClass("SSSimulation",
         representation = representation(model = "SSM",
                                         runs = "numeric",
                                         seed = "numeric",
                                         times = "SStimes"),
         contains = "SSISimulation"
         )
setClass("SSSimList",
         contains = "list"
         )
    # Liste von Simulationen Typpruefung nicht
    # vorgesehen; Erzeugung in Generating Function, sodass alle Anforderungen
    # "passen"
setClass("SSContSimulation",
         representation = representation(SimList = "SSSimList"),
         contains = "SSSimulation"
         )


### Itermediate return values
    # ACHTUNG: entgegen Darstellung am 18.09.12 _nicht_ Rueckgabetyp
    # von createF createV,... sondern Rueckgabetyp der Funktion, die
    # in createF etc zurueckgegeben wird
setClass("SSretValueF",
         representation = representation(x1 = "numeric",
                                         FJcb = "matrix",
                                         RJcb = "matrix",
                                         t = "numeric",
                                         x0 = "numeric",
                                         v = "numeric",
                                         uNew = "numeric",
                                         control = "OptionalList",
                                         dots.propagated = "OptionalList",
                                         call = "call",
                                         diagnostics = "SSDiagnosticRetValue")
         )
setClass("SSretValueZ",
         representation = representation(y = "numeric",
                                         ZJcb = "matrix",
                                         TJcb = "matrix",
                                         t = "numeric",
                                         x1 = "numeric",
                                         eps = "numeric",
                                         wNew = "numeric",
                                         control = "OptionalList",
                                         dots.propagated = "OptionalList",
                                         call = "call",
                                         diagnostics = "SSDiagnosticRetValue")
         )
setClass("SSretValueQ",
         representation = representation(Q = "matrix",
                                         t = "numeric",
                                         x0 = "numeric",
                                         control = "OptionalList",
                                         dots.propagated = "OptionalList",
                                         call = "call",
                                         diagnostics = "SSDiagnosticRetValue")
         )
setClass("SSretValueV",
         representation = representation(V = "matrix",
                                         t = "numeric",
                                         x1 = "numeric",
                                         control = "OptionalList",
                                         dots.propagated = "OptionalList",
                                         call = "call",
                                         diagnostics = "SSDiagnosticRetValue")
         )

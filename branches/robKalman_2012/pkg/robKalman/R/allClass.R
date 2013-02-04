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
setClassUnion("OptionalCall",
               c("call","NULL")
               )


## Class: FunctionWithControl
setClass("FunctionWithControl", contains = "function")
### in validity method check whether has args dots.propagated and control

setClassUnion("OptionalFunctionWithControl",
               c("FunctionWithControl","NULL")
               )

setClassUnion("OptionalDistribution",
               c("Distribution","NULL")
               )


### SSM definitions
setClass("SSstateEq",
          representation = representation(Ffct = "FunctionWithControl",
                                          Qfct = "FunctionWithControl",
                                          muqfct = "OptionalFunction",
                                          Exofct = "OptionalFunctionWithControl",
                                          distrfct = "OptionalFunctionWithControl")
 )
setClass("SSobsEq",
          representation = representation(Zfct = "FunctionWithControl",
                                          Vfct = "FunctionWithControl",
                                          muvfct = "OptionalFunction",
                                          Exofct = "OptionalFunctionWithControl",
                                          distrfct = "OptionalFunctionWithControl")
 )
setClass("SSinitEq",
          representation = representation(a0 = "numeric",
                                          Sigma0 = "matrix",
                                          Exofct = "OptionalFunctionWithControl",
                                          distrfct = "OptionalDistribution")

 )
setClass("SSM",
          representation = representation(initEq  = "SSinitEq",
                                          statesEq = "SSstateEq",
                                          obsEq = "SSobsEq",
                                          pdim = "numeric", qdim = "numeric")
)
setClass("SStimes", representation = representation(times = "numeric",
                                   inX = "logical"))

setClass("SSObs",
          representation = representation(Y = "matrix", ### soll Matrix bleiben?
                                          origData = "ANY"),
          prototype = prototype(Y = as.matrix(1),
                                origData = 1)

 )


### SSM procedures
setClass("SSFilter", representation = representation(initStep = "FunctionWithControl",
                                    prepStep = "OptionalFunctionWithControl",
                                    predStep = "FunctionWithControl",
                                    corrStep = "FunctionWithControl"))
setClass("SSrobFilter", representation = representation(classFilter = "SSFilter",
                                       robFilter = "SSFilter"))

setClass("SSSmoother", representation = representation(filt = "SSfilter",
                                    smoothStep = "FunctionWithControl",
                                    smoothCov = "FunctionWithControl",
                                    lagoneCov = "FunctionWithControl"))
                                    
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
                      call = "OptionalCall",
                      variances = "array",
                      dots.propagated = "list",
                      control = "list",
                      diagnostics = "SSDiagnostic"),
                      contains = "VIRTUAL")

### serve as class of return values for stepfunction initstep, predstep, @Bern: prepstep?,
### correction step (in variant as only 1-dim in time)
### and as slot classes (in variant as multi-step in time) for return value
### of recFilter
setClass("SSPredicted", contains = "SSPredOrFilt")
setClass("SSFiltered",  representation = representation(KalmanGain = "array",
                      CovObs = "array", DeltaY = "matrix"),
                      contains = "SSPredOrFilt")
setClass("SSSmoothed", representation = representation(lagoneCov = "array"),
                     contains = "SSPredOrFilt")

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


### User interfaces
setClass("SSInput", representation = representation(steps = "SSClassOrRobFilter",
                                                   model = "SSM",
                                                   obs = "SSObs",
                                                   times = "SStimes"))

setClass("SSOutput", representation = representation(pred.cl = "SSPredicted",
                                                     filt.cl = "SSFiltered",
                                                     prep.cl = "OptionalSSPrepared",
                                                     smooth.cl = "OptionalSSSmoothed",
                                                     pred.rob = "OptionalSSPredicted",
                                                     filt.rob = "OptionalSSFiltered",
                                                     smooth.rob = "OptionalSSSmoothed",
                                                     prep.rob = "OptionalSSPrepared"))

setClass("SSrecResult", representation = representation(input="SSInput", output="SSOutput"))


### Simulation
setClass("SSISimulation", representation = representation(name = "character",
           obs = "array", states = "array"))
setClass("SSCSimulation", representation = representation(radius = "numeric"),
          contains = "SSISimulation")

setClass("SSSimulation", representation = representation(model = "SSM",
                 runs = "numeric", seed = "numeric", times = "SStimes"),
          contains = "SSISimulation")
setClass("SSSimList", contains = "list") ### Liste von Simulationen Typprüfung nicht
        ## vorgesehen; Erzeugung in Generating Function, sodass alle Anforderungen
        ## "passen"

setClass("SSContSimulation", representation = representation(SimList = "SSSimList"),
          contains = "SSSimulation")


### Itermediate return values
## ACHTUNG: entgegen Darstellung am 18.09.12 _nicht_ Rückgabetyp
###  von createF createV,... sondern Rückgabetyp der Funktion, die
##   in createF etc zurückgegeben wird

setClass("SSretValueF", representation = representation(x1 = "numeric",
                           Fmat = "matrix", Rmat = "matrix", t = "numeric",
                           x0 = "numeric", v = "numeric", u = "numeric",
                           control = "OptionalList", dots.propagated = "OptionalList",
                           call = "call", diagnostics = "SSDiagnostic"))

setClass("SSretValueZ", representation = representation(y = "numeric",
                           Zmat = "matrix", Tmat = "matrix", t = "numeric",
                           x1 = "numeric", eps = "numeric", w = "numeric",
                           control = "OptionalList", dots.propagated = "OptionalList",
                           call = "call", diagnostics = "SSDiagnostic"))
setClass("SSretValueQ", representation = representation( Q = "matrix",
                    t = "numeric", x0 = "numeric", exQ = "ANY",
                    control = "OptionalList", dots.propagated = "OptionalList",
                    call = "call", diagnostics = "SSDiagnostic"))

setClass("SSretValueV", representation = representation( V = "matrix",
                    t = "numeric", x1 = "numeric", exV = "ANY",
                    control = "OptionalList", dots.propagated = "OptionalList",
                    call = "call", diagnostics = "SSDiagnostic"))

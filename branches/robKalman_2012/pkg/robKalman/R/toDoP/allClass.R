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


## Class: FunctionWithControl
setClass("FunctionWithControl",
          representation = representation(fct = "function",
                                          dots = "OptionalList",
                                          control = "OptionalList",
                                          name = "character"),
          prototype = prototype(fct = function(x)x, dots = NULL, control = NULL,
                                name = gettext("a function with control"))
                      )
setClass("SSVar", contains="FunctionWithControl")
setClass("SSTransform", contains="FunctionWithControl")
setClass("SSExo", contains="FunctionWithControl")
setClass("SSstateEq",
          representation = representation(F = "SSTransform",
                                          Q = "SSVar",
                                          Exo = "SSVar",
                                          mu = "function",
                                          distr = "Distribution"),
          prototype = prototype(F = new("SSTransform",
                                         fct=function(...)1, control = NULL,
                                         name="state transition"),
                                Q = new("SSVar",
                                         fct=function(...)1, control = NULL,
                                         name="state variance"),
                                Exo = new("SSExo",
                                         fct=function(...)1, control = NULL,
                                         name="state Exogenous variable"),
                                mu = function(...)0,
                                distr = Norm())
 )
setClass("SSobsEq",
          representation = representation(Z = "SSTransform",
                                          V = "SSVar",
                                          Exo = "SSVar",
                                          mu = "function",
                                          distr = "Distribution"),
          prototype = prototype(Z = new("SSTransform",
                                         fct=function(...)1, control = NULL,
                                         name="state transition"),
                                V = new("SSVar",
                                         fct=function(...)1, control = NULL,
                                         name="state variance"),
                                Exo = new("SSExo",
                                         fct=function(...)1, control = NULL,
                                         name="state Exogenous variable"),
                                mu = function(...)0,
                                distr = Norm())
 )
setClass("SSstartEq",
          representation = representation(a0 = "numeric",
                                          Sigma0 = "matrix",
                                          Exo = "SSVar",
                                          mu = "function",
                                          distr = "Distribution"),
          prototype = prototype(a0 = 1,
                                Sigma0 = matrix(1,1,1),
                                Exo = new("SSExo",
                                         fct=function(...)0, control = NULL,
                                         name="state Exogenous variable"),
                                mu = function(...)0,
                                distr = Norm())

 )
setClass("SSmod",
          representation = representation(StartEq  = "SSstartEq",
                                          StatesEq = "SSstateEq",
                                          ObsEq = "SSobsEq")
)

if(!isGeneric("solve")){
    setGeneric("solve", function(a,b,...) standardGeneric("solve"))
}
############################################################################
# Access methods
############################################################################

if(!isGeneric("name")) 
    setGeneric("name", function(object) standardGeneric("name"))

#if(!isGeneric("fct"))
#    setGeneric("fct", function(object) standardGeneric("fct"))

if(!isGeneric("dots.propagated"))
    setGeneric("dots.propagated", function(object) standardGeneric("dots.propagated"))

if(!isGeneric("control"))
    setGeneric("control", function(object) standardGeneric("control"))

if(!isGeneric("F"))
   setGeneric("F", function(object, ...) standardGeneric("F"))
if(!isGeneric("Z"))
   setGeneric("Z", function(object, ...) standardGeneric("Z"))
if(!isGeneric("Q"))
   setGeneric("Q", function(object, ...) standardGeneric("Q"))
if(!isGeneric("V"))
   setGeneric("V", function(object, ...) standardGeneric("V"))
if(!isGeneric("createF"))
   setGeneric("createF", function(object, ...) standardGeneric("createF"))
if(!isGeneric("createZ"))
   setGeneric("createZ", function(object, ...) standardGeneric("createZ"))
if(!isGeneric("createQ"))
   setGeneric("createQ", function(object, ...) standardGeneric("createQ"))
if(!isGeneric("createV"))
   setGeneric("createV", function(object, ...) standardGeneric("createV"))
if(!isGeneric("createExo"))
   setGeneric("createExo", function(object, ...) standardGeneric("createExo"))
if(!isGeneric("R"))
   setGeneric("R", function(object, ...) standardGeneric("R"))
if(!isGeneric("t"))
   setGeneric("t", function(object, ...) standardGeneric("t"))
if(!isGeneric("T"))
   setGeneric("T", function(object, ...) standardGeneric("T"))
if(!isGeneric("Exo"))
   setGeneric("Exo", function(object, ...) standardGeneric("Exo"))
if(!isGeneric("distr"))
   setGeneric("distr", function(object) standardGeneric("distr"))
if(!isGeneric("Exo.states"))
   setGeneric("Exo.states", function(object, ...) standardGeneric("Exo.states"))
if(!isGeneric("Exo.init"))
   setGeneric("Exo.init", function(object, ...) standardGeneric("Exo.init"))
if(!isGeneric("Exo.obs"))
   setGeneric("Exo.obs", function(object, ...) standardGeneric("Exo.obs"))
if(!isGeneric("distr.states"))
   setGeneric("distr.states", function(object, ...) standardGeneric("distr.states"))
if(!isGeneric("distr.init"))
   setGeneric("distr.init", function(object, ...) standardGeneric("distr.init"))
if(!isGeneric("distr.obs"))
   setGeneric("distr.obs", function(object, ...) standardGeneric("distr.obs"))

if(!isGeneric("a0"))
   setGeneric("a0", function(object) standardGeneric("a0"))
if(!isGeneric("Sigma0"))
   setGeneric("Sigma0", function(object) standardGeneric("Sigma0"))
if(!isGeneric("times"))
   setGeneric("times", function(x,...) standardGeneric("times"))
if(!isGeneric("inX"))
   setGeneric("inX", function(object) standardGeneric("inX"))

if(!isGeneric("origData"))
   setGeneric("origData", function(object) standardGeneric("origData"))

if(!isGeneric("initEq"))
   setGeneric("initEq", function(object) standardGeneric("initEq"))
if(!isGeneric("statesEq"))
   setGeneric("statesEq", function(object) standardGeneric("statesEq"))
if(!isGeneric("obsEq"))
   setGeneric("obsEq", function(object) standardGeneric("obsEq"))

if(!isGeneric("initStep"))
   setGeneric("initStep", function(object) standardGeneric("initStep"))
if(!isGeneric("predStep"))
   setGeneric("predStep", function(object) standardGeneric("predStep"))
if(!isGeneric("prepStep"))
   setGeneric("prepStep", function(object) standardGeneric("prepStep"))
if(!isGeneric("corrStep"))
   setGeneric("corrStep", function(object) standardGeneric("corrStep"))

if(!isGeneric("classFilter"))
   setGeneric("classFilter", function(object) standardGeneric("classFilter"))
if(!isGeneric("robFilter"))
   setGeneric("robFilter", function(object) standardGeneric("robFilter"))

if(!isGeneric("filt"))
   setGeneric("filt", function(object) standardGeneric("filt"))
if(!isGeneric("smoothStep"))
   setGeneric("smoothStep", function(object) standardGeneric("smoothStep"))
if(!isGeneric("smoothCov"))
   setGeneric("smoothCov", function(object) standardGeneric("smoothCov"))
if(!isGeneric("lagoneCov"))
   setGeneric("lagoneCov", function(object) standardGeneric("lagoneCov"))
if(!isGeneric("classSmoother"))
   setGeneric("classSmoother", function(object) standardGeneric("classSmoother"))
if(!isGeneric("robFilter"))
   setGeneric("robSmoother", function(object) standardGeneric("robSmoother"))

if(!isGeneric("diagnostics"))
   setGeneric("diagnostics", function(object) standardGeneric("diagnostics"))
if(!isGeneric("values"))
   setGeneric("values", function(object) standardGeneric("values"))
if(!isGeneric("variances"))
   setGeneric("variances", function(object) standardGeneric("variances"))

if(!isGeneric("KalmanGain"))
   setGeneric("KalmanGain", function(object) standardGeneric("KalmanGain"))
if(!isGeneric("CovObs"))
   setGeneric("CovObs", function(object) standardGeneric("CovObs"))
if(!isGeneric("DeltaY"))
   setGeneric("DeltaY", function(object) standardGeneric("DeltaY"))

if(!isGeneric("steps"))
   setGeneric("steps", function(object) standardGeneric("steps"))
if(!isGeneric("model"))
   setGeneric("model", function(object) standardGeneric("model"))
if(!isGeneric("obs"))
   setGeneric("obs", function(object) standardGeneric("obs"))
if(!isGeneric("states"))
   setGeneric("state", function(object) standardGeneric("states"))
if(!isGeneric("times"))
   setGeneric("times", function(object) standardGeneric("times"))

if(!isGeneric("pred.cl"))
   setGeneric("pred.cl", function(object) standardGeneric("pred.cl"))
if(!isGeneric("filt.cl"))
   setGeneric("filt.cl", function(object) standardGeneric("filt.cl"))
if(!isGeneric("smooth.cl"))
   setGeneric("smooth.cl", function(object) standardGeneric("smooth.cl"))

if(!isGeneric("pred.rob"))
   setGeneric("pred.rob", function(object) standardGeneric("pred.rob"))
if(!isGeneric("filt.rob"))
   setGeneric("filt.rob", function(object) standardGeneric("filt.rob"))
if(!isGeneric("smooth.rob"))
   setGeneric("smooth.rob", function(object) standardGeneric("smooth.rob"))

if(!isGeneric("input"))
   setGeneric("input", function(object) standardGeneric("input"))
if(!isGeneric("output"))
   setGeneric("output", function(object) standardGeneric("output"))


if(!isGeneric("runs"))
   setGeneric("runs", function(object, ...) standardGeneric("runs"))
if(!isGeneric("seed"))
   setGeneric("seed", function(object) standardGeneric("seed"))

if(!isGeneric("radius"))
   setGeneric("radius", function(object) standardGeneric("radius"))

if(!isGeneric("SimList"))
   setGeneric("SimList", function(object) standardGeneric("SimList"))

if(!isGeneric("x0"))
   setGeneric("x0", function(object) standardGeneric("x0"))

if(!isGeneric("x1"))
   setGeneric("x1", function(object) standardGeneric("x1"))

if(!isGeneric("v"))
   setGeneric("v", function(object) standardGeneric("v"))

if(!isGeneric("u"))
   setGeneric("u", function(object) standardGeneric("u"))

if(!isGeneric("eps"))
   setGeneric("eps", function(object) standardGeneric("eps"))

if(!isGeneric("w"))
   setGeneric("w", function(object) standardGeneric("w"))

if(!isGeneric("exQ"))
   setGeneric("exQ", function(object) standardGeneric("exQ"))

if(!isGeneric("exV"))
   setGeneric("exV", function(object) standardGeneric("exV"))



if(!isGeneric("simulate"))
   setGeneric("simulate",
               function(object, nsim=-1, seed=-1, ...)
                         standardGeneric("simulate"))

if(!isGeneric(".make.project")) 
setGeneric(".make.project",function(object, ...) standardGeneric(".make.project"))

if(!isGeneric("kalman")) 
setGeneric("kalman",function(smooth, ...) standardGeneric("kalman"))

if(!isGeneric("kalmanRob")) 
setGeneric("kalmanRob",function(method, smooth, ...) standardGeneric("kalmanRob"))

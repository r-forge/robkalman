SSM <- function(F, Q, Exo.state = NULL, R = NULL, distr.state = NULL,
                Z, V, Exo.obs = NULL, T = NULL, distr.obs = NULL,
                a0, Sigma0, Exo.ini =NULL, distr.ini = NULL,
                p, q){
  Exo.state.ret <- if(!is.null(Exo.state)) createExo(Exo.state) else NULL
  Exo.obs.ret <- if(!is.null(Exo.obs)) createExo(Exo.obs) else NULL
  Exo.ini.ret <- if(!is.null(Exo.ini)) createExo(Exo.ini)  else NULL

  Fret <- createF(F,R, Exo.state.ret)
  Zret <- createZ(Z,T, Exo.state.obs)
  Qret <- createQ(Q)
  Vret <- createV(V)
  
  stateEq <- new("SSstateEq", Ffct=Fret, Qfct=Qret, Exofct = Exo.state.ret, distrfct = distr.state)
  obsEq <- new("SSobsEq", Zfct=Zret, Vfct=Vret, Exofct = Exo.obs.ret, distrfct = distr.obs)
  initEq <- new("SSinitEq", a0=a0, Sigma0=Sigma0, Exofct = Exo.ini.ret, distrfct = distr.ini)

  return(new("SSM",initEq  = initEq, statesEq = stateEq, obsEq = obsEq, p = p, q = q)
}

setMethod("statesEq", "SSM", function(object) object@statesEq)
setMethod("obsEq", "SSM", function(object) object@obsEq)
setMethod("initEq", "SSM", function(object) object@initEq)

setMethod("F", "SSstateEq", function(object) object@F)
setMethod("F", "SSM", function(object) statesEq(object)@F)
setMethod("Q", "SSstateEq", function(object) object@Q)
setMethod("Q", "SSM", function(object) statesEq(object)@Q)
setMethod("Z", "SSstateEq", function(object) object@Z)
setMethod("Z", "SSM", function(object) obsEq(object)@Z)
setMethod("V", "SSstateEq", function(object) object@V)
setMethod("V", "SSM", function(object) obsEq(object)@V)
setMethod("a0", "SSstateEq", function(object) object@a0)
setMethod("a0", "SSM", function(object) initEq(object)@a0)
setMethod("Sigma0", "SSinitEq", function(object) object@Sigma0)
setMethod("Sigma0", "SSM", function(object) initEq(object)@Sigma0)

setMethod("createF", "matrix", function(object, R, Exo){

})
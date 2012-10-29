SSM <- function(F, Q, Exo.state = NULL, R = NULL, distr.state = NULL,
                Z, V, Exo.obs = NULL, T = NULL, distr.obs = NULL,
                a0, Sigma0, Exo.ini =NULL, distr.ini = NULL,
                p, q) {
  # Checking dimensions of the variables of the fct
  
  if(! all(dim(F)==c(p,p))) print("F is of wrong dimension")
  if(! all(dim(Z)==c(q,p))) print("Z is of wrong dimension")
  if(! all(dim(Q)==c(p,p))) print("Q is of wrong dimension")
  if(! all(dim(V)==c(q,q))) print("V is of wrong dimension")
  if(! length(a0)==p) print ("a0 is of wrong dimension")
  if(! all(dim(Sigma0)==c(p,p))) print("Sigma0 is of wrong dimension")
  
  if(! (length(Ex0.ini)==p|identical(Ex0.ini,NULL))) print ("Ex0.ini is of wrong dimension")
  if(! (length(Exo.state)==p|identical(Ex0.state,NULL))) print ("Exo.state is of wrong dimension")
  if(! (length(Exo.obs)==q|identical(Ex0.obs,NULL))) print ("Exo.obs is of wrong dimension")
  
  if(! (length(distr.ini)==p|identical(distr.ini,NULL))) print ("distr.ini is of wrong dimension")
  if(! (length(distr.state)==p|identical(distr.state,NULL))) print ("distr.state is of wrong dimension")
  if(! (length(distr.obs)==q|identical(distr.obs,NULL))) print ("distr.obs is of wrong dimension")
  
  #########################################################################
  
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

  return(new("SSM",initEq  = initEq, statesEq = stateEq, obsEq = obsEq, p = p, q = q))
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
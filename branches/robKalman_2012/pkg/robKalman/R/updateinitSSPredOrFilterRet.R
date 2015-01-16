updateSSPredOrFiltRet <- function(old, new, i, smooth=FALSE){
  ### later: type checking
  old@values[,i] <- new@values
  if(!is.null(new@call)) old@call[[i]] <- new@call
  old@variance[,,i] <- new@variance
  if(smooth) old@lag1variance[,,i] <- new@lag1variance
  if(!is.null(new@uExo)) old@uExo[,i] <- new@uExo
  if(!is.null(new@wExo)) old@wExo[,i] <- new@wExo
  if(!is.null(new@dots.propagated)) old@dots.propagated[[i]] <- new@dots.propagated
  if(!is.null(new@control)&&i==1L) old@control <- new@control
  if(!is.null(new@SSDiagnosticFilter)) old@SSDiagnosticFilter[[i]] <- new@SSDiagnosticFilter

}

initSSPredOrFiltRet <- function(rdim, trdim,  pdim, tdim, qdim, tydim, withuExo, withwExo, withdots.prop,
                                  withcontrol, withDiagnosticFilter, smooth=FALSE){
  v <- matrix(NA,rdim,trdim)
  vm <- array(NA,dim=c(rdim,rdim,trdim))
  uExo <- if(withuExo) matrix(NA,pdim,tdim) else NULL
  wExo <- if(withwExo) matrix(NA,qdim,tydim) else NULL
  dots.prop <- if(withdots.prop) vector("list", trdim) else NULL
  control <- if(withcontrol) vector("list", trdim) else NULL
  DiagnosticFilter <- if(DiagnosticFilter) vector("list",trdim) else NULL
  l1vm <- if(smooth) array(NA,dim=c(rdim,rdim,trdim-1) else NULL
  new("SSPredOrFiltRet", value = v, variance = vm, lag1variance = l1vm,
              uExo = uExo, wExo = wExo,
              dot.propagated = dot.prop, control = control,
              DiagnosticFilter = DiagnosticFilter)
}
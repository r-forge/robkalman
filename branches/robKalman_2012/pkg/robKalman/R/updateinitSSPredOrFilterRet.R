updateSSPredOrFiltRet <- function(old, new, i){
  ### later: type checking
  old@values[,i] <- new@values
  if(!is.null(new@call)) old@call[[i]] <- new@call
  old@variance[,,i] <- new@variance
  if(!is.null(new@uExo)) old@uExo[,i] <- new@uExo
  if(!is.null(new@wExo)) old@wExo[,i] <- new@wExo
  if(!is.null(new@dots.propagated)) old@dots.propagated[[i]] <- new@dots.propagated
  if(!is.null(new@control)&&i==1L) old@control <- new@control
  if(!is.null(new@SSDiagnosticFilter)) old@SSDiagnosticFilter[[i]] <- new@SSDiagnosticFilter

}

initSSPredOrFiltRet <- function(pdim, qdim, tdim, withuExo, withwExo, withdots.prop,
                                  withcontrol, withDiagnosticFilter){
  v <- matrix(NA,pdim,tdim)
  vm <- array(NA,dim=c(pdim,pdim,tdim))
  uExo <- if(withuExo) matrix(NA,pdim,tdim) else NULL
  wExo <- if(withwExo) matrix(NA,qdim,tdim) else NULL
  dots.prop <- if(withdots.prop) vector("list", tdim) else NULL
  control <- if(withcontrol) vector("list", tdim) else NULL
  DiagnosticFilter <- if(DiagnosticFilter) vector("list",tdim) else NULL
  new("SSPredOrFiltRet", value = v, variance = vm, uExo = uExo, wExo = wExo,
              dot.propagated = dot.prop, control = control,
              DiagnosticFilter = DiagnosticFilter)
}
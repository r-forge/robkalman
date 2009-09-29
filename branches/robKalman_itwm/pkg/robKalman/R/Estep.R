EStepSSM <- function(y, a, S = Q, F, Z, Q, V, initSc = .cKinitstep, predSc = .cKpredstep,
                   corrSc = .cKcorrstep, smoothSc = .cKsmoothstep,
                   initSr = NULL, predSr = NULL, corrSr = NULL, smoothSr = NULL,
                   calibrateFilter = NULL,
                   nsim = 0, seed = NULL, ..., dropRuns = FALSE)
{Y <- array(y, dim=c(nrow(y),1,ncol(y)))

 dots <- list(...)
 if(!is.null(calibrateFilter)){
    SS <- limitS(S = S, F = F, Q = Q, Z = Z, V = V)
    calibF <- calibrateFilter(..., S = SS, Z = Z, V = V)
 }
 resFilter <- do.call(recursiveFilter, args = c( list( Y=Y, a=a, S=Q, F=F, Q=Q,
                   Z=Z, V=V, initSc = initSc, predSc = predSc,
                   corrSc = corrSc,
                   initSr = initSr, predSr = predSr, corrSr = corrSr,
                   nsim = nsim, seed = seed), calibF, dots,
                   dropRuns = dropRuns))
  res <- recursiveFPSmoother( Y = Y,
                          F = F, Q = Q, V = V, Z = Z,
                          smoothSc = smoothSc, smoothSr = smoothSr,
                          KG = resFilter$KG, xf.c = resFilter$xf,
                          S0.c = resFilter$S0, S1.c = resFilter$S1,
                          xf.r = resFilter$xrf,
                          S0.r = resFilter$Sr0, S1.r = resFilter$Sr1,
                          KG.r = resFilter$KGr, ..., dropRuns = dropRuns)

  if(is.null(initSr)&&is.null(predSr)&&is.null(corrSr)&&is.null(smoothSr))
     return(list(M = resFilter$KG, x = res$xS, P = res$SS, Pdep = res$SS1))
  else
     return(list(M = resFilter$KGr, x = res$xSr, P = res$SSr,
                  Pdep = res$SS1r))
}

EStepSSMclass <- function(y, a, S = Q, F, Z, Q, V,  ...)
{EStepSSM(y = y, a = a, S = S, F = F, Z = Z , Q = Q, V = V,
          initSc = .cKinitstep, predSc = .cKpredstep,
          corrSc = .cKcorrstep,  smoothSc = .cKsmoothstep,
          initSr = NULL, predSr = NULL, corrSr = NULL, smoothSr = NULL,
          calibrateFilter = NULL,
          nsim = 0, seed = NULL, ..., dropRuns = FALSE)}

EStepSSMrLS.AO <- function(y, a, S = Q, F, Z, Q, V,  ...)
{EStepSSM(y = y, a = a, S = S, F = F, Z = Z , Q = Q, V = V,
          initSc = .cKinitstep, predSc = .cKpredstep,
          corrSc = .cKcorrstep,  smoothSc = .cKsmoothstep,
          initSr = .cKinitstep, predSr = .cKpredstep,
          corrSr = .rLScorrstep, smoothSr = .cKsmoothstep,
          calibrateFilter = rLScalibrateB,
          nsim = 0, seed = NULL, ..., dropRuns = FALSE)}

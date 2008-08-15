#### for Testing purposes:
###
## generation of ideal and contaminated realisations of
## multivariate, [default = Gaussian], linear state space models

rcvcont <-  function(mi, Si, mc, Sc, r, nsim = 1, withIndId = FALSE,
                     rid = mvrnorm, rcont = mvrnorm) 
        {# print(list(mi=mi,Si=Si,mc=mc, Sc=Sc))
         U <- rbinom(nsim, size = 1, prob = r); 
         Xid <- rid(nsim, mi, Si)
         X <- (1-U) * Xid + U * rcont(nsim, mc, Sc)
         if ( withIndId )
             return(list( X   = matrix(X,  ncol = nsim), Ind = (U==1), 
                          Xid = matrix(Xid,ncol = nsim)))
         else return (X)    
         }


if(!isGeneric("simulate")) 
   setGeneric("simulate",
               function(object, nsim = -1, seed = -1, ...)
                        standardGeneric("simulate"))

setMethod("simulate", signature("SSM"), function(object, nsim = 1, seed = -1,
           withOutlier = TRUE,
           r.io = 0, r.ao = 0, 
           r.v0.id = r.v.id, r.v0.cont = r.v.cont,
           r.v.id = mvrnorm, r.v.cont = mvrnorm, 
           r.e.id = mvrnorm, r.e.cont = mvrnorm, 
           m.v0.cont = numeric(object@p),m.v.cont = numeric(object@p), 
           m.e.cont = numeric(object@q), 
           S.cont = object@S, Q.cont = object@Q, V.cont = object@V, 
           drop = FALSE){

    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    if (seed==-1) 
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

    object <- makeArrayRepresentation(object)
    Tn <- length(object@time)

    states    <- array(0, dim = c(object@p, nsim, Tn))
    obs       <- array(0, dim = c(object@q, nsim, Tn-1))


    if(withOutlier){

    ### coerce to standard representation

    S.cont <- matrix(.stdRepres(S.cont, object@p, object@p, withTest = TRUE, 
                               Tn = 2, time = time(object@time)[1]), 
                     nrow = object@p, ncol = object@p)
    Q.cont <- .stdRepres(Q.cont, object@p, object@p, withTest = TRUE, 
                        Tn = Tn, time = time(object@time)[2:Tn])
    V.cont <- .stdRepres(V.cont, object@q, object@q, withTest = TRUE, 
                           Tn = Tn, time = time(object@time)[2:Tn])

    m.v0.cont <- as.numeric(.stdRepres.vec(m.v0.cont, object@p, Tn = 2, 
                               time = time(object@time)[1]))

    m.v.cont <- .stdRepres.vec(m.v.cont, object@p, Tn = Tn, 
                              time = time(object@time)[2:Tn])
    
    m.e.cont <- .stdRepres.vec(m.e.cont, object@q, Tn = Tn, 
                              time = time(object@time)[2:Tn])

    states.id <- array(0, dim = c(object@p, nsim, Tn))
    Ind.IO    <- matrix(FALSE, nrow = nsim, ncol = Tn)
    obs.id    <- array(0, dim = c(object@q, nsim, Tn-1))
    Ind.AO    <- matrix(FALSE, nrow = nsim, ncol = Tn-1)
    }


    if(withOutlier){
       v.sim <- rcvcont(mi = object@a, Si = object@S, 
                     mc = m.v0.cont, Sc = S.cont, 
                     r = r.io, nsim = nsim, withIndId = TRUE,
                     rid = r.v0.id, rcont = r.v0.cont)
       states[ ,,1] <- v.sim$X
       states.id[ ,,1] <- v.sim$Xid
       Ind.IO[,1] <- v.sim$Ind
    }else 
       states[ ,,1] <- t(r.v0.id(nsim, numeric(object@p), object@S))
  
    for (i in (1:(Tn-1)))
        { 
         if(withOutlier){
        
             v.sim <- rcvcont(mi = object@a * 0, Si = object@Q[,,i], 
                              mc = m.v.cont[,i], Sc = Q.cont[,,i], 
                              r = r.io, nsim = nsim, withIndId = TRUE,
                              rid = r.v.id, rcont = r.v.cont)
             Ind.IO[ ,i+1] <- v.sim$Ind
             FF <- matrix(object@F[,,i], nrow = object@p, ncol = object@p)
             states[,,i+1] <- FF %*% matrix(states[,, i], 
                                         nrow = object@p, ncol = nsim) + 
                              matrix(v.sim$X, nrow = object@p,  ncol = nsim)
             states.id[,,i+1] <- FF %*% matrix(states[,, i], 
                                         nrow = object@p, ncol = nsim) + 
                              matrix(v.sim$Xid, nrow = object@p,  ncol = nsim)
           
             e.sim <- rcvcont(mi = numeric(object@q), Si = object@V[,,i], 
                              mc = m.e.cont[,i], Sc = V.cont[,,i], 
                              r = r.ao, nsim = nsim, withIndId = TRUE,
                              rid = r.e.id, rcont = r.e.cont)
             Ind.AO[ ,i] <- e.sim$Ind
             ZZ <- matrix(object@Z[,,i], nrow = object@q, ncol = object@p)
             obs[,, i] <- ZZ %*% matrix(states[,, (i+1)], 
                                         nrow = object@p, ncol = nsim) + 
                                 matrix(e.sim$X, nrow = object@q,  ncol = nsim)
             obs.id[,, i] <- ZZ %*% matrix(states.id[,, (i+1)], 
                                         nrow = object@p, ncol = nsim)
                                 matrix(e.sim$Xid, nrow = object@q,  ncol = nsim)
          }else{
             FF <- matrix(object@F[,,i], nrow = object@p, ncol = object@p)
             states[,,i+1] <- FF %*% matrix(states[,, i], 
                                         nrow = object@p, ncol = nsim) + 
                              matrix(r.v.id(nsim, numeric(object@p), 
                                            object@Q[,,i]),
                                     nrow = object@p,  ncol = nsim)
             ZZ <- matrix(object@Z[,,i], nrow = object@q, ncol = object@p)
             obs[,,i] <- ZZ %*% matrix(states[,, i+1], 
                                         nrow = object@p, ncol = nsim) + 
                              matrix(r.e.id(nsim, numeric(object@q), 
                                            object@V[,,i]), nrow = object@q, 
                                            ncol = nsim)
          }
        }                                

         if(withOutlier){
             return(list(states = states[,,,drop = drop],
                         obs = obs[,,,drop = drop],
                         states.id = states.id[,,,drop = drop],
                         obs.id = obs.id[,,,drop = drop],
                         Ind.AO = Ind.AO, Ind.IO = Ind.IO,
                         RNGstate = RNGstate,
                         r.io = r.io, r.ao = r.ao, 
                         r.v0.id = r.v0.id, r.v0.cont = r.v0.cont,
                         r.v.id = r.v.id, r.v.cont = r.v.cont, 
                         r.e.id = r.e.id, r.e.cont = r.e.cont, 
                         m.v0.cont = m.v0.cont, m.v.cont = m.v.cont, 
                         m.e.cont = m.e.cont,
                         S.cont = S.cont, Q.cont = Q.cont, 
                         V.cont = V.cont))  
         }else 
             return(list(states = states[,,,drop = drop],
                         obs = obs[,,,drop = drop],
                         RNGstate = RNGstate, r.v0.id = r.v0.id, 
                         r.v.id = r.v.id, r.e.id = r.e.id))
})

setMethod("simulate", signature("SSMwithDistribution"), 
               function(object, nsim = 1, seed = -1, drop = FALSE){
           SSM <- object@SSM
           Distr <- object@Distribution    
           erg <- simulate(SSM, nsim = nsim, seed = seed,
                           withOutlier = FALSE,
                           r.io = 0, r.ao = 0, 
                           r.v0.id = Distr@r.init, r.v0.cont = NULL,
                           r.v.id = Distr@r.innov, r.v.cont = NULL, 
                           r.e.id = Distr@r.obs, r.e.cont = NULL, 
                           m.v0.cont = NULL, m.v.cont = NULL, m.e.cont = NULL, 
                           S.cont = NULL, Q.cont = NULL, V.cont = NULL, 
                           drop = drop)
           return(new("SSMsimulation", SSM = SSM, Distr = Distr,
                      RNGstate = erg$RNGstate, states = erg$states, 
                      obs = erg$obs))
})

setMethod("simulate", signature("SSMwithConvDistribution"), 
               function(object, nsim = 1, seed = -1, drop = FALSE){
           SSM <- object@SSM
           Distr <- object@Distribution    
           erg <- simulate(SSM, nsim = nsim, seed = seed,
                           withOutlier = TRUE,
                           r.io = Distr@r.IO, r.ao = Distr@r.AO, 
                           r.v0.id = Distr@ideal@r.init, 
                           r.v0.cont = Distr@cont@r.init,
                           r.v.id = Distr@ideal@r.innov, 
                           r.v.cont = Distr@cont@r.innov, 
                           r.e.id = Distr@ideal@r.obs, 
                           r.e.cont = Distr@cont@r.obs, 
                           m.v0.cont = Distr@cont@m.init,
                           m.v.cont = Distr@cont@m.innov, 
                           m.e.cont = Distr@cont@m.obs, 
                           S.cont = Distr@cont@S.init, 
                           Q.cont = Distr@cont@S.innov, 
                           V.cont = Distr@cont@S.obs,  
                           drop = drop)
           return(new("SSMcontSimulation", SSM = SSM, Distr = Distr,
                      RNGstate = erg$RNGstate, 
                      states = erg$states, obs = erg$obs,
                      states.id = erg$states.id, obs.id = erg$obs.id,
                      Ind.AO = erg$Ind.AO, Ind.IO = erg$Ind.IO))
})


setMethod("kalmanRob", signature(method = "robrecControl", smooth = "missing"),
           function(method = rLSControl(), Y, SSM, nsim = 0, seed = NULL){
   SSMa <- makeArrayRepresentation(SSM)
   erg <- do.call(recursiveFilter,
                  args = c(list(Y = Y, a = SSM@a, S = SSMa@S,
                         F = SSMa@F, Q = SSMa@Q, Z = SSMa@Z, V = SSMa@V,
                         nsim = nsim, seed = seed,
                         initSc = init(method),
                         predSc = predict(method),
                         corrSc = correct(method),
                         initSr = init.rob(method),
                         predSr = predict.rob(method),
                         corrSr = correct.rob(method)),
                         controls(method), dropRuns = FALSE)
                  )

   return(generateRobRecFilter(
            name = name(method), name.rob = name.rob(method),
            SSM = SSM, Y = Y, time = SSM@time,
            Xf = erg$Xf, Xp = erg$Xp, S0 = erg$S0, S1 = erg$S1, KG = erg$KG,
            Xrf = erg$Xrf, Xrp = erg$Xrp, Sr0 = erg$Sr0, Sr1 = erg$Sr1,
            KGr = erg$KGr, IndIO = erg$IndIO, IndAO = erg$IndAO,
            rob0L = erg$rob0L, rob1L = erg$rob1L, St0s = erg$St0s,
            St1s = erg$St1s, nsim = erg$nsim, RNGstate = erg$RNGstate
            ))
   })

setMethod("kalman", signature(smooth = "missing"),
          function(Y, SSM){
   method <- KalmanControl()
   SSMa <- makeArrayRepresentation(SSM)
   erg <- recursiveFilter(Y = Y, a = SSM@a, S = SSMa@S,
                          F = SSMa@F, Q = SSMa@Q, Z = SSMa@Z, V = SSMa@V,
                          initSc = init(method),
                          predSc = predict(method),
                          corrSc = correct(method), dropRuns = FALSE)
   return(generateRecFilter(
          name = name(method), SSM = SSM, Y = Y, time = SSM@time,
          Xf = erg$Xf, Xp = erg$Xp, S0 = erg$S0, S1 = erg$S1, KG = erg$KG
          ))})


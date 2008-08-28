generateRecFilter <- function(name = "classical Kalman Filter",
     SSM, Y, time, Xf, Xp, S0, S1, KG)
new("recFilter", name = name, SSM = SSM, Y = Y,
     X.filtered = Xf, X.predicted = Xp,
     Cov.filtered = S0, Cov.predicted = S1, Kalman.Gain = KG,
     time = time)

generateRobRecFilter <- function(name = "classical Kalman Filter",
     name.rob = "robust recursive Filter",
     SSM, Y, time, Xf, Xp, Xrf, Xrp, S0, S1, KG, Sr0, Sr1,
     KGr, IndIO, IndAO, rob0L, rob1L, St0s, St1s, nsim, RNGstate)
new("robrecFilter", name = name, name.rob = name.rob, SSM = SSM, Y = Y,
     X.filtered = Xf, X.predicted = Xp,
     Cov.filtered = S0, Cov.predicted = S1, Kalman.Gain = KG,
     time = time, X.rob.filtered = Xrf,  X.rob.predicted = Xrp,
     Cov.rob.filtered = Sr0, Cov.rob.predicted = Sr1, Kalman.rob.Gain = KGr,
     IndIO = IndIO, IndAO = IndAO, nsim = nsim, RNGstate = RNGstate,
     rob.correction.ctrl = rob0L, rob.prediction.ctrl = rob1L,
     Cov.rob.filtered.sim = St0s, Cov.rob.predicted.sim = St1s)

#---------------------------------------------------------------------------
# Helping methods (for time projections)
#---------------------------------------------------------------------------

setMethod(".make.project", "SSM", function(object, SSMtime, i, time) {
       if(is.null(i)&is.null(time)){
           return(object)
           }
       if(is.null(i)&!is.null(time))
          i <- match(time, time(SSMtime))
       if(!is.null(i)){
            SSMb <- makeArrayRepresentation(object)
            SSMb@time <- SSMb@time[i]
            SSMb@F <- SSMb@F[,,i]
            SSMb@Z <- SSMb@Z[,,i]
            SSMb@Q <- SSMb@Q[,,i]
            SSMb@V <- SSMb@V[,,i]
            return(SSMb)
            }
       stop("wrong indices or times")
})

setMethod(".make.project", "array", function(object, SSMtime, i, time,
                            minus1 = TRUE) {
       tm <- if(minus1) time(SSMtime)[-1] else time(SSMtime)
       if(is.null(i)&is.null(time)){
           dm <- dimnames(object)
           if(is.null(dm)) dimnames(object) <- list(NULL, NULL, tm)
           else dimnames(object)[[3]] <- tm
           return(object)
           }
       if(is.null(i)&!is.null(time))
          i <- match(time, time(SSMtime))
       if(!is.null(i)){
          if( minus1) i <- i[i!=1]
           tm <- time(SSMtime[i])
           obj0 <- object[,,i,drop=FALSE]
           dm <- dimnames(object)
           dimnames(obj0) <- NULL
           if(is.null(dm)) 
              dimnames(obj0)[[3]] <- tm
           else
              dimnames(obj0) <- list(dm[[1]],dm[[2]],tm)
           return(obj0)
       }
       stop("wrong indices or times")
})

setMethod(".make.project", "matrix", function(object, SSMtime, i, time,
                            minus1 = TRUE) {
       tm <- if(minus1) time(SSMtime)[-1] else time(SSMtime)
       if(is.null(i)&is.null(time))
          return(zoo(object, tm))
       if(is.null(i)&!is.null(time))
          i <- match(time, time(SSMtime))
       if(!is.null(i)){
          if( minus1) i <- i[i!=1]
          return(zoo(object[,i], time(SSMtime[i])))
       }
       stop("wrong indices or times")
})

setMethod(".make.project", "vector", function(object, SSMtime, i, time,
                            minus1 = TRUE) {
       tm <- if(minus1) time(SSMtime)[-1] else time(SSMtime)
       if(is.null(i)&is.null(time)){
          return(zoo(object, tm))
           }
       if(is.null(i)&!is.null(time))
          i <- match(time, time(SSMtime))
       if(!is.null(i)){
          if( minus1) i <- i[i!=1]
          return(zoo(object, time(SSMtime[i])))
       }
       stop("wrong indices or times")
})

#---------------------------------------------------------------------------
# Accessors
#---------------------------------------------------------------------------

setMethod("time", signature = "recFilter", function(x) x@time)
setMethod("name", signature = "recFilter", function(object) object@name)
setMethod("name.rob", signature = "robrecFilter", function(object) object@name.rob)
setMethod("nsim", signature = "robrecFilter", function(object) object@nsim)
setMethod("RNGstate", signature = "robrecFilter", function(object) object@RNGstate)

setMethod("SSM", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@SSM,
                     SSMtime = object@SSM@time,
                     i = i, time = time)
})

setMethod("Y", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Y,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("X.filtered", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@X.filtered,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("X.predicted", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@X.predicted,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("X.rob.filtered", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@X.rob.filtered,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("X.rob.predicted", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@X.rob.predicted,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("IndIO", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@IndIO,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("IndAO", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@IndAO,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("Cov.filtered", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.filtered,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("Cov.predicted", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.predicted,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("Cov.rob.filtered", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.rob.filtered,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("Cov.rob.predicted", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.rob.predicted,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("Kalman.Gain", signature = "recFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Kalman.Gain,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("Kalman.rob.Gain", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Kalman.rob.Gain,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("rob.prediction.ctrl", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@rob.prediction.ctrl,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("rob.correction.ctrl", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@rob.correction.ctrl,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = TRUE)
})

setMethod("Cov.rob.filtered.sim", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.rob.filtered.sim,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("Cov.rob.predicted.sim", signature = "robrecFilter",
           function(object, i = NULL, time = NULL) {
.make.project(object = object@Cov.rob.predicted.sim,
                     SSMtime = object@SSM@time,
                     i = i, time = time, minus1 = FALSE)
})

setMethod("name.rob", "robrecFilter", function(object) {
           return(object@name.rob)
           })

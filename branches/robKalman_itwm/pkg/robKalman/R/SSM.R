###############################################################################
#
# functions for treatment / checking / generation of SSMs 
#
###############################################################################

# generating function

TI.SSM <- function(name = NULL, F, Z, Q, V, a = numeric(nrow(F)), S0 = Q, Tn = 1){
       F <- as.matrix(F)
       Z <- as.matrix(Z)
       Q <- as.matrix(Q)
       V <- as.matrix(V)
       S0 <- as.matrix(S0)
       if(missing(Tn)) 
          Tn <- timeSeries(1,"1")
       if(missing(name)) 
          name <- gettext("a time-invariant state space")
       
       if(length(Tn)==1 && is.integer(Tn)) Tn <- 1:Tn
       if(!is(Tn,"timeSeries"))
          Tn <- as(Tn,"timeSeries")
       new("TimeInvariantSSM",  name = name, p = nrow(F), q=nrow(Z), 
                  F = F, Z = Z, Q = Q, V = V, a = a, S = S0, time = Tn)
       }

## internal checking functions

.check.function.dim <- function(fct, nrow, ncol, possem = FALSE, testargs = seq(0:100))
   all(sapply(testargs,function(x) {
       m <- fct(x)
       tpossem <- if ( possem & !is(m,"PosSemDefSymmMatrix") )
                      !is(try(PosSemDefSymmMatrix(m),silent = TRUE),
                                   "try-error") else TRUE
       tpossem & nrow(m)==nrow & ncol(m)==ncol
       }))

.check.function.dim.vec <- function(fct, n, testargs = seq(0:100))
   all(sapply(testargs,function(x) {
       m <- fct(x)
       length(m) == n
       }))

## internal helper functions for converting SSM to standard form

.fct2Array <- function(fct, m, n, withTest = TRUE, Tn, time)
    {fcts <- paste(deparse(substitute(fct)),sep="",collapse="")
     array0 <- array(0,dim=c(m,n,Tn-1))
     tst1 <- (.check.function.dim(fct = fct, nrow = m, ncol = n, possem = FALSE,
              testargs = time[2:Tn]))
     tst2 <- (.check.function.dim(fct = fct, nrow = m, ncol = n, possem = TRUE,
              testargs = time[2:Tn]))
     if(!tst1)
          stop(gettextf("Function %s has wrong return values", fcts))
     if(!tst2 & withTest)
          stop(gettextf("Function %s gives non p.s.d. matrices", fcts))
     for (i in 2:Tn){
          array0[,,i-1] <- fct(time[i])
         }
     return(array0)
     }

.mat2Array <- function(mat, m, n, withTest = TRUE, Tn)
    {mats <- paste(deparse(substitute(mat)),sep="",collapse="")
     array0 <- array(0,dim=c(m,n,Tn-1))
     tst1 <- nrow(mat)==m & ncol(mat)==n
     tst2 <- !is(try(PosSemDefSymmMatrix(m),silent = TRUE),
                                   "try-error")
     if(!tst1)
          stop(gettextf("Matrix %s has wrong dimensions", mats))
     if(!tst2 & withTest)
          stop(gettextf("Matrix %s is non p.s.d.", mats))
     array0[,,] <- mat
     return(array0)
     }

.fct2Mat <- function(fct, m, Tn, time)
    {fcts <- paste(deparse(substitute(fct)),sep="",collapse="")
     mat0 <- matrix(0,nrow = m, ncol = Tn-1)
     tst <- (.check.function.dim.vec(fct = fct, n = m, testargs = time[2:Tn]))
     if(!tst)
          stop(gettextf("Function %s has wrong return values", fcts))
     for (i in 2:Tn){
          mat0[,i-1] <- fct(time[i])
         }
     return(mat0)
     }

.num2Mat <- function(num, m, Tn)
    {nums <- paste(deparse(substitute(num)),sep="",collapse="")
     mat0 <- matrix(0,nrow = m, ncol = Tn-1)
     tst <- length(num)==m 
     if(!tst)
          stop(gettextf("Vector %s has wrong dimensions", nums))
     mat0[,] <- num
     return(mat0)
     }

## internal helpers for std representations

.stdRepres <- function(hyperpar, m, n, withTest = FALSE, Tn, time){
   if(is.function(hyperpar))
      return(.fct2Array(hyperpar, m = m, n = n, withTest = withTest, Tn = Tn,
                        time = time))
   if(is.matrix(hyperpar))
     return(.mat2Array(hyperpar, m = m, n = n, withTest = withTest, Tn = Tn))
   psd <- TRUE
   if (withTest)
       psd <- all(as.logical(apply(hyperpar,3, 
                   function(m) !is(try(PosSemDefSymmMatrix(m),silent = TRUE),
                                   "try-error")
                                )))   
   ars <- paste(deparse(substitute(hyperpar)),sep="",collapse="")

   if(!psd) 
          stop(gettextf("Matrix %s is non p.s.d.", ars))

   if(isTRUE(all.equal(dim(hyperpar),c(m,n,Tn-1), check.attributes = FALSE)))
      return(hyperpar)   
   else 
       stop(gettextf("Array %s has wrong dimensions", ars))
    }


.stdRepres.vec <- function(shyperpar, m, Tn, time){
   if(is.function(shyperpar))
      return(.fct2Mat(shyperpar, m = m, Tn = Tn, time = time))
   if(is.numeric(shyperpar)&&!(is.matrix(shyperpar)))
      return(.num2Mat(shyperpar, m = m, Tn = Tn))

   mats <- paste(deparse(substitute(hyperpar)),sep="",collapse="")
   if(isTRUE(all.equal(dim(shyperpar),c(m,Tn-1), check.attributes = FALSE)))
      return(shyperpar)   
   else 
       stop(gettextf("Matrix %s has wrong dimensions", mats))

    }

## accessors

setMethod("name", signature = "SSM", function(object) object@name)
setMethod("time", signature = "SSM", function(x) x@time)
setMethod("getp", signature = "SSM", function(object) object@p)
setMethod("getq", signature = "SSM", function(object) object@q)
setMethod("getF", signature = "SSM", function(object) object@F)
setMethod("getZ", signature = "SSM", function(object) object@Z)
setMethod("getQ", signature = "SSM", function(object) object@Q)
setMethod("getV", signature = "SSM", function(object) object@V)
setMethod("geta", signature = "SSM", function(object) object@a)
setMethod("getS", signature = "SSM", function(object) object@S)

## replacement functions

setReplaceMethod("name", signature = "SSM", function(object, value)
    {object@name <- value;object})
setReplaceMethod("time", signature = "SSM", function(x, value)
    {x@time <- value;x})
setReplaceMethod("setp", signature = "SSM", function(object, value)
    {object@p <- value;object})
setReplaceMethod("setq", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("setF", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("setZ", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("setQ", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("setV", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("seta", signature = "SSM", function(object, value)
    {object@q <- value;object})
setReplaceMethod("setS", signature = "SSM", function(object, value)
    {object@q <- value;object})


## convert to standard representation

makeArrayRepresentation<- function(SSM){
 Tn <- length(SSM@time)
 SSM@a <- as.numeric(.stdRepres.vec(SSM@a, SSM@p, Tn = 2, 
                      time = time(object@time)[1]))
 SSM@S <- matrix(.stdRepres(SSM@S, SSM@p, SSM@p, withTest = TRUE, Tn = 2,
                    time = time(object@time)[1]), nrow = SSM@p, ncol = SSM@p)
 SSM@F <- .stdRepres(SSM@F, SSM@p, SSM@p, withTest = FALSE, Tn = Tn,
                       time = time(object@time))
 SSM@Z <- .stdRepres(SSM@Z, SSM@q, SSM@p, withTest = FALSE, Tn = Tn,
                       time = time(object@time))
 SSM@Q <- .stdRepres(SSM@Q, SSM@p, SSM@p, withTest = TRUE, Tn = Tn,
                       time = time(object@time))
 SSM@V <- .stdRepres(SSM@V, SSM@q, SSM@q, withTest = TRUE, Tn = Tn,
                       time = time(object@time))
return(as(SSM,"SSM"))
}

### does not work this way:::
## need to have other access methods
#
#setMethod("[", "Hyperparamtype", function(x, i, j, ..., drop = FALSE){
#          print("W")
#          if (is.null(x))
#              stop("Hyperparamtype to be indexed is NULL")
#          if (is.function(x)) return(x(i, ...))
#          if (is.array(x)) return(x[,,i, drop = TRUE])
#          if (is.matrix(x)) return(x)
 #         })
#
#setReplaceMethod("[", "Hyperparamtype", function(x, i, j, value){
#          if (is.null(x))
#              stop("Hyperparamtype to be indexed is NULL")
#          if (is.function(x))
#              {f0 <- function(t, ...){
#                     ou <- outer(t,i,function(u,v)
#                                     abs(u-v)<.Machine$double.eps^.5)
#                    (1-(rowSums(ou)>0))*x(t, ...) + ou%*%value
#                    }
#               return(f0)
#              }
#          if (is.array(x))
#             x[,,i] <- value
#         if (is.matrix(x)) x <- value
#          return(x)
#          })



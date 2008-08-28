## code borrowed from package distrMod

setMethod("solve", signature(a = "ANY", b = "ANY"), function(a,b, generalized = TRUE,
          tol = .Machine$double.eps, ...) {
                 if(!generalized) return(base::solve(a,b, tol = tol, ...))
                 else if(is(try(return(base::solve(a,b, tol = tol, ...)), 
                                silent = TRUE), "try-error")){
             if (!missing(b))
                if(!(length(b)==nrow(a))) stop("non-conformable arguments")
             a.m <- ginv(a)
             if (missing(b)) return(a.m) 
             else return(a.m %*% b)
             }})

setMethod("solve", signature(a = "PosSemDefSymmMatrix", b = "ANY"), 
           function(a,b, generalized = TRUE, tol = .Machine$double.eps, ...){
          if(!generalized) return(base::solve(a,b, tol = tol, ...))
          else{
            er <- eigen(a)
            d1 <- er$values
            d <- 1/d1[d1 > tol]
            ev <- er$vectors[,d1 > tol]
            A <- if (length(d)) ev %*% (t(ev)*d) else 0*a
            if(missing(b)) return(A)
            else return(A%*%b)}   
})

setMethod("solve", signature(a = "PosDefSymmMatrix", b = "ANY"), function(a,b, ...){
base::solve(a,b, ...)})


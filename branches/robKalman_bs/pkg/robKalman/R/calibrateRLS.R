rLScalibrateB <- function(Z, S, V, repl = 100000, b = NULL, eff = NULL, r = NULL,
                          rlow = 0, rup = NULL, upto = 20, IO = FALSE, seed)#
# calibrates clipping height b to given Z, V, and S_{t|t-1} 
# --- 
#      either to given efficiency in the ideal model
#      or to given (SO)-radius about the ideal model
# expectations are calculated by a LLN approximation 
# with /repl/ replicates; 100000 as default works well for me,
# but might need a change for slower machines
# upto is an upper bound where to search for zeroes
{

 if ( is.null(b) && is.null(eff) && is.null(r) && is.null(rup))
    stop("You must either specify argument 'b' or 'r' or 'eff' or 'rup'")

 qd <- ifelse(length(Z)==1, 1, (dim(Z))[1])
 pd <- ifelse(length(S)==1, 1, (dim(Z))[2])
 
 if(!missing(seed)) set.seed(seed)
 
 dx <- t(mvrnorm(repl, numeric(pd), S))
 eps <- t(mvrnorm(repl, numeric(qd), V))
 dy <- Z %*% dx + eps
 K  <- .getKG(S, Z, .getDelta(S, Z, V))

 trS <- sum(diag(.getcorrCov(S, K, Z)))

 dx0 <- K %*% dy

 if(IO){
    dx0 <- dy - Z %*% dx0
 }
 no  <- sqrt(colSums(dx0^2))

 eff.b <- function(b){
          w <- ifelse(no < b, 1, b / no)
          dxw  <- as.vector(w) * t(dx0)
          if(IO) dxw <-  (t(dy) - dxw) %*% t(ginv(Z))
          trSb <- sum( (t(dx) - dxw)^2 )/repl
          trS / trSb
 }
 r.b <- function(b){
        ex <- mean( pmax(no/b - 1, 0) )
        ex/(ex + 1)
 }

 todo.eff <- TRUE
 todo.r <- TRUE

 if(is.null(b)){
   todo.r.search <- FALSE
   if( (is.null(r)&& is.null(eff))) {
        todo.r.search <- TRUE
        r <- rup
   }
   if (is.null(r)&&!is.null(eff)){  ## calibrated to given efficiency
         f  <- function(b, dX = dx, dX0 = dx0, no0 = no, r0 = r,
                         eff0 = eff, trS0 = trS, repl0 = repl, dY = dy){
              w <- ifelse(no0 < b, 1, b/no0)
              dxw <- as.vector(w) * t(dX0)
              if(IO) dxw <-  (t(dY) - dxw) %*% t(ginv(Z))
              trSb <- sum( (t(dX) - dxw)^2 )/repl0
              trS0 / trSb - eff0
          }
          r1 <- NULL
          eff1 <- eff
          todo.eff <- FALSE
   }else{  ## calibrated to given radius
          f  <- function(b, dX = dx, dX0 = dx0, no0 = no, r0 = r,
                         eff0 = eff,  trS0 = trS, repl0 = repl, dY = dy){
                (1 - r0)/r0 * sum(pmax(no0 / b - 1, 0))/repl0 - 1
          }
          r1 <- r
          eff1 <- NULL
          todo.r <- FALSE
   }

   b <- uniroot(f, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7,
                    dX = dx, dX0 = dx0, no0 = no,
                    eff0 = eff1, trS0 = trS, repl0 = repl, r0 = r1,
                    dY = dy)$root

   if(! todo.r.search){
          if (is.null(r)) ### corresponding radius is calculated
              r  <- r.b(b)
          else           ### corresponding effciency is calculated
              eff  <- eff.b(b)
     }else{
#          if(IO) stop("not yet implemented")
          todo.r <- TRUE
          b.u <- b

          A.r <- function(b1)  trS+ mean((pmax(no-b1,0))^2)
          B.r <- function(b1)  mean(no^2)- mean((pmax(no-b1,0)^2)) + b1^2

          B.u <- B.r(b.u)


          if(rlow <1e-6){
             b.l <- 1e8
             A.l <- trS
          }else{
             b.l <- uniroot(f, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7,
                            dX = dx, dX0 = dx0, no0 = no,
                            eff0 = eff1, trS0 = trS, repl0 = repl, r0 = rlow,
                            dY = dy)$root
             A.l <- A.r(b.l)
          }
          AB <- function(b2)  A.r(b2)/A.l - B.r(b2)/B.u
          b <- uniroot(AB,interval = c(b.l,b.u), tol = 10^-7)$root
     }
 }
 if(todo.r)    r  <- r.b(b)
 if(todo.eff)  eff  <- eff.b(b)

 list( b = b, eff = eff, r = r )
}

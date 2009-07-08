rLScalibrateB <- function(Z, S, V, repl = 100000, eff, r, rlow, rup, upto=20, IO = FALSE, seed)#
# calibrates clipping height b to given Z, V, and S_{t|t-1} 
# --- 
#      either to given efficiency in the ideal model
#      or to given (SO)-radius about the ideal model
# expectations are calculated by a LLN approximation 
# with /repl/ replicates; 100000 as default works well for me,
# but might need a change for slower machines
# upto is an upper bound where to search for zeroes
{

 if ( (missing(eff) || is.null(eff)) &&
      (missing(r) || is.null(r)) &&
      (missing(rup)||is.null(rup)))
    stop("You must either specify argument 'r' , 'eff' or 'rup'")

 if(missing(rlow)||is.null(rlow))
    rlow <- 0

 qd <- ifelse(length(Z)==1, 1, (dim(Z))[1])
 pd <- ifelse(length(S)==1, 1, (dim(Z))[2])
 
 if(!missing(seed)&& !is.null(seed)) set.seed(seed)
 
 dx <- t(mvrnorm(repl, numeric(pd), S))
 dy <- Z %*% dx + t(mvrnorm(repl, numeric(qd), V))
 K  <- .getKG(S, Z, .getDelta(S, Z, V))

 trS <- sum(diag(.getcorrCov(S, K, Z)))

 dx0 <- K %*% dy

 if(IO){
    dx0 <- dy - Z %*% dx0
 }
 no  <- sqrt(colSums(dx0^2))

 todorsearch <- FALSE
 if ((missing(r)||is.null(r))&&!missing(b)&&!is.null(b))  ## calibrated to given efficiency
    {f  <- function(b, dX = dx, dX0 = dx0, no0 = no, r0 = r,
                    eff0 = eff, trS0 = trS, repl0 = repl, dY = dy)
        {w <- ifelse(no0 < b, 1, b/no0)
         dxw <- as.vector(w) * t(dX0)
         if(IO) dxw <-  (t(dY) - dxw) %*% t(ginv(Z))
         trSb <- sum( (t(dX) - dxw)^2 )/repl0
         trS0 / trSb - eff0
        }    
    r1 <- NULL
    eff1 <- eff
    }
 else
    ## calibrated to given radius
   {if(missing(r)||is.null(r)) {
       todorsearch <- TRUE
       r <- rup
       }
    f  <- function(b, dX = dx, dX0 = dx0, no0 = no, r0 = r,
                   eff0 = eff,  trS0 = trS, repl0 = repl, dY = dy)
          {(1 - r0)/r0 * sum(pmax(no0 / b - 1, 0))/repl0 - 1}
    r1 <- r
    eff1 <- NULL
   }

 erg <- uniroot(f, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7,
                dX = dx, dX0 = dx0, no0 = no, 
                eff0 = eff1, trS0 = trS, repl0 = repl, r0 = r1,
                dY = dy)
 b   <- erg$root

 if(!todorsearch){
   if (missing(r)) ### corresponding radius is calculated
      { ex <- mean( pmax(no/b - 1, 0) )
        r  <- ex/(ex + 1) }
   else           ### corresponding effciency is calculated
     { w <- ifelse(no < b, 1, b / no)
         dxw  <- as.vector(w) * t(dx0)
         if(IO) dxw <-  (t(dy) - dxw) %*% t(ginv(Z))
         trSb <- sum( (t(dx) - dxw)^2 )/repl
         eff  <- trS / trSb
     }

 }else{
   b.r <- function(r1){
         sapply(r1, function(r11){if(r11<10^-6) return(Inf)
         uniroot(f, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7,
                dX = dx, dX0 = dx0, no0 = no,
                eff0 = eff1, trS0 = trS, repl0 = repl, r0 = r11,
                dY = dy)$root})
      }
   b.l <- b.r(rlow)
   b.u <- b.r(rup)
   A.r <- function(r1){
          sapply(r1, function(r11){
                        if(r11<10^-6) return(trS)
                        b <- b.r(r11)
                        trS+mean(pmax(no-b,0)^2)})

          }
   B.r <- function(r1){
          sapply(r1, function(r11){
                        mno2 <- mean(no^2)
                        if(r11<10^-6) return(mno2)
                        b <- b.r(r11)
                        mno2-mean(pmax(no-b,0)^2)+b^2})

          }
   AB.r <- function(r1){
          sapply(r1, function(r11){
                        mno2 <- mean(no^2)
                        if(r11<10^-6) return(c(trS,mno2))
                        b <- b.r(r11)
                        mmd2 <- mean(pmax(no-b,0)^2)
                        c(trS+mmd2, mno2-mmd2+b^2)})

          }
    A.l <- A.r(rlow)
    B.u <- B.r(rup)
    f2 <- function(r12){
          AB <- AB.r(r12)
          AB[1]/A.l-AB[2]/B.u
    }
    r <- uniroot(f2, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7)$root
    b <- b.r(r)
    w <- ifelse(no < b, 1, b / no)
    dxw  <- as.vector(w) * t(dx0)
    if(IO) dxw <-  (t(dy) - dxw) %*% t(ginv(Z))
    trSb <- sum( (t(dx) - dxw)^2 )/repl
    eff  <- trS / trSb
 }
 
 return(list( b = b, eff = eff, r = r ))
}

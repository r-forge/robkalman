rLScalibrateB <- function(Z, S, V, repl = 100000, eff, r, upto=20)#  
# calibrates clipping height b to given Z, V, and S_{t|t-1} 
# --- 
#      either to given efficiency in the ideal model
#      or to given (SO)-radius about the ideal model
# expectations are calculated by a LLN approximation 
# with /repl/ replicates; 100000 as default works well for me,
# but might need a change for slower machines
# upto is an upper bound where to search for zeroes
{

 if ( missing(eff) && missing(r) ) 
    stop("You must either specify argument 'r' or 'eff'")

 qd <- ifelse(length(Z)==1, 1, (dim(Z))[1])
 pd <- ifelse(length(S)==1, 1, (dim(Z))[2])
 
 ep <- 1+numeric(pd)

 dx <- t(mvrnorm(repl, numeric(pd), S))
 dy <- Z %*% dx + t(mvrnorm(repl, numeric(qd), V))
 K  <- .getKG(S, Z, .getDelta(S, Z, V))

 trS <- sum(diag(.getcorrCov(S, K, Z)))

 dx0 <- K %*% dy
 no  <- sqrt(t(ep) %*% dx0^2)

   }
 if (missing(r))  ## calibrated to given efficiency
    {f  <- function(b, dX = dx, dX0 = dx0, no0 = no, r0=r
                    eff0 = eff, trS0 = trS, repl0 = repl)
        {w <- ifelse(no0 < b, 1, b/no0)
         dxw <- as.vector(w) * t(dX0)
         trSb <- sum( (t(dX) - dxw)^2 )/repl0
         trS0 / trSb - eff0
        }
    r1 <- NULL
    eff1 <- eff
    }
 else  ## calibrated to given radius
   {f  <- function(b, no0 = no, r0=r, repl0 = repl)
          {(1 - r0)/r0 * sum(pmax(no0 / b - 1, 0))/repl0 - 1}
    r1 <- r
    eff1 <- NULL
   }

 erg <- uniroot(f, interval = c(10^-6, upto*sqrt(trS)), tol = 10^-7)
 b   <- erg$root

 if (missing(r)) ### corresponding radius is calculated
    { ex <- mean( pmax(no/b - 1, 0) )
      r  <- ex/(ex + 1) }
 else           ### corresponding effciency is calculated  
   { w <- ifelse(no < b, 1, b / no)
         dxw  <- as.vector(w) * t(dx0)
         trSb <- sum( (t(dx) - dxw)^2 )/repl
         eff  <- trS / trSb
   }

 list( b = b, eff = eff, r = r )
}

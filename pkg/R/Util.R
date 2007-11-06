### the Euclidean norm

Euclidnorm <- function(x) {sqrt(sum(x^2))}


### huberizing a vector to length b

Huberize <- function(x, b, norm=Euclidnorm, ...)
   x*ifelse(norm(x) < b, 1, b/norm(x, ...))
    

limitS <- function(S, F, Z, Q, V, tol = 10^-4, itmax = 1000)#
## determines lim_{t->infty} S_{t|t-1}
     {SO0 <- S + 1
      S0  <- S
      i   <- 0
      while( (sum( (SO0 - S0)^2 ) > tol^2) && (i < itmax) )
        {i   <- i + 1
         S1  <- .getpredCov(S0, F, Q)
         SO0 <- S0
         K   <- .getKG(S1, Z, V)
         S0  <- .getcorrCov(S1, K, Z)
        }
     S1
     }

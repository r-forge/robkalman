#######################################################
## 
##  extended Kalman filter utility routines
##  [original code by Peter Ruckdeschel]
##  author: Bernhard Spangl
##  version: 0.2 (changed: 2011-12-16, created: 2010-09-07)
##
#######################################################

##  the Euclidean norm

Euclideannorm <- function(x) {if(is.null(dim(x)))
                              sqrt(abs(sum(x^2))) else sqrt(colSums(x^2))}

##  huberizing a vector to length b

Huberize <- function(x, b, norm = Euclideannorm, ...)
{  nx <- norm(x, ...)
   Ind0 <- (nx < b  )
   Ind1 <- (nx < b/2)
   x*(Ind0 + (1-Ind0)* b / (nx + Ind1 + (nx+Ind1==0)) )
}    



getSmoothX   <-   function(  t, S1, Xf, R, Delta, DeltaY  )

{
#arguments
# \item{t}{starting point of the time series }
# \item{S1}{p*p*T - the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the classical filter}
# \item{Xf}{p*n*T+1 - the series \eqn{x_{t|t}} filtered by the classical filter}
# \item{R}{q*p*s - matrix used in the smoothing equations,  \code{s} the final point of the required time series;}
# \item{Delta:}{q*q*T the series \eqn{\Delta_{t}}{Delta_t}  of covariances of
#         \eqn{\Delta y_{t}}{Delta y_t} produced by the classical filter}
# \item{DeltaY}{q*n*T the matrix of the \eqn{\Delta_{t}} entries}	

#dimensions

  p    <- dim(xf)[1]
  q    <- dim(y)[1]
  n    <- dim(xf)[2]
  T    <- dim(xf)[3]-1

# generation of arrays
  XS   <- array(0,dim=c(p,runs))

 XS <-Xf[,,T+1]
 for(  s   in   t+1:T+1)  
	{
	  XS <- XS + S[,,t] * t(R[,,s])%*%ginf(Delta[,,s])%*% DeltaY[,,s]  
	}

return(XS)

}


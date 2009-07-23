getSmoothFixIntervalX   <-   function(  t,T, S1, XSI, R, Delta, DeltaY  )

{
#arguments
# \item{t}{starting point of the time series }
# \item{S1}{p*p*T - the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the classical filter}
# \item{XSI}{p*n - the vector \eqn{x_{t|T-1}} filtered given recursively by the algorithm}
# \item{R}{q*p*s - matrix used in the smoothing equations,  \code{s} the final point of the required time series;}
# \item{Delta:}{q*q*T the series \eqn{\Delta_{t}}{Delta_t}  of covariances of
#         \eqn{\Delta y_{t}}{Delta y_t} produced by the classical filter}
# \item{DeltaY}{q*n*T the matrix of the \eqn{\Delta_{t}} entries}	

#dimensions

  p    <- dim(xf)[1]
  q    <- dim(y)[1]
  n    <- dim(xf)[2]

# generation of arrays
  XSP   <- array(0,dim=c(p,runs))
  XSP   <- XSI

 for(  s   in   t+1:T+1)  
	{
	  XSP <- XSP + S[,,t] * t(R[,,T])%*%ginf(Delta[,,T])%*% DeltaY[,,T]  
	}

return(XSP)

}


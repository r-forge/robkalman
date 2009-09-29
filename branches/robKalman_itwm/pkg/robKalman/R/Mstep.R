MStepSSMclass <- function(Y,M,x,P,Pdep)
{

######################################################################################
##FIRST APPROACH:
##########################################################################
##This algorithm is based on the paper by Shumway&Stoffer,1981:          #
##"An approach to time series smoothing and forecasting using the        #
##EM algorithm".                                                         #
##If one knows the values for the parameters miu, sigma, fi, Q and R,    #
##one can calculate the Kalman smoothing estimators as described in      #
##the *.Rd-documentation file. This is actually the goal of this routine.#
##########################################################################

##SOME MORE EXPLANATION REGARDING THE IMPLEMENTATION:
###################################################################################
##What we would like to do is to compute equations (12),(13), (14).		    #
##For this purpose, we assume(just in this beginner step), that                   #
##we can calculate A,B and C as in equations (9), (10), (11) respectively.        #
##Hence, let us assume, that we already performed the estimation step, and that   #
##this step is realized within the function "Estimation", whose output parameters:#
##x,P, and Pdep are calculated via equations (A3)-(A12) from Appendix A. So:      #
###################################################################################
##
##number of observations:
## to be drawn from input
##
runs <- dim(Y)[2]
n <- dim(x)[2]-1
p <- dim(P)[1]
q <- if(is.null(dim(y))) 1 else dim(y)[1]

##Like mentioned above, we use the values resulted by the "E"-step, i.e.
##Estimation-step:

## comment P.R.: only works for one run at a time --- hence arguments are now:

## M  observation matrix (Z in our notation); q x q x n
## y  observations; q x n
## x  smoothed value x_t|n computed with getSmoother(); p x (n+1)
## P  smoother cov's Sigma_{t|n} comp. with getSmoother(); p x p x (n+1)
## Pdep smoother cov Cov(X_t,X_t-1|y_1:n) comp. with getSmoother(); p x p x n

## output: list of estimated F,Q,V

##for(t in 1:n )
##{
##x[,,t]<-Estimation$x[,,t];
##P[,,t]<-Estimation$P[,,t];
##Pdep[,,t]<-Estimation$Pdep[,,t];
##}
##This part just gives an outline about the way we will procede, when we will also have 
##estimation step(Step 1 in the paper) done. For the calculations presented now, 
##we asssume, we have some input x, P, Pdep.The file MStep.Rd explains the general setting of
##the algorithm and its goal.
########################################################################################

##initialize matrix values:

	R0<-array(0,dim=c(q,q,n))

  null <- (1:n)
  plus <- null +1

  x0 <- x[,null,drop=FALSE]
  x1 <- x[,plus,drop=FALSE]
  P0 <- P[,,null,drop=FALSE]
  P1 <- P[,,plus,drop=FALSE]

##Now: calculate the values of A, B and C as in eq. (9), (10), (11) respectively.
  A <- apply(P0,c(1,2),sum) + x0 %*% t(x0)

	C <- apply(P1,c(1,2),sum) + x1 %*% t(x1)
      
  B <- apply(Pdep,c(1,2),sum) + x1 %*% t(x0)

##having computed these values, one can compute now:
	F <- B %*% ginv(A)
	Q <- (C-F%*%t(B))/n

  yt <- 0 *y
  for (i in 1:n){
     yt[,i] <- M[,,i]%*%x[,i+1]
     R0[,,i] <- M[,,i]%*%P[,,i]%*%t(M[,,i])
  }
  dy <- y - yt
  R <- (apply(R0,c(1,2),sum) + dy %*% t(dy))/n

  list(F=F,Q=MakePositive(Q),V=MakePositive(R))

}



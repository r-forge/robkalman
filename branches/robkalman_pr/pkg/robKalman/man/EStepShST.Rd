\name{MStep}
\alias{MStep}
\title{Implements the Maximization Step of the EM Algorithm.}
\description{ This routine represents an implementation of the Maximization 
 part of the EM-Algorithm. It is based on the recursions provided in the references
 mentioned below.
 
}
\usage{
MStep(M,y,numobs,x,P,Pdep)
}
\arguments{
 \item{M}{Observation matrix(Z in our notation): dimension q*q*n}
 \item{y}{Observations: dimension q*n}
 \item{x}{Smoothed values x_{t|n}, i.e for the estimated state at time t, 
          given observation until time n: dimension p*(n+1) ; 
          output of getSmoother()}
 \item{P}{Smoothed values S_{t|n}, i.e for the estimated state at time t, 
          given observation until time n: dimension p*p*(n+1); 
          output of getSmoother()}
 \item{Pdep}{Smoother covariances Cov(X_t,X_t-1|y_1:n): dimension p*p*n;
             output of getSmoother()}

}
\value{
 List of the components:
 \item{F}{p*p observation matrix}
 \item{Q}{p*p Covariance matrix corresponding to the noise of the states (x's) }
 \item{V}{q*q Covariance matrix corresponding to the noise of the observations (y's)}
 
}

\references{
 R. H. Shumway and D. S. Stoffer (1982): "An approach to time series smoothing 
 and forecasting using the EM algorithm". Journal of Time Series Analysis, vol. 3.
 
 \url{http://www.stat.pitt.edu/stoffer/em.pdf}
}

\note{
    See note on EM_Algo and proceed as indicated there. As in EM_Algo, loading 
 of all libraries and functions is required. This holds for the first example, 
 where the input of MStep depends on the output of the recFilter and of the results of
 the smoothing recursions. For the second example, this is not necessary. 

}



\author{Peter Ruckdeschel(Fraunhofer ITWM), Irina Ursachi (Fraunhofer ITWM)}
\seealso{\code{\link{EM_Algo}}, \code{\link{initEMpseudo}},
 \code{\link{recFilter}}, \code{\link{classKalman}}, \code{\link{getSmoother}}}
\examples
{
 #runs <- 1
 #M <- 20
 T <- 100 ##number of observations
 #eps <- 0.01
 #itmax <- 30
 #sn <- 0.5

 pd <- 2
 qd <- 3
 F <- matrix(c(0.3,0.2,0.2,0.8),2,2)
 Q <- MakePositive(matrix(c(3,1,1,2),2,2)*.1)
 V <- MakePositive(matrix(c(3,1,1,1,2,0.3,1,.3,4),3,3)*.1)
 Z <- matrix(c(3,-1,2,-1,1,1),3,2)
 a <- c(0,0)
 S <- Q

 #Simulation of states and observations according to the state space model:
 x <- simulateState(a=a, S=S, F=F, Qi=Q, Qc=Q, runs=runs, mc=c(0,0),tt=T)
 y <- simulateObs(X=x,Z=Z,Vi=V,mc=numeric(qd),Vc=V,r=0,runs=runs)
 
 ##Some noise-term introduction
 resinitEM <- initEM.pseudo(F,Q,V, scalenoise=sn)
 F0 <- resinitEM$F
 Q0 <- resinitEM$Q
 V0 <- resinitEM$V
 Z1 <- array(Z, dim=c(qd,pd,T))
     resEstep <- KalmanFilter(y, a, S, F0, Q0, Z1, V0, dropRuns = FALSE)
                              x0 <- resEstep$Xp
                              P1 <- resEstep$S1
                              P0 <- resEstep$S0
     resSmoother <- getSmoother.Shumw.Stoff(y = y,
                             xf = resEstep$Xf,
                             S0 = resEstep$S0,
                             S1 = resEstep$S1,
                             KG = resEstep$KG[,,T], F=F0, Q=Q0, V=V0, Z=Z1)

 ##In this following initialization, one can notice, how the M-Step of the EM algorithm
 ##depends on the E-Step and smoothing recursions respectively:
     resMStep <- MStep(M = Z1, y = matrix(y[,1,],qd,T),
                        x = matrix(resSmoother$xS[,1,],pd,T+1),
                        P = resSmoother$SS,
                        Pdep = resSmoother$SS1)
}

\examples{
 ##This is NOT(!!) corresponding to the state space model
 ##and is meant to give some insight in the computations and 
 ##matrix dimensions. 
 M  <- array(rnorm(pd*qd*T, sd=1), dim=c(pd,qd,T));
 y  <-  array(rnorm(qd*T,sd=0.8),dim=c(qd,T));
 x <- array(rnorm(pd*(T+1),sd=0.9),dim=c(pd,(T+1)));
 P <- array(rnorm(pd*pd*(T+1),sd=0.3),dim=c(pd,pd,(T+1)));
 Pdep<- array(rnorm(pd*pd*T,sd=0.7),dim=c(pd,pd,T));
 MStep(M,y,x,P,Pdep);

}
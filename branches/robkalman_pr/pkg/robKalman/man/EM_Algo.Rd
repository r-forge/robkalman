\name{EM_Algo}
\alias{EmAlgo.intern}
\title{The EM algorithm for finding the hyperparameters}


\description{Implements the Expectation Maximization algorithm
for finding the hyperparameters in a time-invariant state-space model
covering a.o. the one presented by Shumway and Stoffer in
1981}
\usage{
EmAlgo.intern(y,Z,Q_init,F_init,V_init,EStepEM=recursiveFilter,
		            MStepEM=MStep,eabs,erel,maxit,noise=NULL)

}
\arguments{
 \item{y}{q x numobs-dimensional matrix of observations. Each column
          corresponds to one period(observation time=1,...,n) }
 \item{Z}{q x p-dimensional known design matrix}
 \item{Q_init}{Initial values for the p*p-dimensional covariance matrix
               corresponding to the noise of the states (x's)}
 \item{F_init}{Initial values for the p*p-dimensional observation matrix}
 \item{V_init}{Initial values for the q*q-dimensional covariance matrix 
               corresponding to the noise of the observations (y's) }
 \item{EStepEM}{Function corresponding to the E-Step \code{\link{recFilter}}}
 \item{MStepEM}{Function refering the M-Step \code{\link{MStep}}}
 \item{eabs}{Absolute tolerance for convergence criteria}
 \item{erel}{Relative tolerance for convergence criteria}
 \item{maxit}{Maximal number of iterations}
 \item{noise}{Noise term, by default NULL}

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

%\note{
%    We strongly recommend installation of the "RobKalman" library. This
%can be found under r-forge. The results of the current EM algorithm are
%not robust. %Please note that the example presented below is only valid, after
%loading all libraries and functions required by the RobKalman library. These can be
%found in the description file:
%\url{https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/robKalman/DESCRIPTION}

%}

\author{Peter Ruckdeschel, Irina Ursachi (Fraunhofer ITWM)}
\seealso{\code{\link{recFilter}}, \code{\link{classKalman}}}
\examples{

require(MASS)
##Set your own path, linking to the file where you have saved the RobKalman-files:
#setwd("D:/My Documents/Arbeit/Kalman/svn/EM-Algo/R-Code")
dir()  ##View files needed to source

source("MakePositive.R")
source("initEMpseudo.R")
source("MStep.R")
source("recFilter.R")
source("getSmoother.R")
source("classKalman.R")
source("SimulateSScont.R")
source("classKalman.R")
source("mACMfilter.R")
source("EM_Algo.R")

##load libraries needed for pkg. RobKalman (can be viewed in Description file)
require("methods")
require("graphics")
require("stats")
require("startupmsg")
require("dse1")
require("dse2")
require("MASS")
require("limma")
require("robustbase")
require("numDeriv")


runs <- 1
T <- 100 ##number of observations
sn <- 0.5 

pd <- 2
qd <- 3
F <- matrix(c(0.3,0.2,0.2,0.8),2,2)
Q <- MakePositive(matrix(c(3,1,1,2),2,2)*.1)
V <- MakePositive(matrix(c(3,1,1,1,2,0.3,1,.3,4),3,3)*.1)
Z <- matrix(c(3,-1,2,-1,1,1),3,2)
a <- c(0,0)
S <- Q

##Simulate values from the state space model:

    x <- simulateState(a=a, S=S, F=F, Qi=Q, Qc=Q, runs=runs, mc=c(0,0),tt=T)
    y <- simulateObs(X=x,Z=Z,Vi=V,mc=numeric(qd),Vc=V,r=0,runs=runs)
    resinitEM <- initEM.pseudo(F,Q,V, scalenoise=sn)
    F0 <- resinitEM$F
    Q0 <- resinitEM$Q
    V0 <- resinitEM$V
    Z1 <- array(Z, dim=c(qd,pd,T))
   


##declare tolerances and maximum number of iterations for EM to run
erel=0.2
eabs=0.1
maxit=10
##Initialize values for F,Q and V:
F_init<-F0
Q_init<-Q0
V_init<-V0

EmAlgo.intern(Z,y,Q_init,F_init,V_init,EStepEM=recursiveFilter,
		            MStepEM=MStep,eabs,erel,maxit,noise=NULL)


}
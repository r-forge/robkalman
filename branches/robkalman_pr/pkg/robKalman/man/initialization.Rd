\name{initialization}
\alias{init}
\title{Provides an input possibility for the EM algorithm.}
\description{
  This routine simmulates the state space models. The output generated can be 
used as input for the EM algorithm.
}

\usage{
 init(p,q,numobs)
}

\arguments{
 \item{p}{}
 \item{q}{length of the observations}
 \item{numobs}{Number of observations, can also be refered to as 
 "number of time periods"}

}

\value{
 List of the components:
 \item{F}{p*p-dimensional observation matrix}
 \item{Q}{p*p Covariance matrix corresponding to the noise of the states(x's)}
 \item{V}{q*q Covariance matrix corresponding to the noise of the observations(y's)}
 \item{Z}{q*p-dimensional design matrix}
 \item{Y}{q*numobs dimensional observation matrix; each column corresponds
          to the observations realized in one period}

}

\note{
  This function is based on the SSM.R function in the RobKalman library. As 
mentioned before, this library can be found on r-forge.
}

\author{Peter Ruckdeschel, Irina Ursachi (Fraunhofer ITWM)}
\seealso{\code{\link{SSM}}}
\examples{
 p<-2
 q<-3
 numobs<-4
 init(p,q,numobs)

}
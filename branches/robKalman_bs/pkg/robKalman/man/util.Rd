\name{utilitiesrobKalman}
\alias{utilitiesrobKalman}
\alias{Huberize}
\alias{EuclideanNorm}
\alias{limitS}

\title{Utility functions of package robKalman}

\description{
These functions are used internally by package robKalman
}
\usage{
EuclideanNorm(x)
Huberize(x, b, norm=EuclideanNorm, ...)
limitS(S, F, Z, Q, V, tol = 10^-4, itmax = 1000)#
}
\arguments{
  \item{x}{a numeric vector}
  \item{b}{clipping bound for \code{Huberize}}
  \item{norm}{a function with a numeric vector \code{x} as first argument, 
              returning a norm of \code{x} - not necessarily, but defaulting to, Euclidean norm}
  \item{...}{additional arguments to function in argument \code{norm}}
  \item{S}{initial state covariance (see below)}
  \item{Z}{observation matrix (see below)}
  \item{V}{observation error covariance (see below)}
  \item{F}{innovation transition matrix (see below)}
  \item{Q}{innovation covariance (see below)}
  \item{tol}{a tolerance bound for determining when the sequence \eqn{S_{t|t-1}} has stabilized}
  \item{itmax}{a maximal number of iterations for \code{limitS}.}
}


\note{\code{limitS} does no dimension checking!}

\value{\code{EuclideanNorm(x)} returns the Euclidean norm of \code{x},
\code{Huberize(x,b,norm)} huberizes \code{x} to length \code{b} measured in norm \code{norm}, 
\code{limitS(S, F, Z, Q, V)} returns the limiting prediction error covariance
of the classical Kalman Filter, in the time-invariant
state space model (S,F,Z,Q,V).  }

\details{

We work in the setup of the time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
with \eqn{p} dimensional states \eqn{x_t} and \eqn{q} dimensional observations \eqn{y_t},
with \strong{initial condition}
\deqn{x_0 \sim {\cal N}_p(a,S),}{x_0 ~ N_p(a,S),}

\strong{state equation}
\deqn{x_t = F x_{t-1} +  v_t, \qquad v_t \sim {\cal N}_p(0,Q),\qquad t\ge 1,}{x_t = F x_{t-1} +  v_t,          v_t ~ N_p(0,Q),  t>=1,}

\strong{observation equation}
\deqn{y_t = Z x_{t}   +  \varepsilon_t, \qquad \varepsilon_t \sim {\cal N}_q(0,V),\qquad t\ge 1,}{y_t = Z x_t   +  e_t,      e_t ~ N_q(0,V),  t>=1,}
and where all random variable \eqn{x_0}, \eqn{v_t}, \eqn{\varepsilon_t}{e_t} are independent.

In this setup, in most cases (confer, e.g., \cite{Anderson and Moore (Appendix)}) the prediction error covariances
of the classical Kalman filter converge.
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  }


\examples{
require(robKalman)
x <- matrix(1:4)

EuclideanNorm(x)

Huberize(x,b=5) ## does clipping
Huberize(x,b=11) ## does no clipping

##Hyper parameter of a lin. time-inv. Gaussian SSM
SS0  <- matrix(0, 2, 2)
F0   <- matrix(c(.7, 0.5, 0.2, 0), 2, 2)
Q0   <- matrix(c(2, 0.5, 0.5, 1), 2, 2)
Z0   <- matrix(c(1, -0.5), 1, 2)
V0i  <- 1

SS <- limitS(S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)
}

\references{
  Anderson, B.D.O. and More, J.B. (1979): \emph{Optimal filtering.} 
  Information and System Sciences Series. Prentice Hall.  \cr
  Ruckdeschel, P. (2001) \emph{Ans\"atze zur Robustifizierung des
Kalman Filters.} Bayreuther Mathematische Schriften, Vol. 64. 
}


\keyword{ts}
\keyword{robust}

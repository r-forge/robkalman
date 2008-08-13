\name{internalKalman}
\alias{internalKalman}
\alias{.getKG}
\alias{.getcorrCov}
\alias{.getpredCov}
\alias{.cKpredstep}
\alias{.cKScorrstep}

\title{Internal functions of package robKalman for the classical Kalman filter}

\description{
These functions are used internally by package robKalman
}
\usage{
.getKG(S1, Z, V)
.getcorrCov(S1, K, Z)
.getpredCov(S0, F, Q)
.cKinitstep(a, S, ...) 
.cKpredstep(x0, S0, F, Q, ...) 
.cKcorrstep(y, x1, S1, Z, V, ...) 
}

\arguments{
  \item{a}{mean of the initial state}
  \item{S}{initial state covariance (see below)}
  \item{Z}{observation matrix (see below)}
  \item{V}{observation error covariance (see below)}
  \item{F}{innovation transition matrix (see below)}
  \item{Q}{innovation covariance (see below)}
  \item{K}{Kalman gain \eqn{K_t}}
  \item{S1}{prediction error covariance \eqn{S_{t|t-1}} of the classical Kalman filter}
  \item{S0}{filter error covariance \eqn{S_{t-1|t-1}} of the classical Kalman filter}
  \item{y}{observation \eqn{y_t}}
  \item{x0}{(classical Kalman)- filtered state \eqn{x_{t-1|t-1}}}
  \item{x1}{(classical Kalman)- predicted state \eqn{x_{t|t-1}}}
  \item{...}{not used here; for compatibility with signatures of other "step"-functions}
}

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

For notation, let us formulate the classical Kalman filter in this context:

\strong{(0) ininitial step}          \deqn{x_{0|0}   = a} 
\eqn{\qquad}{\code{      }} with error covariance      
\deqn{S_{0|0}   = {\rm Cov}(x_0-x_{0|0})   = \code{S}}{S_{0|0}   = Cov(x_0-x_{0|0})   = S}%

\strong{(1) prediction step}       
\deqn{x_{t|t-1} = F x_{t-1|t-1},\qquad t\ge 1}{x_{t|t-1} = F x_{t-1|t-1},         t>=1}
\eqn{\qquad}{\code{      }} with error covariance      
\deqn{S_{t|t-1} = {\rm Cov}(x_t-x_{t|t-1}) = F S_{t-1|t-1} F' + Q}{S_{t|t-1} = Cov(x_t-x_{t|t-1}) = F S_{t-1|t-1} F' + Q} 

\strong{(2) correction step}       
\deqn{x_{t|t}   = x_{t|t-1} + K_t (y_t - Z x_{t|t-1}),\qquad        t\ge 1}{x_{t|t}   = x_{t|t-1} + K_t (y_t - Z x_{t|t-1}),        t>=1}
\eqn{\qquad}{\code{      }} for Kalman Gain   
\deqn{K_t = S_{t|t-1} Z' (Z S_{t|t-1} Z' + V )^-}
\eqn{\qquad}{\code{      }} with error covariance 
\deqn{S_{t|t}   = {\rm Cov}(x_t-x_{t|t}) = S_{t|t-1} - K_t Z S_{t|t-1}}{S_{t|t}   = Cov(x_t-x_{t|t}) = S_{t|t-1} - K_t Z S_{t|t-1}}
}


\value{
\code{.getKG} calculates the Kalman Gain for \code{S1 =}\eqn{S_{t|t-1}}, \code{Z}, \code{V} as above \cr
\code{.getcorrCov}  calculates \eqn{S_{t|t}} for \code{S1 =} \eqn{S_{t|t-1}}, \code{K =}\eqn{K_t} and \code{Z} as above \cr
\code{.getpredCov} calculates \eqn{S_{t|t-1}} for \code{S0 =} \eqn{S_{t-1|t-1}}, \code{F}, and \code{Q} as above\cr
\code{.cKinitstep}  calculates \eqn{x_{0|0}} for \code{a}, \code{S} as above\cr

\code{.cKpredstep}  calculates \eqn{x_{t|t-1}} for \code{x0 =}\eqn{x_{t-1|t-1}}, \code{S0 =}\eqn{S_{t-1|t-1}}, and \code{F}, \code{Q}\cr

\code{.cKcorrstep}  calculates \eqn{x_{t|t}} for \code{x1 =}\eqn{x_{t|t-1}}, \code{y =}\eqn{y_{t}}, 
\code{S1 =}\eqn{S_{t|t-1}}, and \code{Z}, \code{V} \cr

}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  }


\seealso{
\code{\link{internalrLS}}
}


\keyword{internal}

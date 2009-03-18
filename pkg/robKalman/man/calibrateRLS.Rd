\name{calibrateRLS}
\alias{rLScalibrateB}
\alias{calibrateRLS}
\title{Calibration of clipping height b}

\description{
calibrates the clipping height \code{b} of the rLS-filter in a time-invariant, linear, Gaussian state space model
}
\usage{
rLScalibrateB(Z, S, V, repl = 100000, eff, r, upto=20)
}
\arguments{
  \item{Z}{observation matrix in the (ti-l-G-SSM); see below}
  \item{S}{prediction error covariance matrix (of the classical Kalman filter) in the (ti-l-G-SSM); see below}
  \item{V}{observation error covariance matrix in the (ti-l-G-SSM); see below}
  \item{r}{SO-contamination radius}
  \item{eff}{efficiency w.r.t. classical Kalman filter in the ideal model}
  \item{repl}{number of replicates used for a LLN-approximation of the expectations needed in this calibration}
  \item{upto}{an upper bound to \code{b} used in the zero-search of \code{uniroot} within \code{rLScalibrateB}}
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

The clipping height \code{b} given \eqn{Z}, \eqn{V}, and prediction error covariance \eqn{S_{t|t-1}} 
(of the classical Kalman filter) is either calibrated to a given efficiency \code{eff} in the ideal model or to given 
(SO)-radius \code{r} about the ideal model. 

The expectations needed for this calibration are calculated by a LLN 
approximation with \code{repl} replicates;
}

\value{a clipping height \code{b} for the rLS filter}
\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  }


\examples{
require(robKalman)

##Hyper parameter of a lin. time-inv. Gaussian SSM
SS0  <- matrix(0, 2, 2)
F0   <- matrix(c(.7, 0.5, 0.2, 0), 2, 2)
Q0   <- matrix(c(2, 0.5, 0.5, 1), 2, 2)
Z0   <- matrix(c(1, -0.5), 1, 2)
V0i  <- 1

### limiting prediction error covariance
SS <- limitS(S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)

### calibration b
# by efficiency in the ideal model
# efficiency  =  0.9
(B1 <- rLScalibrateB(eff = 0.9, S = SS, Z = Z0, V = V0i))
# by contamination radius
# r  =  0.1
(B2 <- rLScalibrateB(r = 0.1, S = SS, Z = Z0, V = V0i))
}

\keyword{robust}
\keyword{ts}

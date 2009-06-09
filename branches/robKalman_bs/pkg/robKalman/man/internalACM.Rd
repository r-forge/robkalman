\name{internalACM}
\alias{internalACM}
\alias{.getcorrCovACM}
\alias{.ACMinitstep}
\alias{.ACMpredstep}
\alias{.ACMcorrstep}

\title{Internal functions of package robKalman for the ACM filter}

\description{
These functions are used internally by package robKalman for the ACM filter
}

\usage{
.getcorrCovACM(S1, K,  Z, W=diag(nrow(Z)))
.ACMinitstep(a, S, ...) 
.ACMpredstep(x0, S0, F, Q, rob0, s0, ...) 
.ACMcorrstep(y, x1, S1, Z, V, rob1, dum, psi, apsi, bpsi, cpsi, flag, ...) 
}


\arguments{
  \item{a}{mean of the initial state}
  \item{S}{initial state covariance (see below)}
  \item{Z}{observation matrix (see below)}
  \item{V}{observation error covariance (see below)}
  \item{F}{innovation transition matrix (see below)}
  \item{Q}{innovation covariance (see below)}
  \item{K}{Kalman gain \eqn{K_t}}
  \item{W}{weight matrix}
  \item{dum}{dummy variable for compatibility with \dots argument of calling function}
  \item{s0}{scale of nominal Gaussian component of additive noise}
  \item{S1}{prediction error covariance \eqn{S_{t|t-1}} of the ACM filter}
  \item{S0}{filter error covariance \eqn{S_{t-1|t-1}} of the ACM filter}
  \item{rob0}{not used here; included for compatibility reasons; set to \code{NULL}}
  \item{rob1}{used to pass on information recursively; here: \code{st} time-dependent scale parameter}
  \item{psi}{influence function to be used (default: Hampel's \eqn{\psi} function, which is the only one available at the moment)}
  \item{a,b,c}{tuning constants for Hampel's \eqn{\psi}-function, (default: \code{a=b=2.5}, \code{c=5.0})}
  \item{flag}{character, if "weights" (default), use \eqn{\psi(t)/t} to calculate the weights; if "deriv", use \eqn{\psi'(t)}}
  \item{y}{observation \code{y_t}}
  \item{x0}{(ACM)- filtered state \eqn{x_{t-1|t-1}}}
  \item{x1}{(ACM)- predicted state \eqn{x_{t|t-1}}}
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
FURTHER DETAILS TO BE FILLED
}


\value{
\code{.getcorrCovACM} determines filter error covariance \eqn{S_{t-1|t-1}} of the ACM filter. \cr
\code{.ACMinitstep}  calculates \eqn{x_{0|0}}.  \cr
\code{.ACMpredstep}  calculates the ACM-\eqn{x_{t|t-1}}.  \cr
\code{.ACMcorrstep}  calculates the ACM-\eqn{x_{t|t}}.  \cr
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
}

\seealso{
\code{\link{internalKalman}}, \code{\link{internalrLS}}, \code{\link{recFilter}}
}

\references{
Martin, R.D. (1979): Approximate Conditional-mean Type Smoothers and Interpolators.\cr
Martin, R.D. (1981): Robust Methods for Time Series. \cr
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation. \cr
}

\keyword{internal}

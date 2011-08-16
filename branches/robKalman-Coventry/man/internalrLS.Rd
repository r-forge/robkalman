\name{internalrLS}
\alias{internalrLS}
\alias{.rLSinitstep}
\alias{.rLSpredstep}
\alias{.rLScorrstep}

\title{Internal functions of package robKalman for the rLS filter}

\description{
This function is used internally by package robKalman for the rLS filter
}

\usage{
.rLScorrstep(y, x1, S1, Z, V, i,  rob1=NULL, b, norm=EuclideanNorm, ...)
}

\arguments{
  \item{Z}{observation matrix (see below)}
  \item{V}{observation error covariance (see below)}
  \item{b}{clipping height \code{b} for the rLS filter}
  \item{i}{the time instance}
  \item{norm}{a function with a numeric vector \code{x} as first argument, 
              returning a norm of \code{x} - not necessarily, but defaulting to, Euclidean norm; 
              used by rLS filter to determine "too" large corrections}
  \item{S1}{prediction error covariance \eqn{S_{t|t-1}} of the classical Kalman filter}
  \item{rob1}{not used here; included for compatibility reasons; set to \code{NULL}}
  \item{y}{observation \eqn{y_t}}
  \item{x1}{(rLS Kalman)- predicted state \eqn{x_{t|t-1}}}
  \item{...}{not used here; for compatibility with signatures of other "step"-functions}
}

\details{
We work in the setup of the linear, Gaussian state space model (l-G-SSM)
with \eqn{p} dimensional states \eqn{x_t} and \eqn{q} dimensional observations \eqn{y_t},
with \strong{initial condition}
\deqn{x_0 \sim {\cal N}_p(a,S),}{x_0 ~ N_p(a,S),}

\strong{state equation}
\deqn{x_t = F_t x_{t-1} +  v_t, \qquad v_t \sim {\cal N}_p(0,Q_t),\qquad t\ge 1,
      }{x_t = F_t x_{t-1} +  v_t,          v_t ~ N_p(0,Q_t),  t>=1,}

\strong{observation equation}
\deqn{y_t = Z_t x_{t}   +  \varepsilon_t, \qquad \varepsilon_t \sim
{\cal N}_q(0,V_t),\qquad t\ge 1,}{y_t = Z_t x_t   +  e_t,      e_t ~ N_q(0,V_t),  t>=1,}
and where all random variable \eqn{x_0}, \eqn{v_t}, \eqn{\varepsilon_t}{e_t} are independent.

For notation, let us formulate the classical Kalman filter in this context:

\strong{(0) ininitial step}          \deqn{x_{0|0}   = a}
\eqn{\qquad}{\code{      }} with error covariance
\deqn{S_{0|0}   = {\rm Cov}(x_0-x_{0|0})   = \code{S}}{S_{0|0}   = Cov(x_0-x_{0|0})   = S}%

\strong{(1) prediction step}
\deqn{x_{t|t-1} = F_t x_{t-1|t-1},\qquad t\ge 1}{x_{t|t-1} = F_t x_{t-1|t-1},         t>=1}
\eqn{\qquad}{\code{      }} with error covariance
\deqn{S_{t|t-1} = {\rm Cov}(x_t-x_{t|t-1}) =
F_t S_{t-1|t-1} F_t' + Q_t}{S_{t|t-1} = Cov(x_t-x_{t|t-1}) = F_t S_{t-1|t-1} F_t' + Q_t}

\strong{(2) correction step}
\deqn{x_{t|t}   = x_{t|t-1} + K_t (y_t - Z_t x_{t|t-1}),\qquad
t\ge 1}{x_{t|t}   = x_{t|t-1} + K_t (y_t - Z_t x_{t|t-1}),        t>=1}
\eqn{\qquad}{\code{      }} for Kalman Gain
\deqn{K_t = S_{t|t-1} Z_t' (Z_t S_{t|t-1} Z_t' + V_t )^-}
\eqn{\qquad}{\code{      }} with error covariance
\deqn{S_{t|t}   = {\rm Cov}(x_t-x_{t|t}) = S_{t|t-1} - K_t Z_t S_{t|t-1}}{S_{t|t}   =
Cov(x_t-x_{t|t}) = S_{t|t-1} - K_t Z_t S_{t|t-1}}
}


\value{
\code{.rLScorrstep(y, x1, S1, Z, V, b)}  calculates rLS-\eqn{x_{t|t}} for arguments \code{b}, \code{x1 =}\eqn{x_{t|t-1}} (from rLS-past), 
\code{y =}\eqn{y_{t}},  \code{S1 =}\eqn{S_{t|t-1}}, \code{Z}, and  \code{V} as above\cr
The return value is a list with components
\code{x0} (the filtered values), \code{K} (the Kalman gain),
\code{S0} (the filter error covariance),
\code{Delta} (the covariance of \eqn{\Delta y_t}{Delta y_t}),
\code{DeltaY} (the observation residuals \eqn{\Delta y_t}{Delta y_t}),
\code{Ind}(the indicators of clipped runs)
}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  }

\seealso{
\code{\link{internalKalman}}
}

\references{
Ruckdeschel, P. (2001) \emph{Ans\"atze zur Robustifizierung des
Kalman Filters.} Bayreuther Mathematische Schriften, Vol. 64. 
}

\keyword{internal}

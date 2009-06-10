\name{recFilter}
\alias{recFilter}
\alias{rLSFilter}
\alias{KalmanFilter}
\alias{ACMfilter}
\alias{recursiveFilter}

\title{Several recursive filters: the classical Kalman filter, the rLS filter, and the ACM filter}

\description{
These functions are (preliminary) interfaces producing recursive filters to a given series of observations from a 
time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
}

\usage{
recursiveFilter(Y, a, S, F, Q, Z, V, 
                initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep, 
                initSr=NULL, predSr=NULL, corrSr=NULL, ...)
KalmanFilter(Y, a, S, F, Q, Z, V)
rLSFilter(Y, a, S, F, Q, Z, V, b, norm=Euclideannorm)
ACMfilter(Y, a, S, F, Q, Z, V, s0, psi,  apsi, bpsi, cpsi, flag)
}

\arguments{
  \item{a}{mean of the initial state}
  \item{S}{initial state covariance (see below)}
  \item{Z}{observation matrix (see below)}
  \item{V}{observation error covariance (see below)}
  \item{F}{innovation transition matrix (see below)}
  \item{Q}{innovation covariance (see below)}
  \item{b}{clipping height \code{b} for the rLS filter}
  \item{norm}{a function with a numeric vector \code{x} as first argument, 
              returning a norm of \code{x} - not necessarily, but defaulting to, Euclidean norm; 
              used by rLS filter to determine "too" large corrections}
  \item{Y}{observations \eqn{y_t}, in matrix form \eqn{q \times t}{q x t} for \code{t} 
           the length of the observation series and \code{q} the observation dimension}
  \item{s0}{scale of nominal Gaussian component of additive noise}
  \item{psi}{influence function to be used (default: Hampel's \eqn{\psi} function, which is the only one available at the moment)}
  \item{apsi,bpsi,cpsi}{tuning constants for Hampel's \eqn{\psi}-function, (default: \code{a=b=2.5}, \code{c=5.0})}
  \item{flag}{character, if "weights" (default), use \eqn{\psi(t)/t} to calculate the weights; if "deriv", use \eqn{\psi'(t)}}
  \item{initSc}{a function with first arguments \code{a} and \code{S} to produce 
                the classical (non robust) initial filter value \eqn{x_{0|0}}}
  \item{initSr}{either \code{NULL}  or a function with first arguments \code{a} and \code{S} to produce 
                a robust initial filter value \eqn{x_{0|0}}}
  \item{predSc}{a function with first arguments \code{x0}\eqn{=x_{t-1|t-1}} and \code{S0}\eqn{=S_{t-1|t-1}}, \code{F}, 
                and \code{Q} to produce the classical (non robust) prediction value \eqn{x_{t|t-1}}}
  \item{predSr}{either \code{NULL}  or  a function with first arguments \code{x0}\eqn{=x_{t-1|t-1}} and \code{S0}\eqn{=S_{t-1|t-1}}, \code{F}, 
                and \code{Q} to produce a robust prediction value \eqn{x_{t|t-1}}}
  \item{corrSc}{a function with first arguments \code{y}\eqn{=y_t}, \code{x1}\eqn{=x_{t|t-1}} and \code{S1}\eqn{=S_{t|t-1}}, \code{Z}, 
                and \code{V} to produce the classical (non robust) correction value \eqn{x_{t|t}}}
  \item{corrSr}{either \code{NULL}  or a function with first arguments \code{y}\eqn{=y_t}, \code{x1}\eqn{=x_{t|t-1}} and \code{S1}\eqn{=S_{t|t-1}}, \code{Z}, 
                and \code{V} to produce a robust correction value \eqn{x_{t|t}}}  
  \item{...}{further arguments to the "step"-functions}
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
\code{recursiveFilter} returns a list with the following elements
\item{Xf:}{ the series \eqn{x_{t|t}} filtered by the classical filter --- a matrix with dimensions \eqn{p \times t+1}{p x (t+1)}}
\item{Xp:}{ the series \eqn{x_{t|t-1}}  predicted by the classical filter --- a matrix with dimensions \eqn{p \times t}{p x t}}
\item{S0:}{ the series \eqn{S_{t|t}} of filter error covariances produced by the classical filter --- 
          an array with dimensions \eqn{p \times p \times t+1}{p x (t+1)}}
\item{S1:}{ the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the classical filter --- 
          an array with dimensions \eqn{p \times p \times t}{p x t}}
\item{KG:}{ the series \eqn{K_{t}}  of Kalman gains produced by the classical filter --- 
          an array with dimensions \eqn{q \times p \times t}{p x t}}
\item{Xrf:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{x_{t|t}} filtered by the robust filter --- a matrix with dimensions \eqn{p \times t+1}{p x (t+1)} 
          else \code{NULL}}      
\item{Xrp:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{x_{t|t-1}} predicted by the robust filter --- a matrix with dimensions \eqn{p \times t}{p x t} 
          else \code{NULL}}      
\item{Sr0:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{S_{t|t}} of filter error covariances produced by the robust filter --- 
          an array with dimensions \eqn{p \times p \times t+1}{p x p x (t+1)}}
\item{Sr1:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the robust filter --- 
          an array with dimensions \eqn{p \times p \times t}{p x p x t}}
\item{KGr:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{K_{t}}  of Kalman gains produced by the robust filter --- 
          an array with dimensions \eqn{q \times p \times t}{q x p x t}}
\item{rob0:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          a list of length \code{t+1} with the recursively produced values of \code{rob0} --- 
          e.g.  in case of the ACM filter each element contains a corresponding value of \code{st}}
\item{rob1:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          a list of length \code{t+1} with the recursively produced values of \code{rob1} --- 
          e.g.  in case of the ACM filter each element contains a corresponding value of \code{st}}
\item{IndIO:}{ if  \code{predSr} is not \code{NULL}:
          the indicator showing when the robust predictor uses clipping --- a vector with dimensions \eqn{t} 
          else \code{NULL}}      
\item{IndAO:}{ if  \code{corrSr} is not \code{NULL}:
                 the indicator showing when the robust filter uses clipping --- a vector with dimensions \eqn{t} 
                 else \code{NULL}}      
\code{KalmanFilter(Y, a, S, F, Q, Z, V)} is a wrapper to \code{recursiveFilter(Y, a, S, F, Q, Z, V)}.\cr 
\code{rLSFilter(Y, a, S, F, Q, Z, V, b, norm=Euclideannorm)} is a wrapper to\cr  
\code{recursiveFilter(Y, a, S, F, Q, Z, V,  
           initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep,
           initSr=.cKinitstep, predSr=.cKpredstep, corrSr=rLScorrstep, 
           b=b, norm=norm)}.
\code{ACMFilter(Y, a, S, F, Q, Z, V, s0, psi, apsi, bpsi, cpsi, flag)} is a wrapper to \cr
\code{recursiveFilter(Y, a, S, F, Q, Z, V, 
           initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep, 
           initSr=.cKinitstep, predSr=.ACMpredstep, corrSr=.ACMcorrstep, 
           s0,  apsi, bpsi, cpsi, flag)}.
}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
  }


\seealso{
\code{\link{internalrLS}}, \code{\link{internalKalman}}, \code{\link{calibrateRLS}}
\code{\link{utilitiesrobKalman}}, \code{\link{internalACM}}, \code{\link{internalpsi}} 
}

\examples{
require(robKalman)

a0   <- c(1, 0)
SS0  <- matrix(0, 2, 2)
F0   <- matrix(c(.7, 0.5, 0.2, 0), 2, 2)
Q0   <- matrix(c(2, 0.5, 0.5, 1), 2, 2)
TT   <- 100

Z0   <- matrix(c(1, -0.5), 1, 2)
V0i  <- 1
m0c  <- -30
V0c  <- 0.1
ract <- 0.1

X  <- simulateState( a = a0, S = SS0, F = F0, Q = Q0, tt = TT)
Y  <- simulateObs(X = X, Z = Z0, Vi = V0i, mc = m0c, Vc = V0c, r = ract)
SS <- limitS(S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)


### calibration b
# by efficiency in the ideal model
# efficiency  =  0.9
(B1 <- rLScalibrateB(eff = 0.9, S = SS, Z = Z0, V = V0i))
# by contamination radius
# r  =  0.1
(B2 <- rLScalibrateB(r = 0.1, S = SS, Z = Z0, V = V0i))


erg <- KalmanFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)

rerg1 <- rLSFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B1$b)
rerg2 <- rLSFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B2$b)
mean((X - rerg1$Xf)^2) ### empirical MSE of the filters considered
mean((X - rerg1$Xrf)^2)
mean((X - rerg2$Xrf)^2)

}

\references{
Martin, R.D. (1979): Approximate Conditional-mean Type Smoothers and Interpolators.\cr
Martin, R.D. (1981): Robust Methods for Time Series.\cr
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation.\cr 
Ruckdeschel, P. (2001) \emph{Ans\"atze zur Robustifizierung des
Kalman Filters.} Bayreuther Mathematische Schriften, Vol. 64. 
}

\keyword{robust}
\keyword{ts}

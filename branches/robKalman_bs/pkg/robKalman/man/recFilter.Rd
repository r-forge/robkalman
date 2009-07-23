\name{recFilter}
\alias{recFilter}
\alias{rLSFilter}
\alias{rLS.AO.Filter}
\alias{rLS.IO.Filter}
\alias{KalmanFilter}
\alias{ACMfilter}
\alias{recursiveFilter}

\title{Several recursive filters: the classical Kalman filter, the rLS filter, and the ACM filter}

\description{
These functions are (preliminary) interfaces producing recursive filters to a given series of observations from a 
time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
}

\usage{
recursiveFilter(Y, a, S, F, Q, Z, V, initSc = .cKinitstep, predSc = .cKpredstep,
                   corrSc = .cKcorrstep,
                   initSr = NULL, predSr = NULL, corrSr = NULL,
                   nsim = 0, seed = NULL, ..., dropRuns = TRUE,
                   CovRunDep = FALSE, saveOpt = TRUE, dimsCheck = NULL)
KalmanFilter(Y, a, S, F, Q, Z, V, dropRuns = TRUE)
rLSFilter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm, dropRuns = TRUE)
rLS.AO.Filter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm, dropRuns = TRUE)
rLS.IO.Filter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm, dropRuns = TRUE)
ACMfilter(Y, a, S, F, Q, Z, V, s0, psi,  apsi, bpsi, cpsi, flag, dropRuns = TRUE)
}

\arguments{
  \item{a}{mean of the initial state, in matrix form \eqn{p \times runs}{p x runs}
           for \code{p} the state dimension
           and \code{runs} the number of paths to be followed in parallel.}
  \item{S}{initial state covariance (see below), in matrix form \eqn{p \times p}{p x p}
           for \code{p} the state dimension.}
  \item{Z}{observation matrices (see below), in array form \eqn{q \times p \times t}{q x p x t}
           for \code{q} and \code{p} the observation resp.\ state dimension
           and \code{t} the length of the observation series.}
  \item{V}{observation error covariances (see below), in array form \eqn{q \times q \times t}{q x q x t}
           for \code{q} the observation dimension
           and \code{t} the length of the observation series.}
  \item{F}{innovation transition matrices (see below), in array form \eqn{p \times p \times t}{p x p x t}
           for \code{p} the state dimension
           and \code{t} the length of the observation series.}
  \item{Q}{innovation covariances (see below), in array form \eqn{p \times p \times t}{p x p x t}
           for \code{p} the state dimension
           and \code{t} the length of the observation series.}
  \item{b}{clipping height \code{b} for the rLS filter}
  \item{norm}{a function with a numeric vector \code{x} as first argument, 
              returning a norm of \code{x} - not necessarily, but defaulting to, Euclidean norm; 
              used by rLS filter to determine "too" large corrections}
  \item{Y}{observations \eqn{y_t}, in array form \eqn{q \times runs \times t}{q x runs x t} for \code{t}
           the length of the observation series, \code{q} the observation dimension
           and \code{runs} the number of paths to be followed in parallel.}
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
  \item{nsim}{integer; if positive, we simulate a bunch of nsim paths (acc. to ideal model) to get emp. covariances}
  \item{seed}{seed for the simulations}
  \item{dropRuns}{logical; shall run-dimension be collapsed if it is one?}
  \item{CovRunDep}{logical; shall covariances be run-dependent?}
  \item{saveOpt}{logical; either single valued or (named) 4-valued:
                 shall we store Kalman gain, Cov(Delta Y), Delta Y, ...?}
  \item{dimsCheck}{logical or \code{NULL}; shall we check dimensions?}
  \item{...}{further arguments to the "step"-functions}
}

\details{
We work in the setup of the time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
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
\item{Xf:}{ the series \eqn{x_{t|t}} filtered by the classical filter --- a matrix with
            dimensions \eqn{p \times t+1}{p x (t+1)}}
\item{Xp:}{ the series \eqn{x_{t|t-1}}  predicted by the classical filter ---
            a matrix with dimensions \eqn{p \times t}{p x t}}
\item{Xrf:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{x_{t|t}} filtered by the robust filter --- a matrix with dimensions
          \eqn{p \times t+1}{p x (t+1)}
          else \code{NULL}}
\item{Xrp:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{x_{t|t-1}} predicted by the robust filter
          --- a matrix with dimensions \eqn{p \times t}{p x t}
          else \code{NULL}}
\item{S0:}{ the series \eqn{S_{t|t}} of filter error covariances produced by the classical filter ---
          an array with dimensions \eqn{p \times p \times t+1}{p x p x (t+1)}}
\item{S1:}{ the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the classical filter --- 
          an array with dimensions \eqn{p \times p \times t}{p x p x t}}
\item{KG:}{ the series \eqn{K_{t}}  of Kalman gains produced by the classical filter --- 
          an array with dimensions \eqn{q \times p \times t}{q x p x t}}
\item{Delta:}{ the series \eqn{\Delta_{t}}{Delta_t}  of covariances of
          \eqn{\Delta y_{t}}{Delta y_t} produced by the classical filter ---
          an array with dimensions \eqn{q \times q \times t}{q x q x t}}
\item{DeltaY:}{ the series \eqn{\Delta y_{t}}{Delta y_t}  of observation residuals
          produced by the classical filter ---
          an array with dimensions \eqn{q \times runs \times t}{q x runs x t}}
\item{Sr0:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{S_{t|t}} of filter error covariances produced by the robust filter --- 
          an array with dimensions \eqn{p \times p \times t+1}{p x p x (t+1)}}
\item{Sr1:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{S_{t|t-1}}  of prediction error covariances produced by the robust filter --- 
          an array with dimensions \eqn{p \times p \times t}{p x p x t}}
\item{KGr:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          the series \eqn{K_{t}}  of Kalman gains produced by the robust filter --- 
          an array with dimensions \eqn{q \times p \times t}{q x p x t}}
\item{Deltar:}{ the series \eqn{\Delta_{t}}{Delta_t}  of covariances of
          \eqn{\Delta y_{t}}{Delta y_t} produced by the robust filter ---
          an array with dimensions \eqn{q \times q \times t}{q x q x t}}
\item{DeltaYr:}{ the series \eqn{\Delta y_{t}}{Delta y_t}  of observation residuals
          produced by the robust filter ---
          an array with dimensions \eqn{q \times runs \times t}{q x runs x t}}
\item{IndIO:}{ if  \code{predSr} is not \code{NULL}:
          the indicator showing when the robust predictor uses clipping --- a vector with dimensions \eqn{t} 
          else \code{NULL}}      
\item{IndAO:}{ if  \code{corrSr} is not \code{NULL}:
                 the indicator showing when the robust filter uses clipping --- a vector with dimensions \eqn{t} 
                 else \code{NULL}}      
\item{rob0L:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          a list of length \code{t+1} with the recursively produced values of \code{rob0} ---
          e.g.  in case of the ACM filter each element contains a corresponding value of \code{st}}
\item{rob1L:}{if any of the arguments \code{initSr}, \code{predSr}, \code{corrSr} is not \code{NULL}:
          a list of length \code{t+1} with the recursively produced values of \code{rob1} ---
          e.g.  in case of the ACM filter each element contains a corresponding value of \code{st}}

\code{KalmanFilter(Y, a, S, F, Q, Z, V)} is a wrapper to \code{recursiveFilter(Y, a, S, F, Q, Z, V)}.\cr
\code{rLS.AO.Filter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm)} and (synonymously)
\code{rLSFilter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm)} are wrappers to\cr
\code{recursiveFilter(Y, a, S, F, Q, Z, V,  
           initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep,
           initSr=.cKinitstep, predSr=.cKpredstep, corrSr=.rLScorrstep,
           b=b, norm=norm)}.
\code{rLS.IO.Filter(Y, a, S, F, Q, Z, V, b, norm=EuclideanNorm)} is a wrapper to\cr
\code{recursiveFilter(Y, a, S, F, Q, Z, V,
           initSc=.cKinitstep, predSc=.cKpredstep, corrSc=.cKcorrstep,
           initSr=.cKinitstep, predSr=.cKpredstep, corrSr=.rLS.IO.corrstep,
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

X  <- simulateState( a = a0, S = SS0, F = F0, Qi = Q0, tt = TT)
Y  <- simulateObs(X = X, Z = Z0, Vi = V0i, mc = m0c, Vc = V0c, r = ract)
SS <- limitS(S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)


### calibration b
# by efficiency in the ideal model
# efficiency  =  0.9
(B1 <- rLScalibrateB(eff = 0.9, S = SS, Z = Z0, V = V0i))
# by contamination radius
# r  =  0.1
(B2 <- rLScalibrateB(r = 0.1, S = SS, Z = Z0, V = V0i))

# IO-filter
# by efficiency in the ideal model
# efficiency  =  0.9
(B1.IO <- rLScalibrateB(eff = 0.9, S = SS, Z = Z0, V = V0i, IO = TRUE))
# by contamination radius
# r  =  0.1
(B2.IO <- rLScalibrateB(r = 0.1, S = SS, Z = Z0, V = V0i, IO = TRUE))

erg <- KalmanFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i)

rerg1 <- rLSFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B1$b)
rerg2 <- rLSFilter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B2$b)

rerg1.IO <- rLS.IO.Filter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B1.IO$b)
rerg2.IO <- rLS.IO.Filter(Y, a = a0, S = SS0, F = F0, Q = Q0, Z = Z0, V = V0i, b = B2.IO$b)

mean((X[,1,] - rerg1$Xf)^2) ### empirical MSE of the filters considered
mean((X[,1,] - rerg1$Xrf)^2)
mean((X[,1,] - rerg2$Xrf)^2)
### not surprisingly IO-rob filter is not a good idea for AO - situation
mean((X[,1,] - rerg1.IO$Xrf)^2)
mean((X[,1,] - rerg2.IO$Xrf)^2)

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

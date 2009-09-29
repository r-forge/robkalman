\name{simulateSScont}
\alias{simulateSScont}
\alias{rcvmvnorm}
\alias{simulateState}
\alias{simulateObs}

\title{Routines for the simulation of AO-contaminated state space models}

\description{
For testing purposes, with these routines, AO-contaminated observations from a 
multivariate time-invariant, linear, Gaussian state space model may be generated
}
\usage{
rcvmvnorm(runs, mi, Si, mc, Sc, r)
simulateState(a, S, F, Qi, mc=0, Qc=Qi, runs = 1, tt, r=0)
simulateObs(X, Z, Vi, mc=0, Vc=Vi, runs = 1, r=0)
}

\arguments{
  \item{runs}{number of runs to be generated}
  \item{mi}{mean of the ideal multivariate normal distribution}
  \item{mc}{mean of the contaminating multivariate normal distribution}
  \item{Si}{covariance of the ideal multivariate normal distribution}
  \item{Sc}{covariance of the contaminating multivariate normal distribution}
  \item{r}{convex contamination radius/probability}
  \item{a}{mean of the initial state}
  \item{S}{initial state covariance (see below)}
  \item{F}{innovation transition matrix (see below)}
  \item{Qi}{ideal innovation covariance (see below)}
  \item{Qc}{contaminating innovation covariance (see below)}
  \item{tt}{length of the simulated series of states/observations}
  \item{Z}{observation matrix (see below)}
  \item{Vi}{ideal observation error covariance (see below)}
  \item{mc}{contaminating observation error mean (see below)}
  \item{Vc}{contaminating observation error covariance (see below)}
  \item{X}{series of states on basis of which the observations are simulated}
}

\details{
We work in the setup of the time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
with \eqn{p} dimensional states \eqn{x_t} and \eqn{q} dimensional observations \eqn{y_t},
with \strong{initial condition}
\deqn{x_0 \sim {\cal N}_p(a,S),}{x_0 ~ N_p(a,S),}

\strong{state equation}
\deqn{x_t = F x_{t-1} +  v_t, \qquad v_t \sim {\cal N}_p(0,Q),\qquad t\ge 1,}{x_t = F x_{t-1} +  v_t,          v_t ~ N_p(0,Q),  t>=1,}

\strong{ideal observation equation}
\deqn{y_t = Z x_{t}   +  \varepsilon^{\scriptsize \rm id}_t, \qquad %
       \varepsilon^{\scriptsize \rm id}_t 
       \sim {\cal N}_q(0,V_i),\qquad t\ge 1,}{y_t = Z x_t   +  e_{t;id},    e_{t;id} ~ N_q(0,V_i),    t>= 1,}%

\strong{realistic observation equation}
\deqn{y_t = Z x_{t}   +  \varepsilon^{\scriptsize \rm re}_t, \qquad %
       \varepsilon^{\scriptsize \rm re}_t \sim (1-r){\cal N}_q(0,V_i)+r {\cal N}_q(m_c,V_c),\qquad 
       t\ge 1,}{y_t = Z x_t   +  e_{t;re},    e_{t;re} ~ (1-r) N_q(0,V_i) + r N_q(m_c,V_c),    t>= 1,}%

and where all random variable \eqn{x_0}, \eqn{v_t}, \eqn{\varepsilon^{\scriptsize \rm id}_t}{e_{t;id}} 
[respectively, \eqn{\varepsilon^{\scriptsize \rm re}_t}{e_{t;re}}] are independent.

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
\code{rcvmvnorm(mi, Si, mc, Sc, r)} returns a (pseudo) random variable drawn from 
      \deqn{(1-r){\cal N}_q(m_i,S_i)+r {\cal N}_q(m_c,S_c)}
\code{simulateState} simulates a series of \eqn{t=}\code{tt} states plus one initial state from the (ti-l-G-SSM) given by the Hyper parameters
--- yielding a matrix \eqn{p \times t+1}{p x (t+1)}\cr
\code{simulateObs}, on bases of the series of states \code{X} (initial state included) simulates a series of observations of length 
\code{tt} according to the Hyper parameters --- giving a matrix \eqn{q \times t}{q x t}
}

\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr 
  }

\seealso{
\code{\link{internalrLS}}
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

}

\keyword{internal}

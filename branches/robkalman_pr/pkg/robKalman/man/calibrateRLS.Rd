\name{calibrateRLS}
\alias{rLScalibrateB}
\alias{calibrateRLS}
\title{Calibration of clipping height b}

\description{
calibrates the clipping height \code{b} of the rLS-filter in a time-invariant, linear, Gaussian state space model
}
\usage{
rLScalibrateB(Z, S, V, repl = 100000, b = NULL, eff = NULL, r = NULL,
              rlow = 0, rup = NULL, upto=20, IO = FALSE, seed)
}
\arguments{
  \item{Z}{observation matrix in the (ti-l-G-SSM); see below}
  \item{S}{prediction error covariance matrix (of the classical Kalman filter) in the (ti-l-G-SSM); see below}
  \item{V}{observation error covariance matrix in the (ti-l-G-SSM); see below}
  \item{r}{SO/IO-contamination radius}
  \item{b}{given clipping height}
  \item{eff}{efficiency w.r.t. classical Kalman filter in the ideal model}
  \item{rlow}{lower bound for SO/IO-contamination radius}
  \item{rup}{upper bound for SO/IO-contamination radius}
  \item{repl}{number of replicates used for a LLN-approximation of the expectations needed in this calibration}
  \item{upto}{an upper bound to \code{b} used in the zero-search of \code{uniroot} within \code{rLScalibrateB}}
  \item{IO}{logical of length 1: Is it rLS.IO (\code{TRUE}) or rLS[.AO] which is to be calibrated?}
  \item{seed}{if not missing: argument to \code{\link{set.seed}} to set the seed for MC integration}
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
(of the classical Kalman filter) is either calibrated to a given efficiency \code{eff} in the
ideal model or to given (SO/IO)-radius \code{r} about the ideal model.

If this radius is unknown, to a given radius interval [\code{rlow},\code{rup}],
\eqn{0\leq {\tt rlow} < {\tt rup} \leq 1 }{{0 <= \code{rlow} < \code{rup} <= 1 }}
a least favorable radius \code{r0} is distinguished giving a radius minimax procedure.

The expectations needed for this calibration are calculated by a LLN
approximation with \code{repl} replicates;

The hierarchie is done as follows: when argument \code{eff} is present, this is used,
otherwise, if argument \code{r} is given, this is used; and still otherwise,
the corresponding radius interval is used.

If \code{b} is given, \code{rLScalibrate} only determines the corresponding
efficiency loss \code{eff} and corresponding radius \code{r}
for which \code{b} would be SO/IO optimal.
}

\value{a list containing components clipping height \code{b},
efficiency loss \code{eff} and least favorable/resp. given radius \code{r}
for the rLS filter}
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
# by contamination radius interval
# rlow  =  0.05, rup= 0.5
(B3 <- rLScalibrateB(rlow = 0.05, rup= 0.5, S = SS, Z = Z0, V = V0i))
}

\keyword{robust}
\keyword{ts}

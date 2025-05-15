\name{robKalman-package}
\alias{robKalman-package}
\alias{robKalman}
\docType{package}
\title{
robKalman -- routines for robust Kalman filtering
}
\description{
\pkg{robKalman } provides routines for robust Kalman filtering. 
Currently, the ACM-filter and the rLS-Filter are provided.
}
\details{
\tabular{ll}{
Package: \tab robKalman\cr
Version: \tab 2.0.0 \cr
Date: \tab 2025-05-15 \cr
Depends: \tab R(>= 3.4), methods, graphics, startupmsg(>= 1.0), MASS\cr 
Imports: \tab stats\cr
LazyLoad: \tab yes\cr
License: \tab LGPL-3\cr
URL: \tab http://distr.r-forge.r-project.org/\cr
}}

\section{Setup}{
We work in the setup of the time-invariant, linear, Gaussian state space model (ti-l-G-SSM)
with \eqn{p} dimensional states \eqn{x_t} and \eqn{q} dimensional observations \eqn{y_t},
with \strong{initial condition}
\deqn{x_0 \sim {\cal N}_p(a,S),}{x_0 ~ N_p(a,S),}

\strong{state equation}
\deqn{x_t = F x_{t-1} +  v_t, \qquad v_t \sim {\cal N}_p(0,Q),\qquad t\ge 1,}{x_t = F x_{t-1} +  v_t,          v_t ~ N_p(0,Q),  t>=1,}

\strong{observation equation}
\deqn{y_t = Z x_{t}   +  \varepsilon_t, \qquad \varepsilon_t \sim {\cal N}_q(0,V),\qquad t\ge 1,}{y_t = Z x_t   +  e_t,      e_t ~ N_q(0,V),  t>=1,}
and where all random variable \eqn{x_0}, \eqn{v_t}, \eqn{\varepsilon_t}{e_t} are independent.
}

\section{Classes}{
yet to be filled}

\section{Methods}{
yet to be filled}
\section{Functions}{
\preformatted{

general recursive filters
+recursiveFilter
 -KalmanFilter 
 -rLSFilter:
    *rLS.AO.Filter
    *rLS.IO.Filter
 -ACMfilter
 -mACMfilter

ACMfilter:
+ACMfilt

GM-estimators for AR models
+arGM

utilities:
+Euclidnorm,  
+rcvmvnorm,      
+Huberize,       
+limitS

simulation of AO contaminated state space models       
+simulateState,
+simulateObs,
}}

\section{Acknowledgement}{
We thank Paul Gilbert for their help
in preparing this package.}

\author{
Peter Ruckdeschel (Maintainer) \email{peter.ruckdeschel@uni-oldenburg.de},\cr 
Bernhard Spangl \email{bernhard.spangl@boku.ac.at},\cr 
}

\references{
Martin, R.D. and Zeh, J.E. (1978): Generalized M-estimates for  Autoregression Including Small-sample Efficiency Robustness. \cr
Martin, R.D. (1979): Approximate Conditional-mean Type Smoothers and Interpolators.\cr
Martin, R.D. (1980): Robust Estimation of Autoregressive Models.  \cr
Martin, R.D. (1981): Robust Methods for Time Series.\cr
Martin, R.D. and Thomson, D.J. (1982): Robust-resistent Spectrum Estimation. \cr
Ruckdeschel, P., Spangl, B. and Pupashenko, D. (2014): Robust Kalman tracking and smoothing with 
propagating and non-propagating outliers. \emph{Statistical Papers} 55(1), 93--123. \cr
Ruckdeschel, P. (2001) \emph{Ans\"atze zur Robustifizierung des Kalman Filters.} Bayreuther Mathematische Schriften, Vol. 64. \cr
Spangl, B. (2008): On Robust Spectral Density Estimation. PhD Thesis at Technical University, Vienna. \cr
Stockinger, N. and Dutter, R. (1987): Robust Time Series Analysis: A Survey.  \cr
}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}. 

As for general \code{packageStartupMessage}'s, you may also suppress all
 the start-up banner by wrapping the \code{library} or \code{require} 
 call into \code{suppressPackageStartupMessages()} from 
 \pkg{startupmsg}-version 0.5 on.
 }

\section{Demos}{
Demos are available --- see \code{demo(package="robKalman")}}
\keyword{robust}
\keyword{ts}

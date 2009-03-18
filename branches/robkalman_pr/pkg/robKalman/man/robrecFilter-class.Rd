\name{robrecFilter-class}
\docType{class}
\alias{robrecFilter-class}
\alias{X.rob.filtered,robrecFilter-method}
\alias{X.rob.predicted,robrecFilter-method}
\alias{Cov.rob.filtered,robrecFilter-method}
\alias{Cov.rob.predicted.sim,robrecFilter-method}
\alias{Cov.rob.filtered.sim,robrecFilter-method}
\alias{Cov.rob.predicted,robrecFilter-method}
\alias{Kalman.rob.Gain,robrecFilter-method}
\alias{IndIO,robrecFilter-method}
\alias{IndAO,robrecFilter-method}
\alias{RNGstate,robrecFilter-method}
\alias{nsim,robrecFilter-method}
\alias{rob.correction.ctrl,robrecFilter-method}
\alias{rob.prediction.ctrl,robrecFilter-method}
\alias{name.rob,robrecFilter-method}
\alias{X.rob.filtered}
\alias{X.rob.predicted}
\alias{Cov.rob.filtered}
\alias{Cov.rob.predicted}
\alias{Cov.rob.filtered.sim}
\alias{Cov.rob.predicted.sim}
\alias{Kalman.rob.Gain}
\alias{rob.correction.ctrl}
\alias{rob.prediction.ctrl}
\alias{IndIO}
\alias{IndAO}
\alias{RNGstate}
\alias{nsim}
\alias{name.rob}

\title{Robust Recursive filter class}
\description{Return value class for robust recursive filters.
Actual class is \code{robrecFilter}. It comprises all information gathered
along an evaluation of a robust recursive filter.
}

\section{Objects from the Class}{
Objects could be created by calls of the form \code{new("robrecFilter", ...)},
but rather we use a generating function
 \code{\link{generateRobRecFilter}}. Effectively the user will not create
 instances of this class himself, but rather will obtain them as results
 of applications of robust recursive filters.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{character}: name of the classical filter used (slot inherited from class \code{recFilter}).}
    \item{\code{time}:}{\code{zoo}: the time stamps of the observations (inherited from class \code{recFilter}).}
    \item{\code{SSM}:}{\code{SSM}: the SSM at which the filter was evaluated (inherited from class \code{recFilter})}
    \item{\code{Y}:}{\code{matrix}: the observations from the SSM at which the filter were evaluated (inherited from class \code{recFilter})}
    \item{\code{X.filtered}:}{\code{matrix}: the series of filtered states (inherited from class \code{recFilter}); gained by the classical filter}
    \item{\code{X.predicted}:}{\code{matrix}: the series of (one-step-ahead-)predicted states (inherited from class \code{recFilter}); gained by the classical filter}
    \item{\code{Cov.filtered}:}{\code{array}: the set of filter error covariances (inherited from class \code{recFilter}); gained by the classical filter}
    \item{\code{Cov.predicted}:}{\code{array}: the set of (one-step-ahead-)prediction error covariances (inherited from class \code{recFilter}); gained by the classical filter}
    \item{\code{Kalman.Gain}:}{\code{array}: the set of Kalman-Gain matrices (inherited from class \code{recFilter}); gained by the classical filter}
    \item{\code{name.rob}:}{\code{character}: name of the robust filter used.}
    \item{\code{X.rob.filtered}:}{\code{matrix}: the series of filtered states gained by the robust filter}
    \item{\code{X.rob.predicted}:}{\code{matrix}: the series of (one-step-ahead-)predicted states gained by the robust filter}
    \item{\code{Cov.rob.filtered}:}{\code{array}: the set of filter error covariances gained by the robust filter}
    \item{\code{Cov.rob.predicted}:}{\code{array}: the set of (one-step-ahead-)prediction error covariances gained by the robust filter}
    \item{\code{Kalman.rob.Gain}:}{\code{array}: the set of Kalman-Gain matrices gained by the robust filter}
    \item{\code{rob.correction.ctrl}:}{\code{list}: object gathering control informatin for the robust prediction step}
    \item{\code{rob.prediction.ctrl}:}{\code{list}: object gathering control informatin for the robust correction step}
    \item{\code{IndIO}:}{\code{logical}: indicators to time instance where robust filter presumes an innovation outlier}
    \item{\code{IndAO}:}{\code{logical}: indicators to time instance where robust filter presumes an additive/substitutive outlier}
    \item{\code{Cov.rob.filtered.sim}:}{\code{array}: the set of filter error covariances gained by simulations by the robust filter}
    \item{\code{Cov.rob.predicted.sim}:}{\code{array}: the set of (one-step-ahead-)prediction error covariances gained by simulations by the robust filter}
    \item{\code{RNGstate}:}{\code{integer}: the state of the random number generator before evaluating the filter; useful for
                            reproducible simulative results}
    \item{\code{nsim}:}{\code{numeric}: number of simulations used to produce MonteCarlo-estimates for the error covariances
                       of the robust filters}
  }
}
\section{Extends}{
Class \code{"robrecFilter"} directly extends class \code{"recFilter"}.
}
\section{Methods}{
  \describe{
\item{name.rob}{\code{signature(object = "robrecFilter")}: access method for slot \code{name.rob}}
\item{X.rob.filtered}{\code{signature(object = "robrecFilter")}: access method for slot \code{X.rob.filtered}}
\item{X.rob.predicted}{\code{signature(object = "robrecFilter")}: access method for slot \code{X.rob.predicted}}
\item{Cov.rob.filtered}{\code{signature(object = "robrecFilter")}: access method for slot \code{Cov.rob.filtered}}
\item{Cov.rob.predicted}{\code{signature(object = "robrecFilter")}: access method for slot \code{Cov.rob.predicted}}
\item{rob.correction.ctrl}{\code{signature(object = "robrecFilter")}: access method for slot \code{rob.correction.ctrl}}
\item{rob.prediction.ctrl}{\code{signature(object = "robrecFilter")}: access method for slot \code{rob.prediction.ctrl}}
\item{Kalman.rob.Gain}{\code{signature(object = "robrecFilter")}: access method for slot \code{Kalman.rob.Gain}}
\item{IndIO}{\code{signature(object = "robrecFilter")}: access method for slot \code{IndIO}}
\item{IndAO}{\code{signature(object = "robrecFilter")}: access method for slot \code{IndAO}}
\item{Cov.rob.filtered.sim}{\code{signature(object = "robrecFilter")}: access method for slot \code{Cov.rob.filtered.sim}}
\item{Cov.rob.predicted.sim}{\code{signature(object = "robrecFilter")}: access method for slot \code{Cov.rob.predicted.sim}}
\item{RNGstate}{\code{signature(object = "robrecFilter")}: access method for slot \code{RNGstate}}
\item{nsim}{\code{signature(object = "robrecFilter")}: access method for slot \code{nsim}}
  }

Methods \code{X.rob.filtered}, \code{X.rob.predicted},
\code{Cov.rob.filtered}, \code{Cov.rob.predicted},
\code{Cov.rob.filtered.sim}, \code{Cov.rob.predicted.sim},
\code{Kalman.rob.Gain}, \code{rob.correction.ctrl}, \code{rob.prediction.ctrl},
\code{IndIO}, and \code{IndAO} (as well as the accessors to slots inherited from
class \code{recFilter}) may also be used to make a projection on a set of time points;
to this end these methods have additional, optional arguments \code{i} and \code{time}
both defaulting to \code{NULL}. If given, \code{i} has to be a valid index vector,
resp. \code{time} a valid vector of time stamps in the range of the SSM; the corresponding
accessor then returns only those projections matching the index/time stamps.}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\examples{
}
\concept{SSM}
\concept{S4 class}
\keyword{recursive filter}
\keyword{models}

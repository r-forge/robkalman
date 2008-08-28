\name{recFilter-class}
\docType{class}
\alias{recFilter-class}
\alias{name,recFilter-method}
\alias{time,recFilter-method}
\alias{SSM,recFilter-method}
\alias{Y,recFilter-method}
\alias{X.filtered,recFilter-method}
\alias{X.predicted,recFilter-method}
\alias{Cov.filtered,recFilter-method}
\alias{Cov.predicted,recFilter-method}
\alias{Kalman.Gain,recFilter-method}
\alias{time<-,recFilter-method}
\alias{name}
\alias{time}
\alias{nsim}
\alias{SSM}
\alias{Y}
\alias{RNGstate}
\alias{X.filtered}
\alias{X.predicted}
\alias{Cov.filtered}
\alias{Cov.predicted}
\alias{Kalman.Gain}

\title{Recursive filter class}
\description{Return value class for recursive filters.
Actual class is \code{recFilter}. It comprises all information gathered
along an evaluation of a recursive filter.
}

\section{Objects from the Class}{
Objects could be created by calls of the form \code{new("recFilter", ...)},
but rather we use a generating function
 \code{\link{generateRecFilter}}. Effectively the user will not create
 instances of this class himself, but rather will obtain them as results
 of applications of recursive filters.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{character}: name of the recFilter.}
    \item{\code{time}:}{\code{zoo}: the time stamps of the observations.}
    \item{\code{SSM}:}{\code{SSM}: the SSM at which the filter was evaluated}
    \item{\code{Y}:}{\code{matrix}: the observations from the SSM at which the filter were evaluated}
    \item{\code{X.filtered}:}{\code{matrix}: the series of filtered states}
    \item{\code{X.predicted}:}{\code{matrix}: the series of (one-step-ahead-)predicted states}
    \item{\code{Cov.filtered}:}{\code{array}: the set of filter error covariances}
    \item{\code{Cov.predicted}:}{\code{array}: the set of (one-step-ahead-)prediction error covariances}
    \item{\code{Kalman.Gain}:}{\code{array}: the set of Kalman-Gain matrices}
  }
}
\section{Methods}{
  \describe{
\item{name}{\code{signature(object = "recFilter")}: access method for
    slot \code{name}. }
\item{time}{\code{signature(x = "recFilter")}: access method for
    slot \code{time}. }
\item{SSM}{\code{signature(object = "recFilter")}:
           access method for slot \code{SSM}}
\item{Y}{\code{signature(object = "recFilter")}: access method for slot \code{Y}}
\item{X.filtered}{\code{signature(object = "recFilter")}: access method for slot \code{X.filtered}}
\item{X.predicted}{\code{signature(object = "recFilter")}: access method for slot \code{X.predicted}}
\item{Cov.filtered}{\code{signature(object = "recFilter")}: access method for slot \code{Cov.filtered}}
\item{Cov.predicted}{\code{signature(object = "recFilter")}: access method for slot \code{Cov.predicted}}
\item{Kalman.Gain}{\code{signature(object = "recFilter")}: access method for slot \code{Kalman.Gain}}
  }
Methods \code{SSM}, \code{Y}, \code{X.filtered}, \code{X.predicted},
\code{Cov.filtered}, \code{Cov.predicted}, and \code{Kalman.Gain} may
also be used to make a projection on a set of time points; to this end
these methods have additional, optional arguments \code{i} and \code{time}
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

\name{generateRecFilter}
\alias{generateRecFilter}

\title{Generation function for class recFilter}

\description{
This is the generating function for class \code{recFilter}
}
\usage{
generateRecFilter(name = "classical Kalman Filter",
     SSM, Y, time, Xf, Xp, S0, S1, KG)
}
\arguments{
  \item{name}{\code{character}: name of the filter}
  \item{SSM}{\code{SSM}: the SSM at which the filter is to be evaluated}
  \item{Y}{\code{matrix}: the observations at which the filter is evaluated}
  \item{time}{\code{zoo}: the time stamps of the SSM}
  \item{Xf}{\code{matrix}: the series of filtered states}
  \item{Xp}{\code{matrix}: the series of (one-step-ahead) predicted states}
  \item{S0}{\code{array}: the series of covariance matrices for the filtered states}
  \item{S1}{\code{array}: the series of covariance matrices for the (one-step-ahead) predicted states}
  \item{KG}{\code{array}: the series of Kalman-gain matrices}
}



\value{an object of class \code{recFilter}}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr
  }



\keyword{ts}
\keyword{robust}

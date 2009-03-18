\name{generateRobRecFilter}
\alias{generateRobRecFilter}

\title{Generation function for class robrecFilter}

\description{
This is the generating function for class \code{robRecFilter}
}
\usage{
generateRobRecFilter(name = "classical Kalman Filter",
     name.rob = "robust recursive Filter",
     SSM, Y, time, Xf, Xp, Xrf, Xrp, S0, S1, KG, Sr0, Sr1,
     KGr, IndIO, IndAO, rob0L, rob1L, St0s, St1s, nsim, RNGstate)
}
\arguments{
  \item{name}{\code{character}: name of the classical filter}
  \item{name.rob}{\code{character}: name of the robust filter}
  \item{SSM}{\code{SSM}: the SSM at which the filter is to be evaluated}
  \item{Y}{\code{matrix}: the observations at which the filter is evaluated}
  \item{time}{\code{zoo}: the time stamps of the SSM}
  \item{Xf}{\code{matrix}: the series of classically filtered states}
  \item{Xp}{\code{matrix}: the series of (one-step-ahead) classically predicted states}
  \item{S0}{\code{array}: the series of covariance matrices for the classically filtered states}
  \item{S1}{\code{array}: the series of covariance matrices for the classically (one-step-ahead) predicted states}
  \item{KG}{\code{array}: the series of classical Kalman-gain matrices}
  \item{Xrf}{\code{matrix}: the series of robustly filtered states}
  \item{Xrp}{\code{matrix}: the series of (one-step-ahead) robustly predicted states}
  \item{Sr0}{\code{array}: the series of covariance matrices for the robustly filtered states}
  \item{Sr1}{\code{array}: the series of covariance matrices for the robustly (one-step-ahead) predicted states}
  \item{St0s}{\code{array}: the series of covariance matrices for
                        the robustly (one-step-ahead) predicted states gained by simulations}
  \item{St1s}{\code{array}: the series of covariance matrices for the robustly filtered states
                            gained by simulations}
  \item{KGr}{\code{array}: the series of robust Kalman-gain matrices}
  \item{IndIO}{\code{logical}: the indicators to time instance where robust filter presumes an innovation outlier}
  \item{IndAO}{\code{logical}: the indicators to time instance where robust filter presumes an
                               additive / substitutive outlier}
  \item{nsim}{\code{numeric}: number of simulations used to produce simulative error covariances}
  \item{RNGstate}{\code{integer}: state of the random number generator before starting the evaluations of
  the filter}
  \item{rob0L}{\code{list}: object gathering control informatin for the robust correction step}
  \item{rob1L}{\code{list}: object gathering control informatin for the robust prediction step}
}


\value{an object of class \code{robrecFilter}}


\author{
  Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de},\cr
  }

\keyword{ts}
\keyword{robust}

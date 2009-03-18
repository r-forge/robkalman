\name{SSM-class}
\docType{class}
\alias{SSM-class}
\alias{TimeInvariantSSM-class}
\alias{name}
\alias{name,SSM-method}
\alias{name<-}
\alias{name<-,SSM-method}
\alias{time}
\alias{time,SSM-method}
\alias{time<-}
\alias{time<-,SSM-method}
\alias{getp}
\alias{getp,SSM-method}
\alias{setp<-}
\alias{setp<-,SSM-method}
\alias{getq}
\alias{getq,SSM-method}
\alias{setq<-}
\alias{setq<-,SSM-method}
\alias{getF}
\alias{getF,SSM-method}
\alias{setF<-}
\alias{setF<-,SSM-method}
\alias{getZ}
\alias{getZ,SSM-method}
\alias{setZ<-}
\alias{setZ<-,SSM-method}
\alias{getV}
\alias{getV,SSM-method}
\alias{setV<-}
\alias{setV<-,SSM-method}
\alias{getQ}
\alias{getQ,SSM-method}
\alias{setQ<-}
\alias{setQ<-,SSM-method}
\alias{geta}
\alias{geta,SSM-method}
\alias{seta<-}
\alias{seta<-,SSM-method}
\alias{getS}
\alias{getS,SSM-method}
\alias{setS<-}
\alias{setS<-,SSM-method}

\title{State Space Model classes}
\description{Classes implementing State Space Models (SSMs).
Actual classes are \code{SSM} (more general) and \code{TimeInvariantSSM}
realizing a time invariant SSM. \code{TimeInvariantSSM} is the more special
class, with the same slots as \code{SSM}, but requiring matrix form and
time invariance for the slots.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SSM", ...)},
\code{new("TimeInvariantSSM", ...)}, respectively, 
but rather you would use a generating function, e.g.
 \code{\link{TI.SSM}} in case of a time invariant SSM.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{\code{character}: name of the SSM.}  
    \item{\code{F}:}{\code{Hyperparamtype}: in a linear SSM this will be the (set
    of) transition matrix[ces] realized as an array resp. a matrix, or it may also
    be given as function taking time as argument and returning the corresponding
    matrix; this way extended Kalman filters can be realized by implementing
    the linearization of the transition equation.}
    \item{\code{Z}:}{\code{Hyperparamtype}: in a linear SSM this will be the (set
    of) observation matrix[ces] realized as an array resp. a matrix, or it may also
    be given as function taking time as argument and returning the corresponding
    matrix; this way extended Kalman filters can be realized by implementing
    the linearization of the observation equation.}
    \item{\code{Q}:}{\code{Hyperparamtype}: in a linear SSM this will be the (set
    of) innovation covariance matrix[ces] realized as an array resp. a matrix, 
    or it may also be given as function taking time as argument and returning 
    the corresponding matrix.}
    \item{\code{V}:}{\code{Hyperparamtype}: in a linear SSM this will be the (set
    of) observation error covariance matrix[ces] realized as an array resp. a 
    matrix, or it may also be given as function taking time as argument and 
    returning  the corresponding matrix.}
    \item{\code{p}:}{\code{numeric}: the dimension of the state space.}
    \item{\code{q}:}{\code{numeric}: the dimension of the observations.}
    \item{\code{a}:}{\code{numeric}: the mean value of the starting state.}
    \item{\code{S}:}{\code{Hyperparamtype}: in a linear SSM this will be the 
    covariance matrix of the starting state; it may also be given as function 
    taking time as argument and returning  the corresponding matrix.}
    \item{\code{time}:}{\code{zoo}: the time stamps of the observations.}
  }
}
\section{Extends}{
Class \code{"SSM"} is directly extended by class \code{"TimeInvariantSSM"},
which in addition requires that all slots of type \code{Hyperparamtype}
be matrices (i.e. time-invariant).
}
\section{Methods}{
  \describe{
\item{name}{\code{signature(object = "SSM")}: access method for
    slot \code{name}. }
\item{time}{\code{signature(x = "SSM")}: access method for
    slot \code{time}. }
\item{getp}{\code{signature(object = "SSM")}: access method for
    slot \code{p}. }
\item{getq}{\code{signature(object = "SSM")}: access method for
    slot \code{q}. }
\item{getF}{\code{signature(object = "SSM")}: access method for
    slot \code{F}. }
\item{getZ}{\code{signature(object = "SSM")}: access method for
    slot \code{Z}. }
\item{getQ}{\code{signature(object = "SSM")}: access method for
    slot \code{Q}. }
\item{getV}{\code{signature(object = "SSM")}: access method for
    slot \code{V}. }
\item{geta}{\code{signature(object = "SSM")}: access method for
    slot \code{a}. }
\item{getS}{\code{signature(object = "SSM")}: access method for
    slot \code{S}. }
\item{name<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{name}. }
\item{time<-}{\code{signature(x = "SSM")}: replace method for
    slot \code{time}. }
\item{setp<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{p}. }
\item{setq<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{q}. }
\item{setF<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{F}. }
\item{setZ<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{Z}. }
\item{setV<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{Q}. }
\item{setQ<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{V}. }
\item{seta<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{a}. }
\item{setS<-}{\code{signature(object = "SSM")}: replace method for
    slot \code{S}. }
  }
}
%\references{}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\examples{
mySSM <- TI.SSM(F = matrix(c(2,-1,0.2,1),2,2),
                Z = t(c(2,1)),
                Q = matrix(c(2,1,1,1),2,2),
                V = matrix(2,1,1), 
                Tn = 10)
}
\concept{SSM}
\concept{S4 class}
\keyword{models}

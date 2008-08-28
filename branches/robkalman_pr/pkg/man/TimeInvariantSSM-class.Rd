\name{SSM-class}
\docType{class}
\alias{SSM-class}
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

\title{State Space Model class}
\description{The class of a State Space Model (SSM).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SSM", ...)},
but rather you would use a generating function, e.g.
 \code{\link{TI.SSM}} in case of a time invariant SSM.
}
\section{Slots}{
  \describe{
    \item{\code{loc}:}{ real number: location parameter of
      a Gumbel distribution. }
    \item{\code{scale}:}{ positive real number: scale
      parameter of a Gumbel distribution. }
    \item{\code{name}:}{ default name is
      \dQuote{parameter of a Gumbel distribution}. }
  }
}
\section{Extends}{
Class \code{"Parameter"}, directly.\cr
Class \code{"OptionalParameter"}, by class \code{"Parameter"}.
}
\section{Methods}{
  \describe{
    \item{loc}{\code{signature(object = "GumbelParameter")}: access method for
      slot \code{loc}. }
    \item{scale}{\code{signature(x = "GumbelParameter")}: access method for
      slot \code{scale}. }
    \item{loc<-}{\code{signature(object = "GumbelParameter")}: replace method for
      slot \code{loc}. }
    \item{scale<-}{\code{signature(x = "GumbelParameter")}: replace method for
      slot \code{scale}. }
  }
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{Gumbel-class}}, \code{\link[distr]{Parameter-class}}}
\examples{
new("GumbelParameter")
}
\concept{Gumbel distribution}
\keyword{distribution}
\concept{parameter}
\concept{S4 parameter class}
\keyword{models}
\concept{generating function}
\concept{scale}
\concept{location}
\concept{location scale model}
